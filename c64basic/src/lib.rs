//! c64basic provides support for decoding c64 basic programs.
use color_eyre::eyre::{eyre, Result};
use rusty6502::prelude::*;
use std::collections::HashMap;
use std::fmt::{self, Write};
use std::num::Wrapping;
use std::str;
use std::sync::OnceLock;
use strum_macros::{Display, EnumIter, EnumString};

#[cfg(test)]
mod tests;

/// `BASIC_LOAD_ADDR` is the memory location where c64 basic programs
/// are loaded by default. i.e. load "foo",8
pub const BASIC_LOAD_ADDR: u16 = 0x0801;

fn read_addr(r: &impl Memory, pc: u16) -> u16 {
    let low = r.read(pc);
    let high = r.read(pc + 1);
    (u16::from(high) << 8) + u16::from(low)
}

#[derive(Debug, Clone)]
/// `ParseError` indicates a syntax error parsing the Basic input.
pub struct ParseError {
    output_string: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "?SYNTAX ERROR: {}", self.output_string)
    }
}

#[derive(Copy, Clone, Display, Debug, EnumString, EnumIter)]
/// All of the c64 basic keywords. Where the actual keyword can't be expressed directly
/// as an enum use `to_string` from strum to make it usable with `FromStr`, etc.
/// All keyword documentation below comes from the [c64 wiki](https://www.c64-wiki.com/wiki/BASIC_token)
pub enum Keyword {
    /// END ends the processing of the current program which lets interpreter switch
    /// back to direct mode, printing the prompt READY, and waiting for input.
    ///
    /// END
    ///
    /// Abbr: eN
    END,

    /// FOR is the start command of a FOR…TO…STEP…NEXT loop. This FOR...NEXT loop
    /// is executed until the counter variable equals the value in the TO clause.
    /// With <stepsize>, the counter variable value could be either increased if positive,
    /// greater than zero, decreased if negative or remain unchanged if 0. When the
    /// STEP command isn't used at all the step size defaults to 1.
    ///
    /// FOR <Counter-Variable>=<startvalue> TO <endvalue> [ STEP <stepsize>]
    ///
    /// Abbr: fO
    FOR,

    /// NEXT is used with the BASIC command FOR. NEXT denotes the "end" of a
    /// FOR…TO…STEP…NEXT loop construction and is usually paired with a corresponding
    /// FOR statement.
    ///
    /// NEXT [[<Variable>[,<Variable>…]]
    ///
    /// Abbr: nE
    NEXT,

    /// DATA is used to store constant information in the program code, and is used with
    /// the BASIC-command READ. Each DATA-line can contain one or more constants separated
    /// by commas. Expressions containing variables etc. will not be evaluated here.
    ///
    /// DATA <constant> [,<constant>]...
    ///
    /// Abbr: dA
    DATA,

    /// INPUT# is used for reading data from a file stored on peripheral device media such
    /// as disk or tape. INPUT# reads complete data consisting of maximum 80 characters into
    /// variables and not only single characters as the GET# command.
    ///
    /// INPUT# <logic file number>,<variable>[,<variable>...]
    ///
    /// Abbr: iN
    #[strum(to_string = "INPUT#")]
    INPUTIO,

    #[allow(clippy::doc_link_with_quotes)]
    /// INPUT is used to read data from the keyboard into one or more supplied variables.
    /// The INPUT command prints the optional text followed by a question mark (?) and
    /// then activates the screen editor for user input.
    ///
    /// INPUT ["<text/string>";]<Variable>[,<Variable>...]
    ///
    /// Abbr: N/A
    INPUT,

    /// DIM allocates space in array memory for a new array, with one dimension for each
    /// of the dimension sizes d1, d2, d3, etc. given in the DIM statement.
    ///
    /// DIM <Variable>(<d1>[,<d2>[,<d3>...]])[, <Variable>(<d1>[,<d2>[,<d3>...]])...]
    ///
    /// Abbr: dI
    DIM,

    /// READ is used for reading constant values from DATA lines into the indicated variables.
    /// This command is able to read more constants at once with a variable list separated by commas.
    ///
    /// READ <variable> [,<variable]...
    ///
    /// Abbr: rE
    READ,

    /// LET is used to assign numerical values or chars to the right type of variable. Assigning
    /// to the wrong type will result in a ?TYPE MISMATCH ERROR.
    /// NOTE: This is not required for assigning/declaring variables so is not often used.
    ///
    /// LET <variable> = <argument>
    ///
    /// Abbr: lE
    LET,

    /// GOTO makes the BASIC interpreter branch to the indicated line and the execution of the
    /// BASIC program is continued at that line. Executing GOTO without an argument implies GOTO 0.
    ///
    /// GOTO [<line>]
    ///
    /// Abbre: gO
    GOTO,

    /// RUN starts a BASIC program. When no line number is given, the program will be started
    /// at the first line number.
    ///
    /// RUN [<line>]
    ///
    /// Abbr: rU
    RUN,

    /// IF is used to test a "condition". If the condition produces a non-zero value,
    /// the statements after the THEN or GOTO are performed. When the condition is false
    /// the BASIC interpreter will ignore the rest of the BASIC commands in the line.
    ///
    /// IF <equation> THEN <linenumber>
    /// or IF <equation> GOTO <linenumber>
    /// or IF <equation> THEN <command>
    ///
    /// Abbr: N/A
    IF,

    /// RESTORE is used to clear the pointer of the next DATA value. The next read data
    /// value will be the first DATA value.
    ///
    /// RESTORE
    ///
    /// Abbr: rE
    RESTORE,

    /// GOSUB jumps to a subroutine at the indicated line number. This call is placed onto
    /// the stack. The subroutine finalizes using a RETURN command. Program execution continues
    /// at the command following the initial GOSUB command.
    ///
    /// GOSUB <line>
    ///
    /// Abbr: goS
    GOSUB,

    /// RETURN finishes a subroutine, which is called by the BASIC command GOSUB. After
    /// leaving the subroutine the program continues right after the calling GOSUB statement.
    ///
    /// RETURN
    ///
    /// Abbr: reT
    RETURN,

    /// REM is used to place remarks into BASIC programs. The BASIC interpreter ignores all
    /// following text until the end of the line (even if it contains BASIC commands)
    ///
    /// REM [<text>]
    ///
    /// Abbr: N/A
    REM,

    /// STOP breaks a program and it prints the BASIC-Message BREAK IN Line with READY..
    /// The breaked BASIC-Program can resume without deleting the stack with CONT or
    /// GOTO [<Linenumber>].
    /// All opened files and variables won't be lost, because this command has no effect
    /// on the stack.
    ///
    /// STOP
    ///
    /// Abbr: sT
    STOP,

    /// ON is part of a structure which jumps to a specific line in the given list of
    /// BASIC <line> numbers, either as an unconditional GOTO or as a subroutine call
    /// through GOSUB. If <index> (which must be either a floating point or integer
    /// expression) equals 1, the first <line> on the list is taken. If <index> equates
    /// to 2, the second line number is taken, etc. If the numerical <index> expression
    /// evaluates to a non-integer result, ON will round it down to the nearest lower
    /// integer, as if the INT function had been used on the result; e.g. an <index> of 2.2
    /// resolves to 2, and so the second line number on the list is taken.
    ///
    /// ON <index> GOSUB|GOTO <line>[,<line>...]
    ///
    /// Abbr: N/A
    ON,

    /// WAIT waits for a given memory location to match specific bit constellations.
    /// The parameter values are specified as a bit mask, so `WAIT`ing on a value of 32
    /// will cause the program to continue when bit 5 changes to 1. `WAIT`ing on 3 will
    /// cause it to continue when either bit 1 or 2 is set. The optional 3rd parameter
    /// enables a check for unset bits by inverting the locations bits.
    ///
    /// WAIT <address>,<and-mask>[,<flip-mask>]
    ///
    /// Abbr: wA
    WAIT,

    /// LOAD is normally used for loading program files (PRG) like BASIC, machine
    /// language programs, but actually any kind of data from datasette or disk
    /// drive into RAM.
    ///
    /// LOAD ["<filename>" [,<device number> [,<secondary number>]]]
    ///
    /// Abbr: lO
    LOAD,

    #[allow(clippy::doc_link_with_quotes)]
    /// SAVE permanently saves to a storage device such as a datasette, diskdrive
    /// or harddisk as a filetype of program (PRG). The Command SAVE can also be
    /// used in BASIC programs.
    ///
    /// SAVE ["<filename>"] [,<device number>] [,<secondary number>]
    ///
    /// Abbr: sA
    SAVE,

    #[allow(clippy::doc_link_with_quotes)]
    /// VERIFY is used for verifying files, which were written with the
    /// BASIC-Command SAVE on a storage medium, with the data from memory (RAM).
    /// This command can used directly or in programs.
    ///
    /// VERIFY ["<filename>"] [,<device number>]
    ///
    /// Abbr: vE
    VERIFY,

    /// DEF defines a function with exactly one single numeric argument which can
    /// be executed with FN afterwards. The definition may contain any legitimate
    /// mathematical expression consisting of mathematical and logical operands,
    /// functions and variables which finally results in a numeric value. Functions,
    /// operands and system variables such as `ABS`(), `AND`, `ATN`(), `ASC`(), `COS`(), `EXP`(),
    /// `FN`<function name>(), `FRE`(), `INT`(), `LEN`() `LOG`(), `NOT`, `PEEK`(), `POS`(), `OR`, `RND`(),
    /// `SGN`(), `SIN`(), `SQR`(), STATUS (`ST`), `TAN`(), TIME (`TI`) or `VAL`() are possible.
    ///
    /// DEF FN <function name>(parameter name)=<mathmatical expression>
    ///
    /// Abbr: dE
    DEF,

    /// POKE changes the content of any address in the memory address, ranging from
    /// 0 to 65535, to the given byte value in the range 0 through 255.
    ///
    /// POKE <memory address>,<number>
    ///
    /// Abbr: pO
    POKE,

    /// PRINT# is used for storing data in a file. Before PRINT# can be used, the destination
    /// file has to be opened with the BASIC command OPEN. The PRINT# command must
    /// address the same logical file number as the preceding OPEN command.
    ///
    /// PRINT# <logical file number>,<variable>[, (<--alternative ;)<variable>...]
    ///
    /// Abbr: pR
    #[strum(to_string = "PRINT#")]
    PRINTIO,

    /// PRINT is used to print data to the current output device, normally the screen.
    /// The argument list might consist of string or numerical expressions concatenated
    /// by an optional separator. Such expressions could include variables (strings,
    /// integers or floating point) and any allowed mathematical term or known BASIC function.
    /// Especially when printing graphic and control characters or any string "as is"
    /// they must be enclosed with double quotes (" ") as usual for string constants.
    ///
    /// PRINT [<expression>] [[;|,]<expression>...]
    ///
    /// Abbr: ?
    PRINT,

    /// CONT is used to resume execution of a BASIC program, which either ended with an END
    /// or a STOP command.
    ///
    /// CONT
    ///
    /// Abbr: cO
    CONT,

    /// LIST displays the BASIC program currently in memory. By default this listing is sent
    /// to the screen, but it can also be sent to e.g. a printer using CMD.
    ///
    /// LIST [[first line]-[last line]]
    ///
    /// Abbr: lI
    LIST,

    /// CLR deletes all variables, arrays, data read position from DATA lines, defined
    /// functions (DEF FN), return addresses of subroutines (GOSUB) and the state of
    /// loops (FOR and NEXT) by releasing the space in BASIC RAM and on the stack previously
    /// allocated for those structures. Furthermore the state of open files (OPEN) and possibly
    /// buffered data for devices like disk or catasette of the associated files gets lost
    /// because the data is freed.
    ///
    /// CLR
    ///
    /// Abbr: cL
    CLR,

    #[allow(clippy::doc_link_with_quotes)]
    /// CMD changes the data output from the screen to another peripheral device like casette,
    /// modem, printer or disk drive. CMD can be used directly or in programs. The logical
    /// filenumber is in the range 1-255 and must be selected with the BASIC command OPEN.
    ///
    /// CMD <logical filenumber>[, [<expression>][[";" | ","]<expression>...]]
    ///
    /// Addr: cM
    CMD,

    /// SYS is a command in Commodore BASIC V2, that tells the processor to execute the machine
    /// language subroutine at a specific address.
    ///
    /// SYS <address>
    ///
    /// Abbr: sY
    SYS,

    /// PEN is used for opening a logical file, or a "channel" to a peripheral device, like a
    /// printer, cassette or disk drive, and for data input and output operations from external devices.
    ///
    /// OPEN <logical filenumber> [,<device number> [,<secondary number> [,"<filename>[,<type>[,<mode>]]"]]]
    ///
    /// Abbr: oP
    OPEN,

    /// CLOSE is used for closing currently opened files or drive numbers, so that the data can
    /// be written from a write buffer to the actual device like tape or disk.
    ///
    /// CLOSE <logical file number>
    ///
    /// Abbr: clO
    CLOSE,

    /// GET reads one or more chars from the keyboard cache into a variable (string,
    /// floating point or integer) and can only be used in a BASIC program.
    ///
    /// GET <variable>[,<variable>...]
    ///
    /// Abbr: gE
    GET,

    /// NEW releases all of the BASIC RAM and the stack of the C64.
    ///
    /// NEW
    ///
    /// Abbr: N/A
    NEW,

    /// TAB sets the cursor position to the logical screen column for use in BASIC
    /// output commands like PRINT.
    ///
    /// TAB(<numeric>)
    ///
    /// Abbr: tA
    TAB,

    /// TO is used in two contexts:
    ///
    /// As part of a FOR-NEXT loop.
    /// As part of the "GO TO" construct: This does exactly the same as GOTO, but the
    /// "split" into the words "GO" and "TO" has been implemented, seemingly to
    /// maintain compatibility with other (earlier?) "flavors" of BASIC.
    ///
    /// TO
    ///
    /// Abbr: N/A
    TO,

    /// FN executes a function which has been defined previously in the course of the
    /// program with the BASIC-command DEF.
    ///
    /// FN <function name>(numeric argument)
    ///
    /// Abbr: N/A
    FN,

    /// SPC can be used to set a number of spaces, which are either written into a file
    /// or printed onto screen. SPC can be used several times in a sentence.
    ///
    /// SPC(<numeric>)
    ///
    /// Abbr: sP
    SPC,

    /// THEN is used in contexts with a IF...THEN... construct.
    ///
    /// THEN
    ///
    /// Abbr: tH
    THEN,

    /// NOT reverse the boolean "true" into "false". This can be used in IF/THEN
    /// construct.
    ///
    /// NOT <boolean/integer term>
    ///
    /// Abbr: nO
    NOT,

    /// STEP is used in contexts of a FOR...TO...STEP...NEXT loop construction.
    /// This keyword has to be followed by a numerical expression which is
    /// evaluated only once and will be stored for later usage in stepping
    /// through the FOR loop variable.
    ///
    /// STEP <numeric>
    ///
    /// Abbr: stE
    STEP,

    /// PLUS is the addition operator.
    ///
    /// A + B
    ///
    /// Abbr: N/A
    #[strum(to_string = "+")]
    PLUS,

    /// MINUS is the subtraction operator.
    ///
    /// A - B
    ///
    /// Abbr: N/A
    #[strum(to_string = "-")]
    MINUS,

    /// MULTIPLY is the multiplication operator.
    ///
    /// A * B
    ///
    /// Abbr: N/A
    #[strum(to_string = "*")]
    MULTIPLY,

    /// DIVIDE is the division operator.
    ///
    /// A / B
    ///
    /// Abbr: N/A
    #[strum(to_string = "/")]
    DIVIDE,

    /// POWER is the power operator.
    ///
    /// 2 ^ 4
    ///
    /// Would raise 2 to the 4th power (or 16)
    ///
    /// Abbr: N/A
    #[strum(to_string = "^")]
    POWER,

    /// AND performs two different functions:
    ///
    /// In boolean expressions (such as the condition in an IF/THEN construct),
    /// AND yields the boolean value TRUE (numerical: −1) if, and only if, both
    /// of the terms preceding and trailing the AND keyword, evaluates to TRUE.
    ///
    /// In bitwise operations, a bit in the resulting integer is set if, and only
    /// if, the corresponding bits from both integer terms, are set.
    ///
    /// <Boolean/integer term> AND <Boolean/integer term>
    ///
    /// Abbr: aN
    AND,

    /// OR performs two different functions:
    ///
    /// In boolean expressions (such as the condition in an IF/THEN construct),
    /// OR yields the boolean value "true" (numerical: −1) if either or both of
    /// the terms preceding and trailing the OR keyword, evaluates to TRUE.
    ///
    /// In bitwise operations, a bit in the resulting integer is set if either
    /// of both the corresponding bits in the integer terms, are set.
    ///
    /// <Boolean/integer term> OR <Boolean/integer term>
    ///
    /// Abbr: N/A
    OR,

    /// GREATERTHAN is the greater than operator.
    ///
    /// 3 > 2
    ///
    /// Abbr: N/A
    #[strum(to_string = ">")]
    GREATERTHAN,

    /// EQUAL is the equals operator.
    ///
    /// A = 2
    ///
    /// Abbr: N/A
    #[strum(to_string = "=")]
    EQUAL,

    /// LESSTHAN is the less than operator.
    ///
    /// 2 < 3
    ///
    /// Abbr: N/A
    #[strum(to_string = "<")]
    LESSTHAN,

    /// SGN gives the algebraic sign (-1; 0; 1) of a numerical argument.
    ///
    /// values with negative prefix: -1
    /// zero: 0
    /// values with positive prefix: 1
    ///
    /// SGN(<numeric>)
    ///
    /// Abbr: sG
    SGN,

    /// INT is used to round numbers, whereas rounding is different from its common
    /// mathematical definition. For positive numbers the fractional part will be cut,
    /// while for negative numbers the next lower integer value is returned.
    /// By adding 0.5 to the argument the conventional mathemathical rounding can be implemented.
    ///
    /// INT(<numeric>)
    ///
    /// Abbr: N/A
    INT,

    /// ABS evaluates to the absolute value (value without the sign) of the given numeric term.
    ///
    /// ABS(<numeric>)
    ///
    /// Abbr: aB
    ABS,

    /// USR is a user-defined function in the built-in BASIC interpreter: It accepts any valid
    /// BASIC expression (numeric or string) as a parameter, calls a user-defined machine
    /// language routine, and returns a resulting real number.
    ///
    /// USR(<numeric>)
    ///
    /// Abbr: uS
    USR,

    /// FRE accepts any numerical argument within the limits of the floating point format or
    /// any string argument, and returns the number of unused bytes of BASIC RAM.
    ///
    /// FRE(<dummy arg>)
    ///
    /// Abbr: fR
    FRE,

    /// POS determined the actual position of the cursor between 0 (first, leftmost column)
    /// and 79 (last, rightmost column in the second line) of the logical line on screen.
    /// The dummy argument is mandatory, but it does not have any effect on the result.
    /// Usually POS(0) will be used. The fastest possible execution time provides the form
    /// POS(π).
    ///
    /// POS(<dummy arg>)
    ///
    /// Abbr: N/A
    POS,

    /// SQR is a mathemathical function for square root of a number.
    ///
    /// SQR(<numeric>)
    ///
    /// Abbr: sQ
    SQR,

    /// RND generates random floating point numbers in the range of 0.0 (inclusive) to
    /// 1.0 (exclusive). The argument <number> can be positive, negative or zero.
    ///
    /// * RND(<positive number>) gives a different random number each time from a
    ///   predetermined sequence (the sequence number is stored internally).
    /// * RND(<negative number>) jumps to a point in the sequence determined by the
    ///   particular negative number used. Repeatedly calling RND with the same negative
    ///   number results in the same result; typical use is to call RND(<negative number>)
    ///   once and then repeatedly call RND(<positive number>).
    /// * RND(0) on the C64 generates the random number from the internal clock, though
    ///   the possible values are limited as the digits of the clock only fall in the
    ///   range 0-60, so it is not suitable for generating large ranges.
    ///
    /// RND(<number>)
    ///
    /// Abbr: rN
    RND,

    /// LOG is a natural logarithm with the basis e(E).
    ///
    /// LOG(<numeric>)
    ///
    /// Abbr: N/A
    LOG,

    /// EXP is a mathematical function that evaluates the inverse natural LOG of the
    /// argument. That is, the argument X is applied as an exponent to e(2.71828183).
    /// So EXP(X) = e^X
    ///
    /// EXP(<Number>)
    ///
    /// Abbr: eX
    EXP,

    /// COS is a mathematical function which evaluates to the cosine for a given angle,
    /// a number considered to be in radians.
    ///
    /// COS(<numeric>)
    ///
    /// Abbr: N/A
    COS,

    /// SIN is a mathematical function which evaluates to the sine for a given angle,
    /// a number regarded as being in radians.
    ///
    /// SIN(<numeric>)
    ///
    /// Abbr: sI
    SIN,

    /// TAN is a mathematical function which evaluates to the tangent for a given angle,
    /// a number regarded as being in radians.
    ///
    /// TAN(<numeric>)
    ///
    /// Abbr: N/A
    TAN,

    /// ATN is a mathematical function that returns the arc tangent of a numeric value
    /// (the inverse function of TAN). The resulting value is the angle in radians of
    /// the given tangent.
    ///
    /// ATN(<numeric>)
    ///
    /// Abbr: aT
    ATN,

    /// PEEK returns the memory contents of the specified address, which must be in
    /// the range 0 through 65535. The byte value returned will be in the range from
    /// 0 thru 255.
    ///
    /// PEEK(<address>)
    ///
    /// Abbr: pE
    PEEK,

    /// LEN returns the number of characters in a string. This value is always in
    /// the range from 0 to 255.
    ///
    /// LEN(<string>)
    ///
    /// Abbr: N/A
    LEN,

    /// STR$ is used to converting numerical values or variables into a string. When
    /// the number is positive or 0, the first char is a space. When it is a
    /// negative number, the first char is a minus (-).
    ///
    /// STR$(<numeric>)
    ///
    /// Abbr: stR
    #[strum(to_string = "STR$")]
    STR,

    /// VAL finds a numerical value in a string. Space chars and the first minus- (-)
    /// and plus-char (+) are ignored. The first minus char is interpreted as a negative
    /// prefix. Searching over the value stops when the first non-numerical char is reached.
    /// The first dot (.) is interpreted as a decimal point and the first e or E as
    /// an exponent. Mathematical terms and arithmetic operation are ignored.
    ///
    /// VAL(<string>)
    ///
    /// Abbr: vA
    VAL,

    /// ASC keyword takes the first char of a string and maps it to the numeric index
    /// of the Commodore ASCII-table. This value can be converted back to the character
    /// by using the CHR$ function.
    ///
    /// ASC(<string>)
    ///
    /// Abbr: aS
    ASC,

    /// CHR$ is used to convert a number between 0 and 255 into an ASCII character
    /// (of string type) and is the inverse function of ASC.
    ///
    /// CHR$(<numeric>)
    ///
    /// Abbr: cH
    #[strum(to_string = "CHR$")]
    CHR,

    /// LEFT$ is used for cutting strings into component parts beginning at the left side
    /// up to the indicated integer number which is in the range 0-255. When the value is
    /// 0, the outcome is a empty string (for example A$=""). When the number is greater
    /// than the string, nothing will be cut.
    ///
    /// LEFT$(<string>,<number>)
    ///
    /// Abbr: leF
    #[strum(to_string = "LEFT$")]
    LEFT,

    /// RIGHT$ is used for cutting strings into component parts beginning at the right side
    /// up to the indicated integer number which is in the range 0-255. When the value is
    /// 0, the outcome is a empty string (for example A$=""). When the number is greater
    /// than the string, nothing will be cut.
    ///
    /// RIGHT$(<string>,<number>)
    ///
    /// Abbr: rI
    #[strum(to_string = "RIGHT$")]
    RIGHT,

    /// MID$ is used for cutting strings into component parts inside strings beginning
    /// by the startchar up to the optional length number from the left side to the
    /// right side. The ranges must be 0-255.
    ///
    /// MID$(<string>, <start>[, <length>])
    ///
    /// Abbr: mI
    #[strum(to_string = "MID$")]
    MID,

    /// GO keyword does not make a complete statement by itself. It pairs with the TO
    /// keyword to form a synonym for GOTO, so you can type GO TO with a space.
    ///
    /// GO
    ///
    /// Abbr: N/A
    GO,
}

/// list will take the given PC value and disassembles the Basic line at that location
/// returning a string for the line and the PC of the next line. This does no sanity
/// checking so a basic program which points to itself for listing will infinite loop
/// if the PC values passed in aren't compared for loops.
/// On a normal program end (next addr == 0x0000) it will return an empty string and PC of 0x0000.
/// If there is a token parsing problem an error is returned instead with as much of the
/// line as would tokenize. Normally a c64 won't continue so the `newPC` value here will be 0.
/// NOTE: This returns the ASCII characters as parsed, displaying in PETSCII is up to the caller
///      to determine.
///
/// # Errors
///
/// Can return errors for invalid tokens (not all u8 values are valid tokens)
pub fn list(pc: u16, r: &impl Memory) -> Result<(String, u16)> {
    let new_pc = read_addr(r, pc);
    let mut working_pc = (Wrapping(pc) + Wrapping(2)).0;

    // Return an empty string and PC = 0x0000 for end of program.
    if new_pc == 0x0000 {
        return Ok((String::new(), 0x0000));
    }

    // Next 2 are line number also stored in little endian so we can just use readAddr again.
    let line_num = read_addr(r, working_pc);
    working_pc += 2;

    let mut output_string = String::new();

    // Emit the line number
    write!(output_string, "{line_num} ").unwrap();

    // a 1 byte array to stick tok into when we need to stringify it below.
    let mut b: [u8; 1] = [0; 1];

    // Read until we reach a NUL indicating EOL.
    loop {
        let tok = r.read(working_pc);
        working_pc += 1;
        let tmp: String;

        let emit = match tok {
            0x00 => break,
            0x01..=ASCII_END => {
                b[0] = tok;
                str::from_utf8(&b)?
            }
            // We know the range of map keys.
            KEYWORD_START..=KEYWORD_HIGH => {
                tmp = keywords()
                    .get(&tok)
                    .ok_or(eyre!("Internal error: invalid keyword"))?
                    .to_string();
                tmp.as_str()
            }
            KEYWORD_INVALID_LOW..=KEYWORD_INVALID_HIGH => {
                return Err(eyre!(ParseError { output_string }));
            }
        };
        write!(output_string, "{emit}").unwrap();
    }
    Ok((output_string, working_pc))
}

const ASCII_END: u8 = 0x7F;
const KEYWORD_START: u8 = 0x80;
const KEYWORD_HIGH: u8 = 0xCB;
const KEYWORD_INVALID_LOW: u8 = KEYWORD_HIGH + 1;
const KEYWORD_INVALID_HIGH: u8 = 0xFF;

fn keywords() -> &'static HashMap<u8, Keyword> {
    static KEYWORDS: OnceLock<HashMap<u8, Keyword>> = OnceLock::new();
    KEYWORDS.get_or_init(|| {
        HashMap::from([
            (0x80, Keyword::END),
            (0x81, Keyword::FOR),
            (0x82, Keyword::NEXT),
            (0x83, Keyword::DATA),
            (0x84, Keyword::INPUTIO),
            (0x85, Keyword::INPUT),
            (0x86, Keyword::DIM),
            (0x87, Keyword::READ),
            (0x88, Keyword::LET),
            (0x89, Keyword::GOTO),
            (0x8A, Keyword::RUN),
            (0x8B, Keyword::IF),
            (0x8C, Keyword::RESTORE),
            (0x8D, Keyword::GOSUB),
            (0x8E, Keyword::RETURN),
            (0x8F, Keyword::REM),
            (0x90, Keyword::STOP),
            (0x91, Keyword::ON),
            (0x92, Keyword::WAIT),
            (0x93, Keyword::LOAD),
            (0x94, Keyword::SAVE),
            (0x95, Keyword::VERIFY),
            (0x96, Keyword::DEF),
            (0x97, Keyword::POKE),
            (0x98, Keyword::PRINTIO),
            (0x99, Keyword::PRINT),
            (0x9A, Keyword::CONT),
            (0x9B, Keyword::LIST),
            (0x9C, Keyword::CLR),
            (0x9D, Keyword::CMD),
            (0x9E, Keyword::SYS),
            (0x9F, Keyword::OPEN),
            (0xA0, Keyword::CLOSE),
            (0xA1, Keyword::GET),
            (0xA2, Keyword::NEW),
            (0xA3, Keyword::TAB),
            (0xA4, Keyword::TO),
            (0xA5, Keyword::FN),
            (0xA6, Keyword::SPC),
            (0xA7, Keyword::THEN),
            (0xA8, Keyword::NOT),
            (0xA9, Keyword::STEP),
            (0xAA, Keyword::PLUS),
            (0xAB, Keyword::MINUS),
            (0xAC, Keyword::MULTIPLY),
            (0xAD, Keyword::DIVIDE),
            (0xAE, Keyword::POWER),
            (0xAF, Keyword::AND),
            (0xB0, Keyword::OR),
            (0xB1, Keyword::GREATERTHAN),
            (0xB2, Keyword::EQUAL),
            (0xB3, Keyword::LESSTHAN),
            (0xB4, Keyword::SGN),
            (0xB5, Keyword::INT),
            (0xB6, Keyword::ABS),
            (0xB7, Keyword::USR),
            (0xB8, Keyword::FRE),
            (0xB9, Keyword::POS),
            (0xBA, Keyword::SQR),
            (0xBB, Keyword::RND),
            (0xBC, Keyword::LOG),
            (0xBD, Keyword::EXP),
            (0xBE, Keyword::COS),
            (0xBF, Keyword::SIN),
            (0xC0, Keyword::TAN),
            (0xC1, Keyword::ATN),
            (0xC2, Keyword::PEEK),
            (0xC3, Keyword::LEN),
            (0xC4, Keyword::STR),
            (0xC5, Keyword::VAL),
            (0xC6, Keyword::ASC),
            (0xC7, Keyword::CHR),
            (0xC8, Keyword::LEFT),
            (0xC9, Keyword::RIGHT),
            (0xCA, Keyword::MID),
            (0xCB, Keyword::GO),
        ])
    })
}
