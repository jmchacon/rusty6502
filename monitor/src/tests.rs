// Setup a stdin/stdout command pair
// Fire up things like tui does it.
//
// Send commands, validate outputs
//
// Separate test: See if we can close channels to trigger some failure modes
use crate::{cpu_loop, input_loop, Output, StopReason};
use color_eyre::eyre::{Report, Result};
use ntest::timeout;
use rusty6502::prelude::*;
use std::fs::read;
use std::path::Path;
use std::thread::JoinHandle;
use std::{sync::mpsc::channel, sync::mpsc::Receiver, sync::mpsc::Sender, thread};
use tempfile::tempdir;

#[allow(clippy::type_complexity)]
fn setup(
    cpu: CPUType,
    preload: bool,
) -> Result<(
    Sender<String>,
    Receiver<Output>,
    JoinHandle<Result<(), Report>>,
)> {
    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // Pass through channels for cpucommand so we can log for tests.
    let (passcpucommandtx, passcpucommandrx) = channel();
    let passcpu = thread::Builder::new().name("pass through".into());
    passcpu.spawn(move || -> Result<()> {
        loop {
            let resp = passcpucommandrx.recv()?;
            println!("Recevied Command: {resp:?}");
            cpucommandtx.send(resp)?;
        }
    })?;

    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel();
    // Pass through channels for cpucommandresp so we can log for tests.
    let (passcpucommandresptx, passcpucommandresprx) = channel();

    let passcpuresp = thread::Builder::new().name("pass through".into());
    passcpuresp.spawn(move || -> Result<()> {
        loop {
            let resp = passcpucommandresprx.recv()?;
            println!("Recevied Response: {resp:?}");
            cpucommandresptx.send(resp)?;
        }
    })?;

    let cl = thread::Builder::new().name("cpu_loop".into());
    cl.spawn(move || cpu_loop(cpu, &cpucommandrx, &passcpucommandresptx))?;

    let (inputtx, inputrx) = channel();
    let (outputtx, outputrx) = channel();

    if preload {
        // Send a load down for a ROM we know works. Do this before starting the
        // main loop so we can attest preload.
        let path =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/6502_functional_test.bin");
        inputtx.send(format!("L {} 0x0000 0x0400", path.to_string_lossy()))?;
    }

    let il = thread::Builder::new().name("input_loop".into());
    let ilh = il.spawn(move || -> Result<()> {
        input_loop(
            &passcpucommandtx,
            &cpucommandresprx,
            &inputrx,
            &outputtx,
            preload,
        )
    })?;
    Ok((inputtx, outputrx, ilh))
}

macro_rules! basic_startup_quit_test {
    ($suite:ident, $($name:ident: $cpu:expr,)*) => {
        mod $suite {
            use super::*;

            $(
                #[test]
                #[timeout(60000)]
                fn $name() -> Result<()> {
                    let (inputtx, outputrx, ilh) = setup($cpu, false)?;
                    // Should go immediately to a prompt since we didn't preload
                    let resp = outputrx.recv()?;
                    if let Output::Prompt(_) = resp {
                    } else {
                        panic!("Didn't get prompt after startup load? - {resp:?}");
                    }

                    // Send a RESET down since it shouldn't be initialized
                    inputtx.send("RESET".into())?;
                    let resp = outputrx.recv()?;
                    if let Output::CPU(cpu, _) = resp {
                        assert!(
                            cpu.state.pc == 0x0000,
                            "PC not correct. Expected 0x0000 and got {cpu}"
                        );
                    } else {
                        panic!("Didn't get CPUState after reset? - {resp:?}");
                    }
                    let resp = outputrx.recv()?;
                    if let Output::Prompt(_) = resp {
                    } else {
                        panic!("Didn't get prompt after reset? - {resp:?}");
                    }

                    // Send a QUIT down and validate the thread returns.
                    inputtx.send("Q".into())?;
                    let ret = ilh.join();
                    assert!(
                        ret.is_ok(),
                        "Got error from input loop thread after quit - {ret:?}"
                    );
                    Ok(())
                }
            )*
        }
    }
}

basic_startup_quit_test!(
  basic_startup_quit_tests,
  nmos: CPUType::NMOS,
  nmos6510: CPUType::NMOS6510,
  ricoh: CPUType::RICOH,
  cmos: CPUType::CMOS,
);

// NOTE: We use timeout on this function to avoid having to test each
// recv. Instead let an overall timeout control things.
#[test]
#[timeout(60000)]
fn functionality_test() -> Result<()> {
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, true)?;
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/6502_functional_test.bin");

    // We should get a CPU and a prompt since we did a preload.
    let resp = outputrx.recv()?;
    println!("Output - {resp:?}");
    if let Output::CPU(c, _) = resp {
        println!("Stop - {c}");
        assert!(
            c.reason == StopReason::None,
            "Invalid stop reason after startup load. Should be None - {c}"
        );
        assert!(
            c.state.pc == 0x0400,
            "Didn't stop on correct PC. Shoud be 0x0400 and is {:04X} - {c}",
            c.state.pc
        );
    } else {
        panic!("Didn't get PC after startup load? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after startup load? - {resp:?}");
    }

    // Go ahead and ask for HELP to validate the prompt pre strings
    inputtx.send("H".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        assert!(s.len() > 10, "Didn't get help output? - {s}");
    } else {
        println!("Didn't get prompt after startup? - {resp:?}");
    }

    // Send an invalid command that's too long
    inputtx.send("AN INVALID COMMAND I SENT".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid command (too long)");
    } else {
        panic!("Didn't get an error for invalid command (too long). Got {resp:?}");
    }

    // Send an invalid command
    inputtx.send("GOO".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid command");
    } else {
        panic!("Didn't get an error for invalid command. Got {resp:?}");
    }

    // NOTE: Trying to inline all the tests will eventually overflow the stack
    //       for this test (likely local vars piling up, etc). Also for readability
    //       this makes sense so each command variation goes in it's own function.

    load_tests(&inputtx, &outputrx, &path)?;
    breakpoint_tests(&inputtx, &outputrx)?;
    watchpoint_tests(&inputtx, &outputrx)?;
    bin_tests(&inputtx, &outputrx, &path)?;
    reset_tests(&inputtx, &outputrx)?;
    pc_tests(&inputtx, &outputrx)?;
    ram_tests(&inputtx, &outputrx)?;
    cpu_tests(&inputtx, &outputrx)?;
    read_tests(&inputtx, &outputrx)?;
    write_tests(&inputtx, &outputrx)?;

    Ok(())
}

fn load_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>, path: &Path) -> Result<()> {
    // Send an invalid load command
    inputtx.send("L".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid load command");
    } else {
        panic!("Didn't get an error for invalid load command. Got {resp:?}");
    }

    // Load with filename only
    inputtx.send(format!("L {}", path.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::CPU(c, _) = resp {
        println!("Stop - {c}");
        assert!(
            c.reason == StopReason::None,
            "Invalid stop reason after regular load. Should be None - {c}"
        );
        assert!(
            c.state.pc == 0x37A3,
            "Didn't stop on correct PC. Shoud be 0x37A3 and is {:04X} - {c}",
            c.state.pc
        );
    } else {
        panic!("Didn't get PC after regular load? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after regular load? - {resp:?}");
    }

    // Load with location and filename.
    inputtx.send(format!("L {} %0", path.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::CPU(c, _) = resp {
        println!("Stop - {c}");
        assert!(
            c.reason == StopReason::None,
            "Invalid stop reason after location load. Should be None - {c}"
        );
        assert!(
            c.state.pc == 0x37A3,
            "Didn't stop on correct PC. Shoud be 0x37A3 and is {:04X} - {c}",
            c.state.pc
        );
    } else {
        panic!("Didn't get PC after location load? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after location load? - {resp:?}");
    }

    // Load with invalid filename
    inputtx.send("L /no/such/file".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid load (bad filename) command");
    } else {
        panic!("Didn't get an error for invalid load (bad filename) command. Got {resp:?}");
    }

    // Load with an offset that makes it > 64k
    inputtx.send(format!("L {} 1", path.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid load (bad offset) command");
    } else {
        panic!("Didn't get an error for invalid load (bad offset) command. Got {resp:?}");
    }

    // Bad offset value
    inputtx.send(format!("L {} 0x", path.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid load (bad offset parse) command");
    } else {
        panic!("Didn't get an error for invalid load (bad offset parse) command. Got {resp:?}");
    }

    // Bad start PC value
    inputtx.send(format!("L {} %0 0x", path.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid load (bad start PC) command");
    } else {
        panic!("Didn't get an error for invalid load (bad start PC) command. Got {resp:?}");
    }

    Ok(())
}

fn breakpoint_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Set a break point
    inputtx.send("B 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint? - {resp:?}");
    }

    // Invalid breakpoint
    inputtx.send("B".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid breakpoint (no PC) command");
    } else {
        panic!("Didn't get an error for invalid breakpoint (no PC) command. Got {resp:?}");
    }

    // Set a breakpoint with an invalid value
    inputtx.send("B 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid breakpoint command");
    } else {
        panic!("Didn't get an error for invalid breakpoint command. Got {resp:?}");
    }

    // Breakpoints list
    inputtx.send("BPL".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        assert!(
            s.contains("Breakpoints"),
            "prompt from BPL doesn't contain 'Breakpoints' - {s}"
        );
    } else {
        panic!("Didn't get prompt after breakpoint list? - {resp:?}");
    }

    // Delete the breakpoint
    inputtx.send("DB 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after delete breakpoint? - {resp:?}");
    }

    // Delete breakpoint with an invalid command
    inputtx.send("DB".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete breakpoint command");
    } else {
        panic!("Didn't get an error for invalid delete breakpoint command. Got {resp:?}");
    }

    // Delete breakpoint with an invalid index value
    inputtx.send("DB x".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!(
            "Got expected error: {s} from invalid delete breakpoint (invalid index val) command"
        );
    } else {
        panic!("Didn't get an error for invalid delete breakpoint (invalid index val) command. Got {resp:?}");
    }

    // Delete breakpoint with an invalid index value
    inputtx.send("DB 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete breakpoint (invalid index) command");
    } else {
        panic!("Didn't get an error for invalid delete breakpoint (invalid index) command. Got {resp:?}");
    }

    // Set a breakpoint but then delete an invalid index
    inputtx.send("B 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint2? - {resp:?}");
    }
    inputtx.send("DB 1".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete breakpoint (invalid index2) command");
    } else {
        panic!("Didn't get an error for invalid delete breakpoint (invalid index2) command. Got {resp:?}");
    }

    Ok(())
}

fn watchpoint_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Set a watchpoint
    inputtx.send("WP 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after watchpoint? - {resp:?}");
    }

    // Invalid watchpoint
    inputtx.send("WP".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid watchpoint (no PC) command");
    } else {
        panic!("Didn't get an error for invalid watchpoint (no PC) command. Got {resp:?}");
    }

    // Set a watchpoint with an invalid value
    inputtx.send("WP 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid watchpoint command");
    } else {
        panic!("Didn't get an error for invalid watchpoint command. Got {resp:?}");
    }

    // Watchpoints list
    inputtx.send("WPL".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        assert!(
            s.contains("Watchpoints"),
            "prompt from WPL doesn't contain 'Watchpoints' - {s}"
        );
    } else {
        panic!("Didn't get prompt after watchpoint list? - {resp:?}");
    }

    // Delete the watchpoint
    inputtx.send("DW 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after delete watchpoint? - {resp:?}");
    }

    // Delete watchpoint with an invalid command
    inputtx.send("DW".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete watchpoint command");
    } else {
        panic!("Didn't get an error for invalid delete watchpoint command. Got {resp:?}");
    }

    // Delete watchpoint with an invalid index value
    inputtx.send("DW x".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!(
            "Got expected error: {s} from invalid delete watchpoint (invalid index val) command"
        );
    } else {
        panic!("Didn't get an error for invalid delete watchpoint (invalid index val) command. Got {resp:?}");
    }

    // Delete watchpoint with an invalid index value
    inputtx.send("DW 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete watchpoint (invalid index) command");
    } else {
        panic!("Didn't get an error for invalid delete watchpoint (invalid index) command. Got {resp:?}");
    }

    // Set a watchpoint but then delete an invalid index
    inputtx.send("WP 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after watchpoint2? - {resp:?}");
    }
    inputtx.send("DW 1".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid delete watchpoint (invalid index2) command");
    } else {
        panic!("Didn't get an error for invalid delete watchpoint (invalid index2) command. Got {resp:?}");
    }

    Ok(())
}

fn bin_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>, path: &Path) -> Result<()> {
    // Send an invalid BIN command
    inputtx.send("BIN".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid bin command");
    } else {
        panic!("Didn't get an error for invalid bin command. Got {resp:?}");
    }

    // Send over a path that doesn't work
    inputtx.send("BIN /no/such/file".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid bin command");
    } else {
        panic!("Didn't get an error for invalid bin command. Got {resp:?}");
    }

    // Now create a valid path
    let dir = tempdir()?;
    let dest = dir.path().join("bin.bin");

    // Send over a BIN with this path
    inputtx.send(format!("BIN {}", dest.to_string_lossy()))?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after BIN? - {resp:?}");
    }

    // Read in our original path
    let orig = read(path)?;
    let new = read(dest)?;
    assert!(orig == new, "BIN created different file");

    Ok(())
}

fn reset_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Reset and validate it
    inputtx.send("RESET".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x37A3,
            "PC not correct. Expected 0x37A3 and got {cpu}"
        );
    } else {
        panic!("Didn't get CPUState after reset? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after reset? - {resp:?}");
    }

    Ok(())
}
fn pc_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Send an invalid PC command
    inputtx.send("PC".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid pc command");
    } else {
        panic!("Didn't get an error for invalid pc command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("PC 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid pc (addr) command");
    } else {
        panic!("Didn't get an error for invalid pc (addr) command. Got {resp:?}");
    }

    // Set PC to 0x0400 and validate it
    inputtx.send("PC 0x0400".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0400,
            "PC not correct. Expected 0x0400 and got {cpu}"
        );
    } else {
        panic!("Didn't get CPUState after CPU? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after CPU? - {resp:?}");
    }

    Ok(())
}

fn ram_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Get RAM and validate it
    inputtx.send("RAM".into())?;
    let resp = outputrx.recv()?;
    if let Output::RAM(r) = resp {
        assert!(
            r[0x0400] == 0xD8,
            "RAM not correct. Expected 0xD8 at 0x0400 and got {:#06X}",
            r[0x0400]
        );
    } else {
        panic!("Didn't get CPUState after RAM? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after RAM? - {resp:?}");
    }

    Ok(())
}

fn cpu_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Get CPU and validate it
    inputtx.send("CPU".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0400,
            "PC not correct. Expected 0x0400 and got {cpu}",
        );
    } else {
        panic!("Didn't get CPUState after CPU? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after CPU? - {resp:?}");
    }

    Ok(())
}

fn read_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Send an invalid read command
    inputtx.send("R".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid read command");
    } else {
        panic!("Didn't get an error for invalid read command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("R 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid read (addr) command");
    } else {
        panic!("Didn't get an error for invalid read (addr) command. Got {resp:?}");
    }

    // Read 0x0400 and validate it
    inputtx.send("R 0x0400".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let exp = "0400  D8";
        assert!(
            s == exp,
            "Read return didn't match - Got '{s}' and expected '{exp}'"
        );
    } else {
        panic!("Didn't get prompt after read? - {resp:?}");
    }

    // Invalid RR
    inputtx.send("RR".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range read command");
    } else {
        panic!("Didn't get an error for invalid range read command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("RR 0xFFFFF 2".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range read (addr) command");
    } else {
        panic!("Didn't get an error for invalid range read (addr) command. Got {resp:?}");
    }

    // Send an invalid length
    inputtx.send("RR 0x0400 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range read (len) command");
    } else {
        panic!("Didn't get an error for invalid range read (len) command. Got {resp:?}");
    }

    // Send a start+len > MAX_SIZE
    inputtx.send("RR 0x0400 0xFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range read (total) command");
    } else {
        panic!("Didn't get an error for invalid range read (total) command. Got {resp:?}");
    }

    // Read 0x0400 for len 1 and validate it
    inputtx.send("RR 0x0400 2".into())?;
    let resp = outputrx.recv()?;
    if let Output::RAM(r) = resp {
        assert!(
            r[0x0400] == 0xD8,
            "Read return didn't match 0x0400 D8 - Got {}",
            r[0x0400]
        );
        assert!(
            r[0x0401] == 0xA2,
            "Read return didn't match 0x0401 A2 - Got {}",
            r[0x0401]
        );
    } else {
        panic!("Didn't get ram after range read? - {resp:?}");
    }

    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after range read? - {resp:?}");
    }

    Ok(())
}

#[allow(clippy::too_many_lines)]
fn write_tests(inputtx: &Sender<String>, outputrx: &Receiver<Output>) -> Result<()> {
    // Send an invalid read command
    inputtx.send("W".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid write command");
    } else {
        panic!("Didn't get an error for invalid write command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("W 0xFFFFF 0xEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid write (addr) command");
    } else {
        panic!("Didn't get an error for invalid write (addr) command. Got {resp:?}");
    }

    // Send an invalid val
    inputtx.send("W 0x0400 0xFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid write (val) command");
    } else {
        panic!("Didn't get an error for invalid write (val) command. Got {resp:?}");
    }

    // Write 0x0400 EA and validate it
    inputtx.send("W 0x0400 0xEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    // Read 0x0400 and validate it
    inputtx.send("R 0x0400".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let exp = "0400  EA";
        assert!(
            s == exp,
            "Read return didn't match - Got '{s}' and expected '{exp}'"
        );
    } else {
        panic!("Didn't get prompt after read? - {resp:?}");
    }

    // Invalid WR
    inputtx.send("WR".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write command");
    } else {
        panic!("Didn't get an error for invalid range write command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("WR 0xFFFFF 2 0xEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write (addr) command");
    } else {
        panic!("Didn't get an error for invalid range write (addr) command. Got {resp:?}");
    }

    // Send an invalid length
    inputtx.send("WR 0x0400 0xFFFFF 0xEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write (len) command");
    } else {
        panic!("Didn't get an error for invalid range write (len) command. Got {resp:?}");
    }

    // Send an invalid value
    inputtx.send("WR 0x0400 2 0xFFEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write (val) command");
    } else {
        panic!("Didn't get an error for invalid range write (val) command. Got {resp:?}");
    }

    // Send a start+len > MAX_SIZE
    inputtx.send("WR 0x0400 0xFFFF 0xEA".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write (total) command");
    } else {
        panic!("Didn't get an error for invalid range write (total) command. Got {resp:?}");
    }

    // Write 0x0400 for len 2 and validate it
    inputtx.send("WR 0x0400 2 0xEB".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    // Read 0x0400 and 0x0401 and validate it
    inputtx.send("R 0x0400".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let exp = "0400  EB";
        assert!(
            s == exp,
            "Read return didn't match - Got '{s}' and expected '{exp}'"
        );
    } else {
        panic!("Didn't get prompt after read? - {resp:?}");
    }
    inputtx.send("R 0x0401".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let exp = "0401  EB";
        assert!(
            s == exp,
            "Read return didn't match - Got '{s}' and expected '{exp}'"
        );
    } else {
        panic!("Didn't get prompt after read? - {resp:?}");
    }

    // Reset values and validate them.
    inputtx.send("W 0x0400 0xD8".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    inputtx.send("W 0x0401 0xA2".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    inputtx.send("RR 0x0400 2".into())?;
    let resp = outputrx.recv()?;
    if let Output::RAM(r) = resp {
        assert!(
            r[0x0400] == 0xD8,
            "Read return didn't match 0x0400 D8 - Got {}",
            r[0x0400]
        );
        assert!(
            r[0x0401] == 0xA2,
            "Read return didn't match 0x0401 A2 - Got {}",
            r[0x0401]
        );
    } else {
        panic!("Didn't get ram after range read? - {resp:?}");
    }

    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after range read? - {resp:?}");
    }

    Ok(())
}
