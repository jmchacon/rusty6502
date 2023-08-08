//! `monitor_tui` implements a basic monitor program for running a 65xx device.
use clap::Parser;
use clap_num::maybe_hex;
use color_eyre::eyre::Result;
use monitor::{cpu_loop, input_loop, Type};
use std::{io, io::Write, sync::mpsc::channel, thread, time};

/// `monitor_tui` will start a 65xx of the given chip and begin a blank memory session.
///
/// It can take an optional memory image file (.bin generally) or a
/// .prg file (c64 basic program which loads at 0x0801)
/// and load it before turning over control.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    cpu_type: Type,

    #[arg(help = "Filename containing binary image or PRG file")]
    filename: Option<String>,

    #[arg(
        long, value_parser=maybe_hex::<u16>,
        help = "Offset into RAM to start loading data. All other RAM will be zero'd out. Ignored for PRG files."
    )]
    offset: Option<u16>,

    #[arg(long, value_parser=maybe_hex::<u16>, help = "Starting PC value after loading and RESET has been run.")]
    start: Option<u16>,
}

struct Runner {
    h: std::thread::JoinHandle<Result<()>>,
    n: String,
    done: bool,
}

macro_rules! check_thread {
    ($thread:ident) => {
        if $thread.done {
            let res = $thread.h.join();
            if let Err(e) = res {
                println!("ERROR from {} thread: {e:?}", $thread.n);
                std::process::exit(1);
            } else {
                std::process::exit(0);
            }
        }
    };
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel();
    let c = thread::spawn(move || cpu_loop(args.cpu_type, &cpucommandrx, &cpucommandresptx));
    let mut cpu = Runner {
        h: c,
        n: String::from("CPU"),
        done: false,
    };

    let (inputtx, inputrx) = channel();
    let mut load = String::new();
    if let Some(file) = &args.filename {
        let loc = args.offset.unwrap_or_default();
        let pc = args.start.unwrap_or_default();
        load = format!("L {file} {loc} {pc}");
    }
    let s = thread::spawn(move || -> Result<()> {
        // One time initial load handling by simulating it typed in.
        // Much simpler than replicating load logic.
        if !load.is_empty() {
            inputtx.send(load)?;
        }
        loop {
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;
            inputtx.send(buffer)?;
        }
    });
    let mut stdin = Runner {
        h: s,
        n: String::from("Stdin"),
        done: false,
    };

    let (outputtx, outputrx) = channel();
    let o = thread::spawn(move || -> Result<()> {
        loop {
            let (out, prompt) = outputrx.recv()?;
            print!("{out}");
            if prompt {
                print!("> ");
            }
            io::stdout().flush()?;
        }
    });
    let mut stdout = Runner {
        h: o,
        n: String::from("Stdout"),
        done: false,
    };

    let m = thread::spawn(move || -> Result<()> {
        input_loop(
            &cpucommandtx,
            &cpucommandresprx,
            &inputrx,
            &outputtx,
            args.filename.is_some(),
        )
    });
    let mut main = Runner {
        h: m,
        n: String::from("main"),
        done: false,
    };

    loop {
        for r in [&mut main, &mut cpu, &mut stdin, &mut stdout] {
            if r.h.is_finished() {
                r.done = true;
            }
        }
        check_thread!(main);
        check_thread!(cpu);
        check_thread!(stdin);
        check_thread!(stdout);
        thread::sleep(time::Duration::from_millis(100));
    }
}
