use crate::commands::{Command, CommandResponse, Location, StepN, StepNReason, PC};
use crate::{cpu_loop, input_loop, match_cmd, process_running, trim_newline, Output, StopReason};
use color_eyre::eyre::{eyre, Report, Result};
use ntest::timeout;
use rusty6502::prelude::*;
use std::fs::read;
use std::panic;
use std::path::Path;
use std::thread::JoinHandle;
use std::{sync::mpsc::channel, sync::mpsc::Receiver, sync::mpsc::Sender, thread};
use tempfile::tempdir;

#[allow(clippy::type_complexity)]
fn setup(
    cpu_type: CPUType,
    preload: bool,
) -> Result<(
    Sender<String>,
    Receiver<Output>,
    JoinHandle<Result<(), Report>>,
)> {
    let cpu: Box<dyn CPU> = match cpu_type {
        CPUType::NMOS => Box::new(CPU6502::new(ChipDef::default())),
        CPUType::RICOH => Box::new(CPURicoh::new(ChipDef::default())),
        CPUType::NMOS6510 => Box::new(CPU6510::new(ChipDef::default(), None)),
        CPUType::CMOS => Box::new(CPU65C02::new(ChipDef::default())),
        CPUType::CMOSRockwell => Box::new(CPU65C02Rockwell::new(ChipDef::default())),
        CPUType::CMOS65SC02 => Box::new(CPU65SC02::new(ChipDef::default())),
    };

    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // Pass through channels for cpucommand so we can log for tests.
    let (passcpucommandtx, passcpucommandrx) = channel();
    let passcpu = thread::Builder::new().name("pass through".into());
    passcpu.spawn(move || -> Result<()> {
        loop {
            let resp = passcpucommandrx.recv()?;
            println!("Received Command: {resp:#?}");
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
            println!("Received Response: {resp:#?}");
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

// process_runnning deals with the RUN state and gets check after
// commands process. It's similar to the command checking below but
// slightly differently so write it out completely.
#[test]
fn process_running_errors_test() -> Result<()> {
    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel::<Result<CommandResponse>>();
    // Where output goes
    let (outputtx, outputrx) = channel::<Output>();

    // First time send an invalid return
    cpucommandresptx.send(Ok(CommandResponse::Dump))?;

    let resp = process_running(1, &outputtx, &cpucommandresprx);
    println!("Response1: {resp:?}");
    let e = resp.err().unwrap_or_else(|| {
        let r = outputrx.recv();
        panic!("didn't get error from process_running - {r:?}")
    });

    let want = "Invalid return from Run";
    assert!(
        e.to_string().contains(want),
        "didn't get proper error from process_running: {e:?} vs {want}"
    );

    // Now send an error back instead.
    cpucommandresptx.send(Err(eyre!("error from running")))?;

    // The 2nd call should send some output about an error but otherwise return ok
    let resp = process_running(1, &outputtx, &cpucommandresprx);
    println!("Response2: {resp:?}");
    assert!(resp.is_ok(), "Got error from cmd? - {resp:?}");

    let r = outputrx.recv()?;
    match r {
        Output::Error(e) => {
            let want = "Run error - error from running";
            assert!(e.contains(want), "error invalid - {e} vs {want}");
        }
        _ => panic!("Invalid output from cmd - {r:?}"),
    }

    // Now close the resptx end which should generate an error on a call.
    drop(cpucommandresptx);
    let resp = process_running(1, &outputtx, &cpucommandresprx);
    println!("Response3: {resp:?}");
    let e = resp.err().unwrap_or_else(|| {
        let r = outputrx.recv();
        panic!("didn't get error from process_running - {r:?}")
    });

    let want = "Sender channel died";
    assert!(
        e.to_string().contains(want),
        "didn't get proper error from process_running: {e:?} vs {want}"
    );

    Ok(())
}

// Each command has 2 conditions which require setting up a bad reciever/etc.
// Easier to do that in a macro pattern as the calling conventions/errors are the
// same per command. i.e.
//
// The command returns a wrapped error
// The command returns an invalid response
macro_rules! cmd_errors_test {
  ($suite:ident, $($name:ident: $command:expr, $invalid:expr, $c:literal, $parts:expr,)*) => {
    mod $suite {
      use super::*;

      $(
        #[test]
        fn $name() -> Result<()> {
          // The command channel.
          let (cpucommandtx, cpucommandrx) = channel::<Command>();
          // The response channel.
          let (cpucommandresptx, cpucommandresprx) = channel::<Result<CommandResponse>>();
          // Where output goes
          let (outputtx, outputrx) = channel::<Output>();

          let cpuresp = thread::Builder::new().name(format!("cpu {} errors", $command).into());

          cpuresp.spawn(move || -> Result<()> {
              let cmd = cpucommandrx.recv()?;
              println!("Got first cmd: {cmd:?}");

              // First time send an invalid return
              cpucommandresptx.send(Ok($invalid))?;

              // Now send an error back instead.
              let cmd = cpucommandrx.recv()?;
              println!("Got 2nd cmd: {cmd:?}");

              cpucommandresptx.send(Err(eyre!("error from cmd")))?;

              Ok(())
          })?;

          let parts = $parts;

          // The first call should generate an error for invalid return.
          // NOTE: For macro purposes we use match_cmd to call the actual command
          //       we're testing as then the macro only has to define 2 args which
          //       is constant and easier to handle since at macro invocation time
          //       things like the channels aren't identifiers in scope so they can't
          //       be macro args.
          let resp = match_cmd($c, &parts, &outputtx, &cpucommandtx, &cpucommandresprx);
          println!("Response1: {resp:?}");
          let e = resp
              .err()
              .unwrap_or_else(|| {
                let r = outputrx.recv();
                panic!("didn't get error from cmd - {r:?}")
              });

          let want = format!("Invalid return from {}", $command);
          assert!(
              e.to_string().contains(&want),
              "didn't get proper error from cmd: {e:?} vs {want}"
          );

          // The 2nd call should send some output about an error but otherwise return ok
          let resp = match_cmd($c, &parts, &outputtx, &cpucommandtx, &cpucommandresprx);
          println!("Response2: {resp:?}");
          assert!(resp.is_ok(), "Got error from cmd? - {resp:?}");

          let r = outputrx.recv()?;
          match r {
              Output::Error(e) => {
                let want = format!("{} error - error from cmd", $command);
                assert!(e.contains(&want), "error invalid - {e} vs {want}");
              }
              _ => panic!("Invalid output from cmd - {r:?}"),
          }
          Ok(())
        }
      )*
    }
  }
}

cmd_errors_test!(
  cmd_errors_tests,
  b_cmd_error_test: "Break", CommandResponse::Dump, "B", ["B", "12345"],
  bpl_cmd_error_test: "BreakList", CommandResponse::Dump, "BPL", ["BPL"],
  db_cmd_error_test: "Delete Breakpoint", CommandResponse::Dump, "DB", ["DB", "12345"],
  s_cmd_error_test: "Step", CommandResponse::Dump, "S", ["S"],
  stepn_cmd_error_test: "StepN", CommandResponse::Dump, "STEPN", ["STEPN", "1", "1", "FALSE"],
  t_cmd_error_test: "Tick", CommandResponse::Dump, "T", ["T"],
  r_cmd_error_test: "Read", CommandResponse::Dump, "R", ["R", "12345"],
  rr_cmd_error_test: "Read Range", CommandResponse::Dump, "RR", ["RR", "12345", "12345"],
  w_cmd_error_test: "Write", CommandResponse::Dump, "W", ["W", "12345", "1"],
  wr_cmd_error_test: "Write Range", CommandResponse::Dump, "WR", ["WR", "12345", "12345", "1"],
  cpu_cmd_error_test: "Cpu", CommandResponse::Dump, "CPU", ["CPU"],
  ram_cmd_error_test: "Ram", CommandResponse::Dump, "RAM", ["RAM"],
  d_cmd_error_test: "Disassemble", CommandResponse::Dump, "D", ["D", "12345"],
  dr_cmd_error_test: "Disassemble Range", CommandResponse::Dump, "DR", ["DR", "12345", "12345"],
  wp_cmd_error_test: "Watchpoint", CommandResponse::Dump, "WP", ["WP", "12345"],
  wpl_cmd_error_test: "WatchList", CommandResponse::Dump, "WPL", ["WPL"],
  dw_cmd_error_test: "Delete Watchpoint", CommandResponse::Dump, "DW", ["DW", "1"],
  l_cmd_error_test: "Load", CommandResponse::Dump, "L", ["L", "file"],
  bin_cmd_error_test: "Dump", CommandResponse::Break, "BIN", ["BIN", "file"],
  pc_cmd_error_test: "PC", CommandResponse::Dump, "PC", ["PC", "12345"],
  reset_cmd_error_test: "Reset", CommandResponse::Dump, "RESET", ["RESET"],
);

#[test]
#[timeout(60000)]
fn run_invalid_commands() -> Result<()> {
    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel();

    let cl = thread::Builder::new().name("cpu_loop".into());
    cl.spawn(move || {
        cpu_loop(
            Box::new(CPU6502::new(ChipDef::default())),
            &cpucommandrx,
            &cpucommandresptx,
        )
    })?;

    cpucommandtx.send(Command::Run(false))?;
    cpucommandtx.send(Command::Break(Location { addr: 0x0400 }))?;
    cpucommandtx.send(Command::Stop)?;

    let mut errors = 0;
    loop {
        match cpucommandresprx.recv()? {
            Ok(st) => match st {
                CommandResponse::Stop(stop) => match stop.reason {
                    StopReason::Run => continue,
                    StopReason::Stop => break,
                    _ => panic!("Invalid stop reason while running: {stop}"),
                },
                _ => panic!("Invalid response while running: {st}"),
            },
            Err(e) => {
                println!("Got error {e:?}");
                errors += 1;
                assert!(errors < 2, "Too many errors");
            }
        }
    }
    assert!(errors == 1, "Didn't get an error?");
    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn check_speed() -> Result<()> {
    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel();

    let cl = thread::Builder::new().name("cpu_loop".into());
    cl.spawn(move || {
        cpu_loop(
            Box::new(CPU6502::new(ChipDef::default())),
            &cpucommandrx,
            &cpucommandresptx,
        )
    })?;

    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/6502_functional_test.bin");
    cpucommandtx.send(Command::Load(
        format!("{}", path.to_string_lossy()),
        Some(Location { addr: 0 }),
        Some(PC { addr: 0x0400 }),
    ))?;
    let resp = cpucommandresprx.recv()?;
    if let Ok(CommandResponse::Load(_)) = resp {
    } else {
        panic!("didn't get load response got - {resp:?}")
    }

    let cap = 128;
    let capture = vec![CPUState::default(); cap];
    let now = std::time::Instant::now();
    cpucommandtx.send(Command::StepN(StepN {
        reps: 7457,
        capture,
        ram: true,
    }))?;
    let resp = cpucommandresprx.recv()?;
    let n = now.elapsed();
    #[cfg(not(miri))]
    let max = std::time::Duration::from_millis(16);
    // Miri takes an eon so just give it 15m. Allow here because we dynamically
    // include sanitizer (which is nightly only) for those tests.
    #[cfg(any(miri, coverage))]
    let max = std::time::Duration::from_secs(600);

    match resp {
        Ok(CommandResponse::StepN(StepNReason::StepN(res))) => {
            #[allow(clippy::unwrap_used)]
            let last = res.last().unwrap();
            println!("Elapsed: {n:#?} {last}");
            assert!(
                res.len() == cap,
                "length should be {cap} and is {}",
                res.len()
            );
            assert!(n <= max, "too slow. got {n:#?}");
        }
        _ => panic!("Unknown resp: {resp:?}"),
    }

    // Do this again without capturing RAM and validate it's considerably faster
    let cap = 128;
    let capture = vec![CPUState::default(); cap];
    let now = std::time::Instant::now();
    cpucommandtx.send(Command::StepN(StepN {
        reps: 7457,
        capture,
        ram: false,
    }))?;
    let resp = cpucommandresprx.recv()?;
    let n = now.elapsed();
    #[cfg(not(miri))]
    let max = std::time::Duration::from_millis(8);
    // Miri takes an eon so just give it 15m. Allow here because we dynamically
    // include sanitizer (which is nightly only) for those tests.
    #[cfg(any(miri, coverage))]
    let max = std::time::Duration::from_secs(600);

    match resp {
        Ok(CommandResponse::StepN(StepNReason::StepN(res))) => {
            #[allow(clippy::unwrap_used)]
            let last = res.last().unwrap();
            println!("Elapsed: {n:#?} {last}");
            assert!(
                res.len() == cap,
                "length should be {cap} and is {}",
                res.len()
            );
            assert!(n <= max, "too slow. got {n:#?}");
        }
        _ => panic!("Unknown resp: {resp:?}"),
    }
    Ok(())
}

#[test]
#[timeout(60000)]
fn tick_init_test() -> Result<()> {
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, false)?;
    // Should go immediately to a prompt since we didn't preload
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after startup? - {resp:?}");
    }

    // Send a TICK down but we're not init
    inputtx.send("T".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0001,
            "PC not correct. Expected 0x0001 and got {cpu}"
        );
    } else {
        panic!("Didn't get CPUState after tick? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after tick? - {resp:?}");
    }

    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn step_init_test() -> Result<()> {
    // This one can be a little flaky so we let it run twice
    // to consider it broken.
    let res = panic::catch_unwind(step_init_test_impl);
    if res.is_err() {
        return step_init_test_impl();
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn step_init_test_impl() -> Result<()> {
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, false)?;
    // Should go immediately to a prompt since we didn't preload
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after startup? - {resp:?}");
    }

    // Send a STEP down but we're not init
    inputtx.send("S".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0000,
            "PC not correct. Expected 0x0000 and got {cpu}"
        );
    } else {
        panic!("Didn't get CPUState after step? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    drop(inputtx);
    drop(outputrx);

    // Reset for STEPN tests
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, true)?;

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

    // Send an invalid command to cover all of STEPN
    inputtx.send("STEPN".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid watchpoint (no PC) command");
    } else {
        panic!("Didn't get an error for invalid watchpoint (no PC) command. Got {resp:?}");
    }

    // Send an invalid number
    inputtx.send("STEPN 0x 12 FALSE".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        assert!(
            s.contains("parse error on count"),
            "didn't get correct error got {s} instead"
        );
        println!("Got expected error: {s} from invalid watchpoint (no PC) command");
    } else {
        panic!("Didn't get an error for invalid watchpoint (no PC) command. Got {resp:?}");
    }
    inputtx.send("STEPN 12 0x FALSE".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        assert!(
            s.contains("parse error on repeat"),
            "didn't get correct error got {s} instead"
        );
        println!("Got expected error: {s} from invalid watchpoint (no PC) command");
    } else {
        panic!("Didn't get an error for invalid watchpoint (no PC) command. Got {resp:?}");
    }

    // Set a breakpoint so we can STEPN into it
    inputtx.send("B 0x0403".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint? - {resp:?}");
    }

    // StepN to the breakpoint
    // TODO(jchacon): should this return more than just the BP state?
    inputtx.send("STEPN 5 0 TRUE".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        if let StopReason::Break(_) = st.reason {
        } else {
            panic!("Stop reason incorrect. Got {st}");
        }
        println!("Got expected stop output: {st}");
    } else {
        panic!("Didn't get stepn after startup load? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get stop after breakpoint for stepn? - {resp:?}");
    }

    // Set a watchpoint and write 0x01 to 0x0405 so it'll trigger quickly
    inputtx.send("WP 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after watchpoint2? - {resp:?}");
    }
    inputtx.send("W 0x0405 0x01".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    inputtx.send("STEPN 5 0 TRUE".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        if let StopReason::Watch(_, _, _) = st.reason {
        } else {
            panic!("Stop reason incorrect. Got {st}");
        }
        println!("Got expected stop output: {st}");
    } else {
        panic!("Didn't get stop from stepn? - {resp:?}");
    }
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint? - {resp:?}");
    }

    // Delete the breakpoint and watchpoint so it doesn't trigger below
    inputtx.send("DB 0".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after DB? - {resp:?}");
    }
    inputtx.send("DW 0".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after DW? - {resp:?}");
    }

    // Run the expected steps now.

    // Let's validate things like a GUI version will work where
    // it needs to do X steps per refresh period.
    //
    // NES is 1.7Mhz and refresh is 1/60th of a second so we need
    // 1/60 / 1.7Mhz == 29830 clocks per refresh to stay at original speed.
    // Average CPI is 4 for a 6502 so assume we need to run 29830 / 4 == 7457
    let now = std::time::Instant::now();
    inputtx.send("STEPN 7457 64 TRUE".into())?;
    let resp = outputrx.recv()?;
    let n = now.elapsed();

    if let Output::StepN(_) = resp {
    } else {
        panic!("Didn't get stepn after startup load? - {resp:?}");
    }

    // A very fast Sapphire Rapids CPU can do this in 8ms so even assuming
    // slowness for others we should be able to do this in double that.
    #[cfg(not(miri))]
    let timeout = std::time::Duration::from_millis(17);
    // Miri takes an eon so just give it 15m. Allow here because we dynamically
    // include sanitizer (which is nightly only) for those tests.
    #[cfg(any(miri, coverage))]
    let timeout = std::time::Duration::from_secs(900);

    assert!(
        n <= timeout,
        "too slow - time for instructions - {n:#?} vs {timeout:#?}"
    );
    println!("time for instructions - {n:#?} vs {timeout:#?}");

    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn run_init_test() -> Result<()> {
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, false)?;
    // Should go immediately to a prompt since we didn't preload
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after startup? - {resp:?}");
    }

    // Send an invalud RUN down
    inputtx.send("C NO NO".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid RUN");
    } else {
        panic!("Didn't get an error for invalid RUN. Got {resp:?}");
    }

    // Send a RUN down but we're not init
    inputtx.send("C".into())?;

    drop(inputtx);
    drop(outputrx);

    // Give time for the threads to notice and exit
    std::thread::sleep(std::time::Duration::from_millis(6000));
    Ok(())
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
                        panic!("Didn't get prompt after startup? - {resp:?}");
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
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn functionality_test() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

    Ok(())
}

fn init_with_load() -> Result<(Sender<String>, Receiver<Output>)> {
    let (inputtx, outputrx, _) = setup(CPUType::NMOS, true)?;

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
    Ok((inputtx, outputrx))
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn load_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/6502_functional_test.bin");

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn breakpoint_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

    // Delete the breakpoint so we leave state correct.
    inputtx.send("DB 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after delete breakpoint? - {resp:?}");
    }

    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn watchpoint_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

    // Delete the watchpoint so we leave state consistent.
    inputtx.send("DW 0".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after delete watchpoint? - {resp:?}");
    }
    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn bin_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../testdata/6502_functional_test.bin");

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn reset_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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
#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn pc_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn ram_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn cpu_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn read_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
#[allow(clippy::too_many_lines)]
fn write_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

    // Send an invalid write command
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

    // Send an invalid value (parse)
    inputtx.send("WR 0x0400 2 0x".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid range write (parse val) command");
    } else {
        panic!("Didn't get an error for invalid range write (parse val) command. Got {resp:?}");
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

#[test]
#[cfg_attr(not(miri), timeout(60000))]
#[cfg_attr(miri, timeout(900000))]
fn disassemble_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;

    // Send an invalid disassemble
    inputtx.send("D".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble command");
    } else {
        panic!("Didn't get an error for invalid disassemble command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("D 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble (addr) command");
    } else {
        panic!("Didn't get an error for invalid disassemble (addr) command. Got {resp:?}");
    }

    // Disassemble and validate
    inputtx.send("D 0x0400".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let expected = "0400 D8         CLD";
        assert!(
            s.contains(expected),
            "Output from disassemble doesn't contain '{expected}' - {s}"
        );
    } else {
        panic!("Didn't get prompt after disassemble? - {resp:?}");
    }

    // Invalid disassemble range
    inputtx.send("DR".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble range command");
    } else {
        panic!("Didn't get an error for invalid disassemble range command. Got {resp:?}");
    }

    // Send an invalid address
    inputtx.send("DR 0xFFFFF 2".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble range (addr) command");
    } else {
        panic!("Didn't get an error for invalid disassemble range (addr) command. Got {resp:?}");
    }

    // Send an invalid count
    inputtx.send("DR 0x0400 0xFFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble range (count) command");
    } else {
        panic!("Didn't get an error for invalid disassemble range (count) command. Got {resp:?}");
    }

    // Send an invalid range
    inputtx.send("DR 0x0400 0xFFFF".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid disassemble range (range) command");
    } else {
        panic!("Didn't get an error for invalid disassemble range (range) command. Got {resp:?}");
    }

    // Send length 2 over and validate
    inputtx.send("DR 0x0400 2".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(Some(s)) = resp {
        let expected = "0400 D8         CLD\n0401 A2 FF      LDX #$FF";
        assert!(
            s.contains(expected),
            "Output from disassemble range doesn't contain '{expected}' - {s}"
        );
    } else {
        panic!("Didn't get prompt after disassemble range? - {resp:?}");
    }
    Ok(())
}

#[test]
#[cfg_attr(not(miri), timeout(5000))]
#[cfg_attr(miri, timeout(900000))]
#[allow(clippy::too_many_lines)]
fn step_tests() -> Result<()> {
    let (inputtx, outputrx) = init_with_load()?;
    // Send an invalid step
    inputtx.send("S NO NO".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid step command");
    } else {
        panic!("Didn't get an error for invalid step command. Got {resp:?}");
    }

    // Send a regular step
    inputtx.send("S".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        assert!(
            st.reason == StopReason::Step,
            "Reason incorrect. Should be Step and got {st}"
        );
        assert!(
            st.state.pc == 0x0401,
            "PC wrong. Should be 0x0401. Got {st}"
        );
        let expected = "0401 A2 FF      LDX #$FF";
        assert!(
            st.state.dis == expected,
            "Disassembly wrong. Should be '{expected}' and got {st}"
        );
        assert!(
            st.state.ram[0xFFFF] == 0x00,
            "Didn't get blank RAM as expected. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    // Advance one more
    inputtx.send("S".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        assert!(
            st.reason == StopReason::Step,
            "Reason incorrect. Should be Step and got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    // Set a breakpoint at 0x0403 and validate we break there
    inputtx.send("B 0x0403".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint? - {resp:?}");
    }
    inputtx.send("S".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        let StopReason::Break(ref b) = st.reason else {
            panic!("reason incorrect. Should be Break and got {st}");
        };
        assert!(
            b.addr == 0x0403,
            "Invalid break address. Should be 0x0403 and got {st}"
        );
        assert!(
            st.state.pc == 0x0403,
            "PC wrong. Should be 0x0403. Got {st}"
        );
        let expected = "0403 9A         TXS";
        assert!(
            st.state.dis == expected,
            "Disassembly wrong. Should be '{expected}' and got {st}"
        );
        assert!(
            st.state.ram[0xFFFF] == 0x00,
            "Didn't get blank RAM as expected. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    // Set a watchpoint at 0x0200
    inputtx.send("WP 0x0200".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after watchpoint2? - {resp:?}");
    }

    // Write 0x01 to 0x0405 so it triggers the watchpoint
    inputtx.send("W 0x0405 0x01".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }
    // Do 3 more steps. The 3rd one should result in a watchpoint stop reason
    for _ in 0..2 {
        inputtx.send("S".into())?;
        let resp: Output = outputrx.recv()?;
        if let Output::CPU(st, _) = resp {
            assert!(
                st.reason == StopReason::Step,
                "Reason incorrect. Should be Step and got {st}"
            );
        } else {
            panic!("Didn't get a CPU for valid step command. Got {resp:?}");
        }
        let resp: Output = outputrx.recv()?;
        if let Output::Prompt(_) = resp {
        } else {
            panic!("Didn't get prompt after step? - {resp:?}");
        }
    }
    inputtx.send("S".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        let StopReason::Watch(ref pc, ref b, _) = st.reason else {
            panic!("reason incorrect. Should be Watch and got {st}");
        };
        assert!(
            b.addr == 0x0200,
            "Invalid watch address. Should be 0x0200 and got {st}"
        );
        assert!(
            pc.addr == 0x0406,
            "Invalid watch PC. Should be 0x0406 and got {st}"
        );
        assert!(
            st.state.pc == 0x0409,
            "PC wrong. Should be 0x0409. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    // Now step but include RAM which we'll validate
    inputtx.send("S TRUE".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        assert!(
            st.reason == StopReason::Step,
            "Reason incorrect. Should be Step and got {st}"
        );
        assert!(
            st.state.ram[0x0200] == 0x01,
            "Didn't get back written RAM from step"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }
    // Set PC back to 0x0406 so we can run tick tests.
    inputtx.send("PC 0x0406".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0406,
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
    // Set 0x0200 back to 0x00 so the next set of ticks, etc will work
    inputtx.send("W 0x0200 0x00".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }

    // Send an invalid Tick
    inputtx.send("T NO NO".into())?;
    let resp = outputrx.recv()?;
    if let Output::Error(s) = resp {
        println!("Got expected error: {s} from invalid tick command");
    } else {
        panic!("Didn't get an error for invalid tick command. Got {resp:?}");
    }

    // Send a regular tick
    inputtx.send("T".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        assert!(
            st.reason == StopReason::Tick,
            "Reason incorrect. Should be Tick and got {st}"
        );
        assert!(
            st.state.pc == 0x0407,
            "PC wrong. Should be 0x0406. Got {st}"
        );
        assert!(
            st.state.ram[0xFFFF] == 0x00,
            "Didn't get blank RAM as expected. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid tick command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after tick? - {resp:?}");
    }

    // Run 2 more ticks off which should almost complete things
    for i in 0..2 {
        inputtx.send("T".into())?;
        let resp: Output = outputrx.recv()?;
        if let Output::CPU(st, _) = resp {
            assert!(
                st.reason == StopReason::Tick,
                "Reason incorrect. Should be Tick and got {st} on iteration {i}"
            );
        } else {
            panic!("Didn't get a CPU for valid tick command. Got {resp:?}");
        }
        let resp: Output = outputrx.recv()?;
        if let Output::Prompt(_) = resp {
        } else {
            panic!("Didn't get prompt after tick? - {resp:?}");
        }
    }
    // The last tick should trigger a watchpoint
    inputtx.send("T".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        let StopReason::Watch(ref pc, ref b, _) = st.reason else {
            panic!("reason incorrect. Should be Watch and got {st}");
        };
        assert!(
            b.addr == 0x0200,
            "Invalid watch address. Should be 0x0200 and got {b:?}"
        );
        assert!(
            pc.addr == 0x0406,
            "Invalid watch PC. Should be 0x0406 and got {pc:?}"
        );
        assert!(
            st.state.pc == 0x0409,
            "PC wrong. Should be 0x0409. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid tick command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after tick? - {resp:?}");
    }

    // Set a breakpoint at 0x0409 and tick into it with RAM snapshots.
    inputtx.send("B 0x0409".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after breakpoint? - {resp:?}");
    }
    inputtx.send("T TRUE".into())?;
    let resp = outputrx.recv()?;
    if let Output::CPU(st, _) = resp {
        let StopReason::Break(ref b) = st.reason else {
            panic!("reason incorrect. Should be Break and got {st}");
        };
        assert!(
            b.addr == 0x0409,
            "Invalid break address. Should be 0x0409 and got {st}"
        );
        assert!(
            st.state.pc == 0x0409,
            "PC wrong. Should be 0x0409. Got {st}"
        );
        assert!(
            st.state.ram[0x0200] == 0x01,
            "Didn't get set RAM as expected. Got {st}"
        );
    } else {
        panic!("Didn't get a CPU for valid step command. Got {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after step? - {resp:?}");
    }

    // Delete all the breakpoints, reset 0x0200 to 0x00 and PC to 0x0400
    // WP left at 0x0200. Set a BP at 0x0401 to stop right away.
    inputtx.send("DB 0".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after DB? - {resp:?}");
    }
    inputtx.send("DB 0".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after DB? - {resp:?}");
    }
    inputtx.send("B 0x0401".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after B? - {resp:?}");
    }
    inputtx.send("W 0x0200 0x00".into())?;
    let resp = outputrx.recv()?;
    if let Output::Prompt(_) = resp {
    } else {
        panic!("Didn't get prompt after write? - {resp:?}");
    }

    inputtx.send("PC 0x0400".into())?;
    let resp: Output = outputrx.recv()?;
    if let Output::CPU(cpu, _) = resp {
        assert!(
            cpu.state.pc == 0x0400,
            "PC not correct. Expected 0x0400 and got {cpu}"
        );
    } else {
        panic!("Didn't get CPUState after CPU? - {resp:?}");
    }
    let resp: Output = outputrx.recv()?;
    if let Output::Prompt(s) = resp {
        println!("Prompt: {s:?}");
    } else {
        panic!("Didn't get prompt after CPU? - {resp:?}");
    }

    // Now tell it to run and then we send an immediate stop
    println!("Trying RUN tests");
    inputtx.send("C".into())?;

    // Send a bad command and validate we get an error.
    let mut errors = 0;
    let mut prompt = 0;
    let mut run = 0;
    let mut cont = true;

    inputtx.send("B 0x0400".into())?;
    loop {
        let resp = outputrx.recv()?;
        println!("Working {resp:#?}");
        match resp {
            Output::Prompt(_) => {
                prompt += 1;
                assert!(prompt < 10, "Got additional prompts");
            }
            Output::Error(ref e) => {
                println!("Error: {e}");
                errors += 1;
                assert!(errors < 3, "Got additional error back? {resp:?}");
            }
            Output::CPU(ref cpu, _) => match cpu.reason {
                StopReason::Run => {
                    run += 1;
                    if run == 2 {
                        inputtx.send("STOP".into())?;
                    }
                }
                StopReason::Stop => {
                    // Continue after STOP and the WP should kick in eventually
                    inputtx.send("C TRUE".into())?;
                }
                StopReason::Break(_) => {
                    if cont {
                        inputtx.send("C".into())?;
                        cont = false;
                    }
                    continue;
                }
                StopReason::Watch(_, _, _) => break,
                _ => panic!("Unknown reason - {resp:?}"),
            },

            _ => panic!("Unknown response after C, B, STOP - {resp:?}"),
        }
    }
    Ok(())
}

#[test]
fn test_trim() {
    let mut input: String = "test\r\n".into();
    trim_newline(&mut input);
    assert!(
        input == "test",
        "Didn't trim everything off input? '{input}'"
    );
    input = "test2".into();
    trim_newline(&mut input);
    assert!(
        input == "test2",
        "Didn't trim everything off input? '{input}'"
    );
}
