//! `monitor_gui` implements a basic monitor GUII program for running a 65xx device.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use eframe::egui;
use ::egui::{FontFamily, FontId, RichText, TextStyle};

use color_eyre::eyre::Result;
use monitor::{cpu_loop, input_loop};
use rusty6502::prelude::*;
use std::{io, io::Write, sync::mpsc::channel, thread, time};

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

    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions::default();

    let _ret = eframe::run_native(
        "egui example: global font style",
        options,
        Box::new(|cc| Box::new(MyApp::new(cc))),
    );

    // The command channel.
    let (cpucommandtx, cpucommandrx) = channel();
    // The response channel.
    let (cpucommandresptx, cpucommandresprx) = channel();
    let c = thread::spawn(move || cpu_loop(CPUType::NMOS, &cpucommandrx, &cpucommandresptx));
    let mut cpu = Runner {
        h: c,
        n: "CPU".into(),
        done: false,
    };

    let (inputtx, inputrx) = channel();

    let s = thread::spawn(move || -> Result<()> {
        loop {
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;
            inputtx.send(buffer)?;
        }
    });
    let mut stdin = Runner {
        h: s,
        n: "Stdin".into(),
        done: false,
    };

    let (outputtx, outputrx) = channel();
    let o = thread::spawn(move || -> Result<()> {
        loop {
            let out = outputrx.recv()?;
            print!("{out:?}");
            print!("> ");
            io::stdout().flush()?;
        }
    });
    let mut stdout = Runner {
        h: o,
        n: "Stdout".into(),
        done: false,
    };

    let m = thread::spawn(move || -> Result<()> {
        input_loop(&cpucommandtx, &cpucommandresprx, &inputrx, &outputtx, false)
    });
    let mut main = Runner {
        h: m,
        n: "main".into(),
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

fn content(ui: &mut egui::Ui) {
    ui.heading("Top Heading");
    ui.add_space(5.);
    ui.label("foo");
    ui.add_space(15.);
    ui.label(RichText::new("Sub Heading").text_style(heading2()).strong());
    ui.monospace("bar");
    ui.add_space(15.);
    ui.label(RichText::new("Context").text_style(heading3()).strong());
    ui.add_space(5.);
    ui.label("baz");
    let mut s = "bazbaz\nbizbar\n";
    ui.add(
        egui::TextEdit::multiline(&mut s)
            .font(heading2())
            .margin(egui::Vec2 { x: 0.0, y: 0.0 })
            .frame(false)
            .text_color(egui::Color32::DARK_GREEN),
    );
}

#[inline]
fn heading2() -> TextStyle {
    TextStyle::Name("Heading2".into())
}

#[inline]
fn heading3() -> TextStyle {
    TextStyle::Name("ContextHeading".into())
}

struct MyApp;

impl MyApp {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        use FontFamily::{Monospace, Proportional};

        let mut style = (*cc.egui_ctx.style()).clone();
        style.text_styles = [
            (TextStyle::Heading, FontId::new(25.0, Proportional)),
            (heading2(), FontId::new(22.0, Proportional)),
            (heading3(), FontId::new(19.0, Proportional)),
            (TextStyle::Body, FontId::new(16.0, Proportional)),
            (TextStyle::Monospace, FontId::new(12.0, Monospace)),
            (TextStyle::Button, FontId::new(12.0, Proportional)),
            (TextStyle::Small, FontId::new(8.0, Proportional)),
        ]
        .into();
        cc.egui_ctx.set_style(style);

        Self
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default()
            .frame(egui::Frame::none().fill(egui::Color32::BLACK))
            .show(ctx, content);
    }
}
