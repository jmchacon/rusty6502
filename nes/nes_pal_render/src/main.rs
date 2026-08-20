//! `nes_pal_render` reads a given PAL file and then displays a 16x4 grid displaying the palette.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::{collections::BTreeMap, fs::read, path::Path};

use ::egui::{FontFamily, FontId, TextStyle};
use clap::Parser;
use eframe::egui;
use egui::TextureHandle;
use nes_pal::{parse_pal, Color};
use nes_pal_gui::texture_from_palette;

use color_eyre::eyre::{eyre, Result};

/// `nes_pal_render` will load the given PAL file and render the color scheme.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(
        help = "Filenames containing .pal data (can be specified N times)",
        long
    )]
    filename: Vec<String>,
}

struct Data {
    filename: String,
    colors: Vec<Color>,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions::default();

    if args.filename.is_empty() {
        return Err(eyre!("Must supply at least one filename"));
    }

    let mut colors = Vec::new();
    for f in &args.filename {
        let bytes: Vec<u8> = read(f)?;
        let c = parse_pal(&bytes)?;
        let p = Path::new(f)
            .file_name()
            .ok_or(eyre!("Path error for {f}"))?;
        colors.push(Data {
            filename: p.to_string_lossy().into(),
            colors: c,
        });
    }

    let res = eframe::run_native(
        "NES PAL file renderer",
        options,
        Box::new(|cc| Ok(Box::new(MyApp::new(cc, colors)))),
    );

    if let Err(e) = res {
        return Err(eyre!("EGUI error: {e:?}"));
    }
    Ok(())
}

struct MyApp {
    textures: Vec<TextureHandle>,
}

impl MyApp {
    #[allow(clippy::needless_pass_by_value)]
    fn new(cc: &eframe::CreationContext<'_>, datas: Vec<Data>) -> Self {
        use FontFamily::{Monospace, Proportional};

        let text_styles: BTreeMap<_, _> = [
            (TextStyle::Heading, FontId::new(25.0, Proportional)),
            (TextStyle::Body, FontId::new(16.0, Proportional)),
            (TextStyle::Monospace, FontId::new(12.0, Monospace)),
            (TextStyle::Button, FontId::new(12.0, Proportional)),
            (TextStyle::Small, FontId::new(8.0, Proportional)),
        ]
        .into();
        cc.egui_ctx
            .all_styles_mut(move |style| style.text_styles = text_styles.clone());

        let mut textures = Vec::new();

        for d in &datas {
            textures.push(texture_from_palette(cc, &d.filename, &d.colors));
        }
        Self { textures }
    }
}

impl eframe::App for MyApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(egui::Color32::GRAY))
            .show(ui, |ui| {
                for t in &self.textures {
                    ui.label(t.name());
                    ui.image(t);
                    ui.separator();
                }
            });
    }
}
