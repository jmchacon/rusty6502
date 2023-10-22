//! `nes_pal_render` reads a given PAL file and then displays a 16x4 grid displaying the palette.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::{fs::read, path::Path};

use ::egui::{FontFamily, FontId, TextStyle};
use clap::Parser;
use eframe::egui;
use egui::{TextureHandle, TextureOptions};
use nes_pal::{parse_pal, Color};

use color_eyre::eyre::{eyre, Result};

/// `nes_pal_render` will load the given PAL file and render the color scheme.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(help = "Filenames containing .pal data")]
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
        Box::new(|cc| Box::new(MyApp::new(cc, colors))),
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
        const WIDTH: usize = 40;
        const NUM_PER_LINE: usize = 16;
        const LINE_SIZE: usize = WIDTH * NUM_PER_LINE;
        const HEIGHT: usize = 40;
        const NUM_LINES: usize = 4;
        const ENTRIES_PER_PIXEL: usize = 3; // RGB
        const SIZE: usize = LINE_SIZE * HEIGHT * NUM_LINES * ENTRIES_PER_PIXEL;

        use FontFamily::{Monospace, Proportional};

        let mut style = (*cc.egui_ctx.style()).clone();
        style.text_styles = [
            (TextStyle::Heading, FontId::new(25.0, Proportional)),
            (TextStyle::Body, FontId::new(16.0, Proportional)),
            (TextStyle::Monospace, FontId::new(12.0, Monospace)),
            (TextStyle::Button, FontId::new(12.0, Proportional)),
            (TextStyle::Small, FontId::new(8.0, Proportional)),
        ]
        .into();
        cc.egui_ctx.set_style(style);

        let mut data: [u8; SIZE] = [0; SIZE];
        let mut textures = Vec::new();

        for d in &datas {
            for (loc, c) in d.colors.iter().enumerate() {
                // The upper left hand corner of the box we're coloring in.

                // First figure out the row we're on and the first entry for it's
                // first pixel.
                let row_start = loc / NUM_PER_LINE * HEIGHT * LINE_SIZE * ENTRIES_PER_PIXEL;
                // Now move N boxes over to find the box start pixel.
                let box_start = row_start + WIDTH * (loc % NUM_PER_LINE) * ENTRIES_PER_PIXEL;
                for y in 0..HEIGHT {
                    // Finally for each line adjust by the row we're on for each line.
                    let y_off = box_start + y * LINE_SIZE * ENTRIES_PER_PIXEL;
                    for x in 0..WIDTH {
                        // Each x start has to be adjusted by RGB to get the final entry.
                        let start = x * ENTRIES_PER_PIXEL;
                        data[y_off + start] = c.r;
                        data[y_off + start + 1] = c.g;
                        data[y_off + start + 2] = c.b;
                    }
                }
            }
            let im = egui::ColorImage::from_rgb([LINE_SIZE, HEIGHT * NUM_LINES], &data);
            let texture = cc.egui_ctx.load_texture(
                d.filename.clone(),
                egui::ImageData::Color(im.into()),
                TextureOptions::default(),
            );
            textures.push(texture);
        }
        Self { textures }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default()
            .frame(egui::Frame::none().fill(egui::Color32::GRAY))
            .show(ctx, |ui| {
                for t in &self.textures {
                    ui.label(t.name());
                    ui.image(t);
                }
            });
    }
}
