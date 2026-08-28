//! `nes_pal_render` reads a given PAL file and then displays a 16x4 grid displaying the palette.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

use std::{collections::BTreeMap, fs::read, path::Path};

use ::egui::{FontFamily, FontId, TextStyle};
use clap::Parser;
use eframe::egui;
use egui::{TextureHandle, Vec2};
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

enum Stage {
    PreRender(isize),
    FirstRender(Vec2),
    FirstResize(Vec2),
    Initialized(Vec2),
}
struct MyApp {
    render_stage: Stage,
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
        Self {
            render_stage: Stage::PreRender(2_isize),
            textures,
        }
    }

    // Used for initial frames to get full layout figured out.
    fn pre_render(&mut self, ctx: &eframe::egui::Context) {
        egui::Window::new("pre_render")
            .title_bar(false)
            .fixed_pos((0.0, 0.0))
            .show(ctx, |ui| {
                self.render(ui);
            });
    }

    // The actual UI itself.
    fn render(&mut self, ui: &mut egui::Ui) {
        egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(egui::Color32::GRAY))
            .show(ui, |ui| {
                for t in &self.textures {
                    ui.label(t.name());

                    // We already created textures for each PAL so just index and display it.
                    // Now..we want this to look good so it takes a bit of fiddling:
                    //
                    // Stick it in a vertical centered which makes the whole palette image
                    // centered. The construct a fill color around it which is black and
                    // then set to 45% alpha which on NTSC or PAL shows the edge more
                    // distinctly.
                    //
                    // This could try and use stroke/etc to create a border but that is
                    // harder to pull off as the whole bounding box for this image is
                    // the entire centered frame (which includes the image) and not just
                    // the image itself.
                    ui.vertical_centered(|ui| {
                        egui::Frame::new()
                            .fill(egui::Color32::BLACK)
                            .multiply_with_opacity(0.45)
                            .show(ui, |ui| {
                                ui.image(t);
                            });
                    });
                    ui.separator();
                }
            });
    }
}

impl eframe::App for MyApp {
    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        // Logic copied from https://github.com/emilk/egui/discussions/2858 information.
        match self.render_stage {
            // Give it 2 frames to layout and initialize so we can get the size.
            Stage::PreRender(mut pre_render_cycle) => {
                ui.ctx().request_discard("pre_render");
                self.pre_render(ui.ctx());
                pre_render_cycle -= 1;
                if pre_render_cycle > 0 {
                    self.render_stage = Stage::PreRender(pre_render_cycle);
                } else {
                    self.render_stage = Stage::FirstRender(ui.ctx().globally_used_rect().size());
                }
            }
            // Now do a render and then a resize to correctly shape the final window.
            Stage::FirstRender(size) => {
                ui.ctx().request_discard(r"render");
                self.render(ui);
                self.render_stage = Stage::FirstResize(size);
            }
            Stage::FirstResize(size) => {
                ui.ctx().request_discard("first_resize");
                ui.ctx()
                    .send_viewport_cmd(egui::ViewportCommand::InnerSize(size));
                self.render_stage = Stage::Initialized(size);
            }
            Stage::Initialized(_size) => {
                self.render(ui);
            }
        }
    }
}
