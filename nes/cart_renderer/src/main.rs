//! `cart_renderer` will take the given set of PAL files and an NES cart
//! file and render the CHR sections.
//!
//! Colors can be selected by choosing a palette and then assigning the
//! 4 colors (background and 1-3) from it.
use std::fs::read;

use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use egui::{
    ahash::{HashMap, HashMapExt},
    FontFamily, FontId, TextStyle, TextureHandle, TextureOptions,
};
use nes_chr::Tile;
use nes_pal::{parse_pal, Color};
use nes_pal_gui::texture_from_palette;
use std::path::Path;

/// `cart_renderer` will load the given PAL files and the NES and render the CHR sections
/// along with color selection.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(
        help = "Filenames containing .pal data (can be specified N times)",
        long
    )]
    pal: Vec<String>,

    #[arg(help = "Filename for cart in INES format")]
    filename: String,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).
    let options = eframe::NativeOptions::default();

    if args.pal.is_empty() {
        return Err(eyre!("Must supply at least one PAL filename"));
    }

    let mut colors = Vec::new();
    for f in &args.pal {
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

    let bytes = read(args.filename)?;

    // Parse the NES file and then for each CHR ROM make this a processed
    // Tile Vec we keep. Each 8k block equates to 2x256 set of tiles.
    let mut tiles = Vec::new();
    for t in &ines::parse(&bytes)?.chr {
        let tile = nes_chr::map_chr_rom(t)?;
        tiles.push(tile);
    }

    let res = eframe::run_native(
        "NES file CHR renderer",
        options,
        Box::new(|cc| Box::new(MyApp::new(cc, colors, tiles))),
    );

    if let Err(e) = res {
        return Err(eyre!("EGUI error: {e:?}"));
    }

    Ok(())
}

struct Data {
    filename: String,
    colors: Vec<Color>,
}

struct MyApp {
    // The parsed tile data from the NES file.
    tiles: Vec<Vec<Tile>>,

    // The left side CHR tileset (first 256)
    left: TextureHandle,

    // The right side CHR tileset (first 256)
    right: TextureHandle,

    // The textures which show an entire PAL pallete at once.
    pals: Vec<TextureHandle>,

    // Per PAL (indexed from `pal`) provide a 40x40 block texture of each color.
    colors_per_pal: HashMap<usize, Vec<TextureHandle>>,

    // The current PAL texture handle used to display the palette in use. Index
    // is into `pals`.
    selected_pal: usize,

    // The index of the selected CHR block we're viewing
    selected_chr: usize,

    // If a color button is pressed and then if so which one.
    button: Option<usize>,

    // For each color the index into the current PAL palette it should be using.
    colors: [usize; 4],

    // If displaying the modal selection dialog which color is currently picked.
    dialog_selected: usize,

    // The input we setup to change the tile texture each frame.
    data: Box<[u8]>,

    // The original color data parsed from each PAL file.
    color_source: Vec<Data>,

    // If any of these are different from selected_XXX then the tile texture
    // should get redrawn to update.
    last_selected_pal: usize,
    last_selected_chr: usize,
    last_colors: [usize; 4],

    // Number of frames we've done.
    frame_count: usize,
}

const PALETTE_SQ_X: usize = 40;
const PALETTE_SQ_Y: usize = 40;

const PALETTE_SQ_XY: usize = PALETTE_SQ_X * PALETTE_SQ_Y;
const BYTES_PER_PIXEL: usize = 3;
const PALETTE_SQ_SIZE: usize = PALETTE_SQ_XY * BYTES_PER_PIXEL;

const NUM_COLORS: usize = 4;

// The common NTSC and PAL palettes have black as the last entry
// and white as the one on the beginning of the last row so default to those.
const DEFAULT_BACKGROUND: usize = 0x3F;
const DEFAULT_FOREGROUND: usize = 0x30;

const TILE_X: usize = 8;
const TILE_Y: usize = 8;
const TILES_PER_ROW: usize = 16;
const ROWS_OF_TILES: usize = 16;

// By default this is only 128 pixels wide which is hard to see on any modern display
// so we'll upsize by Xx in each direction.
const TILE_MULTIPLIER_X: usize = 2;
const TILE_MULTIPLIER_Y: usize = 2;
const TILE_LINE_SIZE: usize = TILE_X * TILE_MULTIPLIER_X * TILES_PER_ROW;

const TILE_LAYOUT_SIZE: usize = TILE_X
    * TILE_MULTIPLIER_X
    * TILE_Y
    * TILE_MULTIPLIER_Y
    * TILES_PER_ROW
    * ROWS_OF_TILES
    * BYTES_PER_PIXEL;

impl MyApp {
    #[allow(clippy::needless_pass_by_value)]
    fn new(cc: &eframe::CreationContext<'_>, datas: Vec<Data>, tiles: Vec<Vec<Tile>>) -> Self {
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

        let mut pals = Vec::new();
        let mut colors_per_pal = HashMap::new();

        for (i, d) in datas.iter().enumerate() {
            pals.push(texture_from_palette(cc, &d.filename, &d.colors));
            for (ci, c) in d.colors.iter().enumerate() {
                let mut data: [u8; PALETTE_SQ_SIZE] = [0; PALETTE_SQ_SIZE];
                for xy in 0..PALETTE_SQ_XY {
                    let start = xy * BYTES_PER_PIXEL;
                    data[start] = c.r;
                    data[start + 1] = c.g;
                    data[start + 2] = c.b;
                }

                let im = egui::ColorImage::from_rgb([PALETTE_SQ_X, PALETTE_SQ_Y], &data);
                let text = cc.egui_ctx.load_texture(
                    format!("Color: {ci}"),
                    egui::ImageData::Color(im.into()),
                    TextureOptions::default(),
                );
                colors_per_pal
                    .entry(i)
                    .and_modify(|v: &mut Vec<TextureHandle>| v.push(text.clone()))
                    .or_insert(vec![text]);
            }
        }

        // Fill in the initial set of tiles layout with all white.
        // Additional tiles can always be used later with a combo box to select but we need a
        // base texture to update on every draw. i.e. the first update will select the actual
        // correct entries for this.
        let data = vec![0xFF; TILE_LAYOUT_SIZE].into_boxed_slice();
        let im = egui::ColorImage::from_rgb(
            [
                TILE_X * TILE_MULTIPLIER_X * TILES_PER_ROW,
                TILE_Y * TILE_MULTIPLIER_Y * ROWS_OF_TILES,
            ],
            &data,
        );
        let left = cc.egui_ctx.load_texture(
            "Left CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let im = egui::ColorImage::from_rgb(
            [
                TILE_X * TILE_MULTIPLIER_X * TILES_PER_ROW,
                TILE_Y * TILE_MULTIPLIER_Y * ROWS_OF_TILES,
            ],
            &data,
        );
        let right = cc.egui_ctx.load_texture(
            "Right CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        Self {
            tiles,
            left,
            right,
            pals,
            colors_per_pal,
            selected_pal: 0,
            selected_chr: 0,
            button: None,
            colors: [
                DEFAULT_BACKGROUND,
                DEFAULT_FOREGROUND,
                DEFAULT_FOREGROUND,
                DEFAULT_FOREGROUND,
            ],
            dialog_selected: 0,
            data,
            color_source: datas,
            last_selected_pal: 0,
            last_selected_chr: 0,
            last_colors: [0; 4],
            frame_count: 0,
        }
    }
}

const BUTTONS: [&str; NUM_COLORS] = ["Background", "Color 1", "Color 2", "Color 3"];

impl eframe::App for MyApp {
    #[allow(clippy::too_many_lines)]
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        if let Some(bidx) = self.button {
            egui::Window::new("Color picker").show(ctx, |ui| {
                const NUM_PER_ROW: usize = 16;

                let Self {
                    tiles: _,
                    left: _,
                    right: _,
                    pals: _,
                    colors_per_pal: _,
                    selected_pal,
                    selected_chr: _,
                    button,
                    colors,
                    dialog_selected,
                    data: _,
                    color_source: _,
                    last_selected_pal: _,
                    last_selected_chr: _,
                    last_colors: _,
                    frame_count: _,
                } = self;

                // SAFETY: Unwrap is fine since it's based on selected_pal which
                //         is constrained via the combo box in the main UI.
                #[allow(clippy::unwrap_used)]
                let clrs = self.colors_per_pal.get(selected_pal).unwrap();

                for row in 0..clrs.len() / NUM_PER_ROW {
                    ui.horizontal(|ui| {
                        for i in 0..NUM_PER_ROW {
                            let idx = row * NUM_PER_ROW + i;
                            if ui.add(egui::Button::image(&clrs[idx])).clicked() {
                                *dialog_selected = idx;
                            }
                        }
                    });
                    ui.end_row();
                }
                ui.separator();
                ui.horizontal(|ui| {
                    ui.image(&clrs[*dialog_selected]);
                    ui.add_space(100.0);
                    if ui.button("Select").clicked() {
                        colors[bidx] = *dialog_selected;
                        *button = None;
                    }
                    if ui.button("Cancel").clicked() {
                        *button = None;
                    }
                });
            });
        }
        egui::CentralPanel::default()
            .frame(egui::Frame::none().fill(egui::Color32::GRAY))
            .show(ctx, |ui| {
                let Self {
                    tiles: _,
                    left,
                    right,
                    pals: _,
                    colors_per_pal: _,
                    selected_pal,
                    selected_chr,
                    button,
                    colors,
                    dialog_selected,
                    data,
                    color_source,
                    last_selected_pal,
                    last_selected_chr,
                    last_colors,
                    frame_count,
                } = self;
                *frame_count += 1;

                ui.set_enabled(button.is_none());
                egui::ComboBox::from_label(String::from("Palette"))
                    .selected_text(self.pals[*selected_pal].name())
                    .show_ui(ui, |ui| {
                        ui.style_mut().wrap = Some(false);
                        for i in 0..self.pals.len() {
                            ui.selectable_value(selected_pal, i, self.pals[i].name());
                        }
                    });
                ui.end_row();
                ui.image(&self.pals[*selected_pal]);
                ui.separator();

                ui.horizontal(|ui| {
                    // SAFETY: All of these below are restricted to selected_pal range
                    //         which is handled in the combo box above so we can just unwrap.
                    for i in 0..NUM_COLORS {
                        // 4 buttons spaced across the bottom of the palette showing each color
                        // they're selected.
                        #[allow(clippy::unwrap_used)]
                        let text = &self.colors_per_pal.get(selected_pal).unwrap()[colors[i]];

                        if ui
                            .add(egui::Button::image_and_text(text, BUTTONS[i]))
                            .clicked()
                        {
                            *button = Some(i);
                            *dialog_selected = colors[i];
                        }
                        // Space them out a bit so they stretch across the palette.
                        // Determined emperically by adjusting until it lines up visually.
                        // TODO(jchacon): Should be a way to auto lay this out?
                        ui.add_space(78.0);
                    }
                });
                ui.end_row();
                ui.separator();
                egui::ComboBox::from_label(format!("CHR set ({TILE_MULTIPLIER_X}x magnified)",))
                    .selected_text(format!("{selected_chr}"))
                    .show_ui(ui, |ui| {
                        ui.style_mut().wrap = Some(false);
                        for i in 0..self.tiles.len() {
                            ui.selectable_value(selected_chr, i, format!("{i}"));
                        }
                    });
                ui.end_row();

                // File in the selected tile data based on the selected color data
                // from the selected PAL palette data.
                // TODO(jchacon): Track only if selected_chr/selected_pal/colors changed to bother to do this.
                //                Otherwise we're needly moving lots of memory around everytime.
                if *frame_count == 1
                    || *last_selected_pal != *selected_pal
                    || *last_selected_chr != *selected_chr
                    || *last_colors != *colors
                {
                    *last_selected_pal = *selected_pal;
                    *last_selected_chr = *selected_chr;
                    *last_colors = *colors;
                    for (mut loc, t) in self.tiles[*selected_chr].iter().enumerate() {
                        // Once we get over 256 tiles we move to the other image.
                        let mut do_left = true;
                        if loc >= self.tiles[*selected_chr].len() / 2 {
                            do_left = false;
                            loc /= 2;
                        }
                        // First figure out the row we're on and the first entry for it's
                        // first pixel.
                        let row_start = loc / TILES_PER_ROW
                            * TILE_Y
                            * TILE_MULTIPLIER_Y
                            * TILE_LINE_SIZE
                            * BYTES_PER_PIXEL;

                        // Now move N boxes over to find the box start pixel.
                        let box_start = row_start
                            + TILE_X * TILE_MULTIPLIER_X * (loc % TILES_PER_ROW) * BYTES_PER_PIXEL;
                        for y in 0..TILE_Y {
                            for yi in 0..TILE_MULTIPLIER_Y {
                                // Finally for each line adjust by the row we're on for each line.
                                let y_off = box_start
                                    + (y * TILE_MULTIPLIER_Y + yi)
                                        * TILE_LINE_SIZE
                                        * BYTES_PER_PIXEL;
                                for x in 0..TILE_X {
                                    // Each x start has to be adjusted by RGB to get the final entry.
                                    let start = x * TILE_MULTIPLIER_X * BYTES_PER_PIXEL;

                                    // Now lookup the tile data which is in range 0..NUM_COLORS
                                    // Index that into colors to get the PAL entry.
                                    // Now find that in the selected PAL to get the final RGB values.
                                    let td = t.data[y * TILE_Y + x];
                                    let col = colors[usize::from(td)];
                                    let color = &color_source[*selected_pal].colors[col];

                                    for i in 0..TILE_MULTIPLIER_X {
                                        let off = i * BYTES_PER_PIXEL;
                                        data[y_off + off + start] = color.r;
                                        data[y_off + off + start + 1] = color.g;
                                        data[y_off + off + start + 2] = color.b;
                                    }
                                }
                            }
                        }
                        let im = egui::ColorImage::from_rgb(
                            [
                                TILE_X * TILE_MULTIPLIER_X * TILES_PER_ROW,
                                TILE_Y * TILE_MULTIPLIER_Y * ROWS_OF_TILES,
                            ],
                            data,
                        );

                        if do_left {
                            left.set(im, TextureOptions::default());
                        } else {
                            right.set(im, TextureOptions::default());
                        }
                    }
                }
                ui.horizontal(|ui| {
                    ui.image(&*left);
                    // Space so they fill the space equally. Determined emperically.
                    // TODO(jchacon): Should be a way to auto lay this out?
                    ui.add_space(123.0);
                    ui.image(&*right);
                });
            });
        frame.set_window_size(ctx.used_size());
    }
}
