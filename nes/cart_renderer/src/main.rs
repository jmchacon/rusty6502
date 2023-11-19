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
    FontFamily, FontId, TextStyle, TextureHandle, TextureOptions, Ui,
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

    // Details about the left tileset image.
    left_image: Option<egui::Response>,

    // The right side CHR tileset (first 256)
    right: TextureHandle,

    // Details about the right tileset image.
    right_image: Option<egui::Response>,

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

    // The tile we most recently hovered over.
    hovered: Option<usize>,

    // The previous hovered tile.
    last_hovered: Option<usize>,
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
const TILE_X_F: f32 = 8.0;

const TILE_Y: usize = 8;
const TILE_Y_F: f32 = 8.0;
const TILES_PER_ROW: usize = 16;
const TILES_PER_ROW_F: f32 = 16.0;

const ROWS_OF_TILES: usize = 16;
const TOP_BUFFER: usize = 1;
const TOP_BUFFER_F: f32 = 1.0;

const BOTTOM_BUFFER: usize = 1;
const BOTTOM_BUFFER_F: f32 = 1.0;
const LEFT_BUFFER: usize = 1;
const LEFT_BUFFER_F: f32 = 1.0;
const RIGHT_BUFFER: usize = 1;
const RIGHT_BUFFER_F: f32 = 1.0;

// By default this is only 128 pixels wide which is hard to see on any modern display
// so we'll upsize by 2x in each direction.
// TODO(jchacon): Should this be configurable at runtime?
const TILE_MULTIPLIER_X: usize = 2;
const TILE_MULTIPLIER_X_F: f32 = 2.0;

const TILE_MULTIPLIER_Y: usize = 2;
const TILE_MULTIPLIER_Y_F: f32 = 2.0;

const TILE_X_TOTAL_F: f32 = (LEFT_BUFFER_F + TILE_X_F + RIGHT_BUFFER_F) * TILE_MULTIPLIER_X_F;
const TILE_Y_TOTAL_F: f32 = (TOP_BUFFER_F + TILE_Y_F + BOTTOM_BUFFER_F) * TILE_MULTIPLIER_Y_F;

const TILE_LINE_SIZE: usize =
    (TILE_X + LEFT_BUFFER + RIGHT_BUFFER) * TILE_MULTIPLIER_X * TILES_PER_ROW;
const TILE_HEIGHT_SIZE: usize =
    (TILE_Y + TOP_BUFFER + BOTTOM_BUFFER) * TILE_MULTIPLIER_Y * ROWS_OF_TILES;

const TILE_LAYOUT_SIZE: usize = TILE_LINE_SIZE * TILE_HEIGHT_SIZE * BYTES_PER_PIXEL;

const TILES_PER_IMAGE: usize = 256;

struct ChrTiles<'a> {
    tiles: &'a [Vec<Tile>],
    left: &'a mut TextureHandle,
    right: &'a mut TextureHandle,
    selected_pal: &'a usize,
    selected_chr: &'a usize,
    colors: &'a [usize; NUM_COLORS],
    data: &'a mut Box<[u8]>,
    color_source: &'a Vec<Data>,
    hovered: Option<usize>,
}

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

        // Create the various textures we need later on.
        //
        // pals: For each PAL file a 16x4 texture with a block for each color
        // colors_per_pal: For each index of pals a texture with that color block.
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
        let im = egui::ColorImage::from_rgb([TILE_LINE_SIZE, TILE_HEIGHT_SIZE], &data);
        let left = cc.egui_ctx.load_texture(
            "Left CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let im = egui::ColorImage::from_rgb([TILE_LINE_SIZE, TILE_HEIGHT_SIZE], &data);
        let right = cc.egui_ctx.load_texture(
            "Right CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        Self {
            tiles,
            left,
            left_image: None,
            right,
            right_image: None,
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
            hovered: None,
            last_hovered: None,
        }
    }

    // `color_picker` is the modal dialog for chosing a new color when one of
    // the color buttons is selected.
    fn color_picker(&mut self, bidx: usize, ui: &mut Ui) {
        const NUM_PER_ROW: usize = 16;

        let Self {
            tiles: _,
            left: _,
            left_image: _,
            right: _,
            right_image: _,
            pals: _,
            colors_per_pal,
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
            hovered: _,
            last_hovered: _,
        } = self;

        let clrs: &Vec<TextureHandle>;
        unsafe {
            // SAFETY: Unwrap is fine since it's based on selected_pal which
            //         is constrained via the combo box in the main UI.
            clrs = colors_per_pal.get(selected_pal).unwrap_unchecked();
        }

        // Create a 16 x 4 set of colors where each entry is a distinct button
        // rather than just a pallete displayed in the main UI. This way any
        // selection is each to use to show newly selected.
        for row in 0..clrs.len() / NUM_PER_ROW {
            ui.horizontal(|ui| {
                for i in 0..NUM_PER_ROW {
                    let color = row * NUM_PER_ROW + i;
                    if ui.add(egui::Button::image(&clrs[color])).clicked() {
                        // Just record so we can track this on every redraw. It's not
                        // used until Select is pressed later on.
                        *dialog_selected = color;
                    }
                }
            });
            ui.end_row();
        }
        ui.separator();

        // The color selected along with 2 buttons (all spaced out) to select
        // that color or cancel the dialog.
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
    }

    // `main_ui` displays the main UI
    //
    // _________________________
    // | palette selector      |
    // _________________________
    // | PALETTE               |
    // _________________________
    // | BG  C1  C2  C3        |
    // _________________________
    // |chr selector           |
    // _________________________
    // |         |     |       |
    // |   first |     | 2nd   |
    // |    128  |     |  128  |
    // _________________________
    #[allow(clippy::too_many_lines)]
    fn main_ui(&mut self, ui: &mut Ui) {
        let Self {
            tiles: _,
            left,
            left_image,
            right,
            right_image,
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
            hovered,
            last_hovered,
        } = self;
        *frame_count += 1;

        // If a color picker button has been pressed the modal dialog is up
        // so this window is inactive.
        ui.set_enabled(button.is_none());

        // The combo box for determining which palette to display.
        egui::ComboBox::from_label(String::from("Palette"))
            .selected_text(self.pals[*selected_pal].name())
            .show_ui(ui, |ui| {
                ui.style_mut().wrap = Some(false);
                for i in 0..self.pals.len() {
                    ui.selectable_value(selected_pal, i, self.pals[i].name());
                }
            });
        ui.end_row();

        // We already created textures for each PAL so just index and display it.
        ui.image(&self.pals[*selected_pal]);
        ui.separator();

        // Create a new box with 4 buttons for each of the colors.
        ui.horizontal(|ui| {
            for i in 0..NUM_COLORS {
                // 4 buttons spaced across the bottom of the palette showing each color
                // they're selected.

                let text: &TextureHandle;
                // SAFETY: All of these below are restricted to selected_pal range
                //         which is handled in the combo box above so we can just unwrap.
                unsafe {
                    text = &self.colors_per_pal.get(selected_pal).unwrap_unchecked()[colors[i]];
                }

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

        // A combo box to select which CHR page to display.
        // Also indicate how much we've magnified (not currently changeable except by compilation)
        egui::ComboBox::from_label(format!("CHR set ({TILE_MULTIPLIER_X}x magnified)",))
            .selected_text(format!("{selected_chr}"))
            .show_ui(ui, |ui| {
                ui.style_mut().wrap = Some(false);
                for i in 0..self.tiles.len() {
                    ui.selectable_value(selected_chr, i, format!("{i}"));
                }
            });
        ui.end_row();

        // Fill in the selected tile data based on the selected color data
        // from the selected PAL palette data. Only do this when we change
        // relevant data (or this is the first frame).
        if *frame_count == 1
            || *last_selected_pal != *selected_pal
            || *last_selected_chr != *selected_chr
            || *last_colors != *colors
            || *last_hovered != *hovered
        {
            *last_selected_pal = *selected_pal;
            *last_selected_chr = *selected_chr;
            *last_colors = *colors;
            *last_hovered = *hovered;
            Self::create_chr_tiles(&mut ChrTiles {
                tiles: &self.tiles,
                left,
                right,
                selected_pal,
                selected_chr,
                colors,
                data,
                color_source,
                hovered: *hovered,
            });
        }

        // Every frame show the current tilesets with some separation.
        // The above only redraws the textures on actual changes so this is
        // fast since the GPU already has the images generally.

        ui.horizontal(|ui| {
            ui.add_space(10.0);

            *left_image = Some(ui.image(&*left));
            // Space so they fill the space equally. Determined emperically.
            // TODO(jchacon): Should be a way to auto lay this out?
            ui.add_space(250.0);
            *right_image = Some(ui.image(&*right));
            ui.add_space(10.0);
        });

        ui.input(|i| {
            if let Some(hp) = i.pointer.hover_pos() {
                #[allow(clippy::unwrap_used)]
                let left_tile = Self::tile_num(
                    left_image.as_ref().unwrap().rect,
                    hp,
                    TILE_X_TOTAL_F,
                    TILE_Y_TOTAL_F,
                    TILES_PER_ROW_F,
                );
                #[allow(clippy::unwrap_used)]
                let right_tile = Self::tile_num(
                    right_image.as_ref().unwrap().rect,
                    hp,
                    TILE_X_TOTAL_F,
                    TILE_Y_TOTAL_F,
                    TILES_PER_ROW_F,
                );
                match (left_tile, right_tile) {
                    (None, None) => *hovered = None,
                    (None, Some(mut t)) => {
                        t += TILES_PER_IMAGE;
                        *hovered = Some(t);
                    }
                    (Some(t), None) => {
                        *hovered = Some(t);
                    }
                    (Some(_), Some(_)) => panic!("Hovering over both images at once?"),
                }
            }
        });
    }

    fn tile_num(
        window: egui::Rect,
        hover: egui::Pos2,
        tile_x: f32,
        tile_y: f32,
        num_per_row: f32,
    ) -> Option<usize> {
        let min = window.min;
        let max = window.max;
        // Make sure hover is actually inside the box. For a fast moving
        // pointer it might register just outside.
        if hover.x < min.x || hover.x >= max.x || hover.y < min.y || hover.y >= max.y {
            return None;
        }

        // 10.0-330.0 , 269.3-589.3
        // loc 242.7, 537.3
        let x_tile = hover.x - min.x; // 242.7 - 10.0 == 232.7
        let y_tile = hover.y - min.y; // 537.3 - 330.0 == 207.3
        let tx = (x_tile / tile_x).floor(); // 12
        let ty = (y_tile / tile_y).floor(); // 9

        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        Some((tx + (ty * num_per_row)) as usize)
    }

    // `create_chr_tiles` does all of the heavy lifting to take the 256 tiles
    // in the CHR page referenced and render them via the current color sections.
    // This then resets the given left and right textures with the new images.
    fn create_chr_tiles(chrtiles: &mut ChrTiles) {
        let tiles = chrtiles.tiles;
        let selected_chr = chrtiles.selected_chr;
        let selected_pal = chrtiles.selected_pal;
        let colors = chrtiles.colors;
        let color_source = chrtiles.color_source;

        // Mass fill the slice with white. We can just use one index here
        // since white is the same value for all 3.
        chrtiles.data.fill(egui::Color32::WHITE.r());

        // Once we get over 256 tiles we move to the other image.
        let mut do_left = true;

        for (mut loc, t) in tiles[*selected_chr].iter().enumerate() {
            let orig = loc;
            if loc >= TILES_PER_IMAGE {
                // Reset to all WHITE again so hover from left side doesn't
                // carry over.
                if do_left {
                    chrtiles.data.fill(egui::Color32::WHITE.r());
                    do_left = false;
                }
                loc -= TILES_PER_IMAGE;
            }

            // First figure out the row we're on and the first entry for it's
            // first pixel.
            let row_start_base = loc / TILES_PER_ROW
                * (TILE_Y + TOP_BUFFER + BOTTOM_BUFFER)
                * TILE_MULTIPLIER_Y
                * TILE_LINE_SIZE
                * BYTES_PER_PIXEL;

            // Now move N boxes over to find the box start pixel.
            let box_start_base = row_start_base
                + (TILE_X + LEFT_BUFFER + RIGHT_BUFFER)
                    * TILE_MULTIPLIER_X
                    * (loc % TILES_PER_ROW)
                    * BYTES_PER_PIXEL;

            // Now also adjust it N pixels down and over to account for buffers.
            // This way the painting below just deals with correct offsets into
            // the tile data.
            let box_start = box_start_base
                + (TOP_BUFFER * TILE_MULTIPLIER_Y * TILE_LINE_SIZE * BYTES_PER_PIXEL)
                + (LEFT_BUFFER * TILE_MULTIPLIER_X * BYTES_PER_PIXEL);

            // Using box_start_base fill in GREY on each row if this is the tile
            // be hovered over. The rest of the tile painting below will write over the
            // rest so this just becomes our outline.
            if chrtiles.hovered == Some(orig) {
                for y in 0..(TILE_Y + TOP_BUFFER + BOTTOM_BUFFER) * TILE_MULTIPLIER_Y {
                    let row = box_start_base + y * TILE_LINE_SIZE * BYTES_PER_PIXEL;
                    for x in 0..(TILE_X + LEFT_BUFFER + RIGHT_BUFFER) * TILE_MULTIPLIER_X {
                        chrtiles.data[row + x * BYTES_PER_PIXEL] = egui::Color32::GRAY.r();
                        chrtiles.data[row + x * BYTES_PER_PIXEL + 1] = egui::Color32::GRAY.g();
                        chrtiles.data[row + x * BYTES_PER_PIXEL + 2] = egui::Color32::GRAY.b();
                    }
                }
            }

            // For each actual tile use the offsets computed above to just iterate
            // through each 8x8 tiles (blown up as needed).
            for y in 0..TILE_Y {
                for yi in 0..TILE_MULTIPLIER_Y {
                    // Finally for each line adjust by the row we're on for each line.
                    let y_off =
                        box_start + (y * TILE_MULTIPLIER_Y + yi) * TILE_LINE_SIZE * BYTES_PER_PIXEL;
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
                            chrtiles.data[y_off + off + start] = color.r;
                            chrtiles.data[y_off + off + start + 1] = color.g;
                            chrtiles.data[y_off + off + start + 2] = color.b;
                        }
                    }
                }
            }

            let im = egui::ColorImage::from_rgb([TILE_LINE_SIZE, TILE_HEIGHT_SIZE], chrtiles.data);

            if do_left {
                chrtiles.left.set(im, TextureOptions::default());
            } else {
                chrtiles.right.set(im, TextureOptions::default());
            }
        }
    }
}

const BUTTONS: [&str; NUM_COLORS] = ["Background", "Color 1", "Color 2", "Color 3"];

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        // If a color picker button has been selected display the dialog.
        if let Some(bidx) = self.button {
            egui::Window::new("Color picker").show(ctx, |ui| self.color_picker(bidx, ui));
        }

        // Always show the main window.
        egui::CentralPanel::default()
            .frame(egui::Frame::none().fill(egui::Color32::GRAY))
            .show(ctx, |ui| self.main_ui(ui));
        frame.set_window_size(ctx.used_size());
    }
}
