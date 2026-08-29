//! `cart_renderer` will take the given set of PAL files and an NES cart
//! file and render the CHR sections.
//!
//! Colors can be selected by choosing a palette and then assigning the
//! 4 colors (background and 1-3) from it.
use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use egui::{
    FontFamily, FontId, Pos2, Rect, TextStyle, TextWrapMode, TextureHandle, TextureOptions, Ui,
    Vec2,
};
use nes_chr::Tile;
use nes_pal::{parse_pal, Color};
use nes_pal_gui::texture_from_palette;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Write;
use std::fs::read;
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
        Box::new(|cc| Ok(Box::new(MyApp::new(cc, colors, tiles)))),
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
enum Stage {
    PreRender(isize),
    FirstRender(Vec2),
    FirstResize(Vec2),
    Initialized(Vec2),
}

struct MyApp {
    // The stage of rendering so we can resize correctly.
    render_stage: Stage,

    // The various items defining sizes for all the tiles, etc.
    tile_draw_data: TileDrawData,

    // The parsed tile data from the NES file.
    tiles: Vec<Vec<Tile>>,

    // Details about the palette image
    palette: Option<egui::Response>,

    // The left side CHR tileset (first 256)
    left: TextureHandle,

    // Details about the left tileset image.
    left_image: Option<egui::Response>,

    // The right side CHR tileset (first 256)
    right: TextureHandle,

    // Details about the right tileset image.
    right_image: Option<egui::Response>,

    // The single tile in between the 2 tilesets.
    single: TextureHandle,

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

    // The tile data when we display a large single tile between the panels.
    tile_data: Box<[u8]>,

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

    // The previous hovered tile. Used to trigger image redraws once this differs
    // from hovered.
    last_hovered: Option<usize>,

    // If button 1 was clicked hovering is locked. Cleared on button 3.
    hover_locked: bool,

    // The tile location for the large middle tile from the tileset.
    single_title: String,

    // If non-blank is the hover text displayed over the palette.
    palette_hover: String,
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

// All the data used to build up the math needed to operate with the tiles
// we display. Each tile is 8x8 but we want a 1 pixel border around each one.
// This is then 10x10 (which makes manual math simpler) and when multiplied out
// is a 20x20 tile image. Various iterations later when creating the tile image
// will need various combinations of these values and sometimes as floats
// due to how egui works with pixels.
//
// The single tile varieties are there for displaying the 8x tile selected during
// hover over.
#[derive(Default, Debug)]
struct TileDrawData {
    tile_x: usize,
    tile_x_f: f32,
    tile_y: usize,
    tile_y_f: f32,
    tiles_per_row: usize,
    tiles_per_row_f: f32,
    row_of_tiles: usize,
    top_buffer: usize,
    top_buffer_f: f32,
    bottom_buffer: usize,
    bottom_buffer_f: f32,
    left_buffer: usize,
    left_buffer_f: f32,
    right_buffer: usize,
    right_buffer_f: f32,
    single_tile_multiplier_x: usize,
    tile_multiplier_x: usize,
    tile_multiplier_x_f: f32,

    single_tile_multiplier_y: usize,
    tile_multiplier_y: usize,
    tile_multiplier_y_f: f32,

    // Single tile has no border since we want to just see it blown up exactly.
    single_tile_x_total: usize,
    tile_x_total: usize,
    tile_x_total_f: f32,
    single_tile_y_total: usize,
    tile_y_total: usize,
    tile_y_total_f: f32,

    single_tile_image_buffer: f32,

    tile_line_size: usize,
    tile_height_size: usize,

    tile_layout_size: usize,

    single_tile_layout_size: usize,
    tiles_per_image: usize,
}

impl TileDrawData {
    fn new() -> Self {
        let mut s = Self {
            tile_x: 8,
            tile_x_f: 8.0,
            tile_y: 8,
            tile_y_f: 8.0,
            tiles_per_row: 16,
            tiles_per_row_f: 16.0,
            row_of_tiles: 16,
            top_buffer: 1,
            top_buffer_f: 1.0,
            bottom_buffer: 1,
            bottom_buffer_f: 1.0,
            left_buffer: 1,
            left_buffer_f: 1.0,
            right_buffer: 1,
            right_buffer_f: 1.0,
            single_tile_multiplier_x: 8,
            tile_multiplier_x: 2,
            tile_multiplier_x_f: 2.0,
            single_tile_multiplier_y: 8,
            tile_multiplier_y: 2,
            tile_multiplier_y_f: 2.0,
            tiles_per_image: 256,
            ..TileDrawData::default()
        };
        s.single_tile_x_total = s.tile_x * s.single_tile_multiplier_x;
        s.tile_x_total = (s.left_buffer + s.tile_x + s.right_buffer) * s.tile_multiplier_x;
        s.tile_x_total_f =
            (s.left_buffer_f + s.tile_x_f + s.right_buffer_f) * s.tile_multiplier_x_f;
        s.single_tile_y_total = s.tile_y * s.single_tile_multiplier_y;
        s.tile_y_total = (s.top_buffer + s.tile_y + s.bottom_buffer) * s.tile_multiplier_y;
        s.tile_y_total_f =
            (s.top_buffer_f + s.tile_y_f + s.bottom_buffer_f) * s.tile_multiplier_y_f;
        // We know the cast is safe since it's constrained to small values.
        #[allow(clippy::cast_possible_truncation)]
        let buf = 12.5 * f32::from(s.single_tile_multiplier_x as u8);
        s.single_tile_image_buffer = buf;
        s.tile_line_size = s.tile_x_total * s.tiles_per_row;
        s.tile_height_size = s.tile_y_total * s.row_of_tiles;
        s.tile_layout_size = s.tile_line_size * s.tile_height_size * BYTES_PER_PIXEL;
        s.single_tile_layout_size = s.single_tile_x_total * s.single_tile_y_total * BYTES_PER_PIXEL;
        s
    }

    fn _update_multiplier(&mut self, tile_multiplier: usize, tile_multiplier_f: f32) {
        self.tile_multiplier_x = tile_multiplier;
        self.tile_multiplier_x_f = tile_multiplier_f;
        self.tile_multiplier_y = tile_multiplier;
        self.tile_multiplier_y_f = tile_multiplier_f;

        self.tile_x_total =
            (self.left_buffer + self.tile_x + self.right_buffer) * self.tile_multiplier_x;
        self.tile_x_total_f =
            (self.left_buffer_f + self.tile_x_f + self.right_buffer_f) * self.tile_multiplier_x_f;
        self.tile_y_total =
            (self.top_buffer + self.tile_y + self.bottom_buffer) * self.tile_multiplier_y;
        self.tile_y_total_f =
            (self.top_buffer_f + self.tile_y_f + self.bottom_buffer_f) * self.tile_multiplier_y_f;

        self.tile_line_size = self.tile_x_total * self.tiles_per_row;
        self.tile_height_size = self.tile_y_total * self.row_of_tiles;
        self.tile_layout_size = self.tile_line_size * self.tile_height_size * BYTES_PER_PIXEL;
    }

    fn _update_single_tile_multiplier(&mut self, single_tile_multiplier: usize) {
        self.single_tile_multiplier_x = single_tile_multiplier;
        self.single_tile_multiplier_y = single_tile_multiplier;

        self.single_tile_x_total = self.tile_x * self.single_tile_multiplier_x;
        self.single_tile_y_total = self.tile_y * self.single_tile_multiplier_y;
        self.single_tile_layout_size =
            self.single_tile_x_total * self.single_tile_y_total * BYTES_PER_PIXEL;

        // We know the cast is safe since it's constrained to small values.
        #[allow(clippy::cast_possible_truncation)]
        let buf = 12.5 * f32::from(self.single_tile_multiplier_x as u8);
        self.single_tile_image_buffer = buf;
    }
}

// The labels for the 4 buttons used to select colors.
const BUTTONS: [&str; NUM_COLORS] = ["Background", "Color 1", "Color 2", "Color 3"];

// All the data needed for building up the chr tile images and setting new textures.
struct ChrTiles<'a> {
    tiles: &'a [Vec<Tile>],
    left: &'a mut TextureHandle,
    right: &'a mut TextureHandle,
    single: &'a mut TextureHandle,
    selected_pal: &'a usize,
    selected_chr: &'a usize,
    colors: &'a [usize; NUM_COLORS],
    data: &'a mut Box<[u8]>,
    color_source: &'a Vec<Data>,
    hovered: Option<usize>,
    tile_data: &'a mut Box<[u8]>,
}

// When drawing a given tile all the data needed to make that happen.
struct DrawData<'a> {
    box_start: usize,
    mult_x: usize,
    mult_y: usize,
    tile_line_size: usize,
    tile: &'a Tile,
    colors: &'a [usize],
    color_source: &'a Vec<Data>,
    selected_pal: usize,
}

impl MyApp {
    fn new(cc: &eframe::CreationContext<'_>, datas: Vec<Data>, tiles: Vec<Vec<Tile>>) -> Self {
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

        let tdd = TileDrawData::new();
        // Fill in the initial set of tiles layout with all white.
        // Additional tiles can always be used later with a combo box to select but we need a
        // base texture to update on every draw. i.e. the first update will select the actual
        // correct entries for this.
        let data = vec![egui::Color32::WHITE.r(); tdd.tile_layout_size].into_boxed_slice();
        let im = egui::ColorImage::from_rgb([tdd.tile_line_size, tdd.tile_height_size], &data);
        let left = cc.egui_ctx.load_texture(
            "Left CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let im = egui::ColorImage::from_rgb([tdd.tile_line_size, tdd.tile_height_size], &data);
        let right = cc.egui_ctx.load_texture(
            "Right CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let tile_data =
            vec![egui::Color32::WHITE.r(); tdd.single_tile_layout_size].into_boxed_slice();
        let im = egui::ColorImage::from_rgb(
            [tdd.single_tile_x_total, tdd.single_tile_y_total],
            &tile_data,
        );
        let single = cc.egui_ctx.load_texture(
            "Single tile",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );

        Self {
            render_stage: Stage::PreRender(2_isize),
            tile_draw_data: tdd,
            tiles,
            palette: None,
            left,
            left_image: None,
            right,
            right_image: None,
            single,
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
            tile_data,
            color_source: datas,
            last_selected_pal: 0,
            last_selected_chr: 0,
            last_colors: [0; NUM_COLORS],
            frame_count: 0,
            hovered: None,
            last_hovered: None,
            hover_locked: false,
            single_title: String::with_capacity(16),
            palette_hover: String::with_capacity(4),
        }
    }

    // `color_picker` is the modal dialog for chosing a new color when one of
    // the color buttons is selected.
    fn color_picker(&mut self, bidx: usize, ui: &mut Ui) {
        const NUM_PER_ROW: usize = 16;

        let Self {
            render_stage: _,
            tile_draw_data: _,
            tiles: _,
            palette: _,
            left: _,
            left_image: _,
            right: _,
            right_image: _,
            single: _,
            pals: _,
            colors_per_pal,
            selected_pal,
            selected_chr: _,
            button,
            colors,
            dialog_selected,
            data: _,
            tile_data: _,
            color_source: _,
            last_selected_pal: _,
            last_selected_chr: _,
            last_colors: _,
            frame_count: _,
            hovered: _,
            last_hovered: _,
            hover_locked: _,
            single_title: _,
            palette_hover: _,
        } = self;

        let Some(cpl) = colors_per_pal.get(selected_pal) else {
            // This can't fail since it's based on selected_pal which
            // is constrained via the combo box in the main UI.
            panic!("Invalid PAL lookup");
        };
        let clrs = cpl;

        // Create a 16 x 4 set of colors where each entry is a distinct button
        // rather than just a pallete displayed in the main UI. This way any
        // selection is each to use to show newly selected.
        for row in 0..clrs.len() / NUM_PER_ROW {
            ui.horizontal(|ui| {
                for i in 0..NUM_PER_ROW {
                    let color = row * NUM_PER_ROW + i;
                    let br = ui.add(egui::Button::image(&clrs[color]));
                    if br.clicked() {
                        // Just record so we can track this on every redraw. It's not
                        // used until Select is pressed later on.
                        *dialog_selected = color;
                    }
                    br.on_hover_text_at_pointer(format!("{color:#04X}"));
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
    // |   first |  8x | 2nd   |
    // |    128  | tile|  128  |
    // _________________________
    #[allow(clippy::too_many_lines)]
    fn main_ui(&mut self, ui: &mut Ui) {
        let Self {
            render_stage: _,
            tile_draw_data,
            tiles: _,
            palette,
            left,
            left_image,
            right,
            right_image,
            single,
            pals: _,
            colors_per_pal: _,
            selected_pal,
            selected_chr,
            button,
            colors,
            dialog_selected,
            data,
            tile_data,
            color_source,
            last_selected_pal,
            last_selected_chr,
            last_colors,
            frame_count,
            hovered,
            last_hovered,
            hover_locked,
            single_title,
            palette_hover,
        } = self;
        *frame_count += 1;

        // If a color picker button has been pressed the modal dialog is up
        // so this window is inactive.
        if !button.is_none() {
            ui.disable();
        }

        // Make the top area which is the palette box and the buttons for it.
        egui::Panel::top("palette panel").show(ui, |ui| {
            // The combo box for determining which palette to display.
            egui::ComboBox::from_label(String::from("Palette"))
                .selected_text(self.pals[*selected_pal].name())
                .show_ui(ui, |ui| {
                    ui.style_mut().wrap_mode = Some(TextWrapMode::Extend);
                    for i in 0..self.pals.len() {
                        ui.selectable_value(selected_pal, i, self.pals[i].name());
                    }
                });
            ui.end_row();

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
                        // Capture the response so we can use it below for hover.
                        *palette = Some(
                            ui.image(&self.pals[*selected_pal])
                                .on_hover_text_at_pointer(palette_hover.as_str()),
                        );
                    });
            });
            ui.separator();

            // Create a new box with 4 buttons for each of the colors.
            ui.horizontal(|ui| {
                // Chop this into the same number of columns and then they get
                // equally spaced across the frame.
                ui.columns(NUM_COLORS, |columns| {
                    for i in 0..NUM_COLORS {
                        columns[i].vertical_centered(|ui| {
                            // 4 buttons spaced across the bottom of the palette showing each color
                            // they're selected.
                            let Some(cpl) = self.colors_per_pal.get(selected_pal) else {
                                // This can't fail since it's based on selected_pal which
                                // is constrained via the combo box in the main UI.
                                panic!("Invalid PAL lookup");
                            };
                            let text = &cpl[colors[i]];

                            if ui
                                .add(egui::Button::image_and_text(text, BUTTONS[i]))
                                .clicked()
                            {
                                *button = Some(i);
                                *dialog_selected = colors[i];
                            }
                        });
                    }
                });
            });
            ui.end_row();
            ui.separator();
        });

        // A combo box to select which CHR page to display.
        // Also indicate how much we've magnified (not currently changeable except by compilation)
        egui::ComboBox::from_label(format!(
            "CHR set ({}x magnified)",
            tile_draw_data.tile_multiplier_x
        ))
        .selected_text(format!("{selected_chr}"))
        .show_ui(ui, |ui| {
            ui.style_mut().wrap_mode = Some(TextWrapMode::Extend);
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
            Self::create_chr_tiles(
                &mut ChrTiles {
                    tiles: &self.tiles,
                    left,
                    right,
                    single,
                    selected_pal,
                    selected_chr,
                    colors,
                    data,
                    color_source,
                    hovered: *hovered,
                    tile_data,
                },
                tile_draw_data,
            );
        }

        // Every frame show the current tilesets with some separation.
        // The above only redraws the textures on actual changes so this is
        // fast since the GPU already has the images generally.
        ui.horizontal(|ui| {
            ui.add_space(10.0);

            let hover = "Left button to lock\nRight button to clear";
            *left_image = Some(ui.image(&*left).on_hover_text(hover));
            // Space so they fill the space equally.
            // Determined emperically as a function of the multiplying size.
            ui.add_space(tile_draw_data.single_tile_image_buffer);
            ui.vertical(|ui| {
                ui.heading(&*single_title);
                ui.image(&*single);
            });
            ui.add_space(tile_draw_data.single_tile_image_buffer);
            *right_image = Some(ui.image(&*right).on_hover_text(hover));
            ui.add_space(10.0);
        });

        ui.input(|i| {
            // If we're not enabled this means the modal is up so we don't want
            // tile state changing because we overlap that portion.
            if !ui.is_enabled() {
                return;
            }

            // Get the bounding boxes for both tile images so mapping to a tile
            // can be done below.
            let Some(r) = left_image.as_ref() else {
                panic!("left image invalid?");
            };
            let left_rect = r.rect;
            let Some(r) = right_image.as_ref() else {
                panic!("right image invalid?");
            };
            let right_rect = r.rect;

            let Some(pal) = palette.as_ref() else {
                panic!("palette image invalid?");
            };
            let palette_rect = pal.rect;

            if let Some(hp) = i.pointer.hover_pos() {
                // Check if we're inside the palette box and if so tool tip the
                // square we're over (hex value);
                palette_hover.clear();
                if let Some(t) = Self::tile_num(
                    palette_rect,
                    hp,
                    nes_pal_gui::WIDTH_F,
                    nes_pal_gui::HEIGHT_F,
                    nes_pal_gui::NUM_PER_LINE_F,
                ) {
                    write!(palette_hover, "{t:#04X}").unwrap_or_else(|_| panic!("write error"));
                }

                // If we were locked and inside the tiles and hit the secondary
                // button clear the lock and return (no other processing this frame).
                if *hover_locked
                    && i.pointer.secondary_pressed()
                    && Self::hover_tile(tile_draw_data, left_rect, right_rect, None, hp).is_some()
                {
                    *hover_locked = false;
                    return;
                }

                // If we're hovering over something inside the tilesets
                // and we pressed this frame lock it into place.
                if !*hover_locked
                    && i.pointer.primary_pressed()
                    && hovered.is_some()
                    && Self::hover_tile(tile_draw_data, left_rect, right_rect, None, hp).is_some()
                {
                    *hover_locked = true;
                }

                // If we aren't locked update the tile based on the one we're
                // hovering over.
                if !*hover_locked {
                    *hovered =
                        Self::hover_tile(tile_draw_data, left_rect, right_rect, *hovered, hp);

                    // We have to clear each time or it'll just make a long string as we hover.
                    // If this is blank (not on a tile) then leave the last one as the tile
                    // is left also.
                    let orig = single_title.clone();
                    single_title.clear();
                    if let Some(hp) = hovered {
                        write!(single_title, "# {hp}").unwrap_or_else(|_| panic!("write error"));
                    } else if !orig.is_empty() {
                        write!(single_title, "{orig}").unwrap_or_else(|_| panic!("write error"));
                    }
                }
            }
        });
    }

    fn hover_tile(
        tile_draw_data: &TileDrawData,
        left_image: Rect,
        right_image: Rect,
        orig: Option<usize>,
        hp: Pos2,
    ) -> Option<usize> {
        // See if the current hover position matches inside
        // the left or right tile and compute into a tile number.
        let left_tile = Self::tile_num(
            left_image,
            hp,
            tile_draw_data.tile_x_total_f,
            tile_draw_data.tile_y_total_f,
            tile_draw_data.tiles_per_row_f,
        );
        let right_tile = Self::tile_num(
            right_image,
            hp,
            tile_draw_data.tile_x_total_f,
            tile_draw_data.tile_y_total_f,
            tile_draw_data.tiles_per_row_f,
        );
        match (left_tile, right_tile) {
            (None, None) => orig,
            (None, Some(mut t)) => {
                t += tile_draw_data.tiles_per_image;
                Some(t)
            }
            (Some(t), None) => Some(t),
            (Some(_), Some(_)) => panic!("Hovering over both images at once?"),
        }
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
    fn create_chr_tiles(chrtiles: &mut ChrTiles, tile_draw_data: &TileDrawData) {
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
            if loc >= tile_draw_data.tiles_per_image {
                // Reset to all WHITE again so hover from left side doesn't
                // carry over.
                if do_left {
                    chrtiles.data.fill(egui::Color32::WHITE.r());
                    do_left = false;
                }
                loc -= tile_draw_data.tiles_per_image;
            }

            // First figure out the row we're on and the first entry for it's
            // first pixel.
            let row_start_base = loc / tile_draw_data.tiles_per_row
                * tile_draw_data.tile_y_total
                * tile_draw_data.tile_line_size
                * BYTES_PER_PIXEL;

            // Now move N boxes over to find the box start pixel.
            let box_start_base = row_start_base
                + tile_draw_data.tile_x_total
                    * (loc % tile_draw_data.tiles_per_row)
                    * BYTES_PER_PIXEL;

            // Now also adjust it N pixels down and over to account for buffers.
            // This way the painting below just deals with correct offsets into
            // the tile data.
            let box_start = box_start_base
                + (tile_draw_data.top_buffer
                    * tile_draw_data.tile_multiplier_y
                    * tile_draw_data.tile_line_size
                    * BYTES_PER_PIXEL)
                + (tile_draw_data.left_buffer * tile_draw_data.tile_multiplier_x * BYTES_PER_PIXEL);

            // Using box_start_base fill in GREY on each row if this is the tile
            // be hovered over. The rest of the tile painting below will write over the
            // rest so this just becomes our outline.
            if chrtiles.hovered == Some(orig) {
                for y in 0..tile_draw_data.tile_y_total {
                    let row = box_start_base + y * tile_draw_data.tile_line_size * BYTES_PER_PIXEL;
                    for x in 0..tile_draw_data.tile_x_total {
                        chrtiles.data[row + x * BYTES_PER_PIXEL] = egui::Color32::GRAY.r();
                        chrtiles.data[row + x * BYTES_PER_PIXEL + 1] = egui::Color32::GRAY.g();
                        chrtiles.data[row + x * BYTES_PER_PIXEL + 2] = egui::Color32::GRAY.b();
                    }
                }

                // Draw in the 8x tile in between the 2 CHR images.
                Self::draw_a_tile(
                    &DrawData {
                        box_start: 0,
                        mult_x: tile_draw_data.single_tile_multiplier_x,
                        mult_y: tile_draw_data.single_tile_multiplier_y,
                        tile_line_size: tile_draw_data.single_tile_x_total,
                        tile: t,
                        colors,
                        color_source,
                        selected_pal: *selected_pal,
                    },
                    chrtiles.tile_data,
                    tile_draw_data,
                );
                let im = egui::ColorImage::from_rgb(
                    [
                        tile_draw_data.single_tile_x_total,
                        tile_draw_data.single_tile_y_total,
                    ],
                    chrtiles.tile_data,
                );
                chrtiles.single.set(im, TextureOptions::default());
            }

            // For each actual tile use the offsets computed above to just iterate
            // through each 8x8 tiles (blown up as needed).
            Self::draw_a_tile(
                &DrawData {
                    box_start,
                    mult_x: tile_draw_data.tile_multiplier_x,
                    mult_y: tile_draw_data.tile_multiplier_y,
                    tile_line_size: tile_draw_data.tile_line_size,
                    tile: t,
                    colors,
                    color_source,
                    selected_pal: *selected_pal,
                },
                chrtiles.data,
                tile_draw_data,
            );

            let im = egui::ColorImage::from_rgb(
                [
                    tile_draw_data.tile_line_size,
                    tile_draw_data.tile_height_size,
                ],
                chrtiles.data,
            );

            if do_left {
                chrtiles.left.set(im, TextureOptions::default());
            } else {
                chrtiles.right.set(im, TextureOptions::default());
            }
        }
    }

    fn draw_a_tile(draw_data: &DrawData, data: &mut [u8], tile_draw_data: &TileDrawData) {
        for y in 0..tile_draw_data.tile_y {
            for yi in 0..draw_data.mult_y {
                // Finally for each line adjust by the row we're on for each line.
                let y_off = draw_data.box_start
                    + (y * draw_data.mult_y + yi) * draw_data.tile_line_size * BYTES_PER_PIXEL;
                for x in 0..tile_draw_data.tile_x {
                    // Each x start has to be adjusted by RGB to get the final entry.
                    let start = x * draw_data.mult_x * BYTES_PER_PIXEL;

                    // Now lookup the tile data which is in range 0..NUM_COLORS
                    // Index that into colors to get the PAL entry.
                    // Now find that in the selected PAL to get the final RGB values.
                    let td = draw_data.tile.data[y * tile_draw_data.tile_y + x];
                    let col = draw_data.colors[usize::from(td)];
                    let color = &draw_data.color_source[draw_data.selected_pal].colors[col];

                    for i in 0..draw_data.mult_x {
                        let off = i * BYTES_PER_PIXEL;
                        data[y_off + off + start] = color.r;
                        data[y_off + off + start + 1] = color.g;
                        data[y_off + off + start + 2] = color.b;
                    }
                }
            }
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
        // If a color picker button has been selected display the dialog.
        if let Some(bidx) = self.button {
            egui::Window::new("Color picker").show(ui.ctx(), |ui| self.color_picker(bidx, ui));
        }

        // Always show the main window.
        egui::CentralPanel::default()
            .frame(egui::Frame::NONE.fill(egui::Color32::GRAY))
            .show(ui, |ui| self.main_ui(ui));
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
