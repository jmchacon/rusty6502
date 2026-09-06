//! `cart_renderer` loads a set of PAL files and an NES cart and renders the
//! CHR sections, with color selection from a chosen palette.
//!
//! This is the library half of the crate: [`MyApp`] (an [`eframe::App`])
//! plus the [`load_pal`]/[`load_cart_for_editing`] helpers used to build the
//! arguments it needs. The `cart_renderer` binary is a thin CLI wrapper
//! around this.
use color_eyre::eyre::{eyre, Result, WrapErr};
use egui::{
    Color32, FontFamily, FontId, Pos2, Rect, Sense, TextStyle, TextWrapMode, TextureHandle,
    TextureOptions, Ui, Vec2,
};
use nes_chr::Tile;
use nes_pal::{parse_pal, Color};
use nes_pal_gui::texture_from_palette;
use std::collections::BTreeMap;
use std::fs::{read, write};
use std::path::{Path, PathBuf};

#[cfg(test)]
mod tests;

/// A parsed `.pal` file: its colors, plus the file's base name (used as its
/// display label in the palette selector).
pub struct Data {
    /// The file's base name, shown in the palette combo box.
    pub filename: String,
    /// The parsed palette colors.
    pub colors: Vec<Color>,
}

/// Loads and parses a single `.pal` file into a [`Data`] entry.
///
/// # Errors
/// Returns an error if the file can't be read or doesn't parse as a valid
/// PAL file.
pub fn load_pal(path: &str) -> Result<Data> {
    let bytes: Vec<u8> = read(path).wrap_err_with(|| format!("reading {path}"))?;
    let colors = parse_pal(&bytes)?;
    let filename = Path::new(path)
        .file_name()
        .ok_or(eyre!("Path error for {path}"))?
        .to_string_lossy()
        .into();
    Ok(Data { filename, colors })
}

// The fixed iNES header layout this crate relies on (see
// http://wiki.nesdev.com/w/index.php/INES and .../NES_2.0): a 16 byte
// header, an optional 512 byte trainer, then the PRG ROM, then the CHR ROM.
// These never change regardless of cart contents, so they're small enough
// to just hardcode here rather than depend on `ines`'s (private) constants.
const INES_HEADER_SIZE: usize = 16;
const INES_TRAINER_SIZE: usize = 512;
const INES_PRG_BLOCK_SIZE: usize = 16_384;

/// An NES cart loaded for viewing/editing: its decoded tiles (see
/// [`load_cart_for_editing`]) plus enough of the original file to write CHR
/// edits back out to it later (see [`MyApp`]'s Save/Save As handling).
///
/// Saving only ever replaces the CHR ROM region -- everything else in the
/// original file this app doesn't understand or ever changes (PRG code,
/// mapper-specific header fields, trainer, misc ROM, ...) round trips byte
/// for byte instead of being reconstructed from scratch.
pub struct EditableCart {
    /// The decoded tiles, one `Vec` per CHR ROM page (8KB/256 tiles each).
    pub tiles: Vec<Vec<Tile>>,
    // The original file's bytes verbatim, or empty if there was no original
    // file (see `blank`) -- in which case Save/Save As synthesize a minimal
    // new file instead of patching one.
    raw: Vec<u8>,
    // Byte offset of the CHR ROM region within `raw`. Always valid even
    // though CHR edits never change the file's size (only re-encoding the
    // same number of tiles back to the same number of bytes).
    chr_offset: usize,
}

impl EditableCart {
    /// The starting state when no file was given: all white colors have
    /// nothing to do with this (see [`MyApp::new`]), but this is the "empty
    /// tile sets" half of that -- one blank (all background) CHR page, and
    /// no original file to patch on Save (see `raw`'s doc comment above).
    #[must_use]
    pub fn blank() -> Self {
        let blank_page = (0..256).map(|_| Tile::default()).collect();
        Self {
            tiles: vec![blank_page],
            raw: Vec::new(),
            chr_offset: 0,
        }
    }
}

/// Loads an INES cart file and decodes its CHR ROM into per-page tile sets
/// (each 8KB CHR block becomes a 2x256 set of tiles), retaining what's
/// needed to write edits back out to the same file later.
///
/// # Errors
/// Returns an error if the file can't be read or doesn't parse as a valid
/// INES cart.
pub fn load_cart_for_editing(path: &str) -> Result<EditableCart> {
    let raw = read(path).wrap_err_with(|| format!("reading {path}"))?;
    let nes = ines::parse(&raw)?;
    let mut tiles = Vec::new();
    for t in &nes.chr {
        tiles.push(nes_chr::map_chr_rom(t)?);
    }
    let chr_offset = INES_HEADER_SIZE
        + if nes.trainer.is_some() {
            INES_TRAINER_SIZE
        } else {
            0
        }
        + nes.prg.len() * INES_PRG_BLOCK_SIZE;
    Ok(EditableCart {
        tiles,
        raw,
        chr_offset,
    })
}

// Re-encodes every currently loaded CHR page's (possibly edited) tile data
// back into raw CHR ROM bytes and either patches it into the original
// file's bytes at `chr_offset` (preserving everything else -- PRG code,
// header flags (including any NES 2.0-only fields: submapper, exact
// PRG/CHR RAM sizes, CPU timing, ...), trainer, misc ROM -- byte for byte,
// since only the CHR ROM region between `chr_offset` and `chr_offset +
// chr_bytes.len()` is ever touched) or, if `raw` is empty (this session
// started with no file -- see `EditableCart::blank`), synthesizes a
// minimal new plain iNES 1.0 file around it (there being no original
// header to preserve in that case).
fn build_output_bytes(raw: &[u8], chr_offset: usize, tiles: &[Vec<Tile>]) -> Result<Vec<u8>> {
    let mut chr_bytes = Vec::new();
    for page in tiles {
        chr_bytes.extend(nes_chr::tiles_to_chr_rom(page)?);
    }

    if raw.is_empty() {
        let chr_pages = u8::try_from(tiles.len()).unwrap_or(u8::MAX);
        let mut out = Vec::with_capacity(INES_HEADER_SIZE + INES_PRG_BLOCK_SIZE + chr_bytes.len());
        out.extend_from_slice(b"NES\x1A");
        out.push(1); // 1 (empty) PRG bank -- an INES file needs at least one.
        out.push(chr_pages);
        out.extend_from_slice(&[0u8; 10]); // flags 6/7 and the rest of the header default to 0.
        out.extend_from_slice(&[0u8; INES_PRG_BLOCK_SIZE]);
        out.extend_from_slice(&chr_bytes);
        Ok(out)
    } else {
        let mut out = raw.to_vec();
        let end = chr_offset + chr_bytes.len();
        if end > out.len() {
            return Err(eyre!(
                "Edited CHR data ({} bytes) doesn't fit back into the original file's CHR region",
                chr_bytes.len()
            ));
        }
        out[chr_offset..end].copy_from_slice(&chr_bytes);
        Ok(out)
    }
}

enum Stage {
    PreRender(isize),
    FirstRender(Vec2),
    FirstResize(Vec2),
    Initialized(Vec2),
}

/// The `cart_renderer` [`eframe::App`]. Construct with [`MyApp::new`] and
/// hand to `eframe::run_native` (or an `egui_kittest::Harness` in tests).
pub struct MyApp {
    // The stage of rendering so we can resize correctly.
    render_stage: Stage,

    // Bumped every time we start a new `Stage::PreRender` measurement cycle
    // (other than the very first one) and folded into the scratch window's
    // id in `pre_render`. egui remembers a resizable window's size and only
    // ever grows it (it assumes a size change means the user dragged a
    // resize handle), so reusing the same window id across multiplier
    // changes made the measured size latch onto the largest one we'd ever
    // used. A fresh id per remeasure starts that memory over.
    remeasure_generation: usize,

    // The single-tile preview column's actual rendered size (heading +
    // preview image + magnification/Edit controls) from last frame, used to
    // tell `allocate_ui_with_layout` its true size *before* laying it out
    // this frame. `Align::Center` centers using the *requested* size, not
    // the content's eventual size, so a wrong guess (like the default 0
    // desired height) centers a zero-size placeholder and the real content
    // then only grows away from that anchor in one direction -- e.g. only
    // downward, landing low instead of centered. Self-measuring like this
    // (rather than guessing from ambient available space, which circularly
    // depends on this same column during the `PreRender` measurement dance)
    // converges in a frame and matches how every other size in this app is
    // derived from content rather than the other way around.
    single_tile_column_size: Vec2,

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
    colors_per_pal: Vec<Vec<TextureHandle>>,

    // Which palette/CHR page/colors are selected, what's hovered, and enough
    // history to know when a redraw is needed.
    selection: Selection,

    // If a color button is pressed and then if so which one.
    button: Option<usize>,

    // If displaying the modal selection dialog which color is currently picked.
    dialog_selected: usize,

    // The input we setup to change the tile texture each frame.
    data: Box<[Color32]>,

    // The tile data when we display a large single tile between the panels.
    tile_data: Box<[Color32]>,

    // The original color data parsed from each PAL file.
    color_source: Vec<Data>,

    // The tile location for the large middle tile from the tileset.
    single_title: String,

    // If non-blank is the hover text displayed over the palette.
    palette_hover: String,

    // If a tile is being edited (the "Edit" button was pressed) this holds
    // all of that panel's state. `None` means the panel is closed.
    edit_panel: Option<Box<EditPanelState>>,

    // The original file bytes for the currently loaded cart (see
    // `EditableCart::raw`'s doc comment) and the byte offset of its CHR ROM
    // region within them, both needed by File > Save/Save As.
    raw: Vec<u8>,
    chr_offset: usize,

    // The path last loaded from or saved to. `None` until the first
    // successful Save/Save As if the app started with no file (see
    // `EditableCart::blank`) -- in which case File > Save behaves like
    // Save As (there's nothing to overwrite yet).
    current_path: Option<PathBuf>,

    // If Save/Save As is about to overwrite an existing file, the path
    // pending confirmation. `None` means no confirmation dialog is showing.
    pending_overwrite: Option<PathBuf>,
}

const PALETTE_SQ_X: usize = 40;
const PALETTE_SQ_Y: usize = 40;

const NUM_COLORS: usize = 4;

// The common NTSC and PAL palettes have black as the last entry
// and white as the one on the beginning of the last row so default to those.
const DEFAULT_BACKGROUND: usize = 0x3F;
const DEFAULT_FOREGROUND: usize = 0x30;

// Which palette/CHR page/colors are currently selected, what tile is
// hovered, and the previous frame's values (used to detect changes that
// require a redraw).
struct Selection {
    // The current PAL texture handle used to display the palette in use.
    // Index is into `MyApp::pals`.
    pal: usize,

    // The index of the selected CHR block we're viewing.
    chr: usize,

    // For each color the index into the current PAL palette it should be using.
    colors: [usize; NUM_COLORS],

    // The tile we most recently hovered over.
    hovered: Option<usize>,

    // If button 1 was clicked hovering is locked. Cleared on button 3.
    hover_locked: bool,

    // If any of these are different from the field above then the tile
    // texture should get redrawn to update.
    last_pal: usize,
    last_chr: usize,
    last_colors: [usize; NUM_COLORS],
    last_hovered: Option<usize>,
}

impl Selection {
    fn new() -> Self {
        Self {
            pal: 0,
            chr: 0,
            colors: [
                DEFAULT_BACKGROUND,
                DEFAULT_FOREGROUND,
                DEFAULT_FOREGROUND,
                DEFAULT_FOREGROUND,
            ],
            hovered: None,
            hover_locked: false,
            last_pal: 0,
            last_chr: 0,
            last_colors: [0; NUM_COLORS],
            last_hovered: None,
        }
    }
}

// All the data used to build up the math needed to operate with the tiles
// we display. Each tile is 8x8 but we want a 1 pixel border around each one.
// This is then 10x10 (which makes manual math simpler) and when multiplied out
// is a 20x20 tile image. Various iterations later when creating the tile image
// will need various combinations of these values and sometimes as floats
// due to how egui works with pixels.
//
// The single tile varieties are there for displaying the 8x tile selected during
// hover over.
#[derive(Debug)]
struct TileDrawData {
    tile_x: usize,
    tile_y: usize,
    tiles_per_row: usize,
    row_of_tiles: usize,
    top_buffer: usize,
    bottom_buffer: usize,
    left_buffer: usize,
    right_buffer: usize,
    single_tile_multiplier_x: usize,
    tile_multiplier_x: usize,

    single_tile_multiplier_y: usize,
    tile_multiplier_y: usize,

    // Single tile has no border since we want to just see it blown up exactly.
    single_tile_x_total: usize,
    tile_x_total: usize,
    single_tile_y_total: usize,
    tile_y_total: usize,

    single_tile_image_buffer: f32,

    tile_line_size: usize,
    tile_height_size: usize,

    tile_layout_size: usize,

    single_tile_layout_size: usize,
    tiles_per_image: usize,
}

impl TileDrawData {
    fn default() -> Self {
        let mut s = Self {
            tile_x: 8,
            tile_y: 8,
            tiles_per_row: 16,
            row_of_tiles: 16,
            top_buffer: 1,
            bottom_buffer: 1,
            left_buffer: 1,
            right_buffer: 1,
            single_tile_multiplier_x: 8,
            tile_multiplier_x: 2,
            single_tile_multiplier_y: 8,
            tile_multiplier_y: 2,
            tiles_per_image: 256,
            single_tile_x_total: 0,
            tile_x_total: 0,
            single_tile_y_total: 0,
            tile_y_total: 0,
            single_tile_image_buffer: 0.0,
            tile_line_size: 0,
            tile_height_size: 0,
            tile_layout_size: 0,
            single_tile_layout_size: 0,
        };
        s.single_tile_x_total = s.tile_x * s.single_tile_multiplier_x;
        s.tile_x_total = (s.left_buffer + s.tile_x + s.right_buffer) * s.tile_multiplier_x;
        s.single_tile_y_total = s.tile_y * s.single_tile_multiplier_y;
        s.tile_y_total = (s.top_buffer + s.tile_y + s.bottom_buffer) * s.tile_multiplier_y;

        // We know the cast is safe since it's constrained to small values.
        #[allow(clippy::cast_possible_truncation)]
        let buf = 12.5 * f32::from(s.single_tile_multiplier_x as u8);
        s.single_tile_image_buffer = buf;
        s.tile_line_size = s.tile_x_total * s.tiles_per_row;
        s.tile_height_size = s.tile_y_total * s.row_of_tiles;
        // Pixel counts (one `Color32` per pixel), not byte counts.
        s.tile_layout_size = s.tile_line_size * s.tile_height_size;
        s.single_tile_layout_size = s.single_tile_x_total * s.single_tile_y_total;
        s
    }

    fn update_multiplier(&mut self, tile_multiplier: usize) {
        self.tile_multiplier_x = tile_multiplier;
        self.tile_multiplier_y = tile_multiplier;

        self.tile_x_total =
            (self.left_buffer + self.tile_x + self.right_buffer) * self.tile_multiplier_x;
        self.tile_y_total =
            (self.top_buffer + self.tile_y + self.bottom_buffer) * self.tile_multiplier_y;

        self.tile_line_size = self.tile_x_total * self.tiles_per_row;
        self.tile_height_size = self.tile_y_total * self.row_of_tiles;
        self.tile_layout_size = self.tile_line_size * self.tile_height_size;
    }

    fn update_single_tile_multiplier(&mut self, single_tile_multiplier: usize) {
        self.single_tile_multiplier_x = single_tile_multiplier;
        self.single_tile_multiplier_y = single_tile_multiplier;

        self.single_tile_x_total = self.tile_x * self.single_tile_multiplier_x;
        self.single_tile_y_total = self.tile_y * self.single_tile_multiplier_y;
        self.single_tile_layout_size = self.single_tile_x_total * self.single_tile_y_total;

        // We know the cast is safe since it's constrained to small values.
        #[allow(clippy::cast_possible_truncation)]
        let buf = 12.5 * f32::from(self.single_tile_multiplier_x as u8);
        self.single_tile_image_buffer = buf;
    }
}

// The labels for the 4 buttons used to select colors.
const BUTTONS: [&str; NUM_COLORS] = ["Background", "Color 1", "Color 2", "Color 3"];

// `egui::Panel` doesn't auto-size to its content -- it defaults to a fixed
// 200px width, which is narrower than the edit panel's 8x8 pixel grid alone
// (8 * 32px cells + spacing, ~263px). Left at the default, the grid painted
// past the panel's right edge into the main window's CHR grid instead of the
// window growing to fit it. An explicit `exact_size` gives the panel a fixed
// width wide enough for its content and, just as importantly, keeps that
// width identical between the `pre_render` measurement pass and the real
// render, since a resizable panel's drag-adjusted width could otherwise
// drift from what was measured.
const EDIT_PANEL_WIDTH: f32 = 300.0;

// All the data needed for building up the chr tile images and setting new textures.
struct ChrTiles<'a> {
    tiles: &'a [Vec<Tile>],
    left: &'a mut TextureHandle,
    right: &'a mut TextureHandle,
    single: &'a mut TextureHandle,
    selected_pal: &'a usize,
    selected_chr: &'a usize,
    colors: &'a [usize; NUM_COLORS],
    data: &'a mut [Color32],
    color_source: &'a [Data],
    hovered: Option<usize>,
    tile_data: &'a mut [Color32],
}

// When drawing a given tile all the data needed to make that happen.
struct DrawData<'a> {
    box_start: usize,
    mult_x: usize,
    mult_y: usize,
    tile_line_size: usize,
    tile: &'a Tile,
    colors: &'a [usize],
    color_source: &'a [Data],
    selected_pal: usize,
}

// The color-selection state needed to render any tile, bundled up so the
// single-tile redraw helpers don't need a long parameter list.
struct PalContext<'a> {
    colors: &'a [usize; NUM_COLORS],
    color_source: &'a [Data],
    selected_pal: usize,
}

// The palette panel's state, bundled up so `render_palette_panel` doesn't
// need a long parameter list (see `main_ui`'s `let Self { .. } = self;`
// destructure doc for why this can't just be a `&mut self` method).
struct PalettePanel<'a> {
    pals: &'a [TextureHandle],
    colors_per_pal: &'a [Vec<TextureHandle>],
    selection: &'a mut Selection,
    palette: &'a mut Option<egui::Response>,
    palette_hover: &'a mut String,
    button: &'a mut Option<usize>,
    dialog_selected: &'a mut usize,
}

// The state needed to render the left/preview/right image row, bundled up
// for the same reason as `PalettePanel`.
struct ImageRow<'a> {
    left: &'a TextureHandle,
    left_image: &'a mut Option<egui::Response>,
    right: &'a TextureHandle,
    right_image: &'a mut Option<egui::Response>,
    single: &'a mut TextureHandle,
    single_title: &'a mut String,
    tile_draw_data: &'a mut TileDrawData,
    tile_data: &'a mut Box<[Color32]>,
    tiles: &'a [Vec<Tile>],
    selection: &'a Selection,
    edit_panel: &'a mut Option<Box<EditPanelState>>,
    color_source: &'a [Data],
    column_size: Vec2,
    render_stage: &'a mut Stage,
    remeasure_generation: &'a mut usize,
}

// The state needed to redraw the CHR tile textures, bundled up for the same
// reason as `PalettePanel`.
struct RedrawTiles<'a> {
    render_stage: &'a Stage,
    tiles: &'a [Vec<Tile>],
    selection: &'a mut Selection,
    left: &'a mut TextureHandle,
    right: &'a mut TextureHandle,
    single: &'a mut TextureHandle,
    data: &'a mut [Color32],
    tile_data: &'a mut [Color32],
    color_source: &'a [Data],
    tile_draw_data: &'a TileDrawData,
}

// The state needed to interpret pointer input against the tile images,
// bundled up for the same reason as `PalettePanel`.
struct HoverInput<'a> {
    left_image: &'a Option<egui::Response>,
    right_image: &'a Option<egui::Response>,
    palette: &'a Option<egui::Response>,
    palette_hover: &'a mut String,
    selection: &'a mut Selection,
    tile_draw_data: &'a TileDrawData,
    single_title: &'a mut String,
}

// All the state for the "Edit tile" side panel: which tile it's editing, a
// working copy of that tile's pixels (0-3 indices, same as `Tile::data`) and
// a local copy of the 4 color slots used only to preview the tile while
// editing it. Both are snapshotted on open (`original_*`) so Revert can
// restore them without needing to re-read the source tile.
//
// The local colors never change what's actually saved -- only `pixels` is
// written back to the real tile's data on Save. They just control how this
// panel (and its own preview) renders the tile while you're picking which of
// the 4 slots each pixel uses; the tile's real on-screen look afterwards
// still depends on whatever colors are selected on the main screen.
struct EditPanelState {
    chr: usize,
    tile_idx: usize,

    pixels: [u8; 64],
    original_pixels: [u8; 64],

    colors: [usize; NUM_COLORS],
    original_colors: [usize; NUM_COLORS],

    // If one of the 4 local color swatches is pressed, which one (mirrors
    // `MyApp::button`/`MyApp::dialog_selected` but scoped to this panel so
    // it doesn't fight with the main screen's color picker).
    color_button: Option<usize>,
    dialog_selected: usize,

    // The panel's own single-tile preview, rendered the same way as the main
    // screen's (see `redraw_single_preview`) but sized by this panel's own
    // magnification, independent of the main preview's.
    preview: TextureHandle,
    preview_data: Box<[Color32]>,
    preview_draw_data: TileDrawData,
}

impl MyApp {
    /// Builds the app from already-loaded palette data (see [`load_pal`]) and
    /// cart data (see [`load_cart_for_editing`] and [`EditableCart::blank`]).
    ///
    /// `datas` may be empty (no PAL file given at startup) -- this then uses
    /// a single synthetic all-white palette instead, so the rest of the app
    /// (which otherwise assumes at least one palette exists) doesn't need to
    /// special-case it. `current_path` is the file `cart` was loaded from, or
    /// `None` for [`EditableCart::blank`]/no file given.
    #[must_use]
    pub fn new(
        cc: &eframe::CreationContext<'_>,
        mut datas: Vec<Data>,
        cart: EditableCart,
        current_path: Option<PathBuf>,
    ) -> Self {
        use FontFamily::{Monospace, Proportional};

        if datas.is_empty() {
            datas.push(Data {
                filename: "(no palette)".to_string(),
                colors: (0..64)
                    .map(|_| Color {
                        r: 255,
                        g: 255,
                        b: 255,
                    })
                    .collect(),
            });
        }

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
        let mut colors_per_pal = Vec::new();

        for d in &datas {
            pals.push(texture_from_palette(cc, &d.filename, &d.colors));
            let mut per_pal = Vec::with_capacity(d.colors.len());
            for (ci, c) in d.colors.iter().enumerate() {
                let im = egui::ColorImage::filled(
                    [PALETTE_SQ_X, PALETTE_SQ_Y],
                    Color32::from_rgb(c.r, c.g, c.b),
                );
                let text = cc.egui_ctx.load_texture(
                    format!("Color: {ci}"),
                    egui::ImageData::Color(im.into()),
                    TextureOptions::default(),
                );
                per_pal.push(text);
            }
            colors_per_pal.push(per_pal);
        }

        let tdd = TileDrawData::default();
        // Fill in the initial set of tiles layout with all white.
        // Additional tiles can always be used later with a combo box to select but we need a
        // base texture to update on every draw. i.e. the first update will select the actual
        // correct entries for this.
        let data = vec![Color32::WHITE; tdd.tile_layout_size].into_boxed_slice();
        let im = egui::ColorImage::new([tdd.tile_line_size, tdd.tile_height_size], data.to_vec());
        let left = cc.egui_ctx.load_texture(
            "Left CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let im = egui::ColorImage::new([tdd.tile_line_size, tdd.tile_height_size], data.to_vec());
        let right = cc.egui_ctx.load_texture(
            "Right CHR Tiles",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );
        let tile_data = vec![Color32::WHITE; tdd.single_tile_layout_size].into_boxed_slice();
        let im = egui::ColorImage::new(
            [tdd.single_tile_x_total, tdd.single_tile_y_total],
            tile_data.to_vec(),
        );
        let single = cc.egui_ctx.load_texture(
            "Single tile",
            egui::ImageData::Color(im.into()),
            TextureOptions::default(),
        );

        Self {
            render_stage: Stage::PreRender(2_isize),
            remeasure_generation: 0,
            single_tile_column_size: Vec2::ZERO,
            tile_draw_data: tdd,
            tiles: cart.tiles,
            palette: None,
            left,
            left_image: None,
            right,
            right_image: None,
            single,
            pals,
            colors_per_pal,
            selection: Selection::new(),
            button: None,
            dialog_selected: 0,
            data,
            tile_data,
            color_source: datas,
            single_title: String::with_capacity(16),
            palette_hover: String::with_capacity(4),
            edit_panel: None,
            raw: cart.raw,
            chr_offset: cart.chr_offset,
            current_path,
            pending_overwrite: None,
        }
    }

    // `color_picker` is the modal dialog for chosing a new color when one of
    // the color buttons is selected. Takes its target state explicitly
    // (rather than `&mut self`) so it can back both the main screen's global
    // color selection and the edit panel's local one.
    #[allow(clippy::too_many_arguments)]
    fn color_picker(
        colors_per_pal: &[Vec<TextureHandle>],
        pal: usize,
        button: &mut Option<usize>,
        dialog_selected: &mut usize,
        target_colors: &mut [usize; NUM_COLORS],
        bidx: usize,
        ui: &mut Ui,
    ) {
        const NUM_PER_ROW: usize = 16;

        let clrs = &colors_per_pal[pal];

        // Create a 16 x 4 set of colors where each entry is a distinct button
        // rather than just a pallete displayed in the main UI. This way any
        // selection is each to use to show newly selected.
        for (row, chunk) in clrs.chunks(NUM_PER_ROW).enumerate() {
            ui.horizontal(|ui| {
                for (i, texture) in chunk.iter().enumerate() {
                    let color = row * NUM_PER_ROW + i;
                    let br = ui.add(egui::Button::image(texture));
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
                target_colors[bidx] = *dialog_selected;
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
    fn main_ui(&mut self, ui: &mut Ui) {
        let Self {
            tile_draw_data,
            palette,
            left,
            left_image,
            right,
            right_image,
            single,
            selection,
            button,
            dialog_selected,
            data,
            tile_data,
            color_source,
            single_title,
            palette_hover,
            ..
        } = self;

        // If a color picker button has been pressed the modal dialog is up
        // so this window is inactive.
        if button.is_some() {
            ui.disable();
        }

        Self::render_palette_panel(
            ui,
            &mut PalettePanel {
                pals: &self.pals,
                colors_per_pal: &self.colors_per_pal,
                selection,
                palette,
                palette_hover,
                button,
                dialog_selected,
            },
        );

        Self::render_chr_controls(
            ui,
            self.tiles.len(),
            selection,
            tile_draw_data,
            data,
            &mut self.render_stage,
            &mut self.remeasure_generation,
        );

        // Fill in the selected tile data based on the selected color data
        // from the selected PAL palette data. Only do this when we change
        // relevant data (or this is a `PreRender` warm-up cycle -- covers
        // both the very first frame and a magnification change, both of
        // which reset `render_stage` to `Stage::PreRender` specifically to
        // force this).
        Self::redraw_tiles(&mut RedrawTiles {
            render_stage: &self.render_stage,
            tiles: &self.tiles,
            selection,
            left,
            right,
            single,
            data,
            tile_data,
            color_source,
            tile_draw_data,
        });

        // Every frame show the current tilesets with some separation.
        // The above only redraws the textures on actual changes so this is
        // fast since the GPU already has the images generally.
        self.single_tile_column_size = Self::render_image_row(
            ui,
            &mut ImageRow {
                left: &*left,
                left_image,
                right: &*right,
                right_image,
                single,
                single_title,
                tile_draw_data,
                tile_data,
                tiles: &self.tiles,
                selection: &*selection,
                edit_panel: &mut self.edit_panel,
                color_source,
                column_size: self.single_tile_column_size,
                render_stage: &mut self.render_stage,
                remeasure_generation: &mut self.remeasure_generation,
            },
        );

        Self::handle_hover_input(
            ui,
            &mut HoverInput {
                left_image: &*left_image,
                right_image: &*right_image,
                palette: &*palette,
                palette_hover,
                selection,
                tile_draw_data: &*tile_draw_data,
                single_title,
            },
        );
    }

    // Renders the top palette panel: the palette selector combo, the
    // palette image, and the 4 color-selection buttons.
    fn render_palette_panel(ui: &mut Ui, p: &mut PalettePanel) {
        // Make the top area which is the palette box and the buttons for it.
        egui::Panel::top("palette panel").show(ui, |ui| {
            // The combo box for determining which palette to display.
            egui::ComboBox::from_label(String::from("Palette"))
                .selected_text(p.pals[p.selection.pal].name())
                .show_ui(ui, |ui| {
                    ui.style_mut().wrap_mode = Some(TextWrapMode::Extend);
                    for i in 0..p.pals.len() {
                        ui.selectable_value(&mut p.selection.pal, i, p.pals[i].name());
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
                        // Fixed at native size (not the default "fill available
                        // space") since the palette itself shouldn't grow with
                        // the CHR magnification -- only its centering within
                        // the now-wider/narrower panel should change.
                        *p.palette = Some(
                            ui.add(
                                egui::Image::new(&p.pals[p.selection.pal])
                                    .fit_to_original_size(1.0),
                            )
                            .on_hover_text_at_pointer(p.palette_hover.as_str()),
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
                            let text = &p.colors_per_pal[p.selection.pal][p.selection.colors[i]];

                            if ui
                                .add(egui::Button::image_and_text(text, BUTTONS[i]))
                                .clicked()
                            {
                                *p.button = Some(i);
                                *p.dialog_selected = p.selection.colors[i];
                            }
                        });
                    }
                });
            });
            ui.end_row();
            ui.separator();
        });
    }

    // Renders the "CHR set" and "Magnification" combo boxes. Picking a
    // magnification resizes `data` to match the new tile size and forces a
    // full redraw plus a window resize, since the old buffer and window
    // size no longer fit.
    fn render_chr_controls(
        ui: &mut Ui,
        num_chr: usize,
        selection: &mut Selection,
        tile_draw_data: &mut TileDrawData,
        data: &mut Box<[Color32]>,
        render_stage: &mut Stage,
        remeasure_generation: &mut usize,
    ) {
        // A combo box to select which CHR page to display.
        egui::ComboBox::from_label("CHR set")
            .selected_text(format!("{}", selection.chr))
            .show_ui(ui, |ui| {
                ui.style_mut().wrap_mode = Some(TextWrapMode::Extend);
                for i in 0..num_chr {
                    ui.selectable_value(&mut selection.chr, i, format!("{i}"));
                }
            });
        ui.end_row();

        // A combo box to pick the CHR tile magnification (1x-4x).
        egui::ComboBox::from_label("Magnification")
            .selected_text(format!("{}x", tile_draw_data.tile_multiplier_x))
            .show_ui(ui, |ui| {
                for m in 1..=4 {
                    let selected = tile_draw_data.tile_multiplier_x == m;
                    if ui.selectable_label(selected, format!("{m}x")).clicked() {
                        tile_draw_data.update_multiplier(m);
                        *data = vec![Color32::WHITE; tile_draw_data.tile_layout_size]
                            .into_boxed_slice();
                        *render_stage = Stage::PreRender(2_isize);
                        *remeasure_generation += 1;
                    }
                }
            });
        ui.end_row();
    }

    // Redraws either all 512 CHR tiles (`full_redraw`, needed on a
    // `PreRender` warm-up cycle or whenever the selected palette/CHR
    // page/colors change) or, if only the hovered tile moved, just the (at
    // most 2) boxes that actually changed -- avoiding re-rendering and
    // re-uploading all 512 tiles for a hover.
    fn redraw_tiles(r: &mut RedrawTiles) {
        let full_redraw = matches!(r.render_stage, Stage::PreRender(_))
            || r.selection.last_pal != r.selection.pal
            || r.selection.last_chr != r.selection.chr
            || r.selection.last_colors != r.selection.colors;

        if full_redraw {
            r.selection.last_pal = r.selection.pal;
            r.selection.last_chr = r.selection.chr;
            r.selection.last_colors = r.selection.colors;
            r.selection.last_hovered = r.selection.hovered;
            Self::create_chr_tiles(
                &mut ChrTiles {
                    tiles: r.tiles,
                    left: r.left,
                    right: r.right,
                    single: r.single,
                    selected_pal: &r.selection.pal,
                    selected_chr: &r.selection.chr,
                    colors: &r.selection.colors,
                    data: r.data,
                    color_source: r.color_source,
                    hovered: r.selection.hovered,
                    tile_data: r.tile_data,
                },
                r.tile_draw_data,
            );
        } else if r.selection.last_hovered != r.selection.hovered {
            let chr = &r.tiles[r.selection.chr];
            let pal = PalContext {
                colors: &r.selection.colors,
                color_source: r.color_source,
                selected_pal: r.selection.pal,
            };
            if let Some(prev) = r.selection.last_hovered {
                Self::redraw_hover_box(
                    prev,
                    &chr[prev],
                    false,
                    r.left,
                    r.right,
                    &pal,
                    r.tile_draw_data,
                );
            }
            if let Some(cur) = r.selection.hovered {
                Self::redraw_hover_box(
                    cur,
                    &chr[cur],
                    true,
                    r.left,
                    r.right,
                    &pal,
                    r.tile_draw_data,
                );
            }
            // Keep the preview showing something real (tile 0) rather than
            // blank once hover is cleared, instead of leaving it stuck on
            // whatever was last hovered.
            Self::redraw_single_preview(
                r.single,
                &chr[r.selection.hovered.unwrap_or(0)],
                &pal,
                r.tile_draw_data,
                r.tile_data,
            );
            r.selection.last_hovered = r.selection.hovered;
        }
    }

    // Renders the left/right CHR images plus the centered preview column
    // (heading, preview image, its own magnification combo, and the "Edit"
    // button) between them. Returns the preview column's measured size for
    // next frame's centering (see `single_tile_column_size`'s field doc).
    #[allow(clippy::too_many_lines)]
    fn render_image_row(ui: &mut Ui, r: &mut ImageRow) -> Vec2 {
        let mut new_column_size = r.column_size;
        ui.horizontal(|ui| {
            ui.add_space(10.0);

            // All 4 images in this row (left/right CHR halves, the preview
            // below, and the palette above) are pinned to native size rather
            // than the default "fill available space" sizing, since their
            // pixel dimensions already encode the exact intended display
            // size (via the multiplier).
            // `Sense::click()` (rather than `Image`'s default hover-only
            // sense) so these register as "the widget being interacted
            // with" on click. Without it, egui's `Response::hovered()`
            // unconditionally goes false the instant *any* pointer button
            // is down for a hover-only widget (see context.rs: "We don't
            // hover widgets while interacting with *other* widgets" --
            // which a hover-only widget can never be, so the check always
            // fires), making click-driven state changes gated on `hovered`
            // impossible, even with the pointer squarely on the image.
            let hover = "Left button to lock\nRight button to clear";
            *r.left_image = Some(
                ui.add(
                    egui::Image::new(r.left)
                        .fit_to_original_size(1.0)
                        .sense(Sense::click()),
                )
                .on_hover_text(hover),
            );

            // The preview column's size is self-measured (see the
            // `single_tile_column_size` field doc) rather than guessed from
            // ambient available space, so it's centered correctly (both
            // axes) and doesn't depend on `PreRender` measurement state
            // that's still converging -- which was clipping `right_image`.
            let middle_response = ui.allocate_ui_with_layout(
                r.column_size,
                egui::Layout::top_down(egui::Align::Center),
                |ui| {
                    ui.heading(&*r.single_title);
                    ui.add(egui::Image::new(&*r.single).fit_to_original_size(1.0));

                    // The single-tile preview's own magnification, plus a not
                    // yet implemented "Edit" button.
                    ui.horizontal(|ui| {
                        egui::ComboBox::from_id_salt("single tile magnification")
                            .selected_text(format!(
                                "{}x",
                                r.tile_draw_data.single_tile_multiplier_x
                            ))
                            .show_ui(ui, |ui| {
                                for m in 1..=16 {
                                    let selected = r.tile_draw_data.single_tile_multiplier_x == m;
                                    if ui.selectable_label(selected, format!("{m}x")).clicked() {
                                        r.tile_draw_data.update_single_tile_multiplier(m);
                                        *r.tile_data = vec![
                                            Color32::WHITE;
                                            r.tile_draw_data.single_tile_layout_size
                                        ]
                                        .into_boxed_slice();
                                        // A targeted redraw of just the
                                        // preview (tile 0 if nothing's
                                        // hovered) instead of forcing a full
                                        // redraw of all 512 CHR tiles, which
                                        // this doesn't affect.
                                        let pal = PalContext {
                                            colors: &r.selection.colors,
                                            color_source: r.color_source,
                                            selected_pal: r.selection.pal,
                                        };
                                        let preview_idx = r.selection.hovered.unwrap_or(0);
                                        Self::redraw_single_preview(
                                            r.single,
                                            &r.tiles[r.selection.chr][preview_idx],
                                            &pal,
                                            r.tile_draw_data,
                                            r.tile_data,
                                        );
                                        *r.render_stage = Stage::PreRender(2_isize);
                                        *r.remeasure_generation += 1;
                                    }
                                }
                            });
                        // Only meaningful once a tile is locked in (hovering
                        // alone means the preview can still change out from
                        // under an open edit panel), so disable it otherwise.
                        let edit_clicked = ui
                            .add_enabled(r.selection.hover_locked, egui::Button::new("Edit"))
                            .clicked();
                        if edit_clicked {
                            let tile_idx = r.selection.hovered.unwrap_or(0);
                            let chr = r.selection.chr;
                            let pixels = r.tiles[chr][tile_idx].data;
                            let colors = r.selection.colors;

                            // Starts at the same default (8x) as the main
                            // preview and supports the same 1x-16x range.
                            let preview_draw_data = TileDrawData::default();
                            let mut preview_data =
                                vec![Color32::WHITE; preview_draw_data.single_tile_layout_size]
                                    .into_boxed_slice();
                            let im = egui::ColorImage::new(
                                [
                                    preview_draw_data.single_tile_x_total,
                                    preview_draw_data.single_tile_y_total,
                                ],
                                preview_data.to_vec(),
                            );
                            let mut preview = ui.ctx().load_texture(
                                "Edit tile preview",
                                egui::ImageData::Color(im.into()),
                                TextureOptions::default(),
                            );

                            let tile = Tile { data: pixels };
                            let pal = PalContext {
                                colors: &colors,
                                color_source: r.color_source,
                                selected_pal: r.selection.pal,
                            };
                            Self::redraw_single_preview(
                                &mut preview,
                                &tile,
                                &pal,
                                &preview_draw_data,
                                &mut preview_data,
                            );

                            *r.edit_panel = Some(Box::new(EditPanelState {
                                chr,
                                tile_idx,
                                pixels,
                                original_pixels: pixels,
                                colors,
                                original_colors: colors,
                                color_button: None,
                                dialog_selected: 0,
                                preview,
                                preview_data,
                                preview_draw_data,
                            }));

                            // The side panel changes the window's total
                            // width, so force a remeasure/resize just like a
                            // magnification change does.
                            *r.render_stage = Stage::PreRender(2_isize);
                            *r.remeasure_generation += 1;
                        }
                    });
                },
            );
            new_column_size = middle_response.response.rect.size();
            *r.right_image = Some(
                ui.add(
                    egui::Image::new(r.right)
                        .fit_to_original_size(1.0)
                        .sense(Sense::click()),
                )
                .on_hover_text(hover),
            );
            ui.add_space(10.0);
        });
        new_column_size
    }

    // Renders the "Edit tile" side panel: the local color slots, the large
    // interactive 8x8 pixel grid, this panel's own single-tile preview, and
    // the Save/Revert/Exit buttons. Does nothing if the panel isn't open.
    #[allow(clippy::too_many_lines)]
    fn render_edit_panel(&mut self, ui: &mut Ui) {
        const CELL: f32 = 32.0;

        let Self {
            colors_per_pal,
            color_source,
            selection,
            tiles,
            edit_panel,
            render_stage,
            remeasure_generation,
            ..
        } = self;

        let Some(edit) = edit_panel.as_mut() else {
            return;
        };

        ui.heading(format!("Edit Tile #{}", edit.tile_idx));
        ui.add_space(8.0);

        // The 4 local color slots. Editing these only changes how this
        // panel previews the tile -- see `EditPanelState`'s doc comment.
        ui.label("Local colors (preview only)");
        egui::Grid::new("edit_panel_colors")
            .num_columns(2)
            .show(ui, |ui| {
                for i in 0..NUM_COLORS {
                    let texture = &colors_per_pal[selection.pal][edit.colors[i]];
                    if ui
                        .add(egui::Button::image_and_text(texture, BUTTONS[i]))
                        .clicked()
                    {
                        edit.color_button = Some(i);
                        edit.dialog_selected = edit.colors[i];
                    }
                    if i % 2 == 1 {
                        ui.end_row();
                    }
                }
            });

        ui.add_space(8.0);
        ui.separator();

        // The large interactive pixel grid. Each cell shows which of the 4
        // local colors that pixel currently uses; hovering names it and
        // clicking opens a menu to change it.
        egui::Grid::new("edit_panel_grid")
            .spacing(egui::vec2(1.0, 1.0))
            .show(ui, |ui| {
                for y in 0..8 {
                    for x in 0..8 {
                        let i = y * 8 + x;
                        let idx = usize::from(edit.pixels[i]);
                        let pal_idx = edit.colors[idx];
                        let c = &color_source[selection.pal].colors[pal_idx];
                        let color = Color32::from_rgb(c.r, c.g, c.b);

                        let (rect, response) =
                            ui.allocate_exact_size(egui::vec2(CELL, CELL), Sense::click());
                        ui.painter().rect_filled(rect, 0.0, color);
                        ui.painter().rect_stroke(
                            rect,
                            0.0,
                            egui::Stroke::new(1.0, Color32::from_gray(80)),
                            egui::StrokeKind::Inside,
                        );
                        // A manually-painted rect (rather than an `egui::Button`)
                        // has no widget role by default, which would make it
                        // both invisible to accessibility tools and unfindable
                        // by the `egui_kittest` snapshot tests by role/label.
                        response.widget_info(|| egui::WidgetInfo::new(egui::WidgetType::Button));
                        let response = response.on_hover_text(BUTTONS[idx]);

                        egui::Popup::menu(&response).show(|ui| {
                            for (ci, label) in BUTTONS.iter().enumerate() {
                                if ui.selectable_label(ci == idx, *label).clicked() {
                                    edit.pixels[i] = u8::try_from(ci).unwrap_or(0);
                                    ui.close();
                                }
                            }
                        });
                    }
                    ui.end_row();
                }
            });

        ui.add_space(8.0);
        ui.separator();

        // This panel's own single-tile preview, redrawn every frame (it's
        // one 8x8 tile, not the 512-tile CHR sets, so there's no need for
        // the main screen's change-detection before redrawing it).
        let tile = Tile { data: edit.pixels };
        let pal = PalContext {
            colors: &edit.colors,
            color_source: color_source.as_slice(),
            selected_pal: selection.pal,
        };
        Self::redraw_single_preview(
            &mut edit.preview,
            &tile,
            &pal,
            &edit.preview_draw_data,
            &mut edit.preview_data,
        );
        ui.vertical_centered(|ui| {
            ui.add(egui::Image::new(&edit.preview).fit_to_original_size(1.0));
        });

        ui.horizontal(|ui| {
            egui::ComboBox::from_id_salt("edit panel preview magnification")
                .selected_text(format!(
                    "{}x",
                    edit.preview_draw_data.single_tile_multiplier_x
                ))
                .show_ui(ui, |ui| {
                    for m in 1..=16 {
                        let selected = edit.preview_draw_data.single_tile_multiplier_x == m;
                        if ui.selectable_label(selected, format!("{m}x")).clicked() {
                            edit.preview_draw_data.update_single_tile_multiplier(m);
                            edit.preview_data = vec![
                                Color32::WHITE;
                                edit.preview_draw_data.single_tile_layout_size
                            ]
                            .into_boxed_slice();
                        }
                    }
                });
            ui.label("Magnification");
        });

        ui.add_space(8.0);
        ui.separator();

        // Collect which button (if any) was clicked before acting on it, so
        // acting on "Exit" (which needs to write through `edit_panel`
        // itself, not just `edit`) doesn't fight with `edit`'s borrow of it.
        let mut revert_clicked = false;
        let mut save_clicked = false;
        let mut exit_clicked = false;
        ui.horizontal(|ui| {
            revert_clicked = ui.button("Revert").clicked();
            save_clicked = ui.button("Save").clicked();
            exit_clicked = ui.button("Exit").clicked();
        });

        if revert_clicked {
            edit.pixels = edit.original_pixels;
            edit.colors = edit.original_colors;
        }
        if save_clicked {
            tiles[edit.chr][edit.tile_idx].data = edit.pixels;
            edit.original_pixels = edit.pixels;
            edit.original_colors = edit.colors;
            *render_stage = Stage::PreRender(2_isize);
            *remeasure_generation += 1;
        }
        if exit_clicked {
            *edit_panel = None;
            *render_stage = Stage::PreRender(2_isize);
            *remeasure_generation += 1;
        }
    }

    // Interprets pointer input against the palette/CHR images: palette
    // hover text, hover-lock set/clear, and tracking which tile is hovered.
    fn handle_hover_input(ui: &mut Ui, h: &mut HoverInput) {
        ui.input(|i| {
            // If we're not enabled this means the modal is up so we don't want
            // tile state changing because we overlap that portion.
            if !ui.is_enabled() {
                return;
            }

            // Get the bounding boxes for both tile images so mapping to a tile
            // can be done below.
            let Some(r) = h.left_image.as_ref() else {
                panic!("left image invalid?");
            };
            let left_rect = r.rect;
            let left_hovered = r.hovered();
            let Some(r) = h.right_image.as_ref() else {
                panic!("right image invalid?");
            };
            let right_rect = r.rect;
            let right_hovered = r.hovered();

            let Some(pal) = h.palette.as_ref() else {
                panic!("palette image invalid?");
            };
            let palette_rect = pal.rect;

            if let Some(hp) = i.pointer.hover_pos() {
                // Check if we're inside the palette box and if so tool tip the
                // square we're over (hex value);
                *h.palette_hover = Self::tile_num(
                    palette_rect,
                    hp,
                    nes_pal_gui::NUM_PER_LINE_F,
                    nes_pal_gui::NUM_LINES_F,
                )
                .map_or_else(String::new, |t| format!("{t:#04X}"));

                // `hover_pos` is a raw screen position, so it can fall inside
                // the tile rects even when something else -- e.g. an open
                // combo box popup -- is drawn on top and actually receiving
                // the click. `Response::hovered` is occlusion-aware (it's
                // what already keeps the `on_hover_text` tooltips below from
                // showing through an overlay), so gate the two click-driven
                // state changes (lock/unlock) on it -- but not plain hover
                // tracking below, which should keep working continuously
                // regardless of what else is on screen.
                let over_tiles = left_hovered || right_hovered;

                // If we were locked and inside the tiles and hit the secondary
                // button clear the lock and return (no other processing this frame).
                if h.selection.hover_locked
                    && over_tiles
                    && i.pointer.secondary_pressed()
                    && Self::hover_tile(h.tile_draw_data, left_rect, right_rect, None, hp).is_some()
                {
                    h.selection.hover_locked = false;
                    return;
                }

                // If we're hovering over something inside the tilesets
                // and we pressed this frame lock it into place.
                if !h.selection.hover_locked
                    && over_tiles
                    && i.pointer.primary_pressed()
                    && h.selection.hovered.is_some()
                    && Self::hover_tile(h.tile_draw_data, left_rect, right_rect, None, hp).is_some()
                {
                    h.selection.hover_locked = true;
                }

                // If we aren't locked update the tile based on the one we're
                // hovering over.
                if !h.selection.hover_locked {
                    h.selection.hovered = Self::hover_tile(
                        h.tile_draw_data,
                        left_rect,
                        right_rect,
                        h.selection.hovered,
                        hp,
                    );

                    // If nothing is hovered, leave the title showing whatever
                    // tile was last hovered instead of blanking it out.
                    if let Some(hp) = h.selection.hovered {
                        *h.single_title = format!("# {hp}");
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
        // See if the current hover position matches inside the left or right
        // tile and compute into a tile number. These counts are always tiny
        // (currently 16), so the precision loss `as f32` warns about can't
        // actually happen here.
        #[allow(clippy::cast_precision_loss)]
        let (num_cols, num_rows) = (
            tile_draw_data.tiles_per_row as f32,
            tile_draw_data.row_of_tiles as f32,
        );
        let left_tile = Self::tile_num(left_image, hp, num_cols, num_rows);
        let right_tile = Self::tile_num(right_image, hp, num_cols, num_rows);
        // The left and right rects are disjoint, so at most one of these is
        // ever `Some`; prefer the left one if that ever changes.
        match (left_tile, right_tile) {
            (Some(t), _) => Some(t),
            (None, Some(t)) => Some(t + tile_draw_data.tiles_per_image),
            (None, None) => orig,
        }
    }
    fn tile_num(
        window: egui::Rect,
        hover: egui::Pos2,
        num_cols: f32,
        num_rows: f32,
    ) -> Option<usize> {
        let min = window.min;
        let max = window.max;
        // Make sure hover is actually inside the box. For a fast moving
        // pointer it might register just outside.
        if hover.x < min.x || hover.x >= max.x || hover.y < min.y || hover.y >= max.y {
            return None;
        }

        // The image is scaled (uniformly, preserving aspect ratio) to fit
        // whatever space egui gave it, so derive each tile's on-screen size
        // from the rendered rect rather than assuming a fixed points-per-texel
        // ratio. Otherwise this drifts as soon as the window is resized.
        let tile_x = window.width() / num_cols;
        let tile_y = window.height() / num_rows;

        let x_tile = hover.x - min.x;
        let y_tile = hover.y - min.y;
        let tx = (x_tile / tile_x).floor();
        let ty = (y_tile / tile_y).floor();

        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        Some((tx + (ty * num_cols)) as usize)
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

        // Mass fill the slice with white.
        chrtiles.data.fill(Color32::WHITE);

        let image_size = [
            tile_draw_data.tile_line_size,
            tile_draw_data.tile_height_size,
        ];

        // Once we get over 256 tiles we move to the other image.
        let mut do_left = true;

        for (orig, t) in tiles[*selected_chr].iter().enumerate() {
            let mut loc = orig;
            if loc >= tile_draw_data.tiles_per_image {
                if do_left {
                    // The left image is complete, upload it and reset to all
                    // WHITE again so hover from the left side doesn't carry
                    // over into the right image.
                    let im = egui::ColorImage::new(image_size, chrtiles.data.to_vec());
                    chrtiles.left.set(im, TextureOptions::default());
                    chrtiles.data.fill(Color32::WHITE);
                    do_left = false;
                }
                loc -= tile_draw_data.tiles_per_image;
            }

            // First figure out the row we're on and the first entry for it's
            // first pixel.
            let row_start_base = loc / tile_draw_data.tiles_per_row
                * tile_draw_data.tile_y_total
                * tile_draw_data.tile_line_size;

            // Now move N boxes over to find the box start pixel.
            let box_start_base =
                row_start_base + tile_draw_data.tile_x_total * (loc % tile_draw_data.tiles_per_row);

            // Now also adjust it N pixels down and over to account for buffers.
            // This way the painting below just deals with correct offsets into
            // the tile data.
            let box_start = box_start_base
                + (tile_draw_data.top_buffer
                    * tile_draw_data.tile_multiplier_y
                    * tile_draw_data.tile_line_size)
                + (tile_draw_data.left_buffer * tile_draw_data.tile_multiplier_x);

            // Using box_start_base fill in GREY on each row if this is the tile
            // be hovered over. The rest of the tile painting below will write over the
            // rest so this just becomes our outline.
            if chrtiles.hovered == Some(orig) {
                for y in 0..tile_draw_data.tile_y_total {
                    let row = box_start_base + y * tile_draw_data.tile_line_size;
                    for x in 0..tile_draw_data.tile_x_total {
                        chrtiles.data[row + x] = Color32::GRAY;
                    }
                }
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
        }

        // Upload whichever image was last being filled in (the other one was
        // already uploaded above when we crossed over to it).
        let im = egui::ColorImage::new(image_size, chrtiles.data.to_vec());
        if do_left {
            chrtiles.left.set(im, TextureOptions::default());
        } else {
            chrtiles.right.set(im, TextureOptions::default());
        }

        // Keep the preview showing something real (tile 0) rather than
        // blank when nothing is hovered, so its size always matches the
        // current single-tile multiplier instead of only updating -- and
        // only then getting its first real content -- once the user hovers
        // a tile for the first time.
        let preview_idx = chrtiles.hovered.unwrap_or(0);
        Self::redraw_single_preview(
            chrtiles.single,
            &tiles[*selected_chr][preview_idx],
            &PalContext {
                colors,
                color_source,
                selected_pal: *selected_pal,
            },
            tile_draw_data,
            chrtiles.tile_data,
        );
    }

    fn draw_a_tile(draw_data: &DrawData, data: &mut [Color32], tile_draw_data: &TileDrawData) {
        for y in 0..tile_draw_data.tile_y {
            for yi in 0..draw_data.mult_y {
                // Finally for each line adjust by the row we're on for each line.
                let y_off =
                    draw_data.box_start + (y * draw_data.mult_y + yi) * draw_data.tile_line_size;
                for x in 0..tile_draw_data.tile_x {
                    let start = x * draw_data.mult_x;

                    // Now lookup the tile data which is in range 0..NUM_COLORS
                    // Index that into colors to get the PAL entry.
                    // Now find that in the selected PAL to get the final RGB values.
                    let td = draw_data.tile.data[y * tile_draw_data.tile_y + x];
                    let col = draw_data.colors[usize::from(td)];
                    let color = &draw_data.color_source[draw_data.selected_pal].colors[col];
                    let pixel = Color32::from_rgb(color.r, color.g, color.b);

                    for i in 0..draw_data.mult_x {
                        data[y_off + start + i] = pixel;
                    }
                }
            }
        }
    }

    // Redraws the big single-tile preview shown between the 2 CHR images
    // and uploads it. Shared by the full redraw path and the hover-only
    // fast path.
    fn redraw_single_preview(
        single: &mut TextureHandle,
        tile: &Tile,
        pal: &PalContext,
        tile_draw_data: &TileDrawData,
        tile_data: &mut [Color32],
    ) {
        Self::draw_a_tile(
            &DrawData {
                box_start: 0,
                mult_x: tile_draw_data.single_tile_multiplier_x,
                mult_y: tile_draw_data.single_tile_multiplier_y,
                tile_line_size: tile_draw_data.single_tile_x_total,
                tile,
                colors: pal.colors,
                color_source: pal.color_source,
                selected_pal: pal.selected_pal,
            },
            tile_data,
            tile_draw_data,
        );
        let im = egui::ColorImage::new(
            [
                tile_draw_data.single_tile_x_total,
                tile_draw_data.single_tile_y_total,
            ],
            tile_data.to_vec(),
        );
        single.set(im, TextureOptions::default());
    }

    // Redraws just the box for tile `orig` (with its 1px separator border,
    // grey when `highlighted`) and uploads only that region via
    // `set_partial`. Used when the only thing that changed since last frame
    // is which tile is hovered, so we don't have to re-render and re-upload
    // all 512 tiles for a single tile's border to change color.
    fn redraw_hover_box(
        orig: usize,
        tile: &Tile,
        highlighted: bool,
        left: &mut TextureHandle,
        right: &mut TextureHandle,
        pal: &PalContext,
        tile_draw_data: &TileDrawData,
    ) {
        let mut loc = orig;
        let texture = if loc >= tile_draw_data.tiles_per_image {
            loc -= tile_draw_data.tiles_per_image;
            right
        } else {
            left
        };

        let fill = if highlighted {
            Color32::GRAY
        } else {
            Color32::WHITE
        };
        let mut scratch = vec![fill; tile_draw_data.tile_x_total * tile_draw_data.tile_y_total];

        Self::draw_a_tile(
            &DrawData {
                box_start: (tile_draw_data.top_buffer
                    * tile_draw_data.tile_multiplier_y
                    * tile_draw_data.tile_x_total)
                    + (tile_draw_data.left_buffer * tile_draw_data.tile_multiplier_x),
                mult_x: tile_draw_data.tile_multiplier_x,
                mult_y: tile_draw_data.tile_multiplier_y,
                tile_line_size: tile_draw_data.tile_x_total,
                tile,
                colors: pal.colors,
                color_source: pal.color_source,
                selected_pal: pal.selected_pal,
            },
            &mut scratch,
            tile_draw_data,
        );

        let row = loc / tile_draw_data.tiles_per_row;
        let col = loc % tile_draw_data.tiles_per_row;
        let pos = [
            col * tile_draw_data.tile_x_total,
            row * tile_draw_data.tile_y_total,
        ];
        let im = egui::ColorImage::new(
            [tile_draw_data.tile_x_total, tile_draw_data.tile_y_total],
            scratch,
        );
        texture.set_partial(pos, im, TextureOptions::default());
    }

    // Used for initial frames to get full layout figured out.
    fn pre_render(&mut self, ctx: &eframe::egui::Context) {
        // See the `remeasure_generation` field doc: a fresh id per remeasure
        // keeps egui from latching this scratch window onto the largest
        // size it's ever measured.
        let id = format!("pre_render_{}", self.remeasure_generation);
        egui::Window::new(id)
            .title_bar(false)
            .fixed_pos((0.0, 0.0))
            .show(ctx, |ui| {
                self.render(ui);
            });
    }

    // Renders the top "File" menu bar (Load/Save/Save As/Exit) and, when
    // one is pending, the confirm-overwrite dialog Save/Save As show before
    // clobbering an existing file.
    fn render_menu_bar(&mut self, ui: &mut Ui) {
        egui::Panel::top("menu_bar").show(ui, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Load...").clicked() {
                        ui.close();
                        if let Some(path) = rfd::FileDialog::new()
                            .add_filter("NES ROM", &["nes"])
                            .pick_file()
                        {
                            match load_cart_for_editing(&path.to_string_lossy()) {
                                Ok(cart) => self.load_new_cart(cart, path),
                                Err(e) => eprintln!("Failed to load {}: {e:?}", path.display()),
                            }
                        }
                    }
                    if ui.button("Save").clicked() {
                        ui.close();
                        self.handle_save(false);
                    }
                    if ui.button("Save As...").clicked() {
                        ui.close();
                        self.handle_save(true);
                    }
                    ui.separator();
                    if ui.button("Exit").clicked() {
                        ui.close();
                        ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });
            });
        });

        if let Some(path) = self.pending_overwrite.clone() {
            egui::Window::new("Overwrite file?").show(ui.ctx(), |ui| {
                ui.label(format!("{} already exists. Overwrite it?", path.display()));
                ui.horizontal(|ui| {
                    if ui.button("Overwrite").clicked() {
                        self.pending_overwrite = None;
                        match self.write_to(&path) {
                            Ok(()) => self.current_path = Some(path.clone()),
                            Err(e) => eprintln!("Failed to save {}: {e:?}", path.display()),
                        }
                    }
                    if ui.button("Cancel").clicked() {
                        self.pending_overwrite = None;
                    }
                });
            });
        }
    }

    // Handles a "Save" (`force_dialog` false) or "Save As" (`force_dialog`
    // true) click: picks the target path (prompting via a native save
    // dialog for Save As, or plain Save with nothing saved yet), then either
    // writes immediately or -- if that path already exists -- defers to the
    // confirm-overwrite dialog rendered by `render_menu_bar`.
    fn handle_save(&mut self, force_dialog: bool) {
        let target = if force_dialog || self.current_path.is_none() {
            rfd::FileDialog::new()
                .add_filter("NES ROM", &["nes"])
                .save_file()
        } else {
            self.current_path.clone()
        };
        let Some(path) = target else {
            return;
        };
        if path.exists() {
            self.pending_overwrite = Some(path);
        } else {
            match self.write_to(&path) {
                Ok(()) => self.current_path = Some(path),
                Err(e) => eprintln!("Failed to save {}: {e:?}", path.display()),
            }
        }
    }

    // Writes the current (possibly edited) cart out to `path`.
    fn write_to(&self, path: &Path) -> Result<()> {
        let bytes = build_output_bytes(&self.raw, self.chr_offset, &self.tiles)?;
        write(path, bytes)?;
        Ok(())
    }

    // Replaces the currently loaded cart with a freshly loaded one (from
    // File > Load), resetting selection/edit state that no longer applies
    // to the new cart and forcing the window to remeasure/resize.
    fn load_new_cart(&mut self, cart: EditableCart, path: PathBuf) {
        self.tiles = cart.tiles;
        self.raw = cart.raw;
        self.chr_offset = cart.chr_offset;
        self.current_path = Some(path);
        self.selection = Selection::new();
        self.edit_panel = None;
        self.render_stage = Stage::PreRender(2_isize);
        self.remeasure_generation += 1;
    }

    // The actual UI itself.
    fn render(&mut self, ui: &mut egui::Ui) {
        self.render_menu_bar(ui);

        // If a color picker button has been selected display the dialog.
        if let Some(bidx) = self.button {
            egui::Window::new("Color picker").show(ui.ctx(), |ui| {
                Self::color_picker(
                    &self.colors_per_pal,
                    self.selection.pal,
                    &mut self.button,
                    &mut self.dialog_selected,
                    &mut self.selection.colors,
                    bidx,
                    ui,
                );
            });
        }

        // If the tile edit panel is open show it (and its own color picker
        // dialog, if one of its local color slots is being changed) before
        // the central panel, since side panels must be added before it for
        // egui to give the central panel the remaining space correctly.
        if self.edit_panel.is_some() {
            if let Some(bidx) = self.edit_panel.as_ref().and_then(|e| e.color_button) {
                let Self {
                    colors_per_pal,
                    selection,
                    edit_panel,
                    ..
                } = self;
                let pal = selection.pal;
                #[allow(clippy::unwrap_used)]
                let edit = edit_panel.as_mut().unwrap();
                egui::Window::new("Edit tile color picker").show(ui.ctx(), |ui| {
                    Self::color_picker(
                        colors_per_pal.as_slice(),
                        pal,
                        &mut edit.color_button,
                        &mut edit.dialog_selected,
                        &mut edit.colors,
                        bidx,
                        ui,
                    );
                });
            }

            egui::Panel::right("edit_panel")
                .exact_size(EDIT_PANEL_WIDTH)
                .show(ui, |ui| {
                    self.render_edit_panel(ui);
                });
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
