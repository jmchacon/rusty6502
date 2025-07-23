//! `nes_pal_gui` contains common GUI helpers (for binaries) to use when
//! working with PAL files.
//! This is a separate package to exclude from coverage due to bugs with
//! calloop which egui needs on *nix.
use egui::{TextureHandle, TextureOptions};
use nes_pal::Color;
const WIDTH: usize = 40;
const NUM_PER_LINE: usize = 16;
const LINE_SIZE: usize = WIDTH * NUM_PER_LINE;
const HEIGHT: usize = 40;
const NUM_LINES: usize = 4;
const ENTRIES_PER_PIXEL: usize = 3; // RGB
const SIZE: usize = LINE_SIZE * HEIGHT * NUM_LINES * ENTRIES_PER_PIXEL;

/// `texture_from_palette` will take a given set of Colors and generate an
/// egui Texture from it as a 16x4 grid of 40x40 pixel blocks.
pub fn texture_from_palette(
    cc: &eframe::CreationContext<'_>,
    filename: &str,
    colors: &[Color],
) -> TextureHandle {
    let mut data = vec![0; SIZE].into_boxed_slice();

    for (loc, c) in colors.iter().enumerate() {
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
    cc.egui_ctx.load_texture(
        filename,
        egui::ImageData::Color(im.into()),
        TextureOptions::default(),
    )
}
