//! `nes_gui` contains common GUI helpers shared by this workspace's
//! `eframe`-based binaries: PAL-specific rendering
//! ([`texture_from_palette`]) plus general app-startup helpers
//! ([`parse_args_or_show`], [`show_error_and_exit`]) useful to any of them
//! regardless of whether they deal with PAL files.
//! This is a separate package to exclude from coverage due to bugs with
//! calloop which egui needs on *nix.
use clap::Parser;
use egui::{TextureHandle, TextureOptions};
use nes_pal::Color;

/// Width of tile in pixels.
pub const WIDTH: usize = 40;

/// Width of tile in pixels floating point.
pub const WIDTH_F: f32 = 40.0;

/// Number of tiles per line.
pub const NUM_PER_LINE: usize = 16;

/// Number of tiles per line floating point.
pub const NUM_PER_LINE_F: f32 = 16.0;

/// Line size in pixels.
pub const LINE_SIZE: usize = WIDTH * NUM_PER_LINE;

/// Line size in pixels floating point.
pub const LINE_SIZE_F: f32 = WIDTH_F * NUM_PER_LINE_F;

/// Height of tile in pixels.
pub const HEIGHT: usize = 40;

/// Height of tile in pixels floating point.
pub const HEIGHT_F: f32 = 40.0;

/// Number of lines in a palette texture.
pub const NUM_LINES: usize = 4;

/// Number of lines in a palette texture floating point.
pub const NUM_LINES_F: f32 = 4.0;
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

/// Shows `message` in a native, cross-platform dialog and exits the process
/// with status 1.
///
/// These binaries are GUI apps (`windows_subsystem = "windows"` on Windows
/// release builds, so no console is attached for `eprintln!`/a
/// `Result`-returning `main`'s error output to go to -- it's silently lost).
/// Routing every fallible startup step through this instead of printing
/// makes errors visible consistently on every platform, not just whichever
/// ones happen to have a console attached to see them.
pub fn show_error_and_exit(message: &str) -> ! {
    rfd::MessageDialog::new()
        .set_level(rfd::MessageLevel::Error)
        .set_title("Error")
        .set_description(message)
        .show();
    std::process::exit(1);
}

/// Parses `T` from the real command line the same way [`clap::Parser::parse`]
/// would, except a `--help`/`--version` request or a usage error is shown in
/// a native dialog (via [`show_error_and_exit`] for a real error, or a plain
/// info dialog for `--help`/`--version`) using the same text clap would
/// otherwise have printed, instead of to a console that may not exist.
#[must_use]
pub fn parse_args_or_show<T: Parser>() -> T {
    match T::try_parse() {
        Ok(args) => args,
        Err(e) => {
            let message = e.render().to_string();
            if matches!(
                e.kind(),
                clap::error::ErrorKind::DisplayHelp | clap::error::ErrorKind::DisplayVersion
            ) {
                rfd::MessageDialog::new()
                    .set_level(rfd::MessageLevel::Info)
                    .set_title(T::command().get_name())
                    .set_description(message)
                    .show();
                std::process::exit(0);
            }
            show_error_and_exit(&message);
        }
    }
}
