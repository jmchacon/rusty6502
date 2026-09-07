#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

//! `cart_renderer` CLI entry point: parses arguments and hands off to the
//! `cart_renderer` library crate (see `lib.rs`) for the actual app.
use cart_renderer::{load_cart_for_editing, load_pal, EditableCart, MyApp};
use color_eyre::eyre::{eyre, Result};
use std::path::PathBuf;

/// `cart_renderer` will load the given PAL files and the NES and render the CHR sections
/// along with color selection. At least one `--pal` is required; the cart
/// filename is optional -- with none, it starts with an empty tile set,
/// ready for File > Load.
#[derive(clap::Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(
        help = "Filenames containing .pal data (must be specified at least once, can be specified N times)",
        long,
        required = true
    )]
    pal: Vec<String>,

    #[arg(help = "Filename for cart in INES format")]
    filename: Option<String>,
}

// This is a GUI app with no attached console to print an error to (see the
// `windows_subsystem` attribute above), on any platform -- so any startup
// failure goes through `nes_gui::show_error_and_exit` instead of being
// returned/printed, which would otherwise be silently lost.
fn main() {
    if let Err(e) = run() {
        nes_gui::show_error_and_exit(&format!("{e:?}"));
    }
}

fn run() -> Result<()> {
    color_eyre::install()?;
    let args: Args = nes_gui::parse_args_or_show();

    env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).

    // The window is sized to exactly fit its content (see `Stage` in the
    // library crate), so there's nothing sensible for the user to
    // drag-resize it to. Resizing still happens programmatically via
    // `ViewportCommand::InnerSize`, which is independent of this and
    // unaffected by it.
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_resizable(false),
        ..Default::default()
    };

    let mut colors = Vec::new();
    for f in &args.pal {
        colors.push(load_pal(f)?);
    }

    let (cart, current_path) = if let Some(filename) = &args.filename {
        (
            load_cart_for_editing(filename)?,
            Some(PathBuf::from(filename)),
        )
    } else {
        (EditableCart::blank(), None)
    };

    eframe::run_native(
        "NES file CHR renderer",
        options,
        Box::new(|cc| Ok(Box::new(MyApp::new(cc, colors, cart, current_path)))),
    )
    .map_err(|e| eyre!("EGUI error: {e:?}"))?;

    Ok(())
}
