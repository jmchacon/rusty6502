//! `cart_renderer` CLI entry point: parses arguments and hands off to the
//! `cart_renderer` library crate (see `lib.rs`) for the actual app.
use cart_renderer::{load_cart_for_editing, load_pal, EditableCart, MyApp};
use clap::Parser;
use color_eyre::eyre::{eyre, Result};
use std::path::PathBuf;

/// `cart_renderer` will load the given PAL files and the NES and render the CHR sections
/// along with color selection. Both are optional: with neither, it starts
/// with an all-white palette and an empty tile set, ready for File > Load.
#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg(
        help = "Filenames containing .pal data (can be specified N times)",
        long
    )]
    pal: Vec<String>,

    #[arg(help = "Filename for cart in INES format")]
    filename: Option<String>,
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Args = Args::parse();

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

    let res = eframe::run_native(
        "NES file CHR renderer",
        options,
        Box::new(|cc| Ok(Box::new(MyApp::new(cc, colors, cart, current_path)))),
    );

    if let Err(e) = res {
        return Err(eyre!("EGUI error: {e:?}"));
    }

    Ok(())
}
