//! Snapshot tests for the `cart_renderer` UI. These render the app
//! headlessly via `egui_kittest` and compare against baseline images
//! committed under `tests/snapshots/`. Run with `UPDATE_SNAPSHOTS=1 cargo
//! test -p cart_renderer` to (re)generate the baselines after an
//! intentional UI change.
use cart_renderer::{load_cart, load_pal, Data, MyApp};
use color_eyre::eyre::{eyre, Result};
use egui::accesskit::Role;
use egui::Pos2;
use egui_kittest::kittest::{NodeT, Queryable};
use egui_kittest::Harness;
use nes_chr::Tile;

fn workspace_path(rel: &str) -> String {
    format!("{}/../../{rel}", env!("CARGO_MANIFEST_DIR"))
}

/// Loads the shared `NTSC.pal` + `nestest.nes` fixtures already used by
/// this crate's other tests and by manual smoke-testing during development.
fn load_fixtures() -> Result<(Data, Vec<Vec<Tile>>)> {
    let pal = load_pal(&workspace_path("testdata/NTSC.pal"))?;
    let tiles = load_cart(&workspace_path("testdata/nestest.nes"))?;
    Ok((pal, tiles))
}

/// Moves the pointer off the rendered content and lets a frame settle. Used
/// after driving a click via [`kittest::Node::click`], since that leaves the
/// pointer at the clicked node's position, which -- once a popup closes and
/// the layout reflows -- can coincidentally land on a CHR tile and register
/// as an incidental hover.
fn move_pointer_away<S>(harness: &mut Harness<S>) {
    harness.hover_at(Pos2::new(-10.0, -10.0));
    harness.run();
}

#[test]
fn default_view() -> Result<()> {
    let (pal, tiles) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], tiles));

    harness.snapshot("default_view");
    Ok(())
}

#[test]
fn chr_magnification_4x() -> Result<()> {
    let (pal, tiles) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], tiles));

    harness.get_by_label("Magnification").click();
    harness.run();
    harness.get_by_label("4x").click();
    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("chr_magnification_4x");
    Ok(())
}

#[test]
fn preview_magnification_16x() -> Result<()> {
    let (pal, tiles) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], tiles));

    // The preview's magnification combo (unlike "CHR set"/"Magnification")
    // has no `.from_label(...)`, so it isn't found by label; its default
    // selected value ("8x") is unique enough to find it by instead.
    harness.get_by_value("8x").click();
    harness.run();
    harness.get_by_label("16x").click();
    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("preview_magnification_16x");
    Ok(())
}

#[test]
fn color_picker_changes_tile_color() -> Result<()> {
    let (pal, tiles) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], tiles));

    // This ROM's CHR tiles only use palette indices 0 (background) and 3,
    // so "Color 3" is the button that visibly affects the rendered tiles;
    // "Color 1"/"Color 2" would change their own swatch but nothing else.
    harness.get_by_label("Color 3").click();
    harness.run();

    // The 64 palette swatch buttons in the picker are image-only (no
    // accessible label), so find them by position: they're the only
    // unlabeled buttons in the tree, in the same row-major order they're
    // drawn in, so the Nth one is palette index N.
    let swatch_0x23 = harness
        .get_all_by_role(Role::Button)
        .filter(|n| n.accesskit_node().label().is_none())
        .nth(0x23)
        .ok_or_else(|| eyre!("expected 64 unlabeled palette swatch buttons"))?;
    swatch_0x23.click();
    harness.run();

    harness.get_by_label("Select").click();
    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("color_picker_changes_tile_color");
    Ok(())
}
