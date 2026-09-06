//! Snapshot tests for the `cart_renderer` UI. These render the app
//! headlessly via `egui_kittest` and compare against baseline images
//! committed under `tests/snapshots/`. Run with `UPDATE_SNAPSHOTS=1 cargo
//! test -p cart_renderer` to (re)generate the baselines after an
//! intentional UI change.
use cart_renderer::{load_cart_for_editing, load_pal, Data, EditableCart, MyApp};
use color_eyre::eyre::{eyre, Result};
use egui::accesskit::Role;
use egui::Pos2;
use egui_kittest::kittest::{NodeT, Queryable};
use egui_kittest::Harness;

fn workspace_path(rel: &str) -> String {
    format!("{}/../../{rel}", env!("CARGO_MANIFEST_DIR"))
}

/// Loads the shared `NTSC.pal` + `nestest.nes` fixtures already used by
/// this crate's other tests and by manual smoke-testing during development.
fn load_fixtures() -> Result<(Data, EditableCart)> {
    let pal = load_pal(&workspace_path("testdata/NTSC.pal"))?;
    let cart = load_cart_for_editing(&workspace_path("testdata/nestest.nes"))?;
    Ok((pal, cart))
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
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    harness.snapshot("default_view");
    Ok(())
}

#[test]
#[allow(clippy::unnecessary_wraps)] // matches the other tests' `Result<()>` signature
fn blank_start_renders() -> Result<()> {
    // No PAL/cart file given (see `EditableCart::blank`, and `MyApp::new`
    // synthesizing an all-white palette when `datas` is empty) -- this is
    // what happens if `cart_renderer` is launched with no filename.
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![], EditableCart::blank(), None));

    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("blank_start_renders");
    Ok(())
}

#[test]
fn file_menu_opens() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    harness.get_by_label("File").click();
    harness.run();

    harness.snapshot("file_menu_opens");
    Ok(())
}

#[test]
fn chr_magnification_4x() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

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
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    // The preview's magnification combo (unlike "CHR set"/"Magnification")
    // has no `.from_label(...)`, so it isn't found by label; its default
    // selected value ("8x") is unique enough to find it by instead.
    harness.get_by_value("8x").click();
    harness.run();

    // With 16 entries the popup is scrollable and "16x" starts out of
    // view (there's a `ScrollBar` node in the tree once it's open) --
    // clicking it directly just clicks whatever *is* visible underneath
    // instead, silently leaving the selection at "8x". Scroll it into
    // view first, then re-query: `run()` rebuilds the tree, so a `Node`
    // borrowed before it can't be reused after.
    harness.get_by_label("16x").scroll_to_me();
    harness.run();
    harness.get_by_label("16x").click();
    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("preview_magnification_16x");
    Ok(())
}

#[test]
fn color_picker_changes_tile_color() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

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

/// Hovers, then primary-clicks, a point inside the left CHR tile image to
/// lock hover onto whatever tile is there -- the "Edit" button is disabled
/// until a tile is locked in this way (see `render_image_row`'s gating on
/// `hover_locked`). This needs 2 separate frames: locking checks the
/// *previous* frame's hover state, so hovering and clicking in the same
/// frame wouldn't register.
fn lock_tile_0<S>(harness: &mut Harness<S>) {
    // A few pixels inside the left image's top-left corner, comfortably
    // inside tile 0 regardless of magnification.
    let pos = Pos2::new(15.0, 319.0);
    harness.hover_at(pos);
    harness.run();
    harness.drag_at(pos);
    harness.run();
    harness.drop_at(pos);
    harness.run();
}

#[test]
fn edit_panel_opens() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    lock_tile_0(&mut harness);
    harness.get_by_label("Edit").click();
    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("edit_panel_opens");
    Ok(())
}

#[test]
fn edit_button_disabled_until_a_tile_is_locked() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    harness.run();
    move_pointer_away(&mut harness);

    harness.snapshot("edit_button_disabled_until_a_tile_is_locked");
    Ok(())
}

#[test]
fn edit_panel_pixel_dropdown() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    lock_tile_0(&mut harness);
    harness.get_by_label("Edit").click();
    harness.run();

    // The 64 pixel-grid cells are unlabeled (image-only), same as the
    // palette swatches in `color_picker_changes_tile_color` -- click the
    // first one to open its color menu.
    click_pixel_0(&mut harness)?;

    harness.snapshot("edit_panel_pixel_dropdown");
    Ok(())
}

/// Opens pixel #0's color menu (see `edit_panel_pixel_dropdown` -- same
/// approach, factored out since this test needs it twice).
fn click_pixel_0<S>(harness: &mut Harness<S>) -> Result<()> {
    let cell_0 = harness
        .get_all_by_role(Role::Button)
        .find(|n| n.accesskit_node().label().is_none())
        .ok_or_else(|| eyre!("expected at least one unlabeled pixel-grid cell"))?;
    cell_0.click();
    harness.run();
    Ok(())
}

/// Picks an entry from pixel #0's open color menu (see `click_pixel_0`) by
/// label. There are two same-labeled buttons once the menu is open -- this
/// one, and the panel's own "Local colors" swatch above it -- so a plain
/// `get_by_label` is ambiguous. The popup renders lower on screen (it opens
/// at the clicked pixel, below the color slots), so picking the node with
/// the larger `rect().min.y` disambiguates them.
fn choose_pixel_color<S>(harness: &mut Harness<S>, label: &str) -> Result<()> {
    let node = harness
        .get_all_by_label(label)
        .max_by(|a, b| a.rect().min.y.total_cmp(&b.rect().min.y))
        .ok_or_else(|| eyre!("expected a \"{label}\" button in the open pixel color menu"))?;
    node.click();
    harness.run();
    Ok(())
}

#[test]
fn revert_undoes_a_save_back_to_the_panel_opened_state() -> Result<()> {
    let (pal, cart) = load_fixtures()?;
    let mut harness = Harness::new_eframe(|cc| MyApp::new(cc, vec![pal], cart, None));

    lock_tile_0(&mut harness);
    harness.get_by_label("Edit").click();
    harness.run();

    // Pixel #0 starts on "Background" (black). Set it to "Color 3" (white)
    // and Save -- this commits to the main CHR data, which is the point:
    // Revert afterward should still undo it, not treat it as a new baseline.
    click_pixel_0(&mut harness)?;
    choose_pixel_color(&mut harness, "Color 3")?;
    harness.get_by_label("Save").click();
    harness.run();

    // Edit it again post-save (to "Color 1", also white -- the exact color
    // doesn't matter, only that there's now an unsaved edit on top of the
    // saved one) so Revert has two layers of change to undo in one go.
    click_pixel_0(&mut harness)?;
    choose_pixel_color(&mut harness, "Color 1")?;

    harness.get_by_label("Revert").click();
    harness.run();
    move_pointer_away(&mut harness);

    // Pixel #0 should be back to "Background" (black), as it was when the
    // panel was first opened -- not "Color 3" (the saved state) and not
    // "Color 1" (the last unsaved edit).
    harness.snapshot("revert_undoes_a_save_back_to_the_panel_opened_state");
    Ok(())
}
