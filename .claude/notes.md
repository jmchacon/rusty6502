# Session notes: cart_renderer tile editor, ines cleanup, GUI error handling

Covers three merged PRs: #300, #301, #303. Written as context for future
sessions picking this work back up.

## PR #300 — cart_renderer tile editor + ines cleanup

Added a per-pixel tile editor to `cart_renderer` plus a File menu
(Load/Save/Save As/Exit), and cleaned up `ines`/`cart_renderer` overlap.

### Tile editor (`nes/cart_renderer/src/lib.rs`)

- `EditPanelState`: working copy of a locked tile's pixels + a *local* copy
  of the 4 color slots (preview-only — never written back; only `pixels` is
  saved). `original_pixels`/`original_colors` are snapshotted once on open
  and **never updated by Save** — Revert always goes back to how the tile
  looked when the panel was opened, no matter how many times Save was hit
  in between. Only Exit (closing the panel) starts a fresh baseline next
  time.
- Edit button only enabled once a tile is hover-locked.
- While the edit panel is open, the main window is `ui.disable()`d (same
  mechanism the color-picker modal already used) — can't clear hover lock,
  press Edit again, or change colors/CHR set/magnification underneath the
  panel.
- Edit panel's own preview magnification capped at **12x** (not 16x like
  the main preview) — past ~14x it pushes Revert/Save/Exit out of the
  panel, and unlike the main preview this one doesn't trigger a window
  resize to compensate. Not worth the extra logic for that corner case.

### Window auto-resize bug (the hard part)

The app auto-sizes its window to content via a `Stage` state machine
(`PreRender` → `FirstRender` → `FirstResize` → `Initialized`) that measures
"natural size" through a nested scratch `egui::Window`, then issues
`ViewportCommand::InnerSize`. Adding the edit panel exposed two real bugs
here, both now fixed:

1. **Panel width vs. content width**: `egui::Panel` doesn't auto-size to
   its content — it has a fixed default outer size, and if content wants
   more room than that, egui lets it overflow past the panel's allotted
   slot and then clamps the panel back to its declared width *anchored on
   the overflowed edge* rather than the window's true edge. This visibly
   pushed the whole panel past the window's right side. Fix: give the
   panel `.exact_size(EDIT_PANEL_WIDTH)` with `EDIT_PANEL_WIDTH` wide
   enough (380px) that its content (the 8x8 pixel grid, ~344px) never
   overflows.
2. **Remeasuring while the panel is open is unreliable**: at the moment
   the panel opens, the window is still its old, narrower size, so the
   unshrinkable main content and the new panel fight over too little room
   — a remeasure taken at that instant reflects the squeeze, not the true
   size needed. Fix: track `base_size` (window size last measured with the
   panel *closed*) and, when opening/closing the panel, target
   `base_size ± EDIT_PANEL_WIDTH` directly instead of trusting a
   remeasure taken while it's open.
   `render()` also gained a `show_edit_panel: bool` param so the panel is
   never shown during `pre_render`'s scratch-window warm-up passes
   (nothing to gain since the target is now computed analytically, and
   showing it there was actively corrupting `egui::Panel`'s persisted
   per-id layout state).

### File menu / Save / NES 2.0

- File > Load/Save/Save As/Exit, with a confirm-overwrite dialog when
  Save/Save As would clobber an existing file.
- Save patches only the CHR ROM region of the original file's bytes —
  everything else (PRG, header flags including NES 2.0-only fields like
  submapper, trainer, misc ROM) round-trips byte-for-byte. Verified with
  dedicated tests, since an earlier claim that NES 2.0 metadata would be
  lost on save turned out to be **wrong** on inspection — worth
  remembering: verify before "fixing" a suspected bug.
- A from-scratch save (no original file — `EditableCart::blank()`) now
  synthesizes a **minimal NES 2.0 header** (mapper 0/NROM, NTSC timing) via
  `ines::minimal_nes20_header()`, not plain iNES 1.0.
- Caught a real bug while adding tests for the above:
  `EditableCart::blank()` was allocating only 256 tiles instead of the 512
  needed for a full CHR bank (8KB / 16 bytes/tile) — `Save` on a blank
  canvas would have failed outright. First time that path was actually
  exercised by a test.
- `--pal` is required again on the CLI (at least one); cart filename stays
  optional. The library-level "synthesize an all-white palette" fallback
  in `MyApp::new` stays for other callers/tests, just isn't reachable from
  the shipped binary anymore.

### `ines` crate now exposes what `cart_renderer` needs

Avoid duplicating format constants/logic across crates:
- `ines::HEADER_SIZE_U`, `ines::NES20_CART_SIG` made `pub`.
- `NES::chr_offset()` — the header+trainer+PRG-banks offset formula,
  also now used internally by `parse()` itself (previously duplicated).
- `ines::minimal_nes20_header(chr_pages: u8) -> [u8; HEADER_SIZE_U]` for
  synthesizing a from-scratch NES 2.0 header.

## PR #301 — test reorg (unrelated to cart_renderer)

Moved `convertprg`/`disassembler`/`handasm`'s inline `mod tests { ... }`
blocks into sibling `tests.rs` files, matching the pattern used everywhere
else in the workspace (`ines`, `nes_chr`, `nes_pal`, `cpu`, `memory`,
`monitor`, `assemble`, ...). The tiny inline `verify_cli` clap self-check
stays in each `main.rs`, matching `assembler`'s existing precedent.

**Branch-hygiene note for next time**: this got accidentally committed
onto the `cart-renderer-tile-editor` PR branch first (unrelated work
landing on a feature branch). Fixed by creating a new branch at that
commit, then `git reset --soft` + selective `git restore` to pull it back
off the feature branch without losing the in-progress uncommitted edits
sitting on top. When rebasing a branch cut from a not-yet-merged feature
branch onto `main` after that feature merges, a plain rebase replays
*every* commit back to the true merge-base and conflicts hard against the
squashed history — cherry-picking just the one real commit onto fresh
`main` is far cleaner when the diff is self-contained.

## PR #303 — nes_pal_gui → nes_gui rename + GUI error handling

Triggered by: "why does `nes_pal_render` not wait at the prompt for the
window to exit, unlike `cart_renderer`?" — answer: `nes_pal_render`
(and `monitor_gui`) already had
`#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]`,
which on Windows release builds means no attached console, so the
launching shell doesn't wait. `cart_renderer` didn't have it.

Instead of just adding a console to `nes_pal_render`, went the other
direction — made both consistently GUI apps with no console expected,
anywhere:

- `cart_renderer` gained the same `windows_subsystem` attribute.
- New shared helpers in `nes_gui` (renamed from `nes_pal_gui` — it's now a
  general `eframe`-app helper crate, not PAL-specific):
  - `show_error_and_exit(message: &str) -> !` — native cross-platform
    error dialog via `rfd::MessageDialog`, then `exit(1)`.
  - `parse_args_or_show<T: clap::Parser>() -> T` — replaces
    `Args::parse()`; a bad-flag error or `--help`/`--version` now shows in
    a dialog (info-level for help/version, error-level otherwise) instead
    of printing to a console that may not exist, on *any* platform, not
    just Windows release.
- Both binaries' `main()` restructured: `fn main() -> Result<()>` (whose
  `Err` used to vanish silently with no console) → thin `fn main()` that
  calls `run() -> Result<()>` and routes any error through
  `show_error_and_exit`.
- `nes_pal_render`'s `--filename` is now `required = true`, replacing a
  manual post-parse empty check.
- **Known tradeoff**: `--help`/`--version` now also show in a dialog on
  every platform, including a normal terminal — you won't see them
  printed inline anymore. If that's ever unwanted, drop the
  `DisplayHelp`/`DisplayVersion` special case in `parse_args_or_show` and
  let those print+exit normally.

### Coverage exclusion — tested and dropped

`nes_pal_gui` had `test = false`/`doctest = false` and was `--exclude`d
from the `ubuntu / beta / coverage` CI job's `cargo llvm-cov` invocation,
per a comment: "calloop (*nix requirement) can't handle coverage on the
version we inherit." Tested whether this was still true by simply
removing the `--exclude` flag and watching CI: **the coverage job passed
fine**. Whatever calloop bug motivated the exclusion appears fixed
upstream since. The flag is gone for good; `nes_gui` (still has no tests
of its own) was added to `.github/codecov.yml`'s `ignore` list instead,
matching the other GUI-only crates there, so it doesn't drag down the
coverage percentage without actually needing a build-time exclusion.

## Workflow notes for this session

- `gh` wasn't on PATH in this shell; full path is
  `/c/Program Files/GitHub CLI/gh.exe`.
- This repo requires PR review before merge and only allows squash/rebase
  merges (no merge commits) — `gh pr merge <n> --squash --delete-branch
  --admin` is the pattern used to self-merge as the repo owner once CI is
  green.
- `windows-latest / stable` CI intermittently fails with a known flaky
  `STATUS_ACCESS_VIOLATION` (exit 139) from `cart_renderer`'s headless
  WARP/DX12 test rendering — the workflow already retries it 5x; a solo
  red run there isn't necessarily a real regression, re-running the job
  (or just re-pushing) usually clears it.
- Long CI jobs to expect: `miri` (~35-60 min), `sanitizers` (~20-40 min),
  `ubuntu / beta / coverage` (~50-65 min). Everything else finishes in
  under ~10 min.
