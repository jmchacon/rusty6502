---
name: single-step-tests
description: Set up (or verify) the SingleStepTests/65x02 CPU test-vector checkout and get SINGLE_STEP_6502_PROCESSOR_TESTS exported so `cargo test -p cpu` runs the full per-opcode coverage tests instead of silently skipping them. Use whenever the user asks to run/enable/set up the single step tests, mentions SINGLE_STEP_6502_PROCESSOR_TESTS, or `cargo test -p cpu` reports "Skipping tests" / coverage_opcodes_tests failures.
---

# Single step CPU coverage tests

`cpu/src/tests.rs` (`coverage_opcodes_tests`, around line 3020) replays the
[SingleStepTests/65x02](https://github.com/SingleStepTests/65x02) per-opcode JSON
test vectors against every CPU variant (`6502/v1`, `nes6502/v1`, `wdc65c02/v1`,
`rockwell65c02/v1`, `synertek65c02/v1`). If the `SINGLE_STEP_6502_PROCESSOR_TESTS`
env var isn't set to the root of that checkout, these tests print "Skipping
tests because SINGLE_STEP_6502_PROCESSOR_TESTS isn't set" and pass trivially —
they are NOT run. Always get this checkout in place before treating `cargo
test -p cpu` (or a full workspace test run) as a real signal of CPU
correctness.

The checkout is large — full working tree is ~5-6GB (no git-lfs, just a lot of
JSON). Never put it inside this repo (it would make every file-tree walk,
editor index, or `cargo` invocation choke on hundreds of thousands of extra
files, and it must never be committed). Keep it as a sibling directory
outside the repo.

## Steps

1. Find the repo root:
   ```
   REPO_ROOT=$(git rev-parse --show-toplevel)
   ```

2. Check whether the env var is already set to a valid checkout for this
   session (has the 5 top-level dirs the tests read from):
   ```
   [ -n "$SINGLE_STEP_6502_PROCESSOR_TESTS" ] && [ -d "$SINGLE_STEP_6502_PROCESSOR_TESTS/wdc65c02/v1" ]
   ```
   If that's true, skip to step 5 (still worth confirming settings.local.json
   is persisted, see step 6).

3. If not set/invalid, look for an existing checkout before cloning a fresh
   ~6GB copy. Check these candidates in order (both are just conventions,
   the actual test code accepts any path via the env var):
   - `$(dirname "$(dirname "$REPO_ROOT")")/SingleStepTests/65x02` — mirrors a
     GOPATH-style `src/github.com/<org>/<repo>` layout, which is what the
     panic message in `cpu/src/tests.rs` suggests as an example relative
     path (`../../../SingleStepTests/65x02` from the `cpu` crate dir).
   - `$(dirname "$REPO_ROOT")/SingleStepTests-65x02` — plain sibling
     directory, works regardless of layout convention.

   Validate any candidate the same way as step 2 (`wdc65c02/v1` subdir
   present) before trusting it.

4. If no valid checkout is found anywhere, clone one. This is a big,
   slow, one-time download — tell the user before starting and run it with a
   generous timeout (or in the background):
   ```
   git clone --depth 1 https://github.com/SingleStepTests/65x02.git "$(dirname "$REPO_ROOT")/SingleStepTests-65x02"
   ```

5. Export it for the current session and confirm the tests actually run
   (not skip):
   ```
   export SINGLE_STEP_6502_PROCESSOR_TESTS="<resolved absolute path>"
   cargo test -p cpu coverage_opcodes_tests
   ```
   All 6 tests (`c6502`, `c6510`, `ricoh`, `cmos_wdc`, `cmos_rockwell`,
   `cmos_c65sc02`) should pass — if the output still says "Skipping tests",
   the path is wrong.

6. Persist it for future sessions so this skill doesn't need re-running every
   time. `.claude/settings.local.json` is gitignored (machine-local, unlike
   `.claude/settings.json` which is committed and shared — never put an
   absolute local path there). Read the existing
   `.claude/settings.local.json` (create `{}` if missing), merge in a
   top-level `env` key without disturbing the existing `permissions` block,
   and write it back, e.g.:
   ```json
   {
     "permissions": { "...": "existing content, unchanged" },
     "env": {
       "SINGLE_STEP_6502_PROCESSOR_TESTS": "<resolved absolute path>"
     }
   }
   ```
   Once this is saved, future sessions get the env var automatically without
   invoking this skill again — only re-run it if the checkout has moved or
   `cargo test -p cpu` starts reporting "Skipping tests" again.
