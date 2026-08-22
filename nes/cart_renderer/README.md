# Running

To run on windows:

Set-Item -Path "Env:RUST_BACKTRACE" -Value "1"
cargo build --release
c:\Users\chaco\.build\release\cart_renderer.exe --pal ./testdata/NTSC.pal <path to .nes file>
