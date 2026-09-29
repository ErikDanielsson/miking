#!/usr/bin/env fish
# Compiles mi-stats to a WebAssembly module with JavaScript bindings,
# using Emscripten. Used by `make mi-stats-wasm` in the main Makefile.
#
# Usage: EMXX=<compiler, e.g. em++> build.fish <output directory>

if test (count $argv) -ne 1; or not set -q EMXX
    echo "usage: EMXX=<compiler, e.g. em++> "(status filename)" <output directory>" >&2
    exit 1
end

set outDir $argv[1]

set miStatsDir (path resolve (status dirname)/..)
set vendorDir $miStatsDir/vendor
set bindings $miStatsDir/wasm/bindings.cpp
# Emscripten also writes the `.wasm` file next to this one
set output $outDir/mi_stats.mjs

mkdir -p $outDir
$EMXX -std=c++17 -O3 -DBOOST_MATH_STANDALONE \
    -I$vendorDir -I$vendorDir/eigen -I$vendorDir/boost \
    -fwasm-exceptions -lembind \
    -sMODULARIZE -sEXPORT_ES6 -sEXPORT_NAME=createMiStats \
    -sENVIRONMENT=web,node -sALLOW_MEMORY_GROWTH \
    $bindings -o $output
