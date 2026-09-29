// Entry point for the WebAssembly build of mi-stats (`make mi-stats-wasm`
// in the main Makefile; tests in ../test-compare).

#include <emscripten/bind.h>

#include "../cpp/normal.hpp"

using namespace emscripten;

// A global rng stream, mirroring `global_rng` in ../mi_stats.ml: seeded
// with 0 until `setSeed` replaces it.
static mi_rng global_rng(0);

static void mi_set_seed(unsigned int seed) {
  global_rng = mi_rng(seed);
}

static double mi_normal_sample_global(double mu, double sigma) {
  return mi_normal_sample(mu, sigma, global_rng);
}

EMSCRIPTEN_BINDINGS(mi_stats) {
  class_<mi_rng>("Rng")
    .constructor<unsigned int>();

  function("normalLpdf", &mi_normal_lpdf);
  function("normalSample", &mi_normal_sample);

  // Using the global rng stream
  function("setSeed", &mi_set_seed);
  function("normalSampleGlobal", &mi_normal_sample_global);
}
