// Runs a program compiled with `mi compile --to-es`, implementing its
// mi-stats externals with the WebAssembly build of mi-stats.
// Usage: node host.mjs <compiled program .mjs> <mi_stats.mjs>

import { pathToFileURL } from "node:url";

const load = async (path) => (await import(pathToFileURL(path).href)).default;
const [main, createMiStats] = await Promise.all([load(process.argv[2]), load(process.argv[3])]);

// Instantiating the WebAssembly module is asynchronous, so it must be done
// before the (synchronous) program starts.
const S = await createMiStats();

// Keyed by the names of the `external` declarations (see stdlib/ext/dist-ext.mc)
const externals = {
  externalSetSeed: (seed) => S.setSeed(seed),
  externalGaussianSample: (mu, sigma) => S.normalSampleGlobal(mu, sigma),
  externalGaussianLogPdf: (x, mu, sigma) => S.normalLpdf(x, mu, sigma),
};
// The same externals under their newer names on the mi-stats branch
externals.externalNormalSample = externals.externalGaussianSample;
externals.externalNormalLogPdf = externals.externalGaussianLogPdf;

// The rest of the environment a compiled program expects; a reduced
// version of misc/node/node-env.mjs on the es-boot branch.
main({
  externals,
  print: (s) => process.stdout.write(s),
  printError: (s) => process.stderr.write(s),
  flushStdout: () => {},
  flushStderr: () => {},
  exit: (code) => process.exit(code),
  error: (msg) => { throw new Error(msg); },
  argv: () => process.argv.slice(1),
  // Intrinsics; `setSeed` in dist-ext.mc also reseeds these
  randIntU: (lo, hi) => lo + Math.floor(Math.random() * (hi - lo)),
  randSetSeed: (_seed) => {},  // Math.random cannot be seeded
});
