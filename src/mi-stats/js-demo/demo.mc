-- A minimal program using the distribution externals from
-- stdlib/ext/dist-ext.mc, compiled with the ecmascript backend
-- (`mi compile --to-es`) and run by host.mjs, which implements the
-- externals with the WebAssembly build of mi-stats.

include "common.mc"
include "string.mc"
include "ext/dist-ext.mc"

mexpr
setSeed 42;
printLn "Five samples from N(0, 1) with seed 42:";
iter (lam x. printLn (float2string x))
  (create 5 (lam. gaussianSample 0.0 1.0));
printLn (concat "log N(0 | 0, 1) = " (float2string (gaussianLogPdf 0.0 1.0 0.0)))
