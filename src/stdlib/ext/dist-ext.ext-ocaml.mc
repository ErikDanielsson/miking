

include "map.mc"
include "ocaml/ast.mc"
include "string.mc"
include "mexpr/ast-builder.mc"

let distExtMap =
  use OCamlTypeAst in
  mapFromSeq cmpString
  [
    ("externalChiSquareLogPdf", [
      { expr = "Mi_stats.mi_chi_square_lpdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalChiSquareSample", [
      { expr = "Mi_stats.mi_chi_square_rng_",
        ty = tyarrows_ [tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalChiSquareCdf", [
      { expr = "Mi_stats.mi_chi_square_cdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalExponentialSample", [
      { expr = "Owl_stats.exponential_rvs",
        ty = tyarrows_ [otylabel_ "lambda" tyfloat_, tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalGammaLogPdf", [
      { expr = "Mi_stats.mi_gamma_lpdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalGammaSample", [
      { expr = "Mi_stats.mi_gamma_rng_",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalGammaCdf", [
      { expr = "Mi_stats.mi_gamma_cdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBernoulliLogPmf", [
      { expr = "Mi_stats.mi_bernoulli_lpmf",
        ty = tyarrows_ [tybool_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBernoulliSample", [
      { expr = "Mi_stats.mi_bernoulli_rng_",
        ty = tyarrows_ [tyfloat_, tybool_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBinomialLogPmf", [
      { expr = "Mi_stats.mi_binomial_lpmf",
        ty = tyarrows_ [tyint_, tyint_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBinomialSample", [
      { expr = "Mi_stats.mi_binomial_rng_",
        ty = tyarrows_ [tyint_, tyfloat_, tyint_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBetaLogPdf", [
      { expr = "Mi_stats.mi_beta_lpdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBetaSample", [
      { expr = "Mi_stats.mi_beta_rng_",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBetaBinomialLogPmf", [
      { expr = "Mi_stats.mi_beta_binomial_lpmf",
        ty = tyarrows_ [tyint_, tyint_, tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalBetaBinomialSample", [
      { expr = "Mi_stats.mi_beta_binomial_rng_",
        ty = tyarrows_ [tyint_, tyfloat_, tyfloat_, tyint_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalNormalLogPdf", [
      { expr = "Mi_stats.mi_normal_lpdf",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalNormalSample", [
      { expr = "Mi_stats.mi_normal_rng_",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["mi_stats"],
        cLibraries = []
      }
    ]),
    ("externalMultinomialLogPmf", [
      { expr = "Owl_stats.multinomial_logpdf ",
        ty = tyarrows_ [otyarray_ tyint_, otylabel_ "p" (otyarray_ tyfloat_), tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalMultinomialSample", [
      { expr = "Owl_stats.multinomial_rvs ",
        ty = tyarrows_ [tyint_, otylabel_ "p" (otyarray_ tyfloat_), otyarray_ tyint_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalCategoricalSample", [
      { expr = "Owl_stats.categorical_rvs ",
        ty = tyarrows_ [otyarray_ tyfloat_, tyint_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalDirichletLogPdf", [
      { expr = "Owl_stats.dirichlet_logpdf ",
        ty = tyarrows_ [otyarray_ tyfloat_, otylabel_ "alpha" (otyarray_ tyfloat_), tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalDirichletSample", [
      { expr = "Owl_stats.dirichlet_rvs ",
        ty = tyarrows_ [otylabel_ "alpha" (otyarray_ tyfloat_), otyarray_ tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalUniformContinuousSample", [
      { expr = "Owl_stats.uniform_rvs",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalUniformDiscreteSample", [
      { expr = "Owl_stats.uniform_int_rvs",
        ty = tyarrows_ [tyint_, tyint_, tyint_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalLomaxSample", [
      { expr = "Owl_stats.lomax_rvs",
        ty = tyarrows_ [tyfloat_, tyfloat_, tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalLomaxLogPdf", [
      { expr = "Owl_stats.lomax_logpdf",
        ty = tyarrows_ [tyfloat_, otylabel_ "shape" tyfloat_, otylabel_ "scale" tyfloat_, tyfloat_],
        libraries = ["owl"],
        cLibraries = []
      }
    ]),
    ("externalSetSeed", [
      { expr = "
        fun seed -> (
          Random.init seed;
          Owl_base_stats_prng.init seed;
          Owl_stats_prng.sfmt_seed seed;
          Owl_stats_prng.ziggurat_init ();
          Mi_stats.mi_set_seed seed
        )",
        ty = tyarrows_ [tyint_, otyunit_],
        libraries = ["owl", "mi_stats"],
        cLibraries = []
      }
    ])
  ]
