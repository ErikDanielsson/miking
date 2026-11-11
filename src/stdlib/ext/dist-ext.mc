include "math.mc"
include "bool.mc"
include "seq.mc"
include "ext/math-ext.mc"
include "float.mc"

-- Chi-square, with degrees of freedom nu (as in Stan). Outside the support
-- [0, inf), the log-pdf is -inf and the cdf is 0 below, as Stan rejects -inf
-- and negative values. The pdf is 0 where the log-pdf is -inf.
external externalChiSquareLogPdf : Float -> Float -> Float
external externalChiSquareSample ! : Float -> Float
external externalChiSquareCdf : Float -> Float -> Float
let chiSquareLogPdf : Float -> Float -> Float = lam nu. lam x.
  if ltf x 0. then negf inf
  else externalChiSquareLogPdf x nu
let chiSquarePdf : Float -> Float -> Float = lam nu. lam x.
  exp (chiSquareLogPdf nu x)
let chiSquareSample : Float -> Float = lam nu.
  externalChiSquareSample nu
let chiSquareCdf : Float -> Float -> Float = lam nu. lam x.
  if ltf x 0. then 0.
  else externalChiSquareCdf x nu

-- Gamma, with shape and rate as in Stan. Outside the support (0, inf), the
-- log-pdf is -inf and the cdf is 0 below and 1 above, as Stan rejects them.
external externalGammaLogPdf : Float -> Float -> Float -> Float
external externalGammaSample ! : Float -> Float -> Float
external externalGammaCdf : Float -> Float -> Float -> Float
let gammaLogPdf = lam shape:Float. lam rate:Float. lam x:Float.
  if or (leqf x 0.) (eqf x inf) then negf inf
  else externalGammaLogPdf x shape rate
let gammaPdf = lam shape:Float. lam rate:Float. lam x:Float.
  exp (gammaLogPdf shape rate x)
let gammaSample = lam shape:Float. lam rate:Float.
  externalGammaSample shape rate
let gammaCdf = lam shape:Float. lam rate:Float. lam x:Float.
  if ltf x 0. then 0.
  else externalGammaCdf x shape rate

-- Binomial, with N trials and success probability theta (as in Stan). Outside
-- the support {0, ..., N}, the log-pmf is -inf and the pmf is 0, as Stan
-- rejects them. An invalid N (negative) is passed to Stan.
external externalBinomialLogPmf : Int -> Int -> Float -> Float
external externalBinomialSample ! : Int -> Float -> Int
let binomialLogPmf : Int -> Float -> Int -> Float = lam n. lam theta. lam x.
  if and (geqi n 0) (or (lti x 0) (gti x n)) then negf inf
  else externalBinomialLogPmf x n theta
let binomialPmf : Int -> Float -> Int -> Float = lam n. lam theta. lam x.
  exp (binomialLogPmf n theta x)
let binomialSample : Int -> Float -> Int = lam n. lam theta.
  externalBinomialSample n theta

-- Bernoulli, with success probability theta (as in Stan). The value is a Bool,
-- so it is always in the support.
external externalBernoulliLogPmf : Bool -> Float -> Float
external externalBernoulliSample ! : Float -> Bool
let bernoulliPmf : Float -> Bool -> Float = lam theta. lam x.
  exp (externalBernoulliLogPmf x theta)
let bernoulliLogPmf : Float -> Bool -> Float = lam theta. lam x.
  externalBernoulliLogPmf x theta
let bernoulliSample : Float -> Bool = lam theta.
  externalBernoulliSample theta

-- Beta, with shape parameters alpha and beta (as in Stan). Outside the support
-- [0, 1], the log-pdf is -inf and the pdf is 0, as Stan rejects them.
external externalBetaLogPdf : Float -> Float -> Float -> Float
external externalBetaSample ! : Float -> Float -> Float
let betaLogPdf : Float -> Float -> Float -> Float = lam alpha. lam beta. lam x.
  if or (ltf x 0.) (gtf x 1.) then negf inf
  else externalBetaLogPdf x alpha beta
let betaPdf : Float -> Float -> Float -> Float = lam alpha. lam beta. lam x.
  exp (betaLogPdf alpha beta x)
let betaSample : Float -> Float -> Float = lam alpha. lam beta.
  externalBetaSample alpha beta

-- Normal, with mean mu and standard deviation sigma (as in Stan)
external externalNormalLogPdf : Float -> Float -> Float -> Float
external externalNormalSample ! : Float -> Float -> Float
let normalPdf = lam mu:Float. lam sigma:Float. lam x:Float.
  exp (externalNormalLogPdf x mu sigma)
let normalLogPdf = lam mu:Float. lam sigma:Float. lam x:Float.
  externalNormalLogPdf x mu sigma
let normalSample = lam mu:Float. lam sigma:Float.
  externalNormalSample mu sigma

-- Multinomial and Categorical
external externalMultinomialLogPmf : [Int] -> [Float] -> Float
external externalMultinomialSample ! : Int -> [Float] -> [Int]
external externalCategoricalSample ! : [Float] -> Int
let multinomialLogPmf : [Float] -> [Int] -> Float =
  lam ps. lam ns. externalMultinomialLogPmf ns ps
let multinomialPmf : [Float] -> [Int] -> Float =
  lam ps. lam ns. exp (externalMultinomialLogPmf ns ps)
let categoricalLogPmf : [Float] -> Int -> Float =
  lam ps. lam x.
  if geqi x (length ps) then negf inf else log (get ps x)
let categoricalPmf : [Float] -> Int -> Float =
  lam ps. lam x.
  if geqi x (length ps) then negf inf else get ps x
let multinomialSample : [Float] -> Int -> [Int] =
  lam ps. lam n. externalMultinomialSample n ps
let categoricalSample : [Float] -> Int =
  lam ps. externalCategoricalSample ps

-- Dirichlet
external externalDirichletLogPdf : [Float] -> [Float] -> Float
external externalDirichletSample : [Float] -> [Float]
let dirichletLogPdf : [Float] -> [Float] -> Float =
  lam alpha. lam xs. if eqfApprox 1e-15 (foldl addf 0. xs) 1. then externalDirichletLogPdf xs alpha else negf inf
let dirichletPdf : [Float] -> [Float] -> Float =
  lam alpha. lam xs. exp (externalDirichletLogPdf xs alpha)
let dirichletSample : [Float] -> [Float] =
  lam alpha. externalDirichletSample alpha

-- Uniform (continuous between a and b)
external externalUniformContinuousSample ! : Float -> Float -> Float
let uniformContinuousSample = lam a. lam b.
  externalUniformContinuousSample a b
let uniformContinuousLogPdf = lam a. lam b. lam x.
  if geqf x a then
    if leqf x b then subf (log 1.0) (log (subf b a))
    else log 0.
  else log 0.
let uniformContinuousPdf = lam a. lam b. lam x.
  if geqf x a then
    if leqf x b then divf 1.0 (subf b a)
    else 0.
  else 0.

-- Uniform on 0 1
let uniformSample : () -> Float = lam. uniformContinuousSample 0. 1.

-- Random (discrete)
external externalUniformDiscreteSample ! : Int -> Int -> Int
let uniformDiscreteSample = lam a:Int. lam b:Int.
  externalUniformDiscreteSample a b
let uniformDiscreteLogPdf : Int -> Int -> Int -> Float = lam a. lam b. lam x.
  if geqi x a then
    if leqi x b then subf (log 1.0) (log (int2float (addi 1 (subi b a))))
    else log 0.
  else log 0.
let uniformDiscretePdf : Int -> Int -> Int -> Float = lam a. lam b. lam x.
  if geqi x a then
    if leqi x b then divf 1.0 (int2float (addi 1 (subi b a)))
    else 0.
  else 0.

-- Poisson
let poissonLogPmf = lam lambda:Float. lam x:Int.
  subf (subf (mulf (int2float x) (log lambda)) lambda) (logFactorial x)
let poissonPmf = lam lambda:Float. lam x:Int.
  exp (poissonLogPmf lambda x)
-- Simple but inefficient algorithm for drawing from Poisson. Translated from
-- numpy C source code and originally from Knuth according to Wikipedia.
-- OPT(dlunde,2022-05-16): We want to use an external for this eventually.
let poissonSample = lam lambda:Float.
  let enlam = exp (negf lambda) in
  let x = 0 in
  let prod = 1. in
  recursive let rec = lam x. lam prod.
    let u = uniformSample () in
    let prod = mulf prod u in
    if gtf prod enlam then rec (addi x 1) prod
    else x
  in rec x prod

-- Add exponential
external externalExponentialSample ! : Float -> Float
let exponentialSample = lam lambda:Float.
  externalExponentialSample lambda
let exponentialLogPdf : Float -> Float -> Float = lam lambda. lam x.
  subf (log lambda) (mulf lambda x)
let exponentialPdf : Float -> Float -> Float = lam lambda. lam x.
  exp (exponentialLogPdf lambda x)

-- Negative Binomial
let negativeBinomialSample : Int -> Float -> Int = lam n. lam p.
  poissonSample (externalGammaSample (int2float n) (divf p (subf 1. p)))
let negativeBinomialLogPmf : Int -> Float -> Int -> Float = lam n. lam p. lam x.
  subf (addf (log (int2float n)) (binomialLogPmf (addi x n) (subf 1. p) x)) (log (int2float (addi n x)))
let negativeBinomialPmf: Int -> Float -> Int -> Float = lam n. lam p. lam x.
  exp (negativeBinomialLogPmf n p x)

-- Geometric
let geometricSample : Float -> Int = lam p.
  negativeBinomialSample 1 p
let geometricLogPmf : Float -> Int -> Float = lam p. lam x.
  addf (mulf (int2float x) (log (subf 1. p))) (log p)
let geometricPmf : Float -> Int -> Float = lam p. lam x.
  exp (geometricLogPmf p x)

-- Lomax
external externalLomaxLogPdf : Float -> Float -> Float -> Float
external externalLomaxSample ! : Float -> Float -> Float
let lomaxSample = lam shape: Float. lam scale: Float.
  externalLomaxSample shape scale
let lomaxLogPdf:Float -> Float -> Float -> Float = lam shape. lam scale. lam x.
  if ltf x 0. then negf inf else
  let lhs = subf (log shape) (log scale)  in
  let rhs = mulf (log (addf (divf x scale) 1.)) (addf shape 1.) in
  subf lhs rhs
let lomaxPdf = lam shape: Float. lam scale: Float. lam x:Float.
  exp (lomaxLogPdf shape scale x)

-- Beta-binomial, with N trials and shape parameters alpha and beta (as in
-- Stan). Outside the support {0, ..., N}, the log-pmf is -inf and the pmf is 0,
-- as Stan returns -inf there. An invalid N (negative) is passed to Stan.
external externalBetaBinomialLogPmf : Int -> Int -> Float -> Float -> Float
external externalBetaBinomialSample ! : Int -> Float -> Float -> Int
let betaBinomialLogPmf : Int -> Float -> Float -> Int -> Float = lam n. lam alpha. lam beta. lam x.
  if and (geqi n 0) (or (lti x 0) (gti x n)) then negf inf
  else externalBetaBinomialLogPmf x n alpha beta
let betaBinomialPmf : Int -> Float -> Float -> Int -> Float = lam n. lam alpha. lam beta. lam x.
  exp (betaBinomialLogPmf n alpha beta x)
let betaBinomialSample : Int -> Float -> Float -> Int = lam n. lam alpha. lam beta.
  externalBetaBinomialSample n alpha beta

-- Reciprocal (or log uniform) distribution
let reciprocalSample : Float -> Float -> Float = lam a. lam b.
  let logSample = uniformContinuousSample (log a) (log b) in
  exp logSample

let reciprocalPdf : Float -> Float -> Float -> Float = lam a. lam b. lam x.
  if geqf x a then
    if leqf x b then divf 1.0 (mulf (log (divf b a)) x)
    else 0.
  else 0.

let reciprocalLogPdf : Float -> Float -> Float -> Float = lam a. lam b. lam x.
  if geqf x a then
    if leqf x b then negf (log (mulf (log (divf b a)) x))
    else negf inf 
  else negf inf 

-- Pair distribution
let indexOfUnion = lam pairSets. lam u.
  recursive let helper = lam i.
    if eqi i (length pairSets) then negi 1
    else let v = get pairSets i in
      if eqSeq eqi v u then i else helper (addi i 1)
  in helper 0

let treeInferenceCategoricalPmf = lam p. lam pairSets. lam x.
  let idx = indexOfUnion pairSets x in
  if lti idx 0 then 0.0 else get p idx

let treeInferenceCategoricalLogPmf = lam p. lam pairSets. lam x. log (treeInferenceCategoricalPmf p pairSets x)
let treeInferenceCategoricalSample = lam p. lam pairSets.
  let index = categoricalSample p in
  let pair = get pairSets index in
  pair

-- Seed
external externalSetSeed ! : Int -> ()
let setSeed : Int -> () = lam seed.

  -- VERY important to also call this intrinsic here. Otherwise, the compiled code
  -- _self-initializes the seed_ at the first call to the intrinsic randIntU.
  randSetSeed seed;

  externalSetSeed seed

mexpr


-- Functions for testing floats. Should perhaps be in another library.
let maxf = lam r. lam l. if gtf r l then r else l in
let absf = lam f. maxf f (negf f) in
let eqfApprox = lam epsilon. lam r. lam l.
  if leqf (absf (subf r l)) epsilon then true else false in
let _eqf = eqfApprox 1e-11 in

-- Test functions for ranges
let intRange = lam lower. lam upper. lam r. lam l.
  and (and (leqi r upper) (geqi r lower)) (and (leqi l upper) (geqi l lower)) in
let floatRange = lam lower. lam upper. lam r. lam l.
  and (and (leqf r upper) (geqf r lower)) (and (leqf l upper) (geqf l lower)) in

-- Testing Chi-square
utest chiSquarePdf 5. 0.5 with 0.0366159407890 using _eqf in
utest exp (chiSquareLogPdf 6. 3.2) with 0.1292137715166 using _eqf in
utest chiSquareSample 6. with 1. using floatRange 0. inf in
utest chiSquareCdf 4. 4.1 with 0.6073585440572 using _eqf in

-- Testing Gamma
utest gammaPdf 1. 0.5 1. with 0.303265329856 using _eqf in
utest exp (gammaLogPdf 2. (divf 1. 3.) 1.) with 0.0796145900638 using _eqf in
utest gammaSample 1. (divf 1. 2.) with 1. using floatRange 0. inf in
utest gammaCdf 1. 0.5 1. with 0.393469340287 using _eqf in

-- Testing Binomial and Bernoulli
utest binomialPmf 20 0.7 15 with 0.17886305057 using _eqf in
utest exp (binomialLogPmf 40 0.5 20) with 0.12537068762 using _eqf in
utest binomialSample 20 0.7 with 0 using intRange 0 20 in
utest bernoulliPmf 0.3 false with 0.7 using _eqf in
utest exp (bernoulliLogPmf 0.6 true) with 0.6 using _eqf in
utest bernoulliSample 0.6 with false using lam. lam. true in

-- Testing Beta
utest betaPdf 2. 2. 0.5 with 1.5 using _eqf in
utest exp (betaLogPdf 2. 5. 0.2) with 2.4576 using _eqf in
utest betaSample 2. 2. with 0. using floatRange 0. 1. in

-- Testing Normal
utest normalPdf 0. 0.4472 0. with 0.892089178 using _eqf in
utest exp (normalLogPdf 2. 1. 2.) with 0.398942280401 using _eqf in
utest normalSample 0. 0.2 with 0. using lam. lam. true in

-- Testing Multinomial and Categorical
utest multinomialLogPmf [0.1, 0.3, 0.6] [0,1,0] with log 0.3 using _eqf in
utest multinomialPmf [0.1, 0.3, 0.6] [0,0,1] with 0.6 using _eqf in
utest multinomialPmf [0.1, 0.3, 0.6] [0,2,3] with 0.1944 using _eqf in
utest categoricalLogPmf [0.3, 0.2, 0.5] 2 with log 0.5 using _eqf in
utest categoricalPmf [0.1, 0.3, 0.6] 2 with 0.6 using _eqf in
utest categoricalPmf [0.1, 0.3, 0.6] 1 with 0.3 using _eqf in
utest multinomialSample [0.2, 0.8] 3 with [] using
  lam l. lam r. match l with [v1,v2] then eqi (addi v1 v2) 3 else false in
utest categoricalSample [0.1, 0.4, 0.2, 0.3] with 0 using intRange 0 3 in

-- Testing Dirichlet
utest dirichletLogPdf [1.5, 1.5, 1.5] [0.5, 0.25, 0.25]
  with 1.08321533235 using _eqf in
utest dirichletPdf [1.0, 1.0, 2.0] [0.01, 0.01, 0.98] with 5.88 using _eqf in
utest dirichletSample [5.0, 5.0, 5.0] with [0.] using
  lam l. lam r. _eqf (foldl addf 0. l) 1.0 in

-- Testing Continuous uniform
utest uniformContinuousSample 1.0 2.0 with 0. using floatRange 0. 2. in
utest exp (uniformContinuousLogPdf 1.0 2.0 1.5) with 1.0 using _eqf in
utest uniformContinuousPdf 1.0 2.0 1.5 with 1.0 using _eqf in

-- Testing (0,1)-uniform
utest uniformSample () with 0. using floatRange 0. 1. in

-- Testing Discrete uniform
utest uniformDiscreteSample 3 8 with 3 using intRange 3 8 in
utest exp (uniformDiscreteLogPdf 1 2 1) with 0.5 using _eqf in
utest uniformDiscretePdf 1 2 1 with 0.5 using _eqf in

-- Testing Poisson
utest poissonPmf 2.0 2 with 0.270670566473 using _eqf in
utest exp (poissonLogPmf 3.0 2) with 0.224041807655 using _eqf in
utest poissonSample 2.0 with 3 using intRange 0 100000 in

-- Testing Exponential
utest exponentialSample 1.0 with 0. using floatRange 0. inf in
utest exp (exponentialLogPdf 1.0 2.0) with 0.135335283237 using _eqf in
utest exponentialPdf 2.0 2.0 with 0.0366312777775 using _eqf in

-- Testing Negative Binomial
utest exp (negativeBinomialLogPmf 6 0.3 1) with 0.0030618 using _eqf in
utest negativeBinomialPmf 6 0.3 3 with 0.014002632 using _eqf in
utest exp (negativeBinomialLogPmf 6 0.3 5) with 0.03087580356 using _eqf in
utest negativeBinomialPmf 6 0.3 7 with 0.0475487374824 using _eqf in
utest negativeBinomialSample 6 0.3 with 0 using geqi in

-- Testing Geometric
utest geometricPmf 0.3 0 with 0.3 using _eqf in
utest exp (geometricLogPmf 0.3 1) with 0.21 using _eqf in
utest geometricPmf 0.3 2 with 0.147 using _eqf in
utest exp (geometricLogPmf 0.3 3) with 0.1029 using _eqf in
utest geometricSample 0.3 with 0 using geqi in

-- Testing Beta-Binomial
utest betaBinomialLogPmf 5 1. 1. 2 with -1.79175946923 using _eqf in
utest exp (betaBinomialLogPmf 5 1. 1. 3) with 0.166666666667 using _eqf in
utest betaBinomialPmf 5 1. 1. 3 with 0.166666666667 using _eqf in
utest betaBinomialSample 20 1. 1. with 0 using intRange 0 20 in

-- Testing Reciprocal
utest reciprocalLogPdf 1. 2. 1.5 with -0.038952187526 using _eqf in
utest reciprocalLogPdf 1. 2. 2.5 with negf inf using leqf in
utest exp (reciprocalLogPdf 4. 6. 5.) with 0.493260692475 using _eqf in
utest reciprocalPdf 4. 6. 5. with 0.493260692475 using _eqf in
utest reciprocalSample 0.5 0.7 with 0.5 using floatRange 0.5 0.7 in

-- Testing treeInferenceCategorical
let pairSetsTest = [[0,1],[1,2],[2,3]] in
let pTest = [0.2, 0.5, 0.3] in
-- indexOfUnion
utest indexOfUnion pairSetsTest [0,1] with 0 using eqi in
utest indexOfUnion pairSetsTest [1,2] with 1 using eqi in
utest indexOfUnion pairSetsTest [2,3] with 2 using eqi in
utest indexOfUnion pairSetsTest [9,9] with negi 1 using eqi in
-- 
utest treeInferenceCategoricalPmf pTest pairSetsTest [2,3] with 0.3 using _eqf in
utest treeInferenceCategoricalPmf pTest pairSetsTest [9,9] with 0.0 using _eqf in
utest treeInferenceCategoricalLogPmf pTest pairSetsTest [0,1] with log 0.2 using _eqf in
utest treeInferenceCategoricalLogPmf pTest pairSetsTest [9,9] with negf inf using eqf in
-- pairSample: result is one of the support pairs
utest let s = treeInferenceCategoricalSample pTest pairSetsTest in
  any (lam x. eqSeq eqi x s) pairSetsTest
with true in

-- Testing seed
utest setSeed 0; uniformSample (); uniformSample ()
with setSeed 0; uniformSample (); uniformSample () in
utest setSeed 0; normalSample 0. 1.; normalSample 0. 1.
with  setSeed 0; normalSample 0. 1.; normalSample 0. 1. in
utest setSeed 0; uniformSample (); normalSample 0. 1.
with  setSeed 0; uniformSample (); normalSample 0. 1. in
utest setSeed 0; normalSample 0. 1.; normalSample 0. 1.; uniformSample ()
with  setSeed 0; normalSample 0. 1.; normalSample 0. 1.; uniformSample () in

()
