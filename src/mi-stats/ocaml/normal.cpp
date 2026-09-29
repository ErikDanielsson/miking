#include "../cpp/normal.hpp"

#include "./mi_rng.cpp"

#ifdef TEST
#include <caml/fail.h>
#endif

static double mi_normal_lpdf_checked(double x, double mu, double sigma) {
  #ifdef TEST
  try {
  #endif
    return mi_normal_lpdf(x, mu, sigma);
  #ifdef TEST
  }
  catch (const std::domain_error& e) {
    caml_invalid_argument(e.what());
  }
  #endif
}

extern "C" CAMLprim double mi_normal_lpdf_unwrapped(double x, double mu, double sigma) {
  return mi_normal_lpdf_checked(x, mu, sigma);
}

extern "C" CAMLprim value mi_normal_lpdf_wrapped(value x, value mu, value sigma) {
  CAMLparam3(x, mu, sigma);
  CAMLreturn(caml_copy_double(mi_normal_lpdf_checked(Double_val(x), Double_val(mu), Double_val(sigma))));
}

static double mi_normal_sample_checked(double mu, double sigma, mi_rng &rng) {
  #ifdef TEST
  try {
  #endif
    return mi_normal_sample(mu, sigma, rng);
  #ifdef TEST
  }
  catch (const std::domain_error& e) {
    caml_invalid_argument(e.what());
  }
  #endif
}

extern "C" CAMLprim double mi_normal_sample_unwrapped(double mu, double sigma, value mt) {
  return mi_normal_sample_checked(mu, sigma, Mi_rng_val(mt));
}

extern "C" CAMLprim value mi_normal_sample_wrapped(value mu, value sigma, value mt) {
  CAMLparam3(mu, sigma, mt);
  CAMLreturn(caml_copy_double(mi_normal_sample_checked(Double_val(mu), Double_val(sigma), Mi_rng_val(mt))));
}
