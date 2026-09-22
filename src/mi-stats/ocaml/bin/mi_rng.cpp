#include <random>
extern "C" {
  #include <caml/mlvalues.h>
  #include <caml/alloc.h>
  #include <caml/memory.h>
  #include <caml/custom.h>
  #include <caml/fail.h>

  struct mi_rng { std::mt19937_64 rng; };

  static void mi_rng_finalize(value mi_rng_v) {
    // Free the cpp allocation
    ((mi_rng*)Data_custom_val(mi_rng_v))->~mi_rng();
  }

  static struct custom_operations mi_rng_ops {
    "mi_rng_handle",
    mi_rng_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
  };

  CAMLprim value mi_rng_create(value seed) {
      CAMLparam1(seed);
      CAMLlocal1(v);
      v = caml_alloc_custom(&mi_rng_ops, sizeof(mi_rng), 0, 1);
      new (Data_custom_val(v)) mi_rng{std::mt19937_64(Long_val(seed))};
      CAMLreturn(v);
  }

  CAMLprim value mi_rng_sample(value mi_rng_v) {
      mi_rng* rng = (mi_rng*)Data_custom_val(mi_rng_v);
      std::uniform_real_distribution<double> dist(0.0, 1.0);
      return caml_copy_double(dist(rng->rng));
  }
}
