test_that("component extraction", {
  components = list(
    base = list(
      data_args = list(
        l_b = 1,
        r_b = 2
      ),
      data_code = list(
        "int<lower=0> N; // Number of episodes"
      )
    ),
    survival_prior = list(
      data_code = list(
        "int max_S;"
      )
    )
  )
  expect_equal(extract_component(components, "data_code"), "\t int<lower=0> N; // Number of episodes\n\t int max_S;")
})

test_that("utils basic characteristics", {
  util_code = get_util_stan_code()
  expect_type(util_code, "character")
  expect_length(util_code, 1)
})

test_that("basic model compiles", {
  args = c(
    infer_duration(
      pa_model = pa_double_censor(c(1, 1), c(2, 2), c(3, 3), c(4, 4)),
      pt_model = pt_individual(tS = matrix(rep(1, 8), nrow = 2, ncol = 4))
    ),
    iter = 5, chains = 1, refresh = 0, cores = 1, warmup = 2
  )
  result = do.call(rstan::stan, args)
  S = rstan::extract(result, "S[1]")[[1]]
  expect_length(S, 3)
})
