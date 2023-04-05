run_simple_cases = function(n_indiv, max_S) {
  run_simple_case(n_indiv, max_S)
  run_simple_case2(n_indiv, max_S)
}

run_simple_case = function(n_indiv, max_S) {
  test_times = c(1, 4, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(1:0, 2:0, 2:0, rep(Inf, 3))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(rep(c(3, 3, 2, rep(0, max_S - 3)), n_indiv),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S)[[1]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
}

run_simple_case2 = function(n_indiv, max_S) {
  test_times = c(2, 6, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(2:0, 3:0, 0, rep(Inf, 3))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(rep(c(3, 2, 2, 1, rep(0, max_S - 4)), n_indiv),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S)[[1]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
}

test_that("simple case", {
  run_simple_cases(1, 5)
})

test_that("two identical", {
  run_simple_cases(2, 5)
})

test_that("large max S", {
  run_simple_cases(1, 100)
})

test_that("two different", {
  max_S = 100
  test_time_list = list(c(1, 4, 7), c(2, 6, 7))
  expected_result = matrix(
    c(3, 3, 2, rep(0, max_S - 3),
      3, 2, 2, 1, rep(0, max_S - 4)),
    nrow = 2, byrow = TRUE
  ) / 11
  result = form_tS_matrices(test_time_list, 0:10, max_S)[[1]]
  expect_equal(dim(result), c(2, max_S))
  expect_equal(result, expected_result)
})

test_that("simple case with min times", {
  n_indiv = 3
  max_S = 5
  test_times = c(1, 4, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(1:0, 2:0, 2:0, rep(Inf, 3))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(c(3, 3, 2, 0, 0,
                             2, 2, 1, 0, 0,
                             0, 0, 0, 0, 0),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S, min_times = c(-1, 3, 8))[[1]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
})

run_simple_cases_N2 = function(n_indiv, max_S) {
  run_simple_case_N2(n_indiv, max_S)
  run_simple_case2_N2(n_indiv, max_S)
}

run_simple_case_N2 = function(n_indiv, max_S) {
  test_times = c(1, 4, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(4:3, 5:3, rep(Inf, 6))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(rep(c(0, 0, 0, 2, 2, 1, rep(0, max_S - 6)), n_indiv),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S, max_N = 2)[[2]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
}

run_simple_case2_N2 = function(n_indiv, max_S) {
  test_times = c(2, 6, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(6:4, 4:1, rep(Inf, 4))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(rep(c(0, 1, 1, 1, 2, 1, 1, rep(0, max_S - 7)), n_indiv),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S, max_N = 2)[[2]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
}

test_that("simple case, N=2", {
  run_simple_cases_N2(1, 8)
})

test_that("two identical, N=2", {
  run_simple_cases_N2(2, 8)
})

test_that("large max S, N=2", {
  run_simple_cases_N2(1, 100)
})

test_that("two different, N=2", {
  max_S = 100
  test_time_list = list(c(1, 4, 7), c(2, 6, 7))
  expected_result = matrix(
    c(0, 0, 0, 2, 2, 1, rep(0, max_S - 6),
      0, 1, 1, 1, 2, 1, 1, rep(0, max_S - 7)),
    nrow = 2, byrow = TRUE
  ) / 11
  result = form_tS_matrices(test_time_list, 0:10, max_S, max_N = 2)[[2]]
  expect_equal(dim(result), c(2, max_S))
  expect_equal(result, expected_result)
})

test_that("simple case with min times, N=2", {
  n_indiv = 3
  max_S = 6
  test_times = c(1, 4, 7)
  test_time_list = rep(list(test_times), n_indiv)
  # times = 0:10
  # tN_it = c(6:4, 4:1, rep(Inf, 4))
  # stopifnot(length(tN_it) == length(times))
  expected_result = matrix(c(0, 0, 0, 2, 2, 1,
                             0, 0, 0, 1, 1, 0,
                             0, 0, 0, 0, 0, 0),
                           nrow = n_indiv, byrow = TRUE) / 11
  # stopifnot(sum(expected_result) == length(tN_it) - sum(tN_it == Inf))
  result = form_tS_matrices(test_time_list, 0:10, max_S, min_times = c(-1, 3, 8), max_N = 2)[[2]]
  expect_equal(dim(result), c(n_indiv, max_S))
  expect_equal(result, expected_result)
})
