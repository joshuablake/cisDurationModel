#' Creates the tS matrices needed for the pt models
#'
#' @param test_time_list A list where each element is a list of test times for an individual
#' @param times The times where infections are allowed to occur
#' @param max_S The maximal value of S that is allowed
#' @param min_times A vector of times where individual's were able to be infected from, ordered the same as test_time_list
#' @param max_N Create matrices for up to N tests ahead
#' @export
form_tS_matrices = function(test_time_list, times, max_S, min_times = -Inf, max_N = 1) {
  if (length(min_times) == 1) min_times = rep(min_times, length(test_time_list))
  stopifnot(length(min_times) == length(test_time_list))
  stopifnot(all(min_times <= max(times)))
  counts = purrr::map2(
    test_time_list, min_times,
    ~create_tdNs_it(.x, times, max_S, .y, max_N) / length(times)
  )
  combined_matrix = do.call(rbind, counts)
  vector_to_matrix = function(x) {
    matrix(x, nrow = length(test_time_list), ncol = max_S, byrow = TRUE)
  }
  return(
    purrr::map(1:max_N, ~vector_to_matrix(combined_matrix[,.x]))
  )
}

create_tdNs_it = function(test_times, all_times, max_S, min_time, max_N) {
  all_times = all_times[all_times >= min_time]
  sorted_test_times = c(sort(test_times), rep(Inf, max_N))
  tNs_it = do.call(
    rbind,
    purrr::map(
      all_times,
      ~(sorted_test_times[sorted_test_times >= .x])[1:max_N] - .x
    )
  ) |>
    magrittr::set_colnames(1:max_N)
  # Count each unique tN
  result = do.call(
    cbind,
    purrr::map(
      1:max_N,
      ~factor(tNs_it[,.x] + 1, levels = 1:max_S) |>
        table()
    )
  ) |>
    matrix(ncol = max_N)
  return(result)
}
