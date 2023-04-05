#' Creates the tS matrices needed for the pt models
#'
#' @param test_time_list A list where each element is a list of test times for an individual
#' @param times The times where infections are allowed to occur
#' @param max_S The maximal value of S that is allowed
#' @param min_times A vector of times where individual's were able to be infected from, ordered the same as test_time_list
#' @export
form_tS_matrix = function(test_time_list, times, max_S, min_times = -Inf) {
  if (length(min_times) == 1) min_times = rep(min_times, length(test_time_list))
  stopifnot(length(min_times) == length(test_time_list))
  t(mapply(test_times = test_time_list, min_time = min_times, FUN = create_td_row,
           MoreArgs = list(all_times = times, max_S = max_S))) / length(times)
}

create_td_row = function(test_times, all_times, max_S, min_time) {
  all_times = all_times[all_times >= min_time]
  tN_it = purrr::map_dbl(
    all_times,
    ~min(c(test_times[test_times >= .x], Inf)) - .x
  )
  tS_i = purrr::map_dbl(1:max_S, ~sum(tN_it + 1 == .x))
  return(tS_i)
}
