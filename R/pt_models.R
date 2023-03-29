#' @export
pt_individual = function(tS) {
  list(
    data_args = list(tS = tS),
    data_code = list(
      data = list("matrix[n_a, max_S] tS;")
    ),
    model = list("target += -sum(log(tS * S));")
  )
}
