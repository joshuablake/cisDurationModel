#' @export
pa_double_censor = function(l_b, r_b, l_e, r_e) {
  stopifnot(all.equal(length(l_b), length(r_b), length(l_e), length(r_b)))
  n_a = length(l_b)
  list(
    data_args = list(
      E = l_b,
      R = r_b,
      L = l_e,
      U = r_e,
      n_a = n_a,
      max_S = max(r_e - l_b) + 1
    ),
    data_code = list(
      "int<lower=0> n_a; // Number of episodes",
      "int<lower=0> max_S; // Number of days to compute survival for",
      "int E[n_a]; // Day of previous negative",
      "int R[n_a]; // Day of first positive",
      "int L[n_a]; // Day after last positive",
      "int U[n_a]; // Day of first negative"
    ),
    transformed_data_declare = list(
      "int<lower=1, upper=max_S-1> Rdash[n_a]; // Day of first positive",
      "int<lower=1, upper=max_S-1> Ldash[n_a]; // Day after last positive",
      "int<lower=2, upper=max_S> Udash[n_a]; // Day of first negative"
    ),
    transformed_data_code = list(
      "for (i in 1:n_a) {",
      "\tRdash[i] = R[i] - E[i];",
      "\tLdash[i] = L[i] - E[i];",
      "\tUdash[i] = U[i] - E[i];",
      "}"
    ),
    transformed_parameters_declare = list(
      "real ll_numerator[n_a];"
    ),
    transformed_parameters_code = list(
      "for (i in 1:n_a) {",
      "\tll_numerator[i] = 0;",
      "\tfor (t in 1:Rdash[i]) {",
      "\t\tint max_positive = Udash[i] - t + 1;",
      "\t\tint min_positive = Ldash[i] - t + 1;",
      "\t\tll_numerator[i] += S[min_positive] - S[max_positive];",
      "\t}",
      "}"
    ),
    model = list("target += sum(log(ll_numerator));")
  )
}
