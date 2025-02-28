#' Estimate Odds Ratio using sample sizes
#'
#' @param n_cases_exp number of cases in the exposed group
#' @param n_cases_nexp number of cases in the non exposed group
#' @param n_controls_exp number of controls in the exposed group
#' @param n_controls_nexp number of controls in the non exposed group
#'
#' @noRd
.estimate_or_from_n = function (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) {
  zero = which(n_cases_exp == 0 | n_cases_nexp == 0 | n_controls_exp == 0 | n_controls_nexp == 0)
  n_cases_exp[zero] = n_cases_exp[zero] + 0.5
  n_cases_nexp[zero] = n_cases_nexp[zero] + 0.5
  n_controls_exp[zero] = n_controls_exp[zero] + 0.5
  n_controls_nexp[zero] = n_controls_nexp[zero] + 0.5
  returned_df = data.frame(
    value = (n_cases_exp / n_cases_nexp) / (n_controls_exp / n_controls_nexp),
    se = sqrt(1 / n_cases_exp + 1 / n_cases_nexp + 1 / n_controls_exp + 1 / n_controls_nexp)
  )
  return(returned_df)
}

#' Estimate standard error of Odds ratio using the effect size and sample sizes.
#'
#' @param or OR
#' @param n_cases number of cases
#' @param n_controls number of controls
#'
#' @noRd
.estimate_se_from_or = function (or, n_cases, n_controls) {
  ca_ex = 1:(n_cases - 1)
  ca_ne = n_cases - ca_ex
  co_ex = round(n_controls / (1 + ca_ne * or / ca_ex))
  co_ex[which(co_ex < 1 | co_ex > n_controls - 1)] = NA
  co_ne = n_controls - co_ex
  v_or_mean = mean(1/ca_ex + 1/ca_ne + 1/co_ex + 1/co_ne, na.rm = TRUE)
  returned_df = data.frame(
    value = or,
    var = v_or_mean,
    se = sqrt(v_or_mean)
  )
  return(returned_df)
}


#' Convert a Cohen's d to an Odds Ratio
#'
#' @param or SMD value
#' @param var SMD value
#' @param n_exp SMD value
#' @param n_nexp SMD value
#'
#' @noRd
.estimate_n_from_or_and_n_exp = function (or, var, n_exp, n_nexp) {
  res <- data.frame(n_cases_exp = NA, n_cases_nexp = NA, n_controls_exp = NA,
                    n_controls_nexp = NA)
  if (!is.na(or) & !is.na(var) & !is.na(n_exp) & !is.na(n_nexp)) {
    n_controls_exp_sim1 <- 0:n_exp
    n_controls_nexp_sim1 <- round(n_nexp/(1 + (n_exp - n_controls_exp_sim1)/(or *
                                                                               n_controls_exp_sim1)))
    n_cases_exp_sim1 <- n_exp - n_controls_exp_sim1
    n_cases_nexp_sim1 <- n_nexp - n_controls_nexp_sim1
    idx_non_zero <- which(n_cases_nexp_sim1 > 0 & n_controls_nexp_sim1 >
                            0 & n_cases_exp_sim1 > 0 & n_controls_exp_sim1 >
                            0)
    n_cases_nexp_sim1 <- n_cases_nexp_sim1[idx_non_zero]
    n_controls_nexp_sim1 <- n_controls_nexp_sim1[idx_non_zero]
    n_cases_exp_sim1 <- n_cases_exp_sim1[idx_non_zero]
    n_controls_exp_sim1 <- n_controls_exp_sim1[idx_non_zero]
    n_controls_exp_sim2 <- 0:n_exp
    n_controls_nexp_sim2 <- round((n_nexp + 0.5) - ((n_nexp +
                                                       1) * (n_exp - n_controls_exp_sim2 + 0.5))/((n_controls_exp_sim2 +
                                                                                                     0.5) * or + n_exp - n_controls_exp_sim2 + 0.5))
    n_cases_exp_sim2 <- n_exp - n_controls_exp_sim2
    n_cases_nexp_sim2 <- n_nexp - n_controls_nexp_sim2
    idx_some_zero <- which((n_cases_nexp_sim2 == 0 | n_controls_nexp_sim2 ==
                              0 | n_cases_exp_sim2 == 0 | n_controls_exp_sim2 ==
                              0) & (n_cases_nexp_sim2 >= 0 & n_controls_nexp_sim2 >=
                                      0 & n_cases_exp_sim2 >= 0 & n_controls_exp_sim2 >=
                                      0))
    n_cases_nexp_sim2 <- n_cases_nexp_sim2[idx_some_zero]
    n_controls_nexp_sim2 <- n_controls_nexp_sim2[idx_some_zero]
    n_cases_exp_sim2 <- n_cases_exp_sim2[idx_some_zero]
    n_controls_exp_sim2 <- n_controls_exp_sim2[idx_some_zero]
    n_controls_exp_sim <- append(n_controls_exp_sim1, n_controls_exp_sim2)
    n_controls_nexp_sim <- append(n_controls_nexp_sim1,
                                  n_controls_nexp_sim2)
    n_cases_exp_sim <- append(n_cases_exp_sim1, n_cases_exp_sim2)
    n_cases_nexp_sim <- append(n_cases_nexp_sim1, n_cases_nexp_sim2)
    some_zero <- n_cases_exp_sim == 0 | n_controls_exp_sim ==
      0 | n_cases_nexp_sim == 0 | n_controls_nexp_sim ==
      0
    var_sim <- ifelse(some_zero, 1/((n_exp + 1) - (n_controls_exp_sim +
                                                     0.5) + 1/(n_controls_exp_sim + 0.5) + 1/((n_nexp +
                                                                                                 1) - (n_controls_nexp_sim + 0.5)) + 1/(n_controls_nexp_sim +
                                                                                                                                          0.5)), 1/(n_exp - n_controls_exp_sim) + 1/n_controls_exp_sim +
                        1/(n_nexp - n_controls_nexp_sim) + 1/n_controls_nexp_sim)
    best <- order((var_sim - var)^2)[1]
    res$n_controls_exp <- n_controls_exp_sim[best]
    res$n_controls_nexp <- n_controls_nexp_sim[best]
    res$n_cases_exp <- n_exp - res$n_controls_exp
    res$n_cases_nexp <- n_nexp - res$n_controls_nexp
  }
  return(res)
}

#' Convert a d to or
#'
#' @param d OR value
#'
#' @noRd
.d_to_or = function (d) {
  return(exp(d * pi / sqrt(3)))
}

#' Convert an odds ratio to a p-value
#'
#' @param o OR value
#'
#' @noRd
.odds_to_p = function (o) {
  return(o / (1 + o))
}
