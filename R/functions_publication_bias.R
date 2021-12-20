#' Assess publication bias
#'
#' @param value effect size
#' @param se standard error
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param measure effect size measure
#'
#' @noRd
.egger_pb <- function(value, se, n_cases = NULL, n_controls = NULL, measure) {

  if (measure == "SMD") {
     value_ok = .estimate_g_from_d(d = value, n_cases = n_cases, n_controls = n_controls, se = se)$value
     se_ok = .estimate_g_from_d(d = value, n_cases = n_cases, n_controls = n_controls, se = se)$se
  } else if (measure == "G") {
     value_ok = value
     se_ok = se
  } else if (measure == "ratio") {
     value_ok = log(value)
     se_ok = se
  }

  egger_reg <- suppressWarnings(summary(lm(value_ok ~ se_ok, weights = 1 / (se_ok^2))))

  sum_pb <- data.frame(statistic = egger_reg$coefficients[2,3],
                       p.value = egger_reg$coefficients[2,4])
  return(sum_pb)
}
