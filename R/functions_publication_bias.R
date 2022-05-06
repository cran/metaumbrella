#' Assess publication bias
#'
#' @param value effect size
#' @param se standard error
#' @param measure effect size measure
#'
#' @noRd
.egger_pb <- function(value, se, measure) {

  if (measure == "non_ratio") {
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
