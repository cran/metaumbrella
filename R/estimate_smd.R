#' Estimates SMD from means and standard deviations.
#'
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param mean_cases mean of cases
#' @param sd_cases sd of cases
#' @param mean_controls mean of controls
#' @param sd_controls sd of controls
#'
#' @noRd
.estimate_d_from_means = function (n_cases, n_controls, mean_cases, sd_cases, mean_controls, sd_controls) {
  df = n_cases + n_controls - 2
  pooled_sd = sqrt(((n_cases - 1) * sd_cases^2 + (n_controls - 1) * sd_controls^2) / (n_cases + n_controls - 2))
  d = (mean_cases - mean_controls) / pooled_sd
  returned_df <- data.frame(
    value = d,
    se = sqrt(1 / n_cases + 1 / n_controls)
  )
  return(returned_df)
}

#' Estimates standard error of SMD from sample size in each group
#'
#' @param n_cases number of cases
#' @param n_controls number of control
#' @param d SMD value
#'
#' @noRd
.estimate_se_from_d = function (n_cases, n_controls, d) {
  df = n_cases + n_controls - 2
  returned_df = (data.frame(
    value = d,
    se = sqrt(1 / n_cases + 1 / n_controls)
  ))
  return(returned_df)
}

#' Estimates the Hedges' g correction factor
#'
#' @param x applied on df
#'
#' @noRd
.d_j <- function (x) {
  j <- ifelse(x < 1, -1, 1) * exp(lgamma(x / 2) - 0.5 * log(x / 2) - lgamma((x - 1) / 2))
  na.j <- which(is.na(j))
  j[na.j] <- 1 - 3 / (4 * x[na.j] - 1)
  return(j)
}

#' Estimates the Hedges' g from SMD
#'
#' @param d d
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param se standard error
#'
#' @noRd
.estimate_g_from_d <- function (d, n_cases, n_controls, se = NULL) {
  df = n_cases + n_controls - 2
  J = .d_j(df)
  g = d * J
  if (is.null(se)) {
    se_ok = sqrt(1 / n_cases + 1 / n_controls + (1 - (df - 2) / (df * J^2)) * g^2)
  } else {
    se_ok = sqrt(se^2 + (1 - (df - 2) / (df * J^2)) * g^2)
  }
  return(data.frame(value = g, se = se_ok))
}

#' Estimates the SMD from Hedges' g
#'
#' @param g g
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param se standard error
#'
#' @noRd
.estimate_d_from_g <- function (g, n_cases, n_controls, se = NULL) {
  df = n_cases + n_controls - 2
  J = .d_j(df)
  d = g / J
  if (is.null(se)) {
    se_ok = sqrt(1 / n_cases + 1 / n_controls)
  } else {
    se_ok = sqrt(se^2 - (1 - (df - 2) / (df * J^2)) * g^2)
  }
  return(data.frame(value = d, se = se_ok))
}

#' Convert a mean difference to a SMD.
#'
#' @param md mean difference
#' @param ci_lo 95% CI low
#' @param ci_up 95% CI up
#' @param n_cases number of cases
#' @param n_controls number of controls
#'
#' @noRd
.estimate_d_from_md = function (md, ci_lo, ci_up, n_cases, n_controls) {
  df = n_cases + n_controls - 2
  inv_n = 1 / n_cases + 1 / n_controls
  md_me = (ci_up - ci_lo) / 2
  md_se = md_me / qt(0.975, df)
  md_sd = md_se / sqrt(inv_n)
  d = md / md_sd
  return(data.frame(value = d))
}

#' Convert an OR to a SMD
#'
#' @param or odds ratio
#'
#' @noRd
.or_to_d = function (or) {
  return(log(or) * sqrt(3) / pi) # formula (7.1) page 47 Borenstein et al. (2009)
}

#' Convert a Pearson's r to a SMD
#'
#' @param r Pearson's r
#'
#' @noRd
.r_to_d = function (r) {
  return(2 * r / sqrt(1 - r^2)) # formula (7.5) page 48 Borenstein et al. (2009)
}

#' Estimate d (and standard error) from a t value
#'
#' @param t t-value
#' @param n_cases number of cases
#' @param n_controls number of controls
#'
#' @noRd
.estimate_d_from_t = function (t, n_cases, n_controls) {
  df = n_cases + n_controls - 2
  inv_n = 1 / n_cases + 1 / n_controls
  d = sqrt(inv_n) * t
  data.frame(value = d,
             se = sqrt(1 / n_cases + 1 / n_controls)
             )
}
