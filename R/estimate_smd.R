#' Estimate SMD from means and standard deviations.
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
  pooled_sd = sqrt(((n_cases - 1) * sd_cases^2 + (n_controls - 1) * sd_controls^2) / (n_cases + n_controls - 2))
  d = (mean_cases - mean_controls) / pooled_sd
  returned_df <- data.frame(
    value = d,
    se = sqrt(1 / n_cases + 1 / n_controls)
  )
  return(returned_df)
}

#' Estimate standard error of SMD from sample size in each group
#'
#' @param n_cases number of cases
#' @param n_controls number of control
#' @param d SMD value
#'
#' @noRd
.estimate_se_from_d = function (n_cases, n_controls, d) {
  returned_df = (data.frame(
    value = d,
    se = sqrt(1 / n_cases + 1 / n_controls)
  ))
  return(returned_df)
}

#' Estimate the Hedges' g correction factor
#'
#' @param x applied on df
#'
#' @noRd
.d_j <- function (x) {

  # j <- ifelse(x < 1, -1, 1) * exp(lgamma(x / 2) - 0.5 * log(x / 2) - lgamma((x - 1) / 2))
  # na.j <- which(is.na(j) | j == 0)
  # j[na.j] <- 1 - 3 / (4 * x[na.j] - 1)
  # return(j)
  j <- ifelse(x <= 1, NA, 1) * exp(lgamma(x / 2) - 0.5 * log(x / 2) - lgamma((x - 1) / 2))
  return(j)
}

#' Estimate the Hedges' g from SMD
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

  if (any(is.na(se_ok))) {
    message("- An error occured when converting the standard error of SMD to G. The standard error of the SMD was assumed to be equal to 'sqrt(1 / n_cases + 1 / n_controls + (1 - (df - 2) / (df * J^2)) * g^2)'.\n")
  }
  se_ok = ifelse(is.nan(se_ok),
                 sqrt(1 / n_cases + 1 / n_controls + (1 - (df - 2) / (df * J^2)) * g^2),
                 se_ok)

  return(data.frame(value = g, se = se_ok))
}

#' Estimate the SMD from Hedges' g
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
    se_ok = suppressWarnings(sqrt(se^2 - (1 - (df - 2) / (df * J^2)) * g^2))
  }

  if (any(is.nan(se_ok))) {
    ind_pb = which(is.nan(se_ok))
    se_ok = sqrt(1 / n_cases[ind_pb] + 1 / n_controls[ind_pb])
    message("- An error occured when converting the standard error of G to SMD. The standard error of the SMD was assumed to be equal to 'sqrt(1/n_cases + 1/n_controls)'.\n")
  }
  # if (is.null(se)) {
  #   se_ok = sqrt(1 / n_cases + 1 / n_controls)
  # } else {
  #   se_ok = suppressWarnings(sqrt(se^2 - (1 - (df - 2) / (df * J^2)) * g^2))
  #   if (is.nan(se_ok)) {
  #     se_ok = sqrt(1 / n_cases + 1 / n_controls)
  #     message("- An error occured when converting the standard error of G to SMD. The standard error of the SMD was assumed to be equal to 'sqrt(1/n_cases + 1/n_controls)'.\n")
  #   }
  # }
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
  return(log(or) * (sqrt(3) / pi)) # formula (7.1) page 47 Borenstein et al. (2009)
}

#' Convert a Pearson's r to a SMD
#'
#' @param r Pearson's r
#'
#' @noRd
.r_to_d = function (r) {
  return(2 * r / sqrt(1 - r^2)) # formula (7.5) page 48 Borenstein et al. (2009)
}

#' Convert a Fisher's Z to a SMD
#'
#' @param z Fisher's Z
#'
#' @noRd
.z_to_d = function (z) {
  return(2 * .z_to_r(z) / sqrt(1 - .z_to_r(z)^2))
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
  returned_df = data.frame(value = d,
                           se = sqrt(1 / n_cases + 1 / n_controls))

  return(returned_df)
}

# Taken from Wan et al. 2014: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-135#Equ9
meanSD_from_med_quart <- function(q1_cases = NA, med_cases = NA, q3_cases = NA, n_cases = NA,
                                  q1_controls = NA, med_controls = NA, q3_controls = NA, n_controls = NA) {

  mean_cases = (q1_cases + med_cases + q3_cases) / 3
  sd_cases = (q3_cases - q1_cases)/(2*qnorm((0.75*n_cases - 0.125) / (n_cases + 0.25), 0, 1))

  mean_controls = (q1_controls + med_controls + q3_controls) / 3
  sd_controls = (q3_controls - q1_controls)/(2*qnorm((0.75*n_controls - 0.125) / (n_controls + 0.25), 0, 1))

  d_se = .estimate_d_from_means(n_cases, n_controls,
                                mean_cases, sd_cases,
                                mean_controls, sd_controls)

  returned_df = data.frame(value = d_se$value,
                           se = d_se$se)

  return(returned_df)
}
meanSD_from_min_max <- function(min_cases = NA, med_cases = NA, max_cases = NA, n_cases = NA,
                                min_controls = NA, med_controls = NA, max_controls = NA, n_controls = NA) {

  mean_cases = (min_cases + 2*med_cases + max_cases) / 4
  sd_cases = (max_cases - min_cases) / (2*qnorm((n_cases - 0.375) / (n_cases + 0.25), 0, 1))

  mean_controls = (min_controls + 2*med_controls + max_controls) / 4
  sd_controls = (max_controls - min_controls) / (2*qnorm((n_controls - 0.375) / (n_controls + 0.25), 0, 1))

  d_se = .estimate_d_from_means(n_cases, n_controls,
                                mean_cases, sd_cases,
                                mean_controls, sd_controls)

  returned_df = data.frame(value = d_se$value,
                           se = d_se$se)

  return(returned_df)
}
