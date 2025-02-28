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

  j <- ifelse(x <= 1, NA, 1) * exp(lgamma(x/2) - 0.5 * log(x/2) -
                                     lgamma((x - 1)/2))
  return(j)
}

#' Estimate the Hedges' g correction factor
#'
#' @param d applied on df
#' @param vd applied on df
#' @param n_exp applied on df
#' @param n_nexp applied on df
#' @param smd_to_cor applied on df
#' @param n_cov_ancova applied on df
#'
#' @noRd
.smd_to_cor <- function (d, vd, n_exp, n_nexp, smd_to_cor, n_cov_ancova) {
  if (smd_to_cor == "viechtbauer") {
    df <- n_exp + n_nexp - 2 - n_cov_ancova
    h <- df/n_exp + df/n_nexp
    p <- n_exp/(n_exp + n_nexp)
    q <- n_nexp/(n_exp + n_nexp)
    r_pb <- d/sqrt(d^2 + h)
    f <- dnorm(qnorm(p, lower.tail = FALSE))
    r_viechtbauer <- sqrt(p * q)/f * r_pb
    r_trunc = ifelse(r_viechtbauer > 1, 1, ifelse(r_viechtbauer <
                                                    -1, -1, r_viechtbauer))
    vr_viechtbauer <- 1/(n_exp + n_nexp - 1) * (p * q/f^2 -
                                                  (3/2 + (1 - p * qnorm(p, lower.tail = FALSE)/f) *
                                                     (1 + q * qnorm(p, lower.tail = FALSE)/f)) *
                                                  r_trunc^2 + r_trunc^4)
    fzp <- dnorm(qnorm(p))
    a_viechtbauer <- sqrt(fzp)/(p * (1 - p))^(1/4)
    z_viechtbauer <- (a_viechtbauer/2) * log((1 + a_viechtbauer *
                                                r_trunc)/(1 - a_viechtbauer * r_trunc))
    vz_viechtbauer <- 1/(n_exp + n_nexp - 1)
    z_lo_viechtbauer <- z_viechtbauer - qnorm(0.975) * sqrt(vz_viechtbauer)
    z_up_viechtbauer <- z_viechtbauer + qnorm(0.975) * sqrt(vz_viechtbauer)
    r_lo_viechtbauer <- (1/a_viechtbauer) * ((exp(2 * z_lo_viechtbauer/a_viechtbauer) -
                                                1)/(exp(2 * z_lo_viechtbauer/a_viechtbauer) + 1))
    r_up_viechtbauer <- (1/a_viechtbauer) * ((exp(2 * z_up_viechtbauer/a_viechtbauer) -
                                                1)/(exp(2 * z_up_viechtbauer/a_viechtbauer) + 1))
    res <- cbind(r_viechtbauer, vr_viechtbauer, r_lo_viechtbauer,
                 r_up_viechtbauer, z_viechtbauer, vz_viechtbauer,
                 z_lo_viechtbauer, z_up_viechtbauer)
    return(res)
  }
  else if (smd_to_cor == "lipsey_cooper") {
    a <- ((n_exp + n_nexp)^2)/(n_exp * n_nexp)
    p <- n_exp/(n_exp + n_nexp)
    r_lipsey <- d/sqrt(d^2 + 1/(p * (1 - p)))
    vr_lipsey <- a^2 * vd/((d^2 + a)^3)
    z_lipsey <- atanh(r_lipsey)
    vz_lipsey <- vd/(vd + 1/(p * (1 - p)))
    r_lo_lipsey <- r_lipsey - qt(0.975, df = n_exp + n_nexp -
                                   2) * sqrt(vr_lipsey)
    r_up_lipsey <- r_lipsey + qt(0.975, df = n_exp + n_nexp -
                                   2) * sqrt(vr_lipsey)
    z_lo_lipsey <- z_lipsey - qnorm(0.975) * sqrt(vz_lipsey)
    z_up_lipsey <- z_lipsey + qnorm(0.975) * sqrt(vz_lipsey)
    res <- cbind(r_lipsey, vr_lipsey, r_lo_lipsey, r_up_lipsey,
                 z_lipsey, vz_lipsey, z_lo_lipsey, z_up_lipsey)
    return(res)
  }
}

#' Estimate the Hedges' g correction factor
#'
#' @param d applied on df
#' @param d_se applied on df
#' @param n_exp applied on df
#' @param n_nexp applied on df
#' @param n_sample applied on df
#' @param smd_to_cor applied on df
#' @param adjusted applied on df
#' @param n_cov_ancova applied on df
#' @param cov_outcome_r applied on df
#' @param reverse applied on df
#' @param x applied on df
#'
#' @noRd
.es_from_d <- function (d, d_se, n_exp, n_nexp, n_sample, smd_to_cor = "viechtbauer",
          adjusted, n_cov_ancova, cov_outcome_r, reverse) {
  if (missing(d_se))
    d_se <- rep(NA_real_, length(d))
  if (missing(n_exp))
    n_exp <- rep(NA_real_, length(d))
  if (missing(n_nexp))
    n_nexp <- rep(NA_real_, length(d))
  if (missing(n_sample))
    n_sample <- rep(NA_real_, length(d))
  if (missing(adjusted))
    adjusted <- rep(FALSE, length(d))
  if (missing(n_cov_ancova))
    n_cov_ancova <- rep(0, length(d))
  if (missing(cov_outcome_r))
    cov_outcome_r <- rep(0.5, length(d))
  if (missing(reverse))
    reverse <- rep(FALSE, length(d))
  reverse[is.na(reverse)] <- FALSE
  if (length(reverse) == 1)
    reverse = c(rep(reverse, length(d)))
  if (length(reverse) != length(d))
    stop("The length of the 'reverse' argument of incorrectly specified.")
  if (length(adjusted) == 1)
    adjusted = c(rep(adjusted, length(d)))
  if (length(adjusted) != length(d))
    stop("The length of the 'adjusted' argument of incorrectly specified.")
  if (!all(smd_to_cor %in% c("viechtbauer", "lipsey_cooper"))) {
    stop(paste0("'", unique(smd_to_cor[!smd_to_cor %in%
                                         c("viechtbauer", "lipsey_cooper")]), "' not in tolerated values for the 'smd_to_cor' argument.",
                " Possible inputs are: 'viechtbauer', 'lipsey_cooper'"))
  }
  d <- ifelse(reverse, -d, d)
  n_sample <- ifelse(!is.na(n_sample), n_sample, n_exp + n_nexp)
  n_exp <- ifelse(!is.na(n_exp), n_exp, n_sample/2)
  n_nexp <- ifelse(!is.na(n_nexp), n_nexp, n_sample/2)
  df <- ifelse(adjusted, n_exp + n_nexp - 2 - n_cov_ancova,
               n_exp + n_nexp - 2)
  d_se <- ifelse(!is.na(d_se), d_se, ifelse(adjusted, sqrt(((n_exp +
                                                               n_nexp)/(n_exp * n_nexp) * (1 - cov_outcome_r^2)) +
                                                             d^2/(2 * (n_exp + n_nexp))), sqrt((n_exp + n_nexp)/(n_exp *
                                                                                                                   n_nexp) + d^2/(2 * (n_exp + n_nexp)))))
  d_ci_lo <- d - d_se * qt(0.975, df)
  d_ci_up <- d + d_se * qt(0.975, df)
  logor <- d * pi/sqrt(3)
  logor_se <- sqrt(d_se^2 * pi^2/3)
  logor_ci_lo <- logor - logor_se * qnorm(0.975)
  logor_ci_up <- logor + logor_se * qnorm(0.975)
  nn_miss <- which(!is.na(d) & !is.na(d_se) & !is.na(n_exp) &
                     !is.na(n_nexp))
  g <- g_se <- g_ci_lo <- g_ci_up <- r <- r_se <- r_ci_lo <- r_ci_up <- z <- z_se <- z_ci_lo <- z_ci_up <- rep(NA,
                                                                                                               length(d))
  J <- .d_j(df[nn_miss])
  g[nn_miss] <- d[nn_miss] * J
  g_se[nn_miss] <- sqrt(d_se[nn_miss]^2 * (J^2))
  g_ci_lo[nn_miss] <- g[nn_miss] - g_se[nn_miss] * qt(0.975,
                                                      df[nn_miss])
  g_ci_up[nn_miss] <- g[nn_miss] + g_se[nn_miss] * qt(0.975,
                                                      df[nn_miss])
  dat_r <- data.frame(d = d, vd = d_se^2, n_exp = n_exp, n_nexp = n_nexp,
                      smd_to_cor = smd_to_cor, n_cov_ancova = n_cov_ancova)
  if (length(nn_miss) != 0) {
    cor <- t(mapply(.smd_to_cor, d = dat_r$d[nn_miss], vd = dat_r$vd[nn_miss],
                    n_exp = dat_r$n_exp[nn_miss], n_nexp = dat_r$n_nexp[nn_miss],
                    smd_to_cor = dat_r$smd_to_cor[nn_miss], n_cov_ancova = dat_r$n_cov_ancova[nn_miss]))
    r[nn_miss] <- cor[, 1]
    r_se[nn_miss] <- sqrt(cor[, 2])
    r_ci_lo[nn_miss] <- cor[, 3]
    r_ci_up[nn_miss] <- cor[, 4]
    z[nn_miss] <- cor[, 5]
    z_se[nn_miss] <- sqrt(cor[, 6])
    z_ci_lo[nn_miss] <- cor[, 7]
    z_ci_up[nn_miss] <- cor[, 8]
  }
  res <- data.frame(d, d_se, d_ci_lo, d_ci_up, g, g_se, g_ci_lo,
                    g_ci_up, r, r_se, r_ci_lo, r_ci_up, z, z_se, z_ci_lo,
                    z_ci_up, logor, logor_se, logor_ci_lo, logor_ci_up)
  return(res)
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
