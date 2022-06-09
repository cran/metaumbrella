#' Estimate Fisher's z from Pearson's correlation coefficient.
#'
#' @param r value of the Pearson's correlation coefficient
#' @param n_sample total number of participants in the sample
#'
#' @noRd
.estimate_z_from_r <- function(r, n_sample = NA) {

  z = 0.5 * log((1 + r) / (1 - r))

  se_z = sqrt(1 / (n_sample - 3))


  returned_df <- data.frame(
    value = z,
    se = se_z
  )
  return(returned_df)
}

#' Estimate Pearson's correlation coefficient from Fisher's z
#'
#' @param z value of Fisher's z
#'
#' @noRd
.z_to_r <- function(z) {

  r = (exp(2 * z) - 1) / (1 + exp(2 * z))

  return(r)
}

#' Estimate SMD from Pearson's correlation coefficient.
#'
#' @param r value of Pearson correlation coefficient
#' @param n_cases number of cases
#' @param n_controls number of controls
#'
#' @noRd
.d_to_r <- function(d, n_cases, n_controls) {

  # r = d / sqrt(d^2 + 4) #(n_cases+n_controls)^2 / (n_cases * n_controls)
  r = d / sqrt(d^2 + 1/(n_cases/(n_cases + n_controls) * (1 - n_cases/(n_cases + n_controls))))
  return(r)
}
