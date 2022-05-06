#' Estimates SMC from pre /post means and standard deviations.
#'
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param mean_change_cases mean change for cases
#' @param sd_change_cases sd change for cases
#' @param mean_change_controls mean change for controls
#' @param sd_change_controls sd change for controls
#'
#' @noRd
.estimate_smc_change <- function(n_cases, n_controls, mean_change_cases, sd_change_cases, mean_change_controls, sd_change_controls) {

  smc_cases = ((mean_change_cases) / sd_change_cases) * .d_j(n_cases - 1)
  smc_controls = ((mean_change_controls) / sd_change_controls) * .d_j(n_controls - 1)
  var_smc_cases = 1/(n_cases) + smc_cases^2 / (2*(n_cases))
  var_smc_controls = 1/(n_controls) + smc_controls^2 / (2*(n_controls))

  smcc = smc_cases - smc_controls
  se_smcc = sqrt(var_smc_cases + var_smc_controls)

  returned_df <- data.frame(
    value = smcc,
    se = se_smcc
  )

  return(returned_df)
}

#' Estimates SMC from mean changes and standard deviations.
#'
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param mean_pre_cases mean of the cases at baseline
#' @param mean_cases mean of the cases at follow-up
#' @param sd_pre_cases sd of the cases at baseline
#' @param sd_cases mean of the cases at follow-up
#' @param mean_pre_controls mean of the controls at baseline
#' @param mean_controls mean of the controls at follow-up
#' @param sd_pre_controls sd of the controls at baseline
#' @param sd_controls sd of the controls at follow-up
#' @param cor pre-post correlation
#'
#' @noRd
.estimate_smc_raw <- function(n_cases, n_controls,
                               mean_pre_cases, mean_cases, sd_pre_cases, sd_cases,
                               mean_pre_controls, mean_controls, sd_pre_controls, sd_controls,
                               cor) {


  sd_change_cases = sqrt(sd_pre_cases^2 + sd_cases^2 - 2*cor*sd_pre_cases*sd_cases)
  sd_change_controls = sqrt(sd_pre_controls^2 + sd_controls^2 - 2*cor*sd_pre_controls*sd_controls)

  smc_cases = ((mean_cases - mean_pre_cases) / sd_change_cases) * .d_j(n_cases - 1)
  smc_controls = ((mean_controls - mean_pre_controls) / sd_change_controls) * .d_j(n_controls - 1)
  var_smc_cases = 1/(n_cases) + smc_cases^2 / (2*(n_cases))
  var_smc_controls = 1/(n_controls) + smc_controls^2 / (2*(n_controls))

  smcc = smc_cases - smc_controls
  se_smcc = sqrt(var_smc_cases + var_smc_controls)

  returned_df <- data.frame(
    value = smcc,
    se = se_smcc
  )

  return(returned_df)
}
