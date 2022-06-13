#' Hidden function formatting dataset as the umbrella function
#'
#' @noRd
.format_dataset = function (x_i, method.var = "REML", mult.level = FALSE, r = 0.5, verbose = TRUE, pre_post_cor) {

  #### Effect size conversions ------

  # Convert MD to SMD
  for (i in which(x_i[, "measure"] == "MD")) {

    if (is.na(x_i[i, "se"])) {
      tmp = .improve_ci(x_i[i, "value"], x_i[i, "ci_lo"], x_i[i, "ci_up"], FALSE)
      tmp = .estimate_d_from_md(tmp$value, tmp$ci_lo, tmp$ci_up, x_i[i, "n_cases"], x_i[i, "n_controls"])
      x_i[i, "value"] = tmp$value
      x_i[i, "situation"] = gsub("_CI", "", as.character(x_i[i, "situation"]))
    } else {
      x_i[i, "value"] = x_i[i, "value"] / (x_i[i, "se"] / sqrt(1 / x_i[i, "n_cases"] + 1 / x_i[i, "n_controls"]))
      x_i[i, "situation"] = gsub("_SE", "", as.character(x_i[i, "situation"]))
      x_i[i, "situation"] = gsub("_CI", "", as.character(x_i[i, "situation"]))
    }
    x_i[i, "ci_lo"] = NA
    x_i[i, "ci_up"] = NA
    x_i[i, "measure"] = "SMD"
  }

  # Convert g to SMD
  for (i in which(x_i[, "measure"] == "G")) {
    df_i = x_i[i, "n_cases"] + x_i[i, "n_controls"] - 2

    G_i = x_i[i, "value"]

    x_i[i, "value"] = .estimate_d_from_g(g = x_i[i, "value"], n_cases = x_i[i, "n_cases"], n_controls = x_i[i, "n_controls"])$value

    if (is.na(x_i[i, "se"])) {
      if (!is.na(x_i[i, "ci_lo"]) & !is.na(x_i[i, "ci_up"])) {
        se_g_i = (x_i[i, "ci_up"] - x_i[i, "ci_lo"]) / (2 * qt(0.975, df_i))
        se_i = .estimate_d_from_g(g = G_i, n_cases = x_i[i, "n_cases"], n_controls = x_i[i, "n_controls"], se = se_g_i)$se
        x_i[i, "ci_lo"] = x_i[i, "value"] - se_i * qt(0.975, df_i)
        x_i[i, "ci_up"] = x_i[i, "value"] + se_i * qt(0.975, df_i)
      }
    } else {
      se_i = .estimate_d_from_g(g = G_i, n_cases = x_i[i, "n_cases"], n_controls = x_i[i, "n_controls"], se = x_i[i, "se"])$se
      x_i[i, "se"] = se_i
      x_i[i, "ci_lo"] = x_i[i, "value"] - x_i[i, "se"] * qt(0.975, df_i)
      x_i[i, "ci_up"] = x_i[i, "value"] + x_i[i, "se"] * qt(0.975, df_i)
    }

    x_i[i, "measure"] = "SMD"
  }

  # Convert R to Z
  for (i in which(x_i[, "measure"] == "R")) {
    # r + se
    if (!is.na(x_i[i, "se"])) {
      x_i[i, "ci_lo"] = (x_i[i, "value"] - qnorm(0.975) * x_i[i, "se"])
      x_i[i, "ci_up"] = (x_i[i, "value"] + qnorm(0.975) * x_i[i, "se"])
      x_i[i, "ci_lo"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "ci_lo"])$value
      x_i[i, "ci_up"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "ci_up"])$value
      x_i[i, "se"] = (x_i[i, "ci_up"] - x_i[i, "ci_lo"]) / (2 * qnorm(0.975))
      x_i[i, "value"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "value"])$value
      # r + 95% CI
      } else if (!is.na(x_i[i, "ci_lo"]) & x_i[i, "ci_up"]) {
        tmp = .improve_ci(x_i[i, "value"], x_i[i, "ci_lo"], x_i[i, "ci_up"], FALSE)
        x_i[i, "ci_lo"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = tmp$ci_lo)$value
        x_i[i, "ci_up"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = tmp$ci_up)$value
        x_i[i, "se"] = (x_i[i, "ci_up"] - x_i[i, "ci_lo"]) / (2 * qnorm(0.975))
        x_i[i, "value"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "value"])$value
        # r
      } else {
        x_i[i, "se"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "value"])$se
        x_i[i, "value"] = .estimate_z_from_r(n_sample = x_i[i, "n_sample"], r = x_i[i, "value"])$value
        x_i[i, "ci_lo"] = (x_i[i, "value"] - qnorm(0.975) * x_i[i, "se"])
        x_i[i, "ci_up"] = (x_i[i, "value"] + qnorm(0.975) * x_i[i, "se"])
      }
    x_i[i, "measure"] = "Z"
  }

  # Convert log OR to OR
  for (i in which(x_i[, "measure"] == "logOR")) {
    x_i[i, "value"] = exp(x_i[i, "value"])
    x_i[i, "ci_lo"] = exp(x_i[i, "ci_lo"])
    x_i[i, "ci_up"] = exp(x_i[i, "ci_up"])
    x_i[i, "measure"] = "OR"
  }

  # Convert log RR to RR
  for (i in which(x_i[, "measure"] == "logRR")) {
    x_i[i, "value"] = exp(x_i[i, "value"])
    x_i[i, "ci_lo"] = exp(x_i[i, "ci_lo"])
    x_i[i, "ci_up"] = exp(x_i[i, "ci_up"])
    x_i[i, "measure"] = "RR"
  }

  # Convert log IRR to IRR
  for (i in which(x_i[, "measure"] == "logIRR")) {
    x_i[i, "value"] = exp(x_i[i, "value"])
    x_i[i, "ci_lo"] = exp(x_i[i, "ci_lo"])
    x_i[i, "ci_up"] = exp(x_i[i, "ci_up"])
    x_i[i, "measure"] = "IRR"
  }

  # Convert log HR to HR
  for (i in which(x_i[, "measure"] == "logHR")) {
    x_i[i, "value"] = exp(x_i[i, "value"])
    x_i[i, "ci_lo"] = exp(x_i[i, "ci_lo"])
    x_i[i, "ci_up"] = exp(x_i[i, "ci_up"])
    x_i[i, "measure"] = "HR"
  }

  #### CONVERSIONS between ES measures ------
  measure = sort(unique(x_i$measure))

  if (length(measure) > 1) {

    # Users report no SMD/SMC => OR is the target measure
    if (all(!measure  %in% c("SMD", "SMC")) & all(measure != "IRR")) {

      # we convert all HR to OR
      if (any(measure == "HR")) {
        x_i = .convert_HR_to_OR(x_i)
        if (verbose) message(paste("I assumed Hazard Ratio as an Odds Ratio for factor: ", unique(x_i$factor)))
      }
      # we convert all RR to OR
      if (any(measure == "RR")) {
        x_i = .convert_RR_to_OR(x_i)
        if (verbose) message(paste0("I converted Risk Ratio to Odds Ratio for factor: ", unique(x_i$factor)))
      }

      # we convert all Z to SMD and then to OR
      if (any(measure == "Z")) {
        x_i = .convert_Z_to_SMD(x_i)
        x_i = .convert_SMD_to_OR(x_i)
        if (verbose) message(paste0("I converted Z to Odds Ratio for factor: ", unique(x_i$factor)))
      }

      measure = "OR"

    # Users report SMD, which is used as the target measure
    } else if (any(measure %in% c("SMD", "SMC")) & all(measure != "IRR")) {

      # we convert all HR to OR
      if (any(measure == "HR")) {
        x_i = .convert_HR_to_OR(x_i)
        if (verbose) message(paste("I assumed Hazard Ratio as an Odds Ratio for factor: ", unique(x_i$factor)))
      }
      # we convert all RR to OR
      if (any(measure == "RR")) {
        x_i = .convert_RR_to_OR(x_i)
        if (verbose) message(paste0("I converted Risk Ratio to Odds Ratio for factor: ", unique(x_i$factor)))
      }
      # we convert all OR to SMD
      if (any(measure == "OR")) {
        x_i = .convert_OR_to_SMD(x_i)
        if (verbose) message(paste("I converted Odds Ratio to a SMD for factor: ", unique(x_i$factor)))
      }
      # we convert all Z to SMD
      if (any(measure == "Z")) {
        x_i = .convert_Z_to_SMD(x_i)
        if (verbose) message(paste("I converted Fisher's Z to a SMD for factor: ", unique(x_i$factor)))
      }
      # we convert all SMC to SMD
      if (any(measure == "SMC")) {
        x_i = .convert_SMC_to_SMD(x_i, pre_post_cor)
        if (verbose) message(paste("I assumed SMC as a SMD for factor: ", unique(x_i$factor)))
      }

      measure = "SMD"

    } else {
      stop(paste("Different measures (", paste(unique(x_i$measure), collapse = ", ") , ") for the same factor:", unique(x_i$factor),
                 ". Please, provide an unique effect size for this factor (or a combination of effect size measures accepted for a same factor: see the manual for the list of possible combination)."))
    }
  }
  # ------------------------------------------

  if (length(x_i[, "measure"]) == 0) {
    stop("An unexpected error regarding effect size measure occured. Please contact us to resolve this issue.")
  } else if (length(unique(x_i[, "measure"])) == 1) {
    measure = unique(x_i[, "measure"])
  }

  if (!method.var %in% c("DL", "hksj", "REML", "PM", "ML", "FE")) {
    stop("The between-study variance estimator (argument method.var of the umbrella function) should be either 'PM', 'ML', 'DL', 'hksj', 'REML' or 'FE'.")
  }

  #### Multivariate situations ------
  if (any(x_i$duplicate == TRUE)) {

    x_i$all_vals_study = paste0("study: '", x_i$author, " (", x_i$year, ")' contains multiple ", x_i$multiple_es)

    if (mult.level == FALSE) {
      stop(paste(paste(unique(x_i$all_vals_study[x_i$duplicate == TRUE]), collapse = " / "), " and is repeated several times in your dataset. \nPlease, check that it is not a repeated entry. If not, indicate that you have multivariate data by specfying 'mult.level = TRUE' as an argument of the 'umbrella' function."))
    }
    if (r > 1 | r < -1) {
      stop("The r argument of the umbrella function (the r value that will be applied to aggregate studies with multiple outcomes) must be within the range of [-1; 1].")
    }

    ## if the input dataset has a multivariate structure, we create a message to indicate whether each study with multiple outcomes has been handled as having multiple groups or outcomes
    if (verbose) {
      message(paste("In factor '", unique(x_i$factor), "': \n",
                    paste("-", unique(x_i[x_i$duplicate == TRUE, ]$all_vals_study), collapse = "\n"), sep = ""))
    }
    # if the input dataset has multiple outcomes but with no r associated, we create a message to warn users about the r value used
    if (any(x_i$multiple_es %in% c("outcome", "Outcome", "outcomes", "Outcomes") & is.na(x_i$r))) {
      if (verbose) message(paste("In factor '", unique(x_i$factor), "' some studies have multiple outcomes but they are not associated with any within-study correlation (which can be indicated in the 'r' column of the dataset). A value of r = ", r, " is assumed.", sep = ""))
    }
    REPEATED_STUDIES = TRUE
  } else {
    REPEATED_STUDIES = FALSE
  }


  ### data formatting and conversion ----------------------------------------
  n_outcomes = nrow(x_i)
  n_studies = ifelse(REPEATED_STUDIES, length(unique(x_i$all_vals_study)), n_outcomes)
  x_i_ok = NULL
  print_shared = 0

  for (i in 1:n_outcomes) {

    x_raw_i = x_i[i, ]

    # we store information in new objects ----
    n_sample_i = x_raw_i$n_sample
    n_controls_exp_i = x_raw_i$n_controls_exp
    n_controls_nexp_i = x_raw_i$n_controls_nexp
    n_cases_nexp_i = x_raw_i$n_cases_nexp
    n_cases_exp_i = x_raw_i$n_cases_exp
    n_exp_i = x_raw_i$n_exp
    n_nexp_i = x_raw_i$n_nexp
    n_cases_i = x_raw_i$n_cases
    n_controls_i = x_raw_i$n_controls
    time_i = x_raw_i$time
    time_exp_i = x_raw_i$time_exp
    time_nexp_i = x_raw_i$time_nexp
    mean_cases_i = x_raw_i$mean_cases
    mean_pre_cases_i = x_raw_i$mean_pre_cases
    sd_cases_i = x_raw_i$sd_cases
    sd_pre_cases_i = x_raw_i$sd_pre_cases
    mean_controls_i = x_raw_i$mean_controls
    mean_pre_controls_i = x_raw_i$mean_pre_controls
    sd_controls_i = x_raw_i$sd_controls
    sd_pre_controls_i = x_raw_i$sd_pre_controls
    mean_change_cases_i = x_raw_i$mean_change_cases
    sd_change_cases_i = x_raw_i$sd_change_cases
    mean_change_controls_i = x_raw_i$mean_change_controls
    sd_change_controls_i = x_raw_i$sd_change_controls

    # we correct sample sizes if needed
    if (grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
      n_controls_exp_i = x_raw_i$n_controls_exp * x_raw_i$adj_controls
      n_controls_nexp_i = x_raw_i$n_controls_nexp * x_raw_i$adj_controls
      n_controls_i = x_raw_i$n_controls * x_raw_i$adj_controls
      n_exp_i = n_cases_exp_i + n_controls_exp_i
      n_nexp_i = n_cases_nexp_i + n_controls_nexp_i
    } else if (grepl("shared_nexp", x_raw_i$situation, fixed = TRUE)) {
      n_nexp_i = x_raw_i$n_nexp * x_raw_i$adj_nexp
      n_cases_nexp_i = x_raw_i$n_cases_nexp * x_raw_i$adj_nexp
      n_controls_nexp_i = x_raw_i$n_controls_nexp * x_raw_i$adj_nexp
      n_cases_i = n_cases_exp_i + n_cases_nexp_i
      n_controls_i = n_controls_exp_i + n_controls_nexp_i
      time_nexp_i = x_raw_i$time_nexp * x_raw_i$adj_nexp
      time_i = time_exp_i + time_nexp_i
    }

    #====================================== SMD ======================================#

    if (x_raw_i$measure == "SMD") {

      ###########################################
      # SMD situation 1: mean/SD + sample sizes #
      ###########################################
      if (grepl("mean/SD", x_raw_i$situation, fixed = TRUE)) {

        tmp = .estimate_d_from_means(n_cases_i, n_controls_i,
                                     mean_cases_i, sd_cases_i,
                                     mean_controls_i, sd_controls_i)

        value_i = tmp$value
        se_i = tmp$se
        ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)

        ###########################################
        # SMD situation 2: ES + SE + sample sizes #
        ###########################################
      } else if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE)) {

        value_i = mean_cases_i = x_raw_i$value
        mean_controls_i = 0
        sd_cases_i = sd_controls_i = 1

        se_i = x_raw_i$se

        # we correct SE and 95% CI if needed
        if (!grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
          ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
          ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        } else {
          se_i = .estimate_se_from_d(n_cases_i, n_controls_i, value_i)$se
          ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
          ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        }

        ###########################################
        # SMD situation 3: ES + CI + sample sizes #
        ###########################################
      } else if (grepl("ES_CI", x_raw_i$situation, fixed = TRUE)) {

        tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)

        value_i = mean_cases_i = tmp$value
        mean_controls_i = 0
        sd_cases_i = sd_controls_i = 1

        # we correct SE and 95% CI if needed
        if (!grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
          ci_lo_i = tmp$ci_lo
          ci_up_i = tmp$ci_up
          se_i = (ci_up_i - ci_lo_i) / (2 * qt(0.975, n_cases_i + n_controls_i - 2))
        } else {
          se_i = .estimate_se_from_d(n_cases_i, n_controls_i, value_i)$se
          ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
          ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        }

        ######################################
        # SMD situation 4: ES + sample sizes #
        ######################################
      } else {

        value_i = mean_cases_i = x_raw_i$value
        mean_controls_i = 0
        sd_cases_i = sd_controls_i = 1

        se_i = .estimate_se_from_d(n_cases_i, n_controls_i, value_i)$se
        ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)
      }
      #====================================== SMC ======================================#
      # for future updates
    } else if (x_raw_i$measure == "SMC") {

        ####################################################
        # SMC situation 1: pre post mean/SD + sample sizes #
        ####################################################
        if (grepl("mean/SD_pre/post", x_raw_i$situation, fixed = TRUE)) {
          if (is.na(x_raw_i$pre_post_cor) & is.na(pre_post_cor)) {
              pre_post_cor_est_cases = (x_i$sd_pre_cases^2 + x_i$sd_cases^2 - x_i$sd_change_cases^2) / (2 * x_i$sd_pre_cases * x_i$sd_cases)
              pre_post_cor_est_controls = (x_i$sd_pre_controls^2 + x_i$sd_controls^2 - x_i$sd_change_controls^2) / (2 * x_i$sd_pre_controls * x_i$sd_controls)

              if (any(!is.na(pre_post_cor_est_cases) | !is.na(pre_post_cor_est_controls))) {
                row = which(!is.na(pre_post_cor_est_cases) | !is.na(pre_post_cor_est_controls))
                pre_post_cor_est = apply(cbind(pre_post_cor_est_cases, pre_post_cor_est_controls), 1, mean, na.rm = TRUE)
                weights = 1 / ((x_i$n_cases + x_i$n_controls)^2)
                pre_post_cor = sum(weights[row] * pre_post_cor_est[row]) / sum(weights[row])
                if (verbose) message(paste0("The pre/post correlation was calculated using values indicated in studies: ", paste(paste0(x_i$author[row], " (", x_i$year[row], ")"), collapse = " / ")))
              }
            }

        cor_i = ifelse(!is.na(x_raw_i$pre_post_cor), x_raw_i$pre_post_cor,
                       ifelse(!is.na(pre_post_cor), pre_post_cor,
                              stop("The pre/post correlation should be indicated when using the 'SMC' measure.")))

        tmp = .estimate_smc_raw(n_cases = n_cases_i, n_controls = n_controls_i,
                                mean_pre_cases = mean_pre_cases_i, mean_cases = mean_cases_i,
                                sd_pre_cases = sd_pre_cases_i, sd_cases = sd_cases_i,
                                mean_pre_controls = mean_pre_controls_i, mean_controls = mean_controls_i,
                                sd_pre_controls = sd_pre_controls_i, sd_controls = sd_controls_i,
                                cor = cor_i)
        value_i = tmp$value
        se_i = tmp$se
        ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
        ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)

          ##################################################
          # SMC situation 2: mean/sd change + sample sizes #
          ##################################################
        } else if (grepl("mean/SD_change", x_raw_i$situation, fixed = TRUE)) {
          tmp = .estimate_smc_change(n_cases = n_cases_i, n_controls = n_controls_i,
                                      mean_change_cases = mean_change_cases_i, sd_change_cases = sd_change_cases_i,
                                      mean_change_controls = mean_change_controls_i, sd_change_controls = sd_change_controls_i)

          value_i = tmp$value
          se_i = tmp$se
          ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
          ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)

          ###########################################
          # SMC situation 3: ES + SE + sample sizes #
          ###########################################
        } else if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE)) {

          value_i = x_raw_i$value
          se_i = x_raw_i$se
          ci_lo_i = value_i - se_i * qt(0.975, n_cases_i + n_controls_i - 2)
          ci_up_i = value_i + se_i * qt(0.975, n_cases_i + n_controls_i - 2)

          # warning ---
          if (grepl("shared_controls", x_raw_i$situation, fixed = TRUE) & verbose == TRUE & print_shared == 0) {
            message("The 'shared_controls' has no influence on the effect size estimation when working with 'SMC' measure and when only 'value' + ('se' or 'ci_lo' and 'ci_up') are provided.")
            print_shared = 1
          }

          ###########################################
          # SMC situation 4: ES + CI + sample sizes #
          ###########################################
        } else if (grepl("ES_CI", x_raw_i$situation, fixed = TRUE)) {

          tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)
          value_i = tmp$value
          ci_lo_i = tmp$ci_lo
          ci_up_i = tmp$ci_up
          se_i = (ci_up_i - ci_lo_i) / (2 * qt(0.975, n_cases_i + n_controls_i - 2))

          # warning ---
          if (grepl("shared_controls", x_raw_i$situation, fixed = TRUE) & verbose == TRUE & print_shared == 0) {
            message("The 'shared_controls' has no influence on the effect size estimation when working with 'SMC' measure and when only 'value' + ('se' or 'ci_lo' and 'ci_up') are provided.")
            print_shared = 1
          }
        }

      #====================================== Z ======================================#
      # for future updates
    } else if (x_raw_i$measure == "Z") {

      ##################################
      # situation 1: ES + SE + total N #
      ##################################
      if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se
        ci_lo_i = value_i - qnorm(0.975) * se_i
        ci_up_i = value_i + qnorm(0.975) * se_i

        ##################################
        # situation 2: ES + CI + total N #
        ##################################
      } else if (grepl("ES_CI", x_raw_i$situation, fixed = TRUE)) {

        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)
        value_i = tmp1$value
        ci_lo_i = tmp1$ci_lo
        ci_up_i = tmp1$ci_up
        se_i = (ci_up_i - ci_lo_i) / (2 * qnorm(0.975))

        #############################
        # situation 3: ES + total N #
        #############################
      } else {

        value_i = x_raw_i$value
        se_i = sqrt(1 / (x_raw_i$n_sample - 3))
        ci_lo_i = value_i - qnorm(0.975) * se_i
        ci_up_i = value_i + qnorm(0.975) * se_i

      }

    } else if (x_raw_i$measure == "OR") {

      ################
      ###### OR ######
      ################

      #############################
      # OR situation 1: 2x2 table #
      #############################
      if (grepl("2x2", x_raw_i$situation, fixed = TRUE)) {

        tmp = .estimate_or_from_n(n_cases_exp_i, n_cases_nexp_i, n_controls_exp_i, n_controls_nexp_i)

        value_i = tmp$value
        se_i = tmp$se
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        ############################################
        # OR situation 2: ES + SE + Cases/controls #
        ############################################
      } else if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE) &
                 grepl("cases_controls", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se

        # we estimate the 95% CI and the 2x2 table
        if (!grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
          # 95% CI
          ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
          ci_up_i = value_i * exp(qnorm(0.975) * se_i)

          # 2x2 table
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, n_controls_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp

        } else {
          # 95% CI
          se_i = .estimate_se_from_or(value_i, n_cases_i, n_controls_i)$se
          ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
          ci_up_i <- value_i * exp(qnorm(0.975) * se_i)

          # 2x2 table
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, x_raw_i$n_controls)
          n_cases_exp_i = tmp2$n_cases_exp
          n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp * x_raw_i$adj_controls
          n_controls_nexp_i = tmp2$n_controls_nexp * x_raw_i$adj_controls
        }

        ############################################
        # OR situation 3: ES + CI + Cases/controls #
        ############################################
      } else if (grepl("ES_CI_cases_controls", x_raw_i$situation, fixed = TRUE)) {

        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)

        value_i = tmp1$value

        # we estimate the SE and the 2x2 table
        if (!grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
          # 95% CI
          ci_lo_i = tmp1$ci_lo
          ci_up_i = tmp1$ci_up
          se_i = (log(ci_up_i) - log(ci_lo_i)) / (2 * qnorm(0.975))

          # 2x2 table
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, n_controls_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp

        } else {
          # 95% CI
          se_i = .estimate_se_from_or(value_i, n_cases_i, n_controls_i)$se
          ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
          ci_up_i <- value_i * exp(qnorm(0.975) * se_i)

          # 2x2 table
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, x_raw_i$n_controls)
          n_cases_exp_i = tmp2$n_cases_exp
          n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp * x_raw_i$adj_controls
          n_controls_nexp_i = tmp2$n_controls_nexp * x_raw_i$adj_controls
        }

        ######################################
        # OR situation 5: ES + SE + Exp/Nexp #
        ######################################
      } else if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE) &
                 grepl("exp_nexp", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se

        # we estimate the 95% CI and the 2x2 table
        if (!grepl("shared_nexp", x_raw_i$situation, fixed = TRUE)) {
          ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
          ci_up_i <- value_i * exp(qnorm(0.975) * se_i)

          tmp2 = .estimate_n_from_or_and_n_exp(value_i, se_i^2, n_exp_i, n_nexp_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp
          n_cases_i = n_cases_exp_i + n_cases_nexp_i
          n_controls_i = n_controls_exp_i + n_controls_nexp_i

        } else {

          tmp2 = .estimate_n_from_or_and_n_exp(value_i, se^2, n_exp_i, x_raw_i$n_nexp_i)
          se_i = .estimate_or_from_n(tmp2$n_cases_exp, tmp2$n_cases_nexp * x_raw_i$adj_nexp,
                                     tmp2$n_controls_exp, tmp2$n_controls_nexp * x_raw_i$adj_nexp)$se

          ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
          ci_up_i <- value_i * exp(qnorm(0.975) * se_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp
          n_cases_i = n_cases_exp_i + n_cases_nexp_i
          n_controls_i = n_controls_exp_i + n_controls_nexp_i

        }
        ######################################
        # OR situation 5: ES + CI + Exp/Nexp #
        ######################################
      } else if (grepl("ES_CI_exp_nexp", x_raw_i$situation, fixed = TRUE)) {
        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)

        value_i = tmp1$value

        # we estimate the 95% CI and the 2x2 table
        if (!grepl("shared_nexp", x_raw_i$situation, fixed = TRUE)) {
          ci_lo_i = tmp1$ci_lo
          ci_up_i = tmp1$ci_up
          se_i = (log(ci_up_i) - log(ci_lo_i)) / (2 * qnorm(0.975))

          tmp2 = .estimate_n_from_or_and_n_exp(value_i, se_i^2, n_exp_i, n_nexp_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp
          n_cases_i = n_cases_exp_i + n_cases_nexp_i
          n_controls_i = n_controls_exp_i + n_controls_nexp_i

        } else {
          ci_lo = tmp1$ci_lo
          ci_up = tmp1$ci_up
          se = (log(ci_up) - log(ci_lo)) / (2 * qnorm(0.975))

          tmp2 = .estimate_n_from_or_and_n_exp(value_i, se^2, n_exp_i, x_raw_i$n_nexp_i)

          se_i = .estimate_or_from_n(tmp2$n_cases_exp, tmp2$n_cases_nexp * x_raw_i$adj_nexp,
                                     tmp2$n_controls_exp, tmp2$n_controls_nexp * x_raw_i$adj_nexp)$se

          ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
          ci_up_i <- value_i * exp(qnorm(0.975) * se_i)

          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp
          n_cases_i = n_cases_exp_i + n_cases_nexp_i
          n_controls_i = n_controls_exp_i + n_controls_nexp_i

        }
        ######################################
        # OR situation 6: ES + n_cases_controls #
        ######################################
      } else if (grepl("ES_cases_controls", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = .estimate_se_from_or(value_i, n_cases_i, n_controls_i)$se
        ci_lo_i <- value_i / exp(qnorm(0.975) * se_i)
        ci_up_i <- value_i * exp(qnorm(0.975) * se_i)

        # we estimate the 2x2 table
        if (!grepl("shared_controls", x_raw_i$situation, fixed = TRUE)) {
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, n_controls_i)
          n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp; n_controls_nexp_i = tmp2$n_controls_nexp
        } else {
          tmp2 = .estimate_n_from_or_and_n_cases(value_i, se_i^2, n_cases_i, x_raw_i$n_controls)
          n_cases_exp_i = tmp2$n_cases_exp
          n_cases_nexp_i = tmp2$n_cases_nexp
          n_controls_exp_i = tmp2$n_controls_exp * x_raw_i$adj_controls
          n_controls_nexp_i = tmp2$n_controls_nexp * x_raw_i$adj_controls
        }
      }
    } else if (x_raw_i$measure == "RR") {
      ################
      ###### RR ######
      ################

      #############################
      # RR situation 1: 2x2 table #
      #############################
      if (grepl("2x2", x_raw_i$situation, fixed = TRUE)) {

        tmp = .estimate_rr_from_n(n_cases_exp_i, n_exp_i, n_cases_nexp_i, n_nexp_i)

        value_i = tmp$value
        se_i <- tmp$se
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        # metagenRR[i] <- FALSE

        ############################################
        # RR situation 2: ES + SE + Cases/controls #
        ############################################
      } else if (grepl("ES_SE" , x_raw_i$situation, fixed = TRUE) &
                 grepl("cases_controls" , x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se

        # we estimate the 95% CI and the 2x2 table
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        tmp2 = .estimate_n_from_rr(value_i, se_i^2, n_cases_i, n_controls_i)
        n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
        n_exp_i = tmp2$n_exp; n_nexp_i = tmp2$n_nexp
        ############################################
        # RR situation 3: ES + CI + Cases/controls #
        ############################################
      } else if (grepl("ES_CI_cases_controls", x_raw_i$situation, fixed = TRUE)) {

        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)

        value_i = tmp1$value

        # we estimate the 95% CI and the 2x2 table
        ci_lo_i = tmp1$ci_lo
        ci_up_i = tmp1$ci_up
        se_i = (log(ci_up_i) - log(ci_lo_i)) / (2 * qnorm(0.975))

        tmp2 = .estimate_n_from_rr(value_i, se_i^2, n_cases_i, n_controls_i)
        n_cases_exp_i = tmp2$n_cases_exp; n_cases_nexp_i = tmp2$n_cases_nexp
        n_exp_i = tmp2$n_exp; n_nexp_i = tmp2$n_nexp
      }
    } else if (grepl("IRR", x_raw_i$situation, fixed = TRUE)) {
      #################
      ###### IRR ######
      #################

      ##########################
      # situation 1: 2x2 table #
      ##########################
      if (grepl("cases_exp_nexp", x_raw_i$situation, fixed = TRUE) &
          grepl("time_exp_nexp", x_raw_i$situation, fixed = TRUE)) {

        tmp = .estimate_irr_from_n(n_cases_exp_i, time_exp_i, n_cases_nexp_i, time_nexp_i)

        value_i = tmp$value
        se_i = tmp$se
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        # metagenIRR[i] <- FALSE

        ################################
        # situation 2: ES + SE + Cases #
        ################################
      } else if (grepl("SE", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se

        # we estimate the 95% CI and the 2x2 table
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        tmp2 = .estimate_n_from_irr(value_i, se_i^2, n_cases_i, time = time_i, time_exp = time_exp_i, time_nexp = time_nexp_i)
        n_cases_exp_i = tmp2$n_cases_exp
        n_cases_nexp_i = tmp2$n_cases_nexp
        time_exp_i = tmp2$time_exp
        time_nexp_i = tmp2$time_nexp
        ################################
        # situation 3: ES + CI + Cases #
        ################################
      } else {
        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)

        value_i = tmp1$value

        # we estimate the 95% CI and the 2x2 table
        ci_lo_i = tmp1$ci_lo
        ci_up_i = tmp1$ci_up
        se_i = (log(ci_up_i) - log(ci_lo_i)) / (2 * qnorm(0.975))

        tmp2 = .estimate_n_from_irr(value_i, se_i^2, n_cases_i, time = time_i, time_exp = time_exp_i, time_nexp = time_nexp_i)
        n_cases_exp_i = tmp2$n_cases_exp
        n_cases_nexp_i = tmp2$n_cases_nexp
        time_exp_i = tmp2$time_exp
        time_nexp_i = tmp2$time_nexp
      }
    } else if (x_raw_i$measure == "HR") {
      ################
      ###### HR ######
      ################

      #########################################
      # situation 1: ES + SE + Cases/controls #
      #########################################
      if (grepl("ES_SE", x_raw_i$situation, fixed = TRUE)) {

        value_i = x_raw_i$value
        se_i = x_raw_i$se
        ci_lo_i = value_i / exp(qnorm(0.975) * se_i)
        ci_up_i = value_i * exp(qnorm(0.975) * se_i)

        #########################################
        # situation 2: ES + CI + Cases/controls #
        #########################################
      } else {
        tmp1 = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)

        value_i = tmp1$value
        ci_lo_i = tmp1$ci_lo
        ci_up_i = tmp1$ci_up
        se_i = (log(ci_up_i) - log(ci_lo_i)) / (2 * qnorm(0.975))
      }
    }

    # we update the number of total participants
    sum_N_i <- ifelse(measure == "Z", n_sample_i, sum(n_cases_i, n_controls_i, na.rm = TRUE)) # na.rm = TRUE for IRR

    # we score the risk of bias variable
    rob.recoded_i <- ifelse(x_raw_i$rob %in% c("Low", "low"), 1,
                            ifelse(x_raw_i$rob %in% c("unclear", "Unclear", "high", "High"), 0,
                                   ifelse(is.na(x_raw_i$rob) & !all(is.na(x_i$rob)), 0,
                                          ifelse(is.na(x_raw_i$rob) & all(is.na(x_i$rob)), NA_real_, NA_real_))))
    x_i_ok = rbind(x_i_ok,
                   data.frame(row_index = x_raw_i$row_index,
                              author = x_raw_i$author,
                              year = x_raw_i$year,
                              multiple_es = x_raw_i$multiple_es,
                              duplicate = x_raw_i$duplicate,
                              value = value_i, se = se_i,
                              ci_lo = ci_lo_i, ci_up = ci_up_i,
                              n_sample = x_raw_i$n_sample,
                              n_cases = n_cases_i, n_controls = n_controls_i,
                              mean_cases = mean_cases_i, sd_cases = sd_cases_i,
                              mean_controls = mean_controls_i, sd_controls = sd_controls_i,
                              mean_pre_cases = x_raw_i$mean_pre_cases, sd_pre_cases = x_raw_i$sd_pre_cases,
                              mean_pre_controls = x_raw_i$mean_pre_controls, sd_pre_controls = x_raw_i$sd_pre_controls,
                              mean_change_cases = x_raw_i$mean_change_cases, sd_change_cases = x_raw_i$sd_change_cases,
                              mean_change_controls = x_raw_i$mean_change_controls, sd_change_controls = x_raw_i$sd_change_controls,
                              pre_post_cor = x_raw_i$pre_post_cor,
                              n_cases_exp = n_cases_exp_i, n_controls_exp = n_controls_exp_i,
                              n_nexp = n_nexp_i, n_exp = n_exp_i,
                              n_cases_nexp = n_cases_nexp_i, n_controls_nexp = n_controls_nexp_i,
                              time_nexp = time_nexp_i,  time_exp = time_exp_i,
                              sum_N = sum_N_i,
                              time = time_nexp_i + time_exp_i,
                              rob.recoded = rob.recoded_i,
                              thr = x_raw_i$thr,
                              shared_controls = x_raw_i$shared_controls,
                              shared_nexp = x_raw_i$shared_nexp,
                              reverse_es = x_raw_i$reverse_es,
                              r = x_raw_i$r))

  }

  # convert SMD to G
  if (measure == "SMD") {
    value_G = .estimate_g_from_d(d = x_i_ok$value, n_cases = x_i_ok$n_cases, n_controls = x_i_ok$n_controls, se = x_i_ok$se)$value
    se_G = .estimate_g_from_d(d = x_i_ok$value, n_cases = x_i_ok$n_cases, n_controls = x_i_ok$n_controls, se = x_i_ok$se)$se
    ci_lo_G = value_G - se_G * qt(0.975, x_i_ok$n_cases + x_i_ok$n_controls - 2)
    ci_up_G = value_G + se_G * qt(0.975, x_i_ok$n_cases + x_i_ok$n_controls - 2)

    x_i_ok$value = value_G
    x_i_ok$se = se_G
    x_i_ok$ci_lo = ci_lo_G
    x_i_ok$ci_up = ci_up_G
    x_i_ok$measure = "G"
  }


  # reverse data when needed
  for (i in which(x_i_ok[, "reverse_es"] %in% c("reverse", "reversed", "Reverse", "Reversed"))) {

    if (measure %in% c("SMD", "SMC", "Z")) {

      value_inv_i = - x_i_ok$value[i]
      cilo_inv_i = - x_i_ok$ci_up[i]
      ciup_inv_i = - x_i_ok$ci_lo[i]
      mean_cases_inv_i = x_i_ok$mean_controls[i]
      mean_controls_inv_i = x_i_ok$mean_cases[i]
      sd_cases_inv_i = x_i_ok$sd_controls[i]
      sd_controls_inv_i = x_i_ok$sd_cases[i]
      n_cases_inv_i = x_i_ok$n_controls[i]
      n_controls_inv_i = x_i_ok$n_cases[i]

      x_i_ok$reverse_es[i] <- paste0("The effect size has been reversed. Initial value = ", x_i_ok$value[i], " [", x_i_ok$ci_lo[i], ", ", x_i_ok$ci_up[i], "]")

      x_i_ok$value[i] <- value_inv_i
      x_i_ok$ci_lo[i] <- cilo_inv_i
      x_i_ok$ci_up[i] <- ciup_inv_i
      x_i_ok$mean_cases[i] = mean_cases_inv_i
      x_i_ok$mean_controls[i] = mean_controls_inv_i
      x_i_ok$sd_cases[i] = sd_cases_inv_i
      x_i_ok$sd_controls[i] = sd_controls_inv_i
      x_i_ok$n_cases[i] = n_cases_inv_i
      x_i_ok$n_controls[i] = n_controls_inv_i

    } else {

      value_inv_i = 1 / x_i_ok$value[i]
      cilo_inv_i = 1 / x_i_ok$ci_up[i]
      ciup_inv_i = 1 / x_i_ok$ci_lo[i]
      n_exp_inv_i = x_i_ok$n_nexp[i]
      n_nexp_inv_i = x_i_ok$n_exp[i]
      n_cases_exp_inv_i = x_i_ok$n_cases_nexp[i]
      n_cases_nexp_inv_i = x_i_ok$n_cases_exp[i]
      n_controls_exp_inv_i = x_i_ok$n_controls_nexp[i]
      n_controls_nexp_inv_i = x_i_ok$n_controls_exp[i]
      time_exp_inv_i = x_i_ok$time_nexp[i]
      time_nexp_inv_i = x_i_ok$time_exp[i]

      x_i_ok$reverse_es[i] = paste0("The effect size has been reversed. Initial value = ", x_i_ok$value[i], " [", x_i_ok$ci_lo[i], ", ", x_i_ok$ci_up[i], "]")

      x_i_ok$value[i] = value_inv_i
      x_i_ok$ci_lo[i] = cilo_inv_i
      x_i_ok$ci_up[i] = ciup_inv_i
      x_i_ok$n_exp[i] = n_exp_inv_i
      x_i_ok$n_nexp[i] = n_nexp_inv_i
      x_i_ok$n_cases_exp[i] = n_cases_exp_inv_i
      x_i_ok$n_cases_nexp[i] = n_cases_nexp_inv_i
      x_i_ok$n_controls_exp[i] = n_controls_exp_inv_i
      x_i_ok$n_controls_nexp[i] = n_controls_nexp_inv_i
      x_i_ok$time_exp[i] = time_exp_inv_i
      x_i_ok$time_nexp[i] = time_nexp_inv_i
    }
  }

  # save a dataset to compare adjustments
  comparison_adjustment = data.frame(
    row_index = x_i$row_index, author = x_i$author, year = x_i$year,
    value_adj = x_i_ok$value, value_raw = x_i$value,
    ci_lo_adj = x_i_ok$ci_lo, ci_lo_raw = x_i$ci_lo,
    ci_up_adj = x_i_ok$ci_up, ci_up_raw = x_i$ci_up,

    n_cases_adj = x_i_ok$n_cases, n_cases_raw = x_i$n_cases,
    n_controls_adj = x_i_ok$n_controls, n_controls_raw = x_i$n_controls,

    n_exp_adj = x_i_ok$n_exp, n_exp_raw = x_i$n_exp,
    n_nexp_adj = x_i_ok$n_nexp, n_nexp_raw = x_i$n_nexp,

    n_cases_exp_adj = x_i_ok$n_cases_exp, n_cases_exp_raw = x_i$n_cases_exp,
    n_cases_nexp_adj = x_i_ok$n_cases_nexp, n_cases_nexp_raw = x_i$n_cases_nexp,
    n_controls_exp_adj = x_i_ok$n_controls_exp, n_controls_exp_raw = x_i$n_controls_exp,
    n_controls_nexp_adj = x_i_ok$n_controls_nexp, n_controls_nexp_raw = x_i$n_controls_nexp,

    time_adj = x_i_ok$time, time_raw = x_i$time,
    time_exp_adj = x_i_ok$time_exp, time_exp_raw = x_i$time_exp,
    time_nexp_adj = x_i_ok$time_nexp, time_nexp_raw = x_i$time_nexp)

  # aggregate data and save original dataset if multilevel data are present
  if (REPEATED_STUDIES) {
    x_i_ok_full = x_i_ok
    x_i_ok = .agg_data(x_i_ok, r = r, measure = measure)
    rownames(x_i_ok) = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
  } else {
    x_i_ok_full = paste0("The dataset does not have a multivariate structure.")
    rownames(x_i_ok) = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
  }

  attr(x_i_ok, "amstar") <- unique(x_i$amstar)
  attr(x_i_ok, "measure") <- measure
  attr(x_i_ok, "REPEATED_STUDIES") <- REPEATED_STUDIES
  attr(x_i_ok, "n_studies") <- n_studies
  attr(x_i_ok, "data_mult") <- x_i_ok_full
  attr(x_i_ok, "comparison_adjustment") <- comparison_adjustment
  # attr(x_i_ok, "meta") <- path_meta
  x_i_ok
}
