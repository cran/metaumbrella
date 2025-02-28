.format_hom <- function(x_i_mlm) {
  meas = .conv_measure(x_i_mlm$measure)
  # print(meas[1])

  log_es = which(meas %in% c("or", "rr", "irr", "hr"))
  # print(log_es)
  x_i_mlm$value[log_es] <- log(x_i_mlm$value[log_es])
  x_i_mlm$ci_lo[log_es] <- log(x_i_mlm$ci_lo[log_es])
  x_i_mlm$ci_up[log_es] <- log(x_i_mlm$ci_up[log_es])
  x_i_mlm$measure[log_es] <- paste0("log", x_i_mlm$measure[log_es])
  # print(meas)

  r_es = which(meas == "pearson_r")
  x_i_mlm$value[r_es] <- atanh(x_i_mlm$value[r_es])
  x_i_mlm$ci_lo[r_es] <- atanh(x_i_mlm$ci_lo[r_es])
  x_i_mlm$ci_up[r_es] <- atanh(x_i_mlm$ci_up[r_es])
  x_i_mlm$measure[r_es] <- "Z"
  return(x_i_mlm)
}


.format_ci <- function(x_i_hom, measure) {

  meas = .conv_measure(x_i_hom$measure)

  row_miss_se = is.na(x_i_hom$se) & !is.na(x_i_hom$ci_lo) & !is.na(x_i_hom$ci_up)
  row_miss_ci = !is.na(x_i_hom$se) & is.na(x_i_hom$ci_lo) & is.na(x_i_hom$ci_up)
  row_miss_ci_se = is.na(x_i_hom$se) & is.na(x_i_hom$ci_lo) & is.na(x_i_hom$ci_up)
  meas_diff = ifelse(meas %in% c("d", "md", "g", "pearson_r"), TRUE, FALSE)
  x_i_hom$se = ifelse(row_miss_se,
                      ifelse(meas_diff & !is.na(x_i_hom$n_cases) & !is.na(x_i_hom$n_controls),
                             (x_i_hom$ci_up - x_i_hom$ci_lo) / (2 * qt(.975, x_i_hom$n_cases + x_i_hom$n_controls - 2)),
                             (x_i_hom$ci_up - x_i_hom$ci_lo) / (2 * qnorm(.975))),
                      x_i_hom$se)
  # print(paste0(meas[1], row_miss_ci_se))
  x_i_hom$se = ifelse(meas == "fisher_z" & row_miss_ci_se,
                      sqrt(1/(x_i_hom$n_sample - 3)),
                      x_i_hom$se)
  # print(x_i_hom$se[1])
  x_i_hom$ci_lo = ifelse(row_miss_ci,
                      ifelse(meas_diff & !is.na(x_i_hom$n_cases) & !is.na(x_i_hom$n_controls),
                             x_i_hom$value - x_i_hom$se * qt(.975, x_i_hom$n_cases + x_i_hom$n_controls - 2),
                             x_i_hom$value - x_i_hom$se * qnorm(.975)),
                      x_i_hom$ci_lo)

  x_i_hom$ci_up = ifelse(row_miss_ci,
                         ifelse(meas_diff & !is.na(x_i_hom$n_cases) & !is.na(x_i_hom$n_controls),
                                x_i_hom$value + x_i_hom$se * qt(.975, x_i_hom$n_cases + x_i_hom$n_controls - 2),
                                x_i_hom$value + x_i_hom$se * qnorm(.975)),
                         x_i_hom$ci_up)

  or_2x2 = which(x_i_hom$measure == "logOR" &
                   ((is.na(x_i_hom$n_cases) | is.na(x_i_hom$n_controls)) &
                      (!is.na(x_i_hom$n_exp) & !is.na(x_i_hom$n_nexp))))
  if (length(or_2x2) > 0) {
    for (row in or_2x2) {
      cont_imput = NA
      cont_imput = .estimate_n_from_or_and_n_exp(
        or = exp(x_i_hom$value[row]), var = x_i_hom$se[row]^2,
        n_exp = x_i_hom$n_exp[row], n_nexp = x_i_hom$n_nexp[row])
      x_i_hom$n_cases_exp[row] = cont_imput$n_cases_exp
      x_i_hom$n_cases_nexp[row] = cont_imput$n_cases_nexp
      x_i_hom$n_controls_exp[row] = cont_imput$n_controls_exp
      x_i_hom$n_controls_nexp[row] = cont_imput$n_controls_nexp

      x_i_hom$n_cases[row] = x_i_hom$n_cases_exp[row] + x_i_hom$n_cases_nexp[row]
      x_i_hom$n_controls[row] = x_i_hom$n_controls_exp[row] + x_i_hom$n_controls_nexp[row]
      x_i_hom$n_sample[row] = x_i_hom$n_cases[row] + x_i_hom$n_controls[row]

    }
  }

  return(x_i_hom)
}
.format_value <- function(x_i_ci, measure) {

  meas = .conv_measure(x_i_ci$measure)

  x_i_ci[, unique(meas)] <- NA_real_
  x_i_ci[, paste0(unique(meas), "_se")] <- NA_real_
  x_i_ci[, paste0(unique(meas), "_ci_lo")] <- NA_real_
  x_i_ci[, paste0(unique(meas), "_ci_up")] <- NA_real_

  for (col in unique(meas)) {
    x_i_ci[, col] <- ifelse(meas == col, x_i_ci$value, NA_real_)
    x_i_ci[, paste0(col, "_se")] <- ifelse(meas == col, x_i_ci$se, NA_real_)
    x_i_ci[, paste0(col, "_ci_lo")] <- ifelse(meas == col, x_i_ci$ci_lo, NA_real_)
    x_i_ci[, paste0(col, "_ci_up")] <- ifelse(meas == col, x_i_ci$ci_up, NA_real_)
  }

  if (any(meas == "d")) {
    row_d = which(meas=="d")
    df = with(x_i_ci, n_cases + n_controls - 2)
    if (any(is.na(df)) & measure != "SMC") {
      miss_n = which(is.na(df))
      N <- round((2 + x_i_ci$value^2/4)/x_i_ci$se^2) * 2
      cases = ifelse(is.na(x_i_ci$n_cases), N - x_i_ci$n_controls, x_i_ci$n_cases)
      controls = ifelse(is.na(x_i_ci$n_controls), N - x_i_ci$n_cases, x_i_ci$n_controls)
      x_i_ci$n_cases[miss_n] = ifelse(is.na(cases[miss_n]),round(N[miss_n]/2), cases[miss_n])
      x_i_ci$n_controls[miss_n] = ifelse(is.na(controls[miss_n]),round(N[miss_n]/2), controls[miss_n])
      x_i_ci$n_sample[miss_n] = round(N[miss_n])
      df[miss_n] = N[miss_n] - 2
    }
    J <- .d_j(df)
    x_i_ci$value[row_d] <- x_i_ci$value[row_d] * ifelse(is.na(J[row_d]), 1, J[row_d])
    x_i_ci$se[row_d] <- sqrt(x_i_ci$se[row_d]^2 * ifelse(is.na(J[row_d]), 1, J[row_d]^2))
    x_i_ci$ci_lo[row_d] <- x_i_ci$value[row_d] - x_i_ci$se[row_d] * ifelse(is.na(df[row_d]), qnorm(.975), qt(0.975, df[row_d]))
    x_i_ci$ci_up[row_d] <- x_i_ci$value[row_d] + x_i_ci$se[row_d] * ifelse(is.na(df[row_d]), qnorm(.975), qt(0.975, df[row_d]))
  }
  # print(meas[1])
  if (any(meas == "fisher_z")) {
    row_z = which(meas=="fisher_z")
    if (any(is.na(x_i_ci$n_sample))) {
      miss_n = which(is.na(x_i_ci$n_sample))
      N <- (1/(x_i_ci$se^2)) + 3
      x_i_ci$n_sample[miss_n] = round(N[miss_n])
    }
  }
  is_missing <- function(df, col_name) {
    !col_name %in% names(df) | is.na(df[[col_name]])
  }

  if (!"logor_se" %in% names(x_i_ci)) x_i_ci$logor_se = x_i_ci$or_se
  if (!"logrr_se" %in% names(x_i_ci)) x_i_ci$logrr_se = x_i_ci$rr_se
  if (!"loghr_se" %in% names(x_i_ci)) x_i_ci$loghr_se = x_i_ci$hr_se
  if (!"logirr_se" %in% names(x_i_ci)) x_i_ci$logirr_se = x_i_ci$irr_se

  x_i_ci$value[tolower(x_i_ci$measure) != tolower(measure)] <- NA
  x_i_ci$se[tolower(x_i_ci$measure) != tolower(measure)] <- NA
  x_i_ci$ci_lo[tolower(x_i_ci$measure) != tolower(measure)] <- NA
  x_i_ci$ci_up[tolower(x_i_ci$measure) != tolower(measure)] <- NA
  x_i_ci$info_used = NA
  x_i_ci$info_used[which(!is.na(x_i_ci$value) & !is.na(x_i_ci$se) &
                         !is.na(x_i_ci$ci_lo) & !is.na(x_i_ci$ci_up))] <- "es_se|es_ci"

  if (any(x_i_ci$reverse_es) | all(is.na(x_i_ci$reverse_es))) {
    value = ifelse(x_i_ci$reverse_es, -x_i_ci$value, x_i_ci$value)
    ci_lo = ifelse(x_i_ci$reverse_es, -x_i_ci$ci_up, x_i_ci$ci_lo)
    ci_up = ifelse(x_i_ci$reverse_es, -x_i_ci$ci_lo, x_i_ci$ci_up)
    x_i_ci$value = value
    x_i_ci$ci_lo = ci_lo
    x_i_ci$ci_up = ci_up
  }
  return(x_i_ci)
}

.format_effsize = function(x_i_value, measure, max_asymmetry) {

  meas = .conv_measure(x_i_value$measure)

  # print(list("x_i_value=", x_i_value))
  x_d = .es_from_d(d = x_i_value$d, d_se = x_i_value$d_se,
                                 n_exp = x_i_value$n_cases,
                                 n_nexp = x_i_value$n_controls,
                                 reverse = x_i_value$reverse_es)
  x_d$info_used = "SMD (d|g)"
  x_g = metaConvert::es_from_hedges_g(hedges_g = x_i_value$g,
                                      n_exp = x_i_value$n_cases,
                                      n_nexp = x_i_value$n_controls,
                                      reverse_g = x_i_value$reverse_es)
  x_md = metaConvert::es_from_md_ci(
    md = x_i_value$md,
    md_ci_lo = x_i_value$md_ci_lo,
    md_ci_up = x_i_value$md_ci_up,
    n_exp = x_i_value$n_cases, n_nexp = x_i_value$n_controls,
    max_asymmetry = max_asymmetry,
    reverse_md = x_i_value$reverse_es)

  x_means = metaConvert::es_from_means_sd(mean_exp = x_i_value$mean_cases, mean_sd_exp = x_i_value$sd_cases,
                             mean_nexp = x_i_value$mean_controls, mean_sd_nexp = x_i_value$sd_controls,
                             n_exp = x_i_value$n_cases,
                             n_nexp = x_i_value$n_controls,
                             reverse_means =x_i_value$reverse_es)

  x_means_pre_post = metaConvert::es_from_means_sd_pre_post(
                        mean_exp = x_i_value$mean_cases,
                        mean_sd_exp = x_i_value$sd_cases,
                        mean_nexp = x_i_value$mean_controls,
                        mean_sd_nexp = x_i_value$sd_controls,
                        mean_pre_exp = x_i_value$mean_pre_cases,
                        mean_pre_sd_exp = x_i_value$sd_pre_cases,
                        mean_pre_nexp = x_i_value$mean_pre_controls,
                        mean_pre_sd_nexp = x_i_value$sd_pre_controls,
                        n_exp = x_i_value$n_cases,
                        n_nexp = x_i_value$n_controls,
                        pre_post_to_smd = "cooper",
                        r_pre_post_exp = x_i_value$pre_post_cor,
                        r_pre_post_nexp = x_i_value$pre_post_cor,
                        reverse_means_pre_post = x_i_value$reverse_es)

  x_mean_change = metaConvert::es_from_mean_change_sd(
    mean_change_exp = x_i_value$mean_change_cases,
    mean_change_nexp = x_i_value$mean_change_controls,
    mean_change_sd_exp = x_i_value$sd_change_cases,
    mean_change_sd_nexp = x_i_value$sd_change_controls,
    n_exp = x_i_value$n_cases, n_nexp = x_i_value$n_controls,
    r_pre_post_exp = x_i_value$pre_post_cor,
    r_pre_post_nexp = x_i_value$pre_post_cor,
    reverse_mean_change = x_i_value$reverse_es)

  x_or = metaConvert::es_from_or(or = x_i_value$or,
                                 logor = x_i_value$logor,
                                 n_exp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_exp), x_i_value$n_cases, x_i_value$n_exp),
                                 n_nexp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_nexp), x_i_value$n_controls, x_i_value$n_nexp),
                                 n_cases = x_i_value$n_cases,
                                 n_controls = x_i_value$n_controls,
                                 or_to_rr = "metaumbrella_cases",
                                 reverse_or = x_i_value$reverse_es)


  x_or_se = metaConvert::es_from_or_se(or = x_i_value$or,
                          logor = x_i_value$logor,
                          logor_se = x_i_value$logor_se,
                          n_exp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_exp), x_i_value$n_cases, x_i_value$n_exp),
                          n_nexp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_nexp), x_i_value$n_controls, x_i_value$n_nexp),
                          n_cases = x_i_value$n_cases, n_controls = x_i_value$n_controls,
                          or_to_rr = "metaumbrella_cases",
                          reverse_or = x_i_value$reverse_es)

  x_or_ci = suppressWarnings(metaConvert::es_from_or_ci(or = x_i_value$or,
                          or_ci_lo = x_i_value$or_ci_lo,
                          or_ci_up = x_i_value$or_ci_up,
                          logor = x_i_value$logor,
                          logor_ci_lo = x_i_value$logor_ci_lo,
                          logor_ci_up = x_i_value$logor_ci_up,
                          n_exp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_exp), x_i_value$n_cases, x_i_value$n_exp),
                          n_nexp = ifelse(measure %in% c("G", "SMD", "SMC", "MC", "MD") & is.na(x_i_value$n_nexp), x_i_value$n_controls, x_i_value$n_nexp),
                          n_cases = x_i_value$n_cases,
                          n_controls = x_i_value$n_controls,
                          or_to_rr = "metaumbrella_cases",
                          max_asymmetry = max_asymmetry,
                          reverse_or = x_i_value$reverse_es))

  x_rr_se = metaConvert::es_from_rr_se(rr = x_i_value$rr,
                                       logrr = x_i_value$logrr,
                                       logrr_se = x_i_value$logrr_se,
                                       n_exp = x_i_value$n_exp,
                                       n_nexp = x_i_value$n_nexp,
                                       n_cases = x_i_value$n_cases,
                                       n_controls = x_i_value$n_controls,
                                       rr_to_or = "metaumbrella",
                                       reverse_rr = x_i_value$reverse_es)

  x_rr_ci = metaConvert::es_from_rr_ci(rr = x_i_value$rr,
                          rr_ci_lo = x_i_value$rr_ci_lo,
                          rr_ci_up = x_i_value$rr_ci_up,
                          logrr = x_i_value$logrr,
                          logrr_ci_lo = x_i_value$logrr_ci_lo,
                          logrr_ci_up = x_i_value$logrr_ci_up,
                          n_exp = x_i_value$n_exp, n_nexp = x_i_value$n_nexp,
                          n_cases = x_i_value$n_cases, n_controls = x_i_value$n_controls,
                          rr_to_or = "metaumbrella",
                          max_asymmetry = max_asymmetry,
                          reverse_rr = x_i_value$reverse_es)

  x_r = metaConvert::es_from_pearson_r(pearson_r = x_i_value$pearson_r,
                                       n_sample = x_i_value$n_sample,
                                       reverse_pearson_r = x_i_value$reverse_es)
  x_z = metaConvert::es_from_fisher_z(fisher_z = x_i_value$fisher_z,
                                      n_sample = x_i_value$n_sample,
                                      reverse_fisher_z = x_i_value$reverse_es)
  x_cases_time = metaConvert::es_from_cases_time(
    n_cases_exp = x_i_value$n_cases_exp,
    n_cases_nexp = x_i_value$n_cases_nexp,
    time_exp = x_i_value$time_exp,
    time_nexp = x_i_value$time_nexp,
    reverse_irr = x_i_value$reverse_es)
  x_2x2 = metaConvert::es_from_2x2(
    n_cases_exp = x_i_value$n_cases_exp,
    n_cases_nexp = x_i_value$n_cases_nexp,
    n_controls_exp = x_i_value$n_controls_exp,
    n_controls_nexp = x_i_value$n_controls_nexp,
    reverse_2x2 = x_i_value$reverse_es)
  x_2x2_sum = metaConvert::es_from_2x2_sum(
    n_cases_exp = x_i_value$n_cases_exp,
    n_cases_nexp = x_i_value$n_cases_nexp,
    n_exp = x_i_value$n_exp,
    n_nexp = x_i_value$n_nexp,
    reverse_2x2 = x_i_value$reverse_es)
  x_2x2_prop = metaConvert::es_from_2x2_prop(
    prop_cases_exp = x_i_value$prop_cases_exp,
    prop_cases_nexp = x_i_value$prop_cases_nexp,
    n_exp = x_i_value$n_exp,
    n_nexp = x_i_value$n_nexp,
    reverse_prop = x_i_value$reverse_es)

  x_hr = metaConvert::es_from_user_crude(
    measure = rep("loghr", nrow(x_i_value)),
    user_es_measure_crude = rep("logor", nrow(x_i_value)),
    user_es_crude = x_i_value$loghr, user_se_crude = x_i_value$loghr_se,
    user_ci_lo_crude = x_i_value$loghr_ci_lo,
    user_ci_up_crude = x_i_value$loghr_ci_up)

  x_hr_data <- data.frame(
    loghr = x_i_value$loghr,
    loghr_se = x_i_value$loghr_se,
    loghr_ci_lo = x_i_value$loghr_ci_lo,
    loghr_ci_up = x_i_value$loghr_ci_up,

    logrr = x_i_value$loghr,
    logrr_se = x_i_value$loghr_se,
    logrr_ci_lo = x_i_value$loghr_ci_lo,
    logrr_ci_up = x_i_value$loghr_ci_up,

    logor = x_i_value$loghr,
    logor_se = x_i_value$loghr_se,
    logor_ci_lo = x_i_value$loghr_ci_lo,
    logor_ci_up = x_i_value$loghr_ci_up,

    info_used = "Hazard ratio")

  # print(list("x_d=", x_d))
  # print(list("x_or_se=", x_or_se))
  # print(list("x_or_ci=", x_or_ci))


  if (measure %in% c("SMD")) {
    list_es_raw = list(x_g, x_d, x_means, x_md, x_means_pre_post, x_mean_change,
                   x_2x2, x_2x2_sum, x_2x2_prop,
                   x_or_se, x_or_ci, x_or)
    list_es = lapply(list_es_raw, function(x) x[, c("g", "g_se", "g_ci_lo", "g_ci_up", "info_used")])

  } else if (measure %in% c("G")) {
    list_es_raw = list(x_g, x_d, x_means, x_md, x_means_pre_post, x_mean_change,
                       x_2x2, x_2x2_sum, x_2x2_prop,
                       x_or_se, x_or_ci, x_or)
    list_es = lapply(list_es_raw, function(x) x[, c("g", "g_se", "g_ci_lo", "g_ci_up", "info_used")])
    # print(cbind(x_means_pre_post$g, x_means_pre_post$g_se))
  } else if (measure %in% c("SMC")) {
    list_es_raw = list(x_means_pre_post, x_mean_change, x_d, x_g,
                       x_2x2, x_2x2_sum, x_2x2_prop,
                       x_or_se, x_or_ci, x_or)
    list_es = lapply(list_es_raw, function(x) x[, c("g", "g_se", "g_ci_lo", "g_ci_up", "info_used")])
    # print(cbind(x_means_pre_post$g, x_means_pre_post$g_se))
  } else if (measure %in% c("MD")) {
    list_es_raw = list(x_means, x_md, x_mean_change)
    list_es = lapply(list_es_raw, function(x) x[, c("g", "g_se", "g_ci_lo", "g_ci_up", "info_used")])
    list_es_bis = lapply(list(x_means, x_md, x_mean_change),
                         function(x) x[, c("md", "md_se", "md_ci_lo", "md_ci_up", "info_used")])
    # print(paste0("value_raw=", x_i_value$value))
    x_i_value_save = x_i_value
    x_i_value[which(x_i_value$measure == "MD"),
              c("value", "se", "ci_lo", "ci_up")] <- NA
    # print(paste0("value_clean=", x_i_value$value))

    x_i_value$measure = "G"
  } else if (measure %in% c("MC")) {
    list_es_raw = list(x_mean_change, x_means_pre_post, x_md)
    list_es = lapply(list_es_raw, function(x) x[, c("g", "g_se", "g_ci_lo", "g_ci_up", "info_used")])
    list_es_bis = lapply(list(x_md, x_mean_change, x_means_pre_post, x_means),
                         function(x) x[, c("md", "md_se", "md_ci_lo", "md_ci_up", "info_used")])
    x_i_value_save = x_i_value
    x_i_value[which(x_i_value$measure == "MC"), c("value", "se", "ci_lo", "ci_up")] <- NA
    x_i_value$measure = "G"
  } else if (measure %in% c("RR", "logRR")) {
    list_es_raw = list(x_2x2, x_2x2_sum, x_2x2_prop, x_rr_se, x_rr_ci, x_or_se, x_or_ci, x_or, x_hr_data)
    list_es = lapply(list_es_raw, function(x) x[, c("logrr", "logrr_se", "logrr_ci_lo", "logrr_ci_up", "info_used")])
  } else if (measure %in% c("OR", "logOR")) {
    list_es_raw = list(x_2x2, x_2x2_sum, x_2x2_prop,
                       x_or_se, x_or_ci, x_or,
                       x_rr_se, x_rr_ci,x_hr_data,
                       x_d, x_g, x_means)
    list_es = lapply(list_es_raw, function(x) x[, c("logor", "logor_se", "logor_ci_lo", "logor_ci_up", "info_used")])
  } else if (measure %in% c("IRR", "logIRR")) {
    list_es_raw = list(x_cases_time)
    list_es = lapply(list_es_raw, function(x) x[, c("logirr", "logirr_se", "logirr_ci_lo", "logirr_ci_up", "info_used")])
  } else if (measure %in% c("HR", "logHR")) {
    list_es_raw = list(x_hr_data)
    list_es = lapply(list_es_raw, function(x) x[, c("loghr", "loghr_se", "loghr_ci_lo", "loghr_ci_up", "info_used")])
  } else if (measure %in% c("R", "Z")) {
    list_es_raw = list(x_r, x_z, x_d, x_g, x_means)# x_means x_2x2 x_2x2_sum x_2x2_prop)
    list_es = lapply(list_es_raw, function(x) x[, c("z", "z_se", "z_ci_lo", "z_ci_up", "info_used")])
  } else {
    list_es = list(data.frame(cbind(NA, NA, NA, NA, NA)))
  }

  # standard path
  data_es = x_i_value[, c("value", "se", "ci_lo", "ci_up", "info_used")]
  # print(paste0("data_es_raw=", data_es$value))
  # sapply(list_es, function(x) x$info_used[1])
  for (row in 1:nrow(data_es)) {
    if (as.numeric(rowSums(is.na(data_es[row, ]))) > 0) {
      for (list_i in 1:length(list_es)) {
        if (as.numeric(rowSums(!is.na(list_es[[list_i]][row, ]))) == 5) {
          # print(list_i)
          if (as.numeric(rowSums(is.na(data_es[row, ]))) > 0) {
            data_es[row, ] <- list_es[[list_i]][row, ]
          }
        }
      }
    }
  }
  # print(cbind(data_es$value, data_es$se))
  # print(paste0("data_es_clean=", data_es$value))
  # print(paste0("data_es_clean=", data_es$info_used))

  # specific to MD/MC
  if (measure %in% c("MD", "MC")) {
    data_es_bis = x_i_value[, c("value", "se", "ci_lo", "ci_up", "info_used")]
    data_es_bis[!is.na(data_es_bis)] <- NA

    for (row in 1:nrow(data_es_bis)) {
      if (as.numeric(rowSums(is.na(data_es_bis[row, ]))) > 0) {
        for (list_i in 1:length(list_es_bis)) {
          if (as.numeric(rowSums(!is.na(list_es_bis[[list_i]][row, ]))) == 5) {
            data_es_bis[row, ] <- list_es_bis[[list_i]][row, ]
          }
        }
      }
    }
    x_i_md = x_i_value
    x_i_md[, c("info_used", "value", "se", "ci_lo", "ci_up")] <-
        data_es_bis[, c("info_used", "value", "se", "ci_lo", "ci_up")]

  } else {
    x_i_md = NULL
  }

  data_es$measure = .transf_measure(measure)
  x_i_value[, c("measure", "info_used", "value", "se", "ci_lo", "ci_up")] <-
    data_es[, c("measure", "info_used", "value", "se", "ci_lo", "ci_up")]
  # print(paste0("data_final=", data_es$value))

  attr(x_i_value, "x_i_es_bis") <- x_i_md
  return(x_i_value)
}



.conv_measure = function(x) {
  as.character(sapply(x, function(x) switch(x,
            "SMD" = "d",
            "SMC" = "d",
            "MD" = "md",
            "MC" = "md",
            "G" = "g",
            "R" = "pearson_r",
            "Z" = "fisher_z",
            "RR" = "rr",
            "logRR" = "logrr",
            "OR" = "or",
            "logOR" = "logor",
            "HR" = "hr",
            "logHR" = "loghr",
            "IRR" = "irr",
            "logIRR" = "logirr")))
}

.transf_measure = function(x) {
  as.character(sapply(x, function(x) switch(x,
                                            "SMD" = "g",
                                            "SMC" = "g",
                                            "MD" = "md",
                                            "MC" = "md",
                                            "G" = "g",
                                            "R" = "fisher_z",
                                            "Z" = "fisher_z",
                                            "RR" = "logrr",
                                            "logRR" = "logrr",
                                            "OR" = "logor",
                                            "logOR" = "logor",
                                            "HR" = "loghr",
                                            "logHR" = "loghr",
                                            "IRR" = "logirr",
                                            "logIRR" = "logirr")))
}
