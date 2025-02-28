#' Internal function to calculate GRADE evidence
#'
#' @param x an umbrella object
#'
#' @noRd
.add.evidence_GRADE = function (x, eq_range_or, eq_range_g) {
  attr(x, "criteria") = "GRADE"
  for (name in names(x)) {
    x_i = x[[name]]

    print(paste0("--Grading evidence for factor: ", name, "--"))
    if (x_i$measure == "Z") {
      stop("The GRADE criteria cannot be applied with R and Z effect size measures.")
    }

    rawdat = x_i$x
    CI_lo = x_i$ma_results$ci_lo; CI_up = x_i$ma_results$ci_up
    PI_lo = x_i$ma_results$pi_lo; PI_up = x_i$ma_results$pi_up
    n_cases = x_i$n$cases; n_controls = x_i$n$controls
    I2 = x_i$heterogeneity$i2
    rob = x_i$overall_rob
    indirectness = x_i$indirectness
    perc_contradict = x_i$perc_contradict
    n_studies = x_i$n$studies

    egger_p_na = as.numeric(as.character(x_i$egger$p.value))
    esb_p_na = as.numeric(as.character(x_i$esb$p.value))
    report_na = as.numeric(as.character(x_i$report_rob))

    if (nrow(rawdat) < 5) {
      message(paste0("You called the GRADE evidence while the number of studies in ",
                     name, " is very limited. This factor can reach the 'Moderate' GRADE at most."))

    }
    if (is.na(rob)) {
      report_rob = 0
      message(paste0("The GRADE evidence relies on the bias of individual studies,",
                     " that is not reported for factor ", name, ". It is currently assumed ",
                     " that your studies are at high risk of bias."))
    }
    if (is.na(egger_p_na) & is.na(esb_p_na) & is.na(report_na)) {
      report_rob = 0
      message(paste0("The GRADE evidence relies on the reporting bias, egger's test and esb test. ",
                     " Because all this information is missing, for factor ", name, ", it is assumed ",
                     " that your studies are at serious risk of publication bias."))
    }
    if (is.na(indirectness)) {
      indirectness = "very serious"
      message(paste0("The GRADE evidence relies on the indirectness,",
                     " that is not reported for factor ", name, ". It is currently assumed ",
                     " that your studies are at high risk of indirectness."))
    }


    if (x_i$measure == "IRR") {
      n_controls = n_cases/2; n_cases = n_cases/2
    }

    meas_G = ifelse(x_i$measure %in% c("G", "SMD", "SMC", "MC", "MD"), TRUE, FALSE)
    meas_OR = ifelse(x_i$measure %in% c("logOR", "logRR", "logHR", "logIRR",
                                        "OR", "RR", "HR", "IRR"), TRUE, FALSE)

    if (meas_OR) {
      CI_lo = exp(CI_lo); CI_up = exp(CI_up)
      PI_lo = exp(PI_lo); PI_up = exp(PI_up)
    }
    if (!meas_G & !meas_OR) {
      stop("The effect measure is not tolerated")
    }

    # ------------------------- #
    # ---------- RoB ---------- #
    # ------------------------- #
    down_rob = ifelse(is.na(rob), 2,
                      ifelse(rob >= 75, 0,
                             ifelse(rob >= 50, 1, 2)))

    # ----------------------------------- #
    # ---------- Heterogeneity ---------- #
    # ----------------------------------- #

    down_het = NA

    if (!is.na(CI_lo) & !is.na(CI_up) &
        !is.na(PI_lo) & !is.na(PI_up)) {

      low_ci_neg_range =
        ((meas_G  & CI_lo < 0)  & CI_lo >= eq_range_g[1]) |
        ((meas_OR & CI_lo < 1) & CI_lo >= eq_range_or[1])

      low_pi_neg_range =
        (meas_G  & PI_lo < 0 & PI_lo >= eq_range_g[1]) |
        (meas_OR & PI_lo < 1 & PI_lo >= eq_range_or[1])

      up_ci_pos_range =
        (meas_G  & CI_up >= 0 & CI_up <= eq_range_g[2]) |
        (meas_OR & CI_up >= 1 & CI_up <= eq_range_or[2])

      up_pi_pos_range =
        (meas_G  & PI_up >= 0 & PI_up <= eq_range_g[2]) |
        (meas_OR & PI_up >= 1 & PI_up <= eq_range_or[2])

      low_ci_pos = (meas_G  & CI_lo >= 0) |
        (meas_OR & CI_lo >= 1)

      low_pi_pos = (meas_G  & PI_lo >= 0) |
        (meas_OR & PI_lo >= 1)

      up_ci_neg = (meas_G  & CI_up < 0) |
        (meas_OR & CI_up < 1)

      up_pi_neg = (meas_G  & PI_up < 0) |
        (meas_OR & PI_up < 1)


      low_ci_out_range = (meas_G  & CI_lo < eq_range_g[1]) |
        (meas_OR & CI_lo < eq_range_or[1])

      low_pi_out_range = (meas_G  & PI_lo < eq_range_g[1]) |
        (meas_OR & PI_lo < eq_range_or[1])

      up_ci_out_range = (meas_G  & CI_up > eq_range_g[2]) |
        (meas_OR & CI_up > eq_range_or[2])

      up_pi_out_range = (meas_G  & PI_up > eq_range_g[2]) |
        (meas_OR & PI_up > eq_range_or[2])

      # Condition 1 - Row 8 CINeMA
      down_het[((low_ci_neg_range & low_pi_out_range) &
                  (up_ci_pos_range  & up_pi_out_range))] <- 2

      # Condition 3 - Row 4 CINeMA (a)
      down_het[is.na(down_het) &
                 ((low_ci_pos & low_pi_out_range) |
                    (up_ci_neg  & up_pi_out_range))] <- 2

      # Condition 2 - Row 6 CINeMA
      down_het[is.na(down_het) &
                 ((low_ci_neg_range & low_pi_out_range) |
                    (up_ci_pos_range  & up_pi_out_range))] <- 1

      # Condition 4 - Row 4 CINeMA (b)
      down_het[is.na(down_het) &
                 ((low_ci_pos & low_pi_neg_range) |
                    (up_ci_neg  & up_pi_pos_range))] <- 1

      # Condition 5 - Row 1 CINeMA
      down_het[is.na(down_het) &
                 ((low_ci_pos & low_pi_pos) |
                    (up_ci_neg  & up_pi_neg))] <- 0

      # Condition 6 - Row 2 & 7 CINeMA
      down_het[is.na(down_het) &
                 ((low_ci_neg_range & low_pi_neg_range) |
                    (up_ci_pos_range  & up_pi_pos_range))] <- 0

      # Condition 7 - 5
      down_het[is.na(down_het) &
                 ((low_ci_out_range & low_pi_out_range) |
                    (up_ci_out_range  & up_pi_out_range))] <- 0

    } else {
      down_het = NA
    }

    if (is.na(down_het)) {

      I2 = as.numeric(as.character(I2))

      down_het = ifelse(
        is.na(I2),
        ifelse(perc_contradict >= 0.10, 2, 0),
        ifelse(
          I2 >= 50 & perc_contradict >= 0.10, 2,
          ifelse(I2 >= 30 & perc_contradict >= 0.10, 1, 0)
        )
      )
    }

    # ----------------------------- #
    # ------- indirectness -------- #
    # ----------------------------- #
    down_ind = ifelse(is.na(indirectness) | indirectness == "very serious", 2,
                      ifelse(indirectness == "serious", 1, 0))

    # ------------------------- #
    # ------ Imprecision ------ #
    # ------------------------- #
    cross_low_high =
      (meas_G  & CI_lo <= 0    & CI_up >= 0.8) |
      (meas_G  & CI_lo <= -0.8 & CI_up >= 0) |
      (meas_OR & CI_lo <= 1    & CI_up >= 5) |
      (meas_OR & CI_lo <= 0.2  & CI_up >= 1)

    n_fail_detect_small_effects =
      n_cases < 394 | n_controls < 394

    n_fail_detect_large_effects =
      n_cases < 64 | n_controls < 64

    down_imp =
      ifelse(cross_low_high & n_fail_detect_small_effects, 2,
             ifelse(n_fail_detect_large_effects, 2,
                    ifelse(cross_low_high | n_fail_detect_small_effects, 1, 0)))

    # -------------------------- #
    # ---- publication bias ---- #
    # -------------------------- #
    down_pubbias = ifelse(
      (is.na(egger_p_na) & is.na(esb_p_na) & is.na(report_na)), NA,
      ifelse(
        (!is.na(egger_p_na) & egger_p_na <= 0.10) |
          (!is.na(esb_p_na) & esb_p_na <= 0.10) |
          (!is.na(report_na) & report_na < 50),
        1, 0)
    )


    # print(paste0("RoB = ", down_rob))
    # print(paste0("Het = ", down_het))
    # print(paste0("Indi = ", down_ind))
    # print(paste0("Impr = ", down_imp))
    # print(paste0("Pub = ", down_pubbias))
    # print(paste0("stud = ", n_studies >= 5))
    down_rob[is.na(down_rob)] <- 2
    down_het[is.na(down_het)] <- 2
    down_ind[is.na(down_ind)] <- 2
    down_imp[is.na(down_imp)] <- 2
    down_pubbias[is.na(down_pubbias)] <- 1

    down_grade = ifelse(
      n_studies >= 5,
      sum(down_rob, down_het, down_ind, down_imp,  down_pubbias),
      sum(down_rob, down_het, down_ind, down_imp,  down_pubbias) + 1
    )

    evidence = ifelse(down_grade == 0, "High",
                      ifelse(down_grade == 1, "Moderate",
                             ifelse(down_grade == 2, "Weak", "Very weak")))

    x[[name]]$evidence = evidence
  }
  return(x)
}

#' Internal function to calculate Ioannidis evidence
#'
#' @param x an umbrella object
#'
#' @noRd
.add.evidence_Ioannidis = function (x, verbose) {
  attr(x, "criteria") = "Ioannidis"
  for (name in names(x)) {
    x_i = x[[name]]
    p.value <- x_i$ma_results$p.value
    if (is.na(x_i$n$cases)) {
      x_i$n$cases <- 0
      if (verbose) warning("The number of cases has been assumed to be equal to 0. You can manually set up the number of cases without affecting the meta-analytic calculations using the n_cases column.")
    }
    if (is.na(p.value)) {
      warning("Error calculating evidence in ", rownames(x_i$data))
    }
    if (is.na(p.value) || p.value >= 0.05) {
      y_i = "ns"
    } else if (x_i$n$cases > 1000) {
      if (p.value < 1e-6) {
        if (
          x_i$heterogeneity$i2 < 50 &&
          !is.na(x_i$ma_results$pi_lo) &&
          sign(x_i$ma_results$pi_lo) == sign(x_i$ma_results$pi_up) &&
          x_i$egger$p.value > 0.05 &&
          x_i$esb$p.value > 0.05
        ) {
          y_i = "I"
        } else if (sign(x_i$largest$ci_lo) == sign(x_i$largest$ci_up) &&
                   sign(x_i$largest$ci_lo) == sign(x_i$ma_results$ci_lo) &&
                   sign(x_i$largest$ci_up) == sign(x_i$ma_results$ci_up)) {
          y_i = "II"
        } else {
          y_i = "III"
        }
      } else if (p.value < 0.001) {
        y_i = "III"
      } else {
        y_i = "IV"
      }
    } else {
      y_i = "IV"
    }
    x[[name]]$evidence = y_i
  }
  return(x)
}



#' stratify evidence for personalised criteria
#'
#' @param x an umbrella object
#' @param class_I a vector or list of threshold values required for reaching Class I in the Personalised criteria (see details below).
#' @param class_II a vector or list of threshold values required for reaching Class II in the Personalised criteria (see details below).
#' @param class_III a vector or list of threshold values required for reaching Class III in the Personalised criteria (see details below).
#' @param class_IV a vector or list of threshold values required for reaching Class IV in the Personalised criteria (see details below).
#' @param verbose logical variable indicating whether text outputs and messages should be generated. Give important information when the data have a multivariate / hierarchical structure.
#'
#' @noRd
.add.evidence_personalised <-  function (x, class_I, class_II, class_III, class_IV, verbose) {

    attr(x, "criteria") = "Personalised"

    factors_limited = criteria_limited = NULL

    for (name in names(x)) {
      # extraction information from the umbrella object
      x_i = x[[name]]
      measure <- x_i$measure
      total_n <- ifelse(is.na(x_i$n$total_n), 0, x_i$n$total_n)
      n_studies <- x_i$n$studies
      n_cases <- ifelse(is.na(x_i$n$cases), 0, x_i$n$cases)
      p_value <- x_i$ma_results$p.value
      I2 <- x_i$heterogeneity$i2
      overall_rob <- x_i$overall_rob
      amstar <- x_i$amstar
      JK_p <- max(x_i$jk)
      egger_p <- x_i$egger$p.value
      esb_p <- x_i$esb$p.value
      pi <- ifelse(sign(x_i$ma_results$pi_lo) == sign(x_i$ma_results$pi_up), "notnull", "null")
      largest_CI <- ifelse(
        sign(x_i$largest$ci_lo) == sign(x_i$largest$ci_up) &&
          sign(x_i$largest$ci_lo) == - sign(x_i$ma_results$ci_lo) &&
          sign(x_i$largest$ci_up) == - sign(x_i$ma_results$ci_up), "opposite direction",
        ifelse(sign(x_i$largest$ci_lo) == sign(x_i$largest$ci_up), "notnull", "null"))

      if (any(duplicated(colnames(t(unlist(class_I))))) |
          any(duplicated(colnames(t(unlist(class_II))))) |
          any(duplicated(colnames(t(unlist(class_III))))) |
          any(duplicated(colnames(t(unlist(class_IV)))))) {
        stop("There is a repeated entry of a criterion for a class. Check criteria used for each class.")
      }

      # modify the format of criteria inputs; unlist in case users provide a list
      c1 <- data.frame(t(unlist(class_I)))
      c2 <- data.frame(t(unlist(class_II)))
      c3 <- data.frame(t(unlist(class_III)))
      c4 <- data.frame(t(unlist(class_IV)))

      # names of all possible criteria
      col_names <- c("n_studies", "total_n", "n_cases", "p_value", "I2", "imprecision", "rob", "amstar", "egger_p", "esb_p", "JK_p", "pi", "largest_CI")
      col_types <- c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character")
      # non-used criteria are set to NA
      for (col in col_names) {
        if (!(col %in% colnames(c1))) {
          c1[, col] <- NA
        }
        if (!(col %in% colnames(c2))) {
          c2[, col] <- NA
        }
        if (!(col %in% colnames(c3))) {
          c3[, col] <- NA
        }
        if (!(col %in% colnames(c4))) {
          c4[, col] <- NA
        }
      }
      # reorder and merge criteria
      if (!all(c(colnames(c1), colnames(c2), colnames(c3), colnames(c4)) %in% col_names)) {
        stop(paste0("Criteria '", paste(c(colnames(c1), colnames(c2), colnames(c3), colnames(c4))[which(!c(colnames(c1), colnames(c2), colnames(c3), colnames(c4)) %in% col_names)], collapse = "', '"), "' not tolerated. Please, see the manual for more information on the possible criteria of the add.evidence() function."))
      }
      c1 <- c1[, col_names]
      c2 <- c2[, col_names]
      c3 <- c3[, col_names]
      c4 <- c4[, col_names]
      data_class <- rbind(c1, c2, c3, c4)


      for (j in 1:length(col_names)) {
        idx = which(colnames(data_class) == col_names[j])
        data_class[, idx] = as.character(data_class[, idx])
        if (col_types[j] == "numeric") {
          if (any(grepl(".", data_class[, idx])) & any(grepl(",", data_class[, idx]))) {
            dot_and_points <- which(grepl(".", data_class[, idx])) & any(grepl(",", data_class[, idx]))
            stop("The 'Personalized' criteria inputs contain both '.' and ',' and could thus not be converted to a numeric format. Please, homogeneise the notation.")
          } else if (any(!grepl('^[0-9]|-', na.omit(data_class[, idx])))) {
            not_num <- which(!grepl('^[0-9]|-', na.omit(data_class[, idx])))
            stop("The 'Personalized' criteria contain non-numeric characters while this is expected. Please check inputs.")
          } else if (any(grepl(",", data_class[, idx]))) {
            data_class[, idx] <- gsub(",", ".", data_class[, idx])
          }
          data_class[, idx] = as.numeric(data_class[, idx])
        }
      }

      ## check inputs on each criteria

      # 1. total_n
      if (any(!is.na(data_class$total_n))) {
        if(any(data_class$total_n[which(!is.na(data_class$total_n))] < 0)) {
          stop("The 'total_n' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 2. n_studies
      if (any(!is.na(data_class$n_studies))) {
        if(any(data_class$n_studies[which(!is.na(data_class$n_studies))] < 0)) {
          stop("The 'n_studies' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 3. total_n
      if (any(!is.na(data_class$total_n))) {
        if(any(data_class$total_n[which(!is.na(data_class$total_n))] < 0)) {
          stop("The 'total_n' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 4. n_cases
      if (any(!is.na(data_class$n_cases))) {
        if(any(data_class$n_cases[which(!is.na(data_class$n_cases))] < 0)) {
          stop("The 'n_cases' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 5. p_value
      if (any(!is.na(data_class$p_value))) {
        if(any(data_class$p_value[which(!is.na(data_class$p_value))] < 0) |
           any(data_class$p_value[which(!is.na(data_class$p_value))] > 1)) {
          stop("The 'p_value' inputs should be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 6. I2
      if (any(!is.na(data_class$I2))) {
        if(any(data_class$I2[which(!is.na(data_class$I2))] < 0) |
           any(data_class$I2[which(!is.na(data_class$I2))] > 100)) {
          stop("The 'I2' inputs should be numbers within the [0, 100] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 7. imprecision
      if (any(!is.na(data_class$imprecision))) {
        if (any(!is.numeric(data_class$imprecision[which(!is.na(data_class$imprecision))]))) {
          stop("The 'imprecision' inputs should be numeric values. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 8. rob
      if (any(!is.na(data_class$rob))) {
        if(any(data_class$rob[which(!is.na(data_class$rob))] < 0) |
           any(data_class$rob[which(!is.na(data_class$rob))] > 100)) {
          stop("The 'rob' inputs should be numbers within the [0, 100] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 9. amstar
      if (any(!is.na(data_class$amstar))) {
        if(any(data_class$amstar[which(!is.na(data_class$amstar))] < 0)) {
          stop("The 'amstar' inputs should be positive numbers. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 8. egger_p
      if (any(!is.na(data_class$egger_p))) {
        if(any(data_class$egger_p[which(!is.na(data_class$egger_p))] < 0) |
           any(data_class$egger_p[which(!is.na(data_class$egger_p))] > 1)) {
          stop("The 'egger_p' inputs should be p-values and shoud thus be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 10. esb_p.criteria
      if (any(!is.na(data_class$esb_p))) {
        if(any(data_class$esb_p[which(!is.na(data_class$esb_p))] < 0) |
           any(data_class$esb_p[which(!is.na(data_class$esb_p))] > 1)) {
          stop("The 'esb_p' inputs should be p-values and shoud thus be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }
      # 11. JK
      if (any(!is.na(data_class$JK_p))) {
        if(any(data_class$JK_p[which(!is.na(data_class$JK_p))] < 0) |
           any(data_class$JK_p[which(!is.na(data_class$JK_p))] > 1)) {
          stop("The 'JK_p' inputs should be p-values and shoud thus be numbers within the [0, 1] range. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }


      # 12. pi
      if (any(!is.na(data_class$pi))) {
        if(any(!data_class$pi[which(!is.na(data_class$pi))] %in% c("notnull"))) { #"null",  either 'null' or
          stop("The 'pi' inputs should be 'notnull'. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }

      # 13. largest_CI
      if (any(!is.na(data_class$largest_CI))) {
        if(any(!data_class$largest_CI[which(!is.na(data_class$largest_CI))] %in% c("notnull"))) { #"null", either 'null' or
          stop("The 'largest_CI' inputs should be 'notnull'. See manual for more details on the formatting of the 'Personalized' criteria.")
        }
      }
      # identify criteria retained for stratification of evidence by users
      used_criteria <- apply(data_class, 2, function(x) any(!is.na(x)))

      # if the user selects the personalised system but did not indicate any criterion, an error is returned
      if (sum(used_criteria) == 0) {
        stop("You have chosen the 'Personalised' rating but indicated no criteria to apply. Choose at least one criteria.")
      }

      # subsetting the vector containing all criteria to those chosen by the user
      list.criteria.nom.user <- col_names[used_criteria]

      if (is.na(n_cases)) {
        n_cases <- total_n / 2
        if ("n_cases" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "n_cases")
        }
      }
      if (is.na(egger_p)) {
        egger_p <- -1
        if ("egger_p" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "egger_p")
          }
      }

      if (is.na(esb_p)) {
        esb_p <- -1
        if ("esb_p" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "esb_p")
        }
      }

      if (is.na(pi)) {
        pi <- "null"
        if ("pi" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "pi")
        }
      }

      if (is.na(overall_rob)) {
        overall_rob = -1
        if ("rob" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "rob")
        }
      }

      if (is.na(amstar)) {
        amstar = -1
        if ("amstar" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "amstar")
        }
      }

      if (is.na(I2)) {
        I2 = 100
        if ("I2" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "I2")
        }
      }
      if (JK_p == "Only one study") {
        JK_p = 2
        if ("JK_p" %in% list.criteria.nom.user) {
          factors_limited = append(factors_limited, name)
          criteria_limited = append(criteria_limited, "JK_p")
        }
      }
      # for criteria non-retained by users, we set values preventing their influence during the stratification of evidence
      data_class$n_cases[is.na(data_class$n_cases)] = - Inf
      data_class$total_n[is.na(data_class$total_n)] = - Inf
      data_class$n_studies[is.na(data_class$n_studies)] = - Inf
      data_class$p_value[is.na(data_class$p_value)] = Inf
      data_class$I2[is.na(data_class$I2)] = Inf
      data_class$rob[is.na(data_class$rob)] = - Inf
      data_class$amstar[is.na(data_class$amstar)] = - Inf
      data_class$egger_p[is.na(data_class$egger_p)] = - Inf
      data_class$esb_p[is.na(data_class$esb_p)] = - Inf
      data_class$JK_p[is.na(data_class$JK_p)] = Inf
      data_class$pi[is.na(data_class$pi)] = pi
      data_class$largest_CI[is.na(data_class$largest_CI)] = largest_CI

      imprecision <- rep(NA, 4)
      for (i in which(!is.na(data_class$imprecision))) {
        imprecision[i] <- switch(measure,
                                 "logIRR"=, "IRR" = .power_d(x_i$n$cases / 2, x_i$n$cases / 2, data_class$imprecision[i]),
                                 "R"=, "Z" = .power_d(x_i$n$total_n / 2, x_i$n$total_n / 2, data_class$imprecision[i]),
                                 "MD"=, "MC"=,
                                 "SMD"=,
                                 "SMC"=,
                                 "OR"=,
                                 "logOR"=,
                                 "RR"=,
                                 "logRR"=,
                                 "HR"=,
                                 "logHR"= .power_d(x_i$n$cases, x_i$n$controls, data_class$imprecision[i]))
      }
      # we create an imprecision vector based on user's input to have a more personalised output
      x[[name]]$imprecision <- c(rep(NA_real_, 4))

      x[[name]]$imprecision[1] <- imprecision[1]
      x[[name]]$imprecision[2] <- imprecision[2]
      x[[name]]$imprecision[3] <- imprecision[3]
      x[[name]]$imprecision[4] <- imprecision[4]


      imprecision[is.na(imprecision)] = Inf

      # stratification of evidence
      if (n_studies > data_class$n_studies[1] & total_n > data_class$total_n[1] & n_cases > data_class$n_cases[1] & p_value < data_class$p_value[1] &
           I2 < data_class$I2[1] & imprecision[1] > 0.80 & overall_rob > data_class$rob[1] &
           amstar > data_class$amstar[1] & egger_p > data_class$egger_p[1] & esb_p > data_class$esb_p[1] & pi == data_class$pi[1] & largest_CI == data_class$largest_CI[1] & JK_p < data_class$JK_p[1]) {
          evidence <- 1
        } else if (n_studies > data_class$n_studies[2] & total_n > data_class$total_n[2] & n_cases > data_class$n_cases[2] & p_value < data_class$p_value[2] &
                   I2 < data_class$I2[2] & imprecision[2] > 0.80 & overall_rob > data_class$rob[2] &
                   amstar > data_class$amstar[2] & egger_p > data_class$egger_p[2] & esb_p > data_class$esb_p[2] & pi == data_class$pi[2] & largest_CI == data_class$largest_CI[2] & JK_p < data_class$JK_p[2]) {
          evidence <- 2
        } else if (n_studies > data_class$n_studies[3] & total_n > data_class$total_n[3] & n_cases > data_class$n_cases[3] & p_value < data_class$p_value[3] &
                   I2 < data_class$I2[3] & imprecision[3] > 0.80 & overall_rob > data_class$rob[3] &
                   amstar > data_class$amstar[3] & egger_p > data_class$egger_p[3] & esb_p > data_class$esb_p[3] & pi == data_class$pi[3] & largest_CI == data_class$largest_CI[3] & JK_p < data_class$JK_p[3]) {
          evidence <- 3
        } else if (n_studies > data_class$n_studies[4] & total_n > data_class$total_n[4] & n_cases > data_class$n_cases[4] & p_value < data_class$p_value[4] &
                   I2 < data_class$I2[4] & imprecision[4] > 0.80 & overall_rob > data_class$rob[4] &
                   amstar > data_class$amstar[4] & egger_p > data_class$egger_p[4] & esb_p > data_class$esb_p[4] & pi == data_class$pi[4] & largest_CI == data_class$largest_CI[4] & JK_p < data_class$JK_p[4]) {
          evidence <- 4
        } else {
          evidence <- 5
        }

      if (evidence == 1) {
        x[[name]]$evidence = "I"
      } else if (evidence == 2) {
        x[[name]]$evidence = "II"
      } else if(evidence == 3) {
        x[[name]]$evidence = "III"
      } else if(evidence == 4) {
        x[[name]]$evidence = "IV"
      } else if(evidence == 5) {
        x[[name]]$evidence = "V"
      }
    }
    # print some warning messages
    if (verbose) {
      if (any("n_cases" == criteria_limited)) {
        message("- For ", length(factors_limited[which("n_cases" == criteria_limited)]), " factors the 'n_cases' criteria is used but the number of cases is not indicated in the dataset. In this situation, we assumed the number of cases to be half the total number of participants ('n_sample').")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("n_cases" == criteria_limited)]), " factors the 'n_cases' criteria is used but the number of cases is not indicated in the dataset. In this situation, we assumed the number of cases to be half the total number of participants ('n_sample').")
      }
      if (any("egger_p" == criteria_limited)) {
        message("- For ", length(factors_limited[which("egger_p" == criteria_limited)]), " factors the 'egger_p' criteria is used but cannot be calculated due to the small number of studies (n < 2). In this situation, the p-value of the Egger test is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("egger_p" == criteria_limited)]), " factors the 'egger_p' criteria is used but cannot be calculated due to the small number of studies (n < 2). In this situation, the p-value of the Egger test is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
      }
      if (any("esb_p" == criteria_limited)) {
        message("- For ", length(factors_limited[which("esb_p" == criteria_limited)]), " factors the 'esb_p' criteria is used but cannot be estimated due to the small number of studies (n < 2). In this situation, the p-value of the ESB test is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("esb_p" == criteria_limited)]), " factors the 'egger_p' criteria is used but cannot be calculatedestimated due to calculations issues. In this situation, the p-value of the ESB test is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
      }

      if (any("pi" == criteria_limited)) {
        message("- For ", length(factors_limited[which("pi" == criteria_limited)]), " factors the 'pi' criteria is used but cannot be calculated due to the small number of studies (n < 2). In this situation, the 95% PI value is conservatively assumed to to include the null value. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("pi" == criteria_limited)]), " factors the 'pi' criteria is used but cannot be calculated due to the small number of studies (n < 2). In this situation, the 95% PI value is conservatively assumed to to include the null value. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
      }

      if (any("rob" == criteria_limited)) {
        message("- For ", length(factors_limited[which("rob" == criteria_limited)]), " factors the 'rob' criteria is used but cannot be calculated because this information is not indicated in the dataset. In this situation, the rob value is conservatively assumed to be higher than the thresold requested. This is indicated in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("rob" == criteria_limited)]), " factors the 'rob' criteria is used but cannot be calculated because this information is not indicated in the dataset. In this situation, the rob value is conservatively assumed to be higher than the thresold requested. This is indicated in the dataframe returned by the 'add.evidence()' function.")
      }

      if (any("amstar" == criteria_limited)) {
        message("- For ", length(factors_limited[which("amstar" == criteria_limited)]), " factors the 'amstar' criteria is used but cannot be calculated because this information is not indicated in the dataset. In this situation, the amstar value is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("amstar" == criteria_limited)]), " factors the 'amstar' criteria is used but cannot be calculated because this information is not indicated in the dataset. In this situation, the amstar value is conservatively assumed to be lower than the thresold requested. These factors have a NA value for this criteria in the dataframe returned by the 'add.evidence()' function.")
      }

      if (any("I2" == criteria_limited)) {
        message("- For ", length(factors_limited[which("I2" == criteria_limited)]), " factors the 'I2' criteria is used but cannot be calculated due to the small number of studies (n = 1). In this situation, the value of the I2 value is conservatively assumed to be higher than the thresold requested. This is indicated in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("I2" == criteria_limited)]), " factors the 'I2' criteria is used but cannot be calculated due to the small number of studies (n = 1). In this situation, the value of the I2 value is conservatively assumed to be higher than the thresold requested. This is indicated in the dataframe returned by the 'add.evidence()' function.")
      }
      if (any("JK_p" == criteria_limited)) {
        message("- For ", length(factors_limited[which("JK_p" == criteria_limited)]), " factors the 'JK_p' criteria is used but cannot be calculated due to the small number of studies (n = 1). In this situation, the value of the JK_p value is conservatively assumed to be higher than the requested threshold. This is indicated in the dataframe returned by the 'add.evidence()' function.")
        attr(x, "message") = paste(attr(x, "message"), "\n- For ", length(factors_limited[which("JK_p" == criteria_limited)]), " factors the 'JK_p' criteria is used but cannot be calculated due to the small number of studies (n = 1). In this situation, the value of the JK_p value is conservatively assumed to be higher than the requested threshold. This is indicated in the dataframe returned by the 'add.evidence()' function.")
      }
        # print the criteria for each factor
        message("\n", paste0("Criteria retained in the 'Personalized' classification for this umbrella review are:\n- ", paste0(list.criteria.nom.user, collapse = "\n- ")))
        attr(x, "message") = paste(attr(x, "message"), "\n\n", paste0("Criteria retained in the 'Personalized' classification for this umbrella review are:\n- ", paste0(list.criteria.nom.user, collapse = "\n- ")))
    }

    return(x)
}

