#' Synthesize information of an object of class \dQuote{umbrella} in a dataframe
#'
#' @param object an object of class \dQuote{umbrella}
#' @param digits an integer value specifying the number of decimal places for the rounding of numeric values. Default is 3.
#' @param het_max a logical variable indicating whether additional information on heterogeneity should be printed (\eqn{tau^2}, Q-statistic estimate and p-value).
#' @param ... other arguments that can be passed to the function
#'
#' @details
#' Summary method for objects of class \dQuote{umbrella}.
#'
#' @return
#' All main results of an object of class \dQuote{umbrella} are synthesized in a dataframe, with the results of each factors stored in their own row.
#' Depending on the classification used, the dataframe returned include certain information presented below:
#' \tabular{ll}{
#'  \code{Factor} \tab the name of the factor.\cr
#'  \tab \cr
#'  \code{Class} \tab the class assigned during the stratification of evidence.\cr
#'  \tab \cr
#'  \code{n_studies} \tab the number of independent studies included in the factor.\cr
#'  \tab \cr
#'  \code{total_n} \tab the total number of participants included in the factor.\cr
#'  \tab \cr
#'  \code{n_cases} \tab the number of cases included in the factor.\cr
#'  \tab \cr
#'  \code{n_controls} \tab the number of controls included in the factor.\cr
#'  \tab \cr
#'  \code{measure} \tab the measured used in the calculations.\cr
#'  \tab \cr
#'  \code{value} \tab the value of the pooled effect size expressed in its original metric. Note that\cr
#'  \tab  if a factor includes only one study, its effect size is used as the pooled effect size.\cr
#'  \tab \cr
#'  \code{value_CI} \tab the 95% confidence interval (CI) around the pooled effect size expressed \cr
#'  \tab in its original metric. Note that if a factor includes only one study, its 95% CI is\cr
#'  \tab used as the pooled 95% CI.\cr
#'  \tab \cr
#'  \code{eG} \tab the value of the pooled effect size expressed in equivalent Hedges' g (eG).\cr
#'  \tab \cr
#'  \code{eG_CI} \tab the 95% CI around the pooled effect size expressed in eG.\cr
#'  \tab \cr
#'  \code{eOR} \tab the value of the pooled effect size expressed in equivalent Odds ratio (eOR).\cr
#'  \tab \cr
#'  \code{eOR_CI} \tab the 95% CI around the pooled effect size expressed in eOR.\cr
#'  \tab \cr
#'  \code{p_value} \tab the p-value of the pooled effect size.\cr
#'  \tab \cr
#'  \code{I2} \tab the inconsistency (\eqn{I^2}) value (calculated only if the number of studies in the\cr
#'  \tab meta-analysis is equal or larger to 2).\cr
#'  \tab \cr
#'  \code{PI_eG} \tab the 95% prediction interval (PI) expressed in eG (calculated only if the number\cr
#'  \tab of studies in the meta-analysis is equal or larger to 3).\cr
#'  \tab \cr
#'  \code{PI_eOR} \tab the 95% PI expressed in eOR (calculated only if the number of studies in the \cr
#'  \tab meta-analysis is equal or larger to 3).\cr
#'  \tab \cr
#'  \code{PI_sign} \tab whether the 95% PI includes the null value ("notnull" vs. "null").\cr
#'  \tab \cr
#'  \code{egger_p} \tab the p-value of the Egger's test for publication bias (calculated only\cr
#'  \tab if the number of studies in the meta-analysis is equal or larger to 3).\cr
#'  \tab \cr
#'  \code{egger_sign} \tab whether the p-value of the Egger's test is < .05 ("sig." vs. "ns").\cr
#'  \tab \cr
#'  \code{ESB_p} \tab the p-value of the test for excess of significance bias.\cr
#'  \tab \cr
#'  \code{ESB_sign} \tab whether the p-value of the excess of significance test is < .05 ("sig." vs. "ns").\cr
#'  \tab \cr
#'  \code{power_med} \tab the power to detect a SMD of 0.5 at an alpha of .05 based on the number of\cr
#'  \tab cases and controls included in the meta-analysis (when IRR is used as effect size\cr
#'  \tab measure, the number of cases and controls in this calculation is assumed to be equal to\cr
#'  \tab half the total number of cases included in the meta-analysis).\cr
#'  \tab \cr
#'  \code{power} \tab present only in the 'Personalized' classification. \cr
#'  \tab - If the user did not use the 'power' criteria to stratify the evidence, this column \cr
#'  \tab contains the power to detect a small effect size (SMD = 0.2), a moderate effect\cr
#'  \tab size (SMD = 0.5) and a large effect size (SMD = 0.8) at an alpha of .05 based on \cr
#'  \tab the number of cases and controls included in the meta-analysis.\cr
#'  \tab - If the user used the 'power' criteria to stratify the evidence, this column contains\cr
#'  \tab the power to detect the values entered by the user at an alpha of .05 based on the \cr
#'  \tab number of cases and controls included in the meta-analysis.\cr
#'  \tab \cr
#'  \code{JK_p} \tab the largest p-value obtained in the jackknife leave-one-out meta-analysis (calculated\cr
#'  \tab only if the number of studies in the meta-analysis is equal or larger to 2)\cr
#'  \tab \cr
#'  \code{JK_sign} \tab whether the largest p-value in the jackknife meta-analysis is < .05 ("sig." vs. "ns")\cr
#'  \tab \cr
#'  \code{largest_CI_eG} \tab the 95% CI of the largest study expressed in eG\cr
#'  \tab \cr
#'  \code{largest_CI_eOR} \tab the 95% CI of the largest study expressed in eOR\cr
#'  \tab \cr
#'  \code{largest_sign} \tab whether the 95% CI of the largest study includes the null value ("notnull" vs. \cr
#'  \tab "null")\cr
#'  \tab \cr
#'  \code{rob} \tab the percentage of participants included in studies at low risk of bias (calculated\cr
#'  \tab only if this information is indicated in the dataset)\cr
#'  \tab \cr
#'  \code{amstar} \tab the AMSTAR score of the meta-analysis (calculated only if this information is\cr
#'  \tab indicated in the dataset)\cr
#'  \tab \cr
#'}
#'
#' @seealso
#' \code{\link{metaumbrella-package}()} for the formatting of well-formatted datasets\cr
#' \code{\link{umbrella}()} for conducting calculations needed for an umbrella review\cr
#' \code{\link{add.evidence}()} for stratifying evidence in an umbrella review\cr
#'
#' @exportS3Method
#'
#' @export summary.umbrella
#'
#' @md
#'
#' @examples
#' ### generate a summary of the results of an umbrella object
#' summary(umbrella(df.SMD))
summary.umbrella = function(object, digits = 3, het_max = FALSE, ...) {

  y = NULL

  criteria = attr(object, "criteria")

  for (name in names(object)) {

    x_i = object[[name]]

    measure = x_i$measure

    if (measure %in% c("SMD", "SMC")) {
      value = round(x_i$ma_results$value, digits)
      ci_lo = round(x_i$ma_results$ci_lo, digits); ci_up = round(x_i$ma_results$ci_up, digits)
      pi_lo = round(x_i$ma_results$pi_lo, digits); pi_up = round(x_i$ma_results$pi_up, digits)
      largest_ci_lo = round(x_i$largest$ci_lo, digits)
      largest_ci_up = round(x_i$largest$ci_up, digits)
    } else if (measure %in% c("OR", "RR", "HR", "IRR")) {
      value = round(exp(x_i$ma_results$value), digits)
      ci_lo = round(exp(x_i$ma_results$ci_lo), digits);  ci_up = round(exp(x_i$ma_results$ci_up), digits)
      pi_lo = round(exp(x_i$ma_results$pi_lo), digits); pi_up = round(exp(x_i$ma_results$pi_up), digits)
      largest_ci_lo = round(exp(x_i$largest$ci_lo), digits)
      largest_ci_up = round(exp(x_i$largest$ci_up), digits)
    } else if (measure == "Z") {
      value = round(.z_to_r(x_i$ma_results$value), digits)
      ci_lo = round(.z_to_r(x_i$ma_results$ci_lo), digits);  ci_up = round(.z_to_r(x_i$ma_results$ci_up), digits)
      pi_lo = round(.z_to_r(x_i$ma_results$pi_lo), digits); pi_up = round(.z_to_r(x_i$ma_results$pi_up), digits)
      largest_ci_lo = round(.z_to_r(x_i$largest$ci_lo), digits)
      largest_ci_up = round(.z_to_r(x_i$largest$ci_up), digits)
    }

    value_CI =  paste0("[", ci_lo, ", ", ci_up, "]")

    eOR = switch(as.character(measure),
                 "SMD" =, "SMC" = round(.d_to_or(x_i$ma_results$value), digits),
                 "Z" = round(.d_to_or(.z_to_d(x_i$ma_results$value)), digits),
                 "OR" = , "RR" = , "IRR" = , "HR" = value)

    eOR_CI = switch(as.character(measure),
                 "SMD" =, "SMC" = paste0("[", round(.d_to_or(x_i$ma_results$ci_lo), digits), ", ", round(.d_to_or(x_i$ma_results$ci_up), digits), "]"),
                 "Z" = paste0("[", round(.d_to_or(.z_to_d(x_i$ma_results$ci_lo)), digits), ", ", round(.d_to_or(.z_to_d(x_i$ma_results$ci_up)), digits), "]"),
                 "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", ci_lo, ", ", ci_up, "]"))

    eG = switch(as.character(measure),
                 "SMD" =, "SMC" = value,
                 "Z" = round(.z_to_d(x_i$ma_results$value), digits),
                 "OR" = , "RR" = , "IRR" = , "HR" = round(.or_to_d(exp(x_i$ma_results$value)), digits))

    eG_CI = switch(as.character(measure),
                   "SMD" =, "SMC" = paste0("[", ci_lo, ", ", ci_up, "]"),
                   "Z" = paste0("[", round(.z_to_d(ci_lo), digits), ", ", round(.z_to_d(x_i$ma_results$ci_up), digits), "]"),
                   "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", round(.or_to_d(exp(x_i$ma_results$ci_lo)), digits), ", ", round(.or_to_d(exp(x_i$ma_results$ci_up)), digits), "]"))

    p_value = ifelse(!is.na(as.numeric(as.character(x_i$ma_results$p.value))),
                     sprintf(paste0("%.", digits - 1, "e"), .as_numeric(x_i$ma_results$p.value)),
                     as.character(x_i$ma_results$p.value))
    # sample sizes
    n_studies = x_i$n$studies
    n_cases = x_i$n$cases
    n_controls = x_i$n$controls
    total_n = x_i$n$total_n

    # I2
    if (het_max) {
      I2 = data.frame(
        tau2 = ifelse(nrow(x_i$x) == 1, "only 1 study", round(.as_numeric(x_i$heterogeneity$tau2), digits)),
        I2 = ifelse(nrow(x_i$x) == 1, "only 1 study", round(.as_numeric(x_i$heterogeneity$i2), digits)),
        Q = ifelse(nrow(x_i$x) == 1, "only 1 study", round(.as_numeric(x_i$heterogeneity$qe), digits)),
        Q_p_value = ifelse(nrow(x_i$x) == 1, "only 1 study", sprintf(paste0("%.", digits - 1, "e"), .as_numeric(x_i$heterogeneity$p.value))))
    } else {
      I2 = ifelse(nrow(x_i$x) == 1, "only 1 study", round(.as_numeric(x_i$heterogeneity$i2), digits))
    }

    # PI
    if (x_i$n$studies < 3) {
      PI_eOR = "< 3 studies"
      PI_eG = "< 3 studies"

    } else {
      PI_eG = switch(as.character(measure),
                      "SMD" =, "SMC" = paste0("[", pi_lo, ", ", pi_up, "]"),
                      "Z" = paste0("[", round(.z_to_d(x_i$ma_results$pi_lo), digits), ", ", round(.z_to_d(x_i$ma_results$pi_up), digits), "]"),
                      "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", round(.or_to_d(exp(x_i$ma_results$pi_lo)), digits), ", ", round(.or_to_d(exp(x_i$ma_results$pi_up)), digits), "]"))

      PI_eOR = switch(as.character(measure),
                     "SMD" =, "SMC" =  paste0("[", round(.d_to_or(x_i$ma_results$pi_lo), digits), ", ",
                                                   round(.d_to_or(x_i$ma_results$pi_up), digits), "]"),
                     "Z" = paste0("[", round(.d_to_or(.z_to_d(x_i$ma_results$pi_lo)), digits), ", ",
                                       round(.d_to_or(.z_to_d(x_i$ma_results$pi_up)), digits), "]"),
                     "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", pi_lo, ", ", pi_up, "]"))

    }

    PI_sign = ifelse(is.numeric(pi_lo),
                     ifelse(sign(x_i$ma_results$pi_lo) == sign(x_i$ma_results$pi_up), "notnull", "null"),
                     "NA")
    # Egger
    egger_p = ifelse(x_i$n$studies < 3,
                     "< 3 studies",
                     ifelse(!is.na(x_i$egger$p.value),
                       sprintf(paste0("%.", digits - 1, "e"), .as_numeric(x_i$egger$p.value)),
                       as.character(x_i$egger$p.value))
    )
    egger_sign = factor(x_i$egger$p.value < 0.05, levels = c(FALSE, TRUE), labels = c("ns", "sig."))

    # ESB
    ESB_p = ifelse(!is.na(x_i$esb$p.value),
                   sprintf(paste0("%.", digits - 1, "e"), .as_numeric(x_i$esb$p.value)),
                   as.character(x_i$esb$p.value))

    ESB_sign = factor(x_i$esb$p.value < 0.05, levels = c(FALSE, TRUE), labels = c("ns", "sig."))

    # Largest
    largest_sign = factor(sign(x_i$largest$ci_lo) == sign(x_i$largest$ci_up), levels = c(FALSE, TRUE), labels = c("null", "notnull"))

    largest_CI_eOR = switch(as.character(measure),
                            "SMD" =, "SMC" =  paste0("[", round(.d_to_or(x_i$largest$ci_lo), digits), ", ",
                                                          round(.d_to_or(x_i$largest$ci_up), digits), "]"),
                            "Z" = paste0("[", round(.d_to_or(.z_to_d(x_i$largest$ci_lo)), digits), ", ",
                                              round(.d_to_or(.z_to_d(x_i$largest$ci_up)), digits), "]"),
                            "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", largest_ci_lo, ", ", largest_ci_up, "]"))

    largest_CI_eG = switch(as.character(measure),
                   "SMD" =, "SMC" = paste0("[", largest_ci_lo, ", ", largest_ci_up, "]"),
                   "Z" = paste0("[", round(.z_to_d(x_i$largest$ci_lo), digits), ", ",
                                     round(.z_to_d(x_i$largest$ci_up), digits), "]"),
                   "OR" = , "RR" = , "IRR" = , "HR" = paste0("[", round(.or_to_d(exp(x_i$largest$ci_lo)), digits), ", ",
                                                                  round(.or_to_d(exp(x_i$largest$ci_up)), digits), "]"))

    # JK
    JK_p = ifelse(x_i$n$studies == 1, "only 1 study",
                  sprintf(paste0("%.", digits - 1, "e"), .as_numeric(max(x_i$jk)))
                  )
    JK_sign = factor(JK_p < .05, levels = c(FALSE, TRUE), labels = c("n.s.", "sig."))

    # RoB
    rob = round(x_i$riskofbias, 2)

    # AMSTAR
    amstar = x_i$amstar

    # Power
    power = if (measure == "IRR") {
      paste0(
        "Low es = ", round(.power_d(x_i$n$cases/2, x_i$n$cases/2, 0.2), digits)*100, "; Med. es = ",
        round(.power_d(x_i$n$cases/2, x_i$n$cases/2, 0.5), digits)*100, "; Large es = ",
        round(.power_d(x_i$n$cases/2, x_i$n$cases/2, 0.8), digits)*100)
    } else if (measure == "Z") {
      paste0(
        "Low es = ", round(.power_d(x_i$n$total_n/2, x_i$n$total_n/2, 0.2), digits)*100, "; Med. es = ",
        round(.power_d(x_i$n$total_n/2, x_i$n$total_n/2, 0.5), digits)*100, "; Large es = ",
        round(.power_d(x_i$n$total_n/2, x_i$n$total_n/2, 0.8), digits)*100)
    } else {
      paste0(
        "Low es = ", round(.power_d(x_i$n$cases, x_i$n$controls, 0.2), digits)*100, "; Med. es = ",
        round(.power_d(x_i$n$cases, x_i$n$controls, 0.5), digits)*100, "; Large es = ",
        round(.power_d(x_i$n$cases, x_i$n$controls, 0.8), digits)*100)
    }

    power_med = switch(as.character(measure),
                       "IRR" = round(.power_d(x_i$n$cases/2, x_i$n$cases/2, 0.5), digits)*100,
                       "Z" = round(.power_d(x_i$n$total_n/2, x_i$n$total_n/2, 0.5), digits)*100,
                       "SMD"=, "SMC" =, "OR"=, "RR"=, "HR" = round(.power_d(x_i$n$cases, x_i$n$controls, 0.5), digits)*100)

    measure = ifelse(measure %in% c("SMD"), "G", ifelse(measure == "Z", "R", measure))


######################################################################################
    if (is.null(criteria)) {

      out_i = data.frame(
        Factor = name, n_studies, total_n, n_cases, n_controls,
        measure,
        value, value_CI,
        eG, eG_CI,
        eOR, eOR_CI,
        p_value,
        I2,
        PI_eG, PI_eOR,
        egger_p, ESB_p,
        power_med, JK_p,
        largest_CI_eG, largest_CI_eOR,
        rob, amstar
      )
      y = rbind(y, out_i)
    } else {
######################################################################################
      switch (criteria,
              "Ioannidis" = {
                out_i = data.frame(
                  Factor = name,
                  Criteria = "Ioannidis",
                  Class = x_i$evidence,
                  measure = measure,
                  value,
                  value_CI,
                  eG,
                  eG_CI,
                  eOR,
                  eOR_CI,
                  p_value,
                  n_cases,
                  PI_eG, PI_eOR,
                  PI_sign,
                  I2,
                  egger_p,
                  egger_sign,
                  ESB_p,
                  ESB_sign,
                  largest_CI_eG, largest_CI_eOR,
                  largest_sign
                )
                y = rbind(y, out_i)
              },
              ######################################################################################
              "Personalised" = {
                out_i = data.frame(Factor = name,
                                   Criteria = "Personalized",
                                   Class = x_i$evidence,
                                   measure = measure,
                                   value,
                                   value_CI,
                                   eG,
                                   eG_CI,
                                   eOR,
                                   eOR_CI,
                                   p_value,
                                   n_studies,
                                   total_n,
                                   n_cases,
                                   PI_eG, PI_eOR,
                                   PI_sign,
                                   I2,
                                   egger_p,
                                   ESB_p,
                                   largest_CI_eG, largest_CI_eOR,
                                   largest_sign,
                                   JK_p,
                                   JK_sign,
                                   rob,
                                   amstar,
                                   power_med,
                                   power = if (all(is.na(x_i$imprecision))) {
                                     power
                                     } else {
                                         paste0(
                                           "es1 = ", round(x_i$imprecision[1]*100), "; es2 = ",
                                           round(x_i$imprecision[2]*100), "; es3 = ",
                                           round(x_i$imprecision[3]*100), "; es4 = ",
                                           round(x_i$imprecision[4]*100))
                                       }
                                   )
                y = rbind(y, out_i)
              },
              ######################################################################################
              "GRADE" = {
                out_i = data.frame(
                  Factor = name,
                  Criteria = "GRADE",
                  Class = x_i$evidence,
                  measure = measure,
                  value,
                  value_CI,
                  eG,
                  eG_CI,
                  eOR,
                  eOR_CI,
                  p_value,
                  n_studies,
                  total_n,
                  I2,
                  egger_p,
                  egger_sign,
                  rob,
                  power
                )
                y = rbind(y, out_i)
              }
      )
    }
  }
  if (is.null(criteria)) {
  } else if (criteria == "Ioannidis") {
    y$Class = factor(y$Class, levels = c('I','II','III','IV','ns'))
    y = y[order(log(y$eOR), decreasing = TRUE),]
    y = y[order(y$Class), ]
  } else if (criteria == "Personalised") {
    y$Class = factor(y$Class, levels = c("I", "II", "III", "IV", "V"))
    y = y[order(y$eOR, decreasing = TRUE),]
    y = y[order(y$Class), ]
  } else if (criteria == "GRADE") {
    y$Class = factor(y$Class, levels = c('High','Moderate','Weak','Very weak'))
    y = y[order(y$eOR, decreasing = TRUE),]
    y = y[order(y$Class), ]
  }
  return(y)
}


#' Print a summary of an object of class \dQuote{umbrella}
#'
#' @param x an object of class \dQuote{umbrella}
#' @param ... other arguments that can be passed to the function
#'
#' @details
#' Summary method for objects of class \dQuote{umbrella}.
#'
#' @return
#' Implicitly calls the \code{\link{summary.umbrella}()} function and displays error or warning messages below the object returned.
#' This is useful when many factors are included in the review and that the results of the \code{\link{summary.umbrella}()} are not stored in an object.
#'
#' @export
#'
#' @md
#'
#' @seealso
#' \code{\link{summary.umbrella}()}
#'
#' @examples
#' \donttest{
#' ### print the results of an object of class umbrella
#' umbrella(df.OR.multi, mult.level = TRUE)
#' }
print.umbrella = function (x, ...) {
  cat("\nUmbrella review:\n")

  if (!is.null(attr(x, "message"))) {
    y <- summary.umbrella(x, ...)
    print(y)
    message(attr(x, "message"))
  } else {
    y = summary.umbrella(x, ...)
    invisible(y)
    return(print(y))
  }
}


