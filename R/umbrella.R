#' Conduct the calculations for an umbrella review
#'
#' This function performs various calculations needed for an umbrella review.
#'
#' @param x a well-formatted dataset.
#' @param method.var the estimator used to quantify the between-study variance in the random-effects meta-analysis. Default is the Restricted Likelihood Maximum ("REML") estimator. Alternatively, Der-Simonian and Laird \code{"DL"}, Hartung-Knapp-Sidik-Jonkman \code{"hksj"}, maximum-likelihood \code{"ML"} or Paule-Mandel \code{"PM"} estimators can be used.
#' @param mult.level a logical variable indicating the presence of multiple effect sizes per study in at least one factor of the umbrella review. Default is \code{FALSE} (i.e., each study of all factors include only one effect size). If \code{mult.level = TRUE} is specified, the Borenstein's methods are used to generate only one effect size per study. See \code{\link{metaumbrella-package}} for more information.
#' @param r a correlation coefficient indicating the strength of the association between multiple outcomes (or time-points) within the same study. The \code{r} value is applied to all studies with multiple outcomes that have no indication of correlation in the well-formatted dataset. Default is 0.5.
#' @param true_effect the method to estimate the true effect in the test for excess of significance. It must be \code{"largest"}, \code{"pooled"} or a numeric value (see details). Default is "largest".
#' @param seed an integer value used as an argument by the set.seed() function. Only used for the Ioannidis' test for excess of significance with ratios (i.e., \dQuote{OR}, \dQuote{RR},\dQuote{HR},\dQuote{IRR} or their logarithm) as effect size measures.
#' @param verbose a logical variable indicating whether text outputs and messages should be generated. We recommend turning this option to FALSE only after having carefully read all the generated messages.
#'
#' @details
#' This function automatically performs calculations allowing to stratify evidence according to various criteria.
#' More precisely, this function :
#'  * performs fixed- and random-effects meta-analyses with or without a Hartung-Knapp-Sidik-Jonkman correction.
#'  * provides an estimation of the between-study variance and heterogeneity using three indicators (\eqn{tau^2}, Q-statistic and \eqn{I^2} statistic).
#'  * estimates the 95% prediction interval (if the number of studies is equal or larger to 3).
#'  * provides an identification of the statistical significance of the largest study included in the meta-analysis.
#'  * provides an assessment of publication bias using the Egger's test (if the number of studies is equal or larger to 3).
#'  * provides an assessment of excess significance bias using the Ioannidis' test.
#'  * performs a jackknife leave-one-out meta-analysis(if the number of studies is equal or larger to 2).
#'  * calculates the proportion of participants included in studies at low risk of bias (if study quality is indicated in the dataset).
#'
#' A specificity of this function is that it does not include arguments to specify the name of the columns of the dataset used as input.
#' Instead, the function requires users to build a dataset that meets fixed rules.
#' Details on how building this \code{well-formatted dataset} are given in the \code{\link{metaumbrella-package}} section of this manual and a vignette is specifically dedicated to this topic.
#' Moreover, examples of \code{well-formatted datasets} are available as data distributed along with the package (see \link{df.OR}, \link{df.OR.multi}, \link{df.SMD}, \link{df.RR}, \link{df.HR}, \link{df.IRR}).
#'
#' When estimating the test for excess of significance, the \code{\link{umbrella}()} function should assume a value for the true effeect. The \code{true_effect} argument can be used to select the method that will be applied to estimate the true effect.
#' * If \code{"largest"} is selected, the true effect size is assumed to be equal to the effect size of the largest study included in the meta-analysis.
#' * If \code{"pooled"} is selected, the true effect size is assumed to be equal to the pooled effect of the meta-analysis.
#' * If a \code{numeric} value is entered, the true effect size is assumed to be equal to the value entered by the user.
#'
#' @return
#' The \code{umbrella()} function returns an object of class \dQuote{umbrella}, which is a list containing information required for stratifying the evidence.
#' This list contains, for each factor included in the umbrella review:
#' \tabular{ll}{
#'  \code{measure} \tab the measure of the effect used to perform the calculations: SMD, OR,\cr
#'  \tab RR, HR, or IRR.\cr
#'  \tab \cr
#'  \code{x} \tab the data used to conduct the meta-analysis. Note that these data may be\cr
#'  \tab slightly different from the raw data introduced.\cr
#'  \tab \cr
#'  \code{x_multi} \tab the original data when there is a multivariate structure.\cr
#'  \tab Note that these data may be slightly different from the raw data introduced.\cr
#'  \tab \cr
#'  \code{x_shared} \tab dataframe allowing to compare adjustments made when a shared_nexp\cr
#'  \tab or shared_controls correction is requested\cr
#'  \tab (see \code{\link{metaumbrella-package}} for more information).\cr
#'  \tab \cr
#'  \code{n} \tab the overall number of studies, cases and controls.\cr
#'  \tab \cr
#'  \code{method.var} \tab the estimator used for fitting the random effects meta-analyses\cr
#'  \tab \cr
#'  \code{random} \tab pooled effect size, p-value and 95% confidence interval and prediction\cr
#'  \tab  interval of the random-effects meta-analysis.\cr
#'  \tab \cr
#'  \code{fixed} \tab pooled effect size, p-value and 95% confidence interval and prediction\cr
#'  \tab  interval of the fixed-effect meta-analysis.\cr
#'  \tab \cr
#'  \code{largest} \tab 95% confidence interval of the largest study.\cr
#'  \tab \cr
#'  \code{heterogeneity} \tab \eqn{tau^2}, \eqn{I^2} and Q test.\cr
#'  \tab \cr
#'  \code{egger} \tab estimate and p-value of the Egger's test for publication bias.\cr
#'  \tab \cr
#'  \code{esb} \tab results of the Ioannidis' test for excess of significance bias. See\cr
#'  \tab \code{\link{esb.test}()} for more information.\cr
#'  \tab \cr
#'  \code{riskofbias} \tab percentage of participants in studies at low risk of bias.\cr
#'  \tab \cr
#'  \code{amstar} \tab AMSTAR score obtained by the meta-analysis.\cr
#'  \tab \cr
#'  \code{evidence} \tab evidence class according to some criteria.\cr
#'  \tab \cr
#'}
#' The functions \code{print} and \code{summary} may be used to print the details or a summary of the results.
#'
#' @references
#' Fusar-Poli, P., Radua, J. (2018).  Ten simple rules for conducting umbrella reviews.
#' \emph{Evidence-Based Mental Health}, \bold{21}, 95--100.\cr
#' Radua, J., Ramella-Cravaro, V., Ioannidis, J.P.A., Reichenberg, A., Phiphopthatsanee, N., Amir, T., Yenn Thoo, H., Oliver, D., Davies, C., Morgan, C., McGuire, P., Murray, R.M., Fusar-Poli, P. (2018)
#' What causes psychosis? An umbrella review of risk and protective factors.
#' \emph{World Psychiatry}, \bold{17}, 49--66.
#'
#' @seealso
#' \code{\link{metaumbrella-package}} for the formatting of well-formatted datasets.
#' \code{\link{add.evidence}()} for adding evidence classes to an umbrella review.
#' \code{\link{forest}()} for drawing a forest plot of the factors included in an umbrella review.
#' \code{\link{subset.umbrella}()} for retrieving a subset of the factors included in an umbrella review.
#' \code{\link{union.umbrella}()} for combining the factors included in two umbrella reviews.
#'
#' @export umbrella
#'
#' @md
#'
#' @examples
#' \donttest{
#' ### Perform an umbrella review with random-effects meta-analyses
#' ### with a Hartung-Knapp-Sidik-Jonkman estimator
#' umb <- umbrella(df.IRR, method.var = "hksj")
#'
#' ### obtain the results of the calculations in a dataframe
#' summary(umb)
#'
#' ### manually inspect the results of the umbrella review calculations for the 'Smoking' factor
#' ### included in the dataset.
#' umb$Smoking
#'
#' ### Perform a meta-analysis with multilevel data, assuming a correlation of 0.8
#' ### between all outcomes of the same study
#' umb.multi <- umbrella(df.OR.multi, mult.level = TRUE, r = 0.8)
#'
#' ### the stratification of evidence can then normally be applied on this umbrella object
#' add.evidence(umb.multi, criteria = "Ioannidis")
#' }
umbrella = function (x, method.var = "REML", mult.level = FALSE, r = 0.5, true_effect = "largest", seed = NA, verbose = TRUE) {

  # initial checkings ------
  checkings <- .check_data(x)

  x <- attr(checkings, "data")

  if (attr(checkings, "status") == "ERRORS") {
    stop("Data did not pass the checkings. Resolve formatting errors using the 'view.errors.umbrella()' function.")
  }

  # if the dataset is well formatted, initialize some settings ------
  list_author_concern = list_factor_concern = NULL
  eG_threshold = 5 # a warning occurs for all studies with a eG value > 5
  eOR_threshold = 15 # a warning occurs for all studies with a eOR value > 15

  y = list()

  # run factor-by-factor calculations

  for (factor in unique(x$factor)) {

    if (verbose) cat(paste("Analyzing factor:", factor, '\n'))

    x_i = x[which(x$factor == factor), ]


      x_i_ok <- .format_dataset(x_i, method.var = method.var, mult.level = mult.level, r = r, verbose = verbose)

      measure <- attr(x_i_ok, "measure")
      measure_length <- rep(measure, nrow(x_i_ok))
      REPEATED_STUDIES <- attr(x_i_ok, "REPEATED_STUDIES")
      n_studies <- attr(x_i_ok, "n_studies")

      # we check whether some studies have very large effect sizes
      if (any((measure_length == "SMD" & abs(x_i_ok$value) > eG_threshold) | (measure_length != "SMD" & ( (x_i_ok$value) > eOR_threshold | (x_i_ok$value) < 1/eOR_threshold )))) {
        row_concern = which( (measure_length == "SMD" & abs(x_i_ok$value) > eG_threshold) | (measure_length != "SMD" & ( (x_i_ok$value) > eOR_threshold | (x_i_ok$value) < 1/eOR_threshold )) )
        author_concern = paste(paste0(x_i_ok$author[row_concern], " ", x_i_ok$year[row_concern], " (", measure_length[row_concern], " = ", round(x_i_ok$value[row_concern], 2), ")"), collapse = ", ")
        factor_concern = factor
        list_author_concern = append(list_author_concern, author_concern)
        list_factor_concern = append(list_factor_concern, factor)
      }


      # create an object storing information on sample sizes
      n <- data.frame(studies = n_studies,
                      cases = sum(x_i_ok$n_cases),
                      controls = sum(x_i_ok$n_controls, na.rm = TRUE),
                      cases_and_controls = sum(sum(x_i_ok$n_cases), sum(x_i_ok$n_controls), na.rm = TRUE))
      rownames(n) = "n"

      # select the function performing the meta-analysis
      .meta <- switch(as.character(attr(x_i_ok, "meta")),
                      "SMD_standard_raw_information" = .meta_d,
                      "OR_standard_raw_information" = .meta_or,
                      "RR_standard_raw_information" = .meta_rr,
                      "IRR_standard_raw_information" = .meta_irr,
                      "SMD_multilevel_raw_information" =,
                      "SMD_multilevel_generic" =,
                      "SMD_standard_generic" = .meta_gen_smd,
                      "OR_multilevel_raw_information" =,
                      "OR_multilevel_generic" =,
                      "OR_standard_generic" = ,
                      "RR_multilevel_raw_information" =,
                      "RR_multilevel_generic" =,
                      "RR_standard_generic" = ,
                      "IRR_multilevel_raw_information" =,
                      "IRR_multilevel_generic" =,
                      "IRR_standard_generic" = ,
                      "HR_multilevel_generic" =,
                      "HR_standard_generic" = .meta_gen_log)

      # perform the meta-analysis
      if (nrow(x_i_ok) > 1) {
        m = .meta(x_i_ok, method.var)
      } else if (nrow(x_i_ok) == 1) {
        m = NA
      } else {
        stop("An unexpected error occured during a meta-analysis. Please, contact us for more information.")
      }

      # extraction of meta-analytic results
      # if the dataset contains only one study, the results of this study are used
      if (nrow(x_i_ok) == 1) {
        k = 1
        coef = ifelse(measure == "SMD", x_i_ok$value, log(x_i_ok$value))
        se = x_i_ok$se
        z = ifelse(measure == "SMD", x_i_ok$value / x_i_ok$se, log(x_i_ok$value) / x_i_ok$se)
        p.value = ifelse(x_i_ok$value == 0, 1,
                         .two_tail(ifelse(measure == "SMD",
                                          pt(z, x$n_cases + x$n_controls - 2),
                                          pnorm(z))))
        ci_lo = ifelse(measure == "SMD", x_i_ok$ci_lo, log(x_i_ok$ci_lo))
        ci_up = ifelse(measure == "SMD", x_i_ok$ci_up, log(x_i_ok$ci_up))
        tau2 = NA
        i2 = NA
        qe = NA
        qe_p.value = NA
        } else if (measure == "SMD" & !REPEATED_STUDIES & any(is.na(x_i_ok$mean_cases))) { #
        # metansue object ----------------------------------------------------------------------
        # this is currently not used but this feature will be proposed in a future update
        k = length(m$known$i) + length(m$unknown$i)
        coef = m$hypothesis$coef
        se = m$hypothesis$se
        p.value = m$hypothesis$p.value
        ci_lo = m$hypothesis$ci[1]
        ci_up = m$hypothesis$ci[2]
        tau2 = m$heterogeneity$tau2
        i2 = m$heterogeneity$i2 * 100
        qe = m$heterogeneity$qe
        qe_p.value = m$heterogeneity$p.value
        # ---------------------------------------------------------------------------------------
      } else {
        # meta object
        k = m$k
        coef = m$TE.random
        se = m$seTE.random
        z = m$zval.random
        p.value = m$pval.random
        ci_lo = m$lower.random
        ci_up = m$upper.random
        tau2 = m$tau^2
        i2 = m$I2 * 100
        qe = m$Q
        qe_p.value = m$pval.Q
      }

      # calculate prediction interval
      # only for meta-analyses with at least 3 studies
      if (n_studies < 3) {
        pi_lo = NA
        pi_up = NA
      } else {
        half_pi = qt(0.975, n_studies - 2) * sqrt(tau2 + se^2)
        pi_lo = coef - half_pi
        pi_up = coef + half_pi
      }

      # Create summary datasets of the random-effects model
      random = data.frame(value = coef, z, p.value, ci_lo, ci_up, pi_lo, pi_up)
      rownames(random) =
        ifelse(as.character(measure) %in% c("HR", "IRR", "OR", "RR"),
               paste0("log(", measure, ")", collapse = ""),
               "bias-corrected SMD")

      # Create summary datasets of the fixed-effects model
      if (nrow(x_i_ok) > 1) {
        fixed = data.frame(value = m$TE.fixed, p.value = m$pval.fixed)
        rownames(fixed) = ifelse(as.character(measure) %in% c("HR", "IRR", "OR", "RR"),
                                paste0("log(", measure, ")", collapse = ""),
                                "bias-corrected SMD")
      } else {
        fixed = data.frame("only one study" = 1)
        }

      # Create summary datasets of the heterogeneity
      heterogeneity = data.frame(tau2, i2, qe, p.value = qe_p.value)

      # identification of the largest study
      if (measure == "IRR") {
        largest = log(.largest_irr(x_i_ok))
      } else if (measure %in% c("OR", "HR", "RR")) {
        largest = log(.largest_or_rr_hr(x_i_ok))
      } else if (measure == "SMD") {
        largest = .largest_smd(x_i_ok)
      }

      rownames(largest) =
        ifelse(as.character(measure) %in% c("HR", "IRR", "OR", "RR"),
               paste0("log(", measure, ")", collapse = ""),
               "SMD")

      # publication bias
      # only for meta-analyses with at least 3 studies
      if (n_studies < 3) {
        egger = data.frame(statistic = NA, p.value = NA)
      } else {
        if (measure == "SMD" & !REPEATED_STUDIES & any(is.na(x_i_ok$mean_cases))) { #

          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "SMD",
                         n_cases = x_i_ok$n_cases, n_controls = x_i_ok$n_controls)

          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        } else if (measure == "SMD") {

          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "SMD",
                         n_cases = x_i_ok$n_cases, n_controls = x_i_ok$n_controls)

          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        } else if (measure %in% c("OR", "HR", "RR", "IRR")) {

          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "ratio")
          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        }
      }

      # excess significance bias
      true_value = ifelse(true_effect == "largest", "largest",
                          ifelse(true_effect == "pooled", ifelse(measure == "SMD", .as_numeric(random$coef), exp(.as_numeric(random$coef))),
                                 ifelse(is.numeric(true_effect), true_effect, stop("The input in the 'true_effect' argument should be 'pooled', 'largest' or a numeric value. Please see the manual for more information.")
                          )))

      esb = esb.test(x_i_ok, measure = measure, input = "other", true_effect = true_value, seed = seed)

      # risk of bias
      riskofbias = weighted.mean(x_i_ok$rob.recoded, x_i_ok$sum_N) * 100

      # AMSTAR
      amstar = attr(x_i_ok, "amstar")

      # Jackknife
      jk = data.frame(value = c(), p.value = c())

      if (nrow(x_i_ok) > 1) {
        for (i in 1:nrow(x_i_ok)) {
            m_i = .meta(x_i_ok[(1:n_studies)[-i], ], method.var)
            if (measure == "SMD" & !REPEATED_STUDIES & any(is.na(x_i_ok$mean_cases))) {
              jk_i = data.frame(value = m_i$hypothesis$coef, p.value = m_i$hypothesis$p.value)
            } else {
              jk_i = data.frame(value = m_i$TE.random, p.value = m_i$pval.random)
            }
            rownames(jk_i) = paste(rownames(x_i_ok)[i], collapse = "/")
            jk = rbind(jk, jk_i)
        }
      } else {
        jk = data.frame(value = "Only one study", p.value = "Only one study")
      }

    # FINAL
    y[[factor]] = list(
      measure = measure,
      x = x_i_ok,
      x_multi = attr(x_i_ok, "data_mult"),
      x_shared = attr(x_i_ok, "comparison_adjustment"),
      n = n,
      method.var = method.var,
      random = random,
      fixed = fixed,
      largest = largest,
      heterogeneity = heterogeneity,
      egger = egger,
      esb = esb,
      riskofbias = riskofbias,
      amstar = amstar,
      jk = jk$p.value,
      evidence = NA
    )
  }

  attr(y, "status") = attr(x, "status")
  attr(y, "criteria") = NULL
  attr(y, "data") = x
  attr(y, "message") = NULL
  class(y) = "umbrella"

  if (verbose) {

    if (!is.null(list_author_concern) | !is.null(list_factor_concern)) {

      concern = aggregate(x = list_factor_concern,
                          by = list(list_author_concern),
                          FUN = function (x) { return(paste(x, collapse = ", ")) })

      if (attr(checkings, "status") == "WARNINGS") {
        attr(y, "message") = paste("Some effect sizes are very large. Please verify that no typo or extraction errors have been made for factors:",
                paste("\n- '", concern$x, "', studies: ", concern$Group.1, collapse = ""),
                "\nMoreover, some warnings occurred during the chekings. Please verify error messages shown using the 'view.errors.umbrella()' function.")
      } else {
        attr(y, "message") = paste("Some effect sizes are very large. Please verify that no typo or extraction errors have been made for factors:",
                paste("\n- '", concern$x, "', studies: ", concern$Group.1, collapse = ""))
      }

    } else if (attr(checkings, "status") == "WARNINGS") {
      attr(y, "message") = paste("\nSome warnings occurred during the chekings. Please verify error messages shown using the 'view.errors.umbrella()' function.")
    }
  }

  message(attr(y, "message"))
  return(y)
}
