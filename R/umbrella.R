#' Conduct the calculations for an umbrella review
#'
#' This function performs various calculations needed for an umbrella review.
#'
#' @param x a well-formatted dataset.
#' @param method.var the estimator used to quantify the between-study variance in the random-effects meta-analysis. Default is the Restricted Likelihood Maximum ("REML") estimator. Alternatively, DerSimonian and Laird \code{"DL"}, Hartung-Knapp-Sidik-Jonkman \code{"hksj"} (applies a Hartung-Knapp-Sidik-Jonkman adjustment on the results of a \code{"DL"} estimator), maximum-likelihood \code{"ML"} or Paule-Mandel \code{"PM"} estimators can be used. A fixed-effect meta-analysis can be obtained by indicated the \code{method.var = "FE"} argument.
#' @param mult.level a logical variable indicating the presence of multiple effect sizes per study in at least one factor of the umbrella review. Default is \code{FALSE} (i.e., each study of all factors include only one effect size). If \code{mult.level = TRUE} is specified, the Borenstein's methods are used to generate only one effect size per study. See \code{\link{metaumbrella-package}} for more information.
#' @param r a correlation coefficient indicating the strength of the association between multiple outcomes (or time-points) within the same study. The \code{r} value is applied to all studies with a \code{"outcomes"} value in the \code{reverse_es} column that have no indication of correlation in the well-formatted dataset. Default is 0.5.
#' @param method.esb the method used to conduct the excess of statistical significance test. It must be \code{"IT.binom"}, \code{"IT.chisq"}, \code{"PSST"}, \code{"TESS"} or \code{"TESSPSST"}  (see details). Default is \code{TESSPSST}.
#' @param true_effect the method to estimate the true effect in the test for excess significance. It must be \code{"largest"}, \code{"UWLS"}, \code{"pooled"} or a numeric value (see details). Default is \code{"UWLS"}.
#' @param pre_post_cor The value of the correlation coefficient between baseline and follow-up scores in pre-post studies. If your umbrella review includes pre-post controlled studies, you should indicate the mean pre-post correlation across groups. Only needed when using the SMC measure.
#' @param seed an integer value used as an argument by the set.seed() function. Only used for the \code{"IT.binom"} and \code{"IT.chisq"} tests for excess significance with ratios (i.e., \dQuote{OR}, \dQuote{RR}, \dQuote{IRR} or their logarithm) as effect size measures.
#' @param verbose a logical variable indicating whether text outputs and messages should be generated. We recommend turning this option to FALSE only after having carefully read all the generated messages.
#'
#' @details
#' This function automatically performs calculations allowing to stratify evidence according to various criteria.
#' For each factor included in a well-formatted dataset, this function automatically:
#'  * performs fixed- or random-effects meta-analyses.
#'  * provides an estimation of the between-study variance and heterogeneity using three indicators (\eqn{tau^2}, Q-statistic and \eqn{I^2} statistic).
#'  * estimates the 95% prediction interval (if the number of studies is equal or larger to 3).
#'  * provides an identification of the statistical significance of the largest study included in the meta-analysis.
#'  * provides an assessment of publication bias using the Egger's test (if the number of studies is equal or larger to 3).
#'  * provides an assessment of excess significance using various methods.
#'  * performs a jackknife leave-one-out meta-analysis (if the number of studies is equal or larger to 2).
#'  * calculates the proportion of participants included in studies at low risk of bias (if study quality is indicated in the dataset).
#'
#' A specificity of the \code{\link{umbrella}()} function is that it does not include arguments to specify the name of the columns of the dataset used as input.
#' Instead, the function requires users to prepare a dataset that meets fixed rules.
#' Details on how building this \code{well-formatted dataset} are given in the \code{\link{metaumbrella-package}} section of this manual. A vignette also provides a step-by-step tutorial.
#' Moreover, examples of \code{well-formatted datasets} are available as data distributed along with the package (see \link{df.OR}, \link{df.OR.multi}, \link{df.R}, \link{df.SMC},\link{df.SMD}, \link{df.RR}, \link{df.HR}, \link{df.IRR}).
#'
#' When estimating the test for excess significance, the \code{\link{umbrella}()} function must assume a best approximation of the true effect.
#' The \code{true_effect} argument can be used to select the method that will be applied to estimate the true effect.
#' * If \code{"largest"} is entered, the true effect size is assumed to be equal to the effect size of the largest study included in the meta-analysis.
#' * If \code{"pooled"} is entered, the true effect size is assumed to be equal to the meta-analytic pooled effect size.
#' * If \code{"UWLS"} is entered, the true effect size is assumed to be equal to unrestricted weighted least squares weighted average.
#' * If a \code{numeric} value is entered, the true effect size is assumed to be equal to the value entered by the user (note that the value of ratios must be in their natural scale).
#'
#' Last, this function performs a statistical test to determine whether the observed number of statistically significant studies is higher than expected given the mean statistical power. The \code{method.esb} argument can be used to select the test. Details on each method can be found in the \code{\link{esb.test}} section.
#'
#' @return
#' The \code{umbrella()} function returns an object of class \dQuote{umbrella}, which is a list containing information required for stratifying the evidence.
#' This list contains, for each factor included in the umbrella review:
#' \tabular{ll}{
#'  \code{factor} \tab the name of the factor analyzed.\cr
#'  \tab \cr
#'  \code{measure} \tab the measure of the effect used to perform the calculations.\cr
#'  \tab \cr
#'  \code{x} \tab the data used to conduct the meta-analysis. Note that these data may be\cr
#'  \tab slightly different from the raw data introduced.\cr
#'  \tab \cr
#'  \code{x_multi} \tab the original data when there is a multivariate structure.\cr
#'  \tab Note that these data may be slightly different from the raw data introduced.\cr
#'  \tab \cr
#'  \code{x_shared} \tab a dataframe allowing to visualize adjustments made when a shared_nexp\cr
#'  \tab or shared_controls correction is requested\cr
#'  \tab (see \code{\link{metaumbrella-package}} for more information).\cr
#'  \tab \cr
#'  \code{n} \tab the overall number of studies, cases and controls.\cr
#'  \tab \cr
#'  \code{method.var} \tab the estimator used for fitting the random effects meta-analyses\cr
#'  \tab \cr
#'  \code{ma_results} \tab pooled effect size, p-value and 95% confidence interval and prediction\cr
#'  \tab  interval of the meta-analysis.\cr
#'  \tab \cr
#'  \code{largest} \tab 95% confidence interval of the largest study.\cr
#'  \tab \cr
#'  \code{heterogeneity} \tab \eqn{tau^2}, \eqn{I^2} and results of the Q-test.\cr
#'  \tab \cr
#'  \code{egger} \tab estimate and p-value of the Egger's test for publication bias.\cr
#'  \tab \cr
#'  \code{esb} \tab results of the test for excess significance bias. See\cr
#'  \tab \code{\link{esb.test}()} for more information.\cr
#'  \tab \cr
#'  \code{riskofbias} \tab percentage of participants in studies at low risk of bias.\cr
#'  \tab \cr
#'  \code{amstar} \tab AMSTAR score obtained by the meta-analysis.\cr
#'  \tab \cr
#'  \code{evidence} \tab evidence class according to some criteria.
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
#' \code{\link{metaumbrella-package}} for the formatting of well-formatted datasets\cr
#' \code{\link{add.evidence}()} for stratifying the evidence in an umbrella review\cr
#' \code{\link{forest}()} for drawing a forest plot of the factors included in an umbrella review\cr
#' \code{\link{subset.umbrella}()} for retrieving a subset of the factors included in an umbrella review\cr
#' \code{\link{union.umbrella}()} for combining the factors included in two umbrella reviews
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
#' ### obtain a stratification of the evidence according to the Ioannidis classification
#' add.evidence(umb.multi, criteria = "Ioannidis")
#' }
umbrella = function (x, method.var = "REML", mult.level = FALSE, r = 0.5, method.esb = "TESSPSST", true_effect = "UWLS", pre_post_cor = NA, seed = NA, verbose = TRUE) {

  # initial checkings ------
  checkings <- .check_data(x)

  x <- attr(checkings, "data")

  if (attr(checkings, "status") == "ERRORS") {
    stop("Data did not pass checks. Resolve formatting errors using the 'view.errors.umbrella()' function.")
  }
  # JAMOVI
  # if (attr(checkings, "status") == "ERRORS") {
  #   stop("Data did not pass the checkings. You can find the list of error messages and problematic rows in the table below.")
  # }


  # if the dataset is well formatted, initialize some settings ------
  list_author_concern = list_factor_concern = NULL

  y = list()

  # run factor-by-factor calculations

  for (factor in unique(x$factor)) {

    if (verbose) cat(paste("Analyzing factor:", factor, '\n'))

    x_i = x[which(x$factor == factor), ]


      x_i_ok <- .format_dataset(x_i = x_i, method.var = method.var, mult.level = mult.level, r = r, verbose = verbose, pre_post_cor = pre_post_cor)

      measure <- attr(x_i_ok, "measure")
      measure_length <- rep(measure, nrow(x_i_ok))
      REPEATED_STUDIES <- attr(x_i_ok, "REPEATED_STUDIES")
      n_studies <- attr(x_i_ok, "n_studies")

      # create an object storing information on sample sizes
      n <- data.frame(studies = n_studies,
                      cases = ifelse(measure == "Z", NA_real_, sum(x_i_ok$n_cases, na.rm = TRUE)),
                      controls = ifelse(measure == "Z", NA_real_, sum(x_i_ok$n_controls, na.rm = TRUE)),
                      total_n = ifelse(measure == "Z",
                                       sum(x_i_ok$n_sample),
                                       sum(sum(x_i_ok$n_cases), sum(x_i_ok$n_controls), na.rm = TRUE)))

      rownames(n) = "n"

      .meta <- switch(as.character(measure),
                      "SMD" =,
                      "Z" =,
                      "SMC" = .meta_gen,
                      "OR" = ,
                      "RR" = ,
                      "IRR" = ,
                      "HR" = .meta_gen_log)

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
        coef = ifelse(measure %in% c("SMD", "SMC", "Z"), x_i_ok$value, log(x_i_ok$value))
        se = x_i_ok$se
        z = coef / se
        p.value = ifelse(coef == 0, 1,
                         .two_tail(ifelse(measure %in% c("SMD", "SMC"),
                                          pt(z, x$n_cases + x$n_controls - 2),
                                          pnorm(z))))
        ci_lo = ifelse(measure %in% c("SMD", "SMC", "Z"), x_i_ok$ci_lo, log(x_i_ok$ci_lo))
        ci_up = ifelse(measure %in% c("SMD", "SMC", "Z"), x_i_ok$ci_up, log(x_i_ok$ci_up))
        tau2 = NA
        i2 = NA
        qe = NA
        qe_p.value = NA
        } else if (method.var == "FE") { #
          k = m$k
          coef = m$TE.fixed
          se = m$seTE.fixed
          z = m$zval.fixed
          p.value = m$pval.fixed
          ci_lo = m$lower.fixed
          ci_up = m$upper.fixed
          tau2 = 0
          i2 = m$I2 * 100
          qe = m$Q
          qe_p.value = m$pval.Q
        } else {
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
      ma_results = data.frame(value = coef, z, p.value, ci_lo, ci_up, pi_lo, pi_up)
      rownames(ma_results) = switch(as.character(measure),
                                "SMD" = "Bias-corrected SMD",
                                "Z" = "Fisher's Z",
                                "SMC" = "SMC",
                                "OR" = "log (OR)",
                                "RR" = "log (RR)",
                                "IRR" = "log (IRR)",
                                "HR" = "log (HR)")

      # Create summary datasets of the heterogeneity
      heterogeneity = data.frame(tau2, i2, qe, p.value = qe_p.value)

      # identification of the largest study
      if (measure == "IRR") {
        largest = log(.largest_irr(x_i_ok))
      } else if (measure %in% c("OR", "HR", "RR")) {
        largest = log(.largest_or_rr_hr(x_i_ok))
      } else if (measure %in% c("SMD", "SMC")) {
        largest = .largest_smd(x_i_ok)
      } else if (measure %in% c("Z")) {
        largest = .largest_z(x_i_ok)
      }

      rownames(largest) = switch(as.character(measure),
                                 "SMD" = "Bias-corrected SMD",
                                 "Z" = "Fisher's Z",
                                 "SMC" = "Standardized mean change",
                                 "OR" = "log (OR)",
                                 "RR" = "log (RR)",
                                 "IRR" = "log (IRR)",
                                 "HR" = "log (HR)")

      # publication bias
      # only for meta-analyses with at least 3 studies
      if (n_studies < 3) {
        egger = data.frame(statistic = NA, p.value = NA)
      } else {
        if (measure == "SMD" & !REPEATED_STUDIES & any(is.na(x_i_ok$mean_cases))) {
          # for future updates
          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "SMD")
          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        } else if (measure %in% c("SMD", "SMC", "Z")) {

          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "non_ratio")
          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        } else if (measure %in% c("OR", "HR", "RR", "IRR")) {

          mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "ratio")
          egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)

        }
      }

      # excess significance bias
      if (true_effect == "largest") {
        true_value = "largest"
      } else if (true_effect == "UWLS") {
        true_value = "UWLS"
      } else if (true_effect == "pooled") {
        true_value = ifelse(measure %in% c("SMD", "SMC", "Z"), .as_numeric(ma_results$coef), exp(.as_numeric(ma_results$coef)))
      } else if (is.numeric(true_effect)) {
        true_value = true_effect
      } else {
        stop("The input in the 'true_effect' argument should be 'pooled', 'UWLS', 'largest' or a numeric value. Please see the manual for more information.")
      }

      if (n_studies < 3) {
        esb = data.frame(p.value = NA)
      } else {
        esb = esb.test(x_i_ok, method.esb = method.esb, measure = measure, input = "other", true_effect = true_value, seed = seed, tau2 = tau2)
      }

      # risk of bias
      riskofbias = weighted.mean(x_i_ok$rob.recoded, x_i_ok$sum_N) * 100

      # AMSTAR
      amstar = attr(x_i_ok, "amstar")

      # Jackknife
      jk = data.frame(value = c(), p.value = c())

      if (nrow(x_i_ok) > 1) {
        for (i in 1:nrow(x_i_ok)) {
            m_i = .meta(x_i_ok[(1:n_studies)[-i], ], method.var)

            if (method.var == "FE") {
              jk_i = data.frame(value = m_i$TE.fixed, p.value = m_i$pval.fixed)
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
      factor = factor,
      measure = measure,
      x = x_i_ok,
      x_multi = attr(x_i_ok, "data_mult"),
      x_shared = attr(x_i_ok, "comparison_adjustment"),
      n = n,
      method.var = method.var,
      ma_results = ma_results,
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
    if (attr(checkings, "status") == "WARNINGS") {
      attr(y, "message") = paste("\nSome warnings occurred during the chekings. Please verify error messages shown using the 'view.errors.umbrella()' function.")
      message(attr(y, "message"))
    }
  }
  return(y)
}
