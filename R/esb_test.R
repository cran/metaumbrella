#' Perform the Ioannidis test for excess of significance
#'
#' The \code{esb.test()} function performs the 'Ioannidis' test to examine the presence of an excess of significance in a given set of studies.
#' This test aims to determine whether there is an excess in the observed number of studies with statistically significant results given the mean statistical power.
#' An exact binomial test or a Chi-squared test is used.
#'
#' @param x a well-formatted dataset or an object of class \dQuote{rma} or \dQuote{meta}. If a well-formatted dataset is used, only one factor should be included.
#' @param input the type of object used as input. It must be \code{"dataframe"}, \code{"rma"} or \code{"meta"}.
#' @param n_cases vector with the number of cases of each included studies. Only required when \code{x} is an object of class \dQuote{rma} or \dQuote{meta}.
#' This information  can be indicated via the \code{n_cases} argument of the \code{esb.test()} function or directly when calling the \code{rma()} or \code{meta()} functions (see examples below).
#' @param n_controls vector with the number of controls of each included studies. Only required when \code{x} is an object of class \dQuote{rma} or \dQuote{meta}
#' This information  can be indicated via the \code{n_controls} argument of the \code{esb.test()} function or directly when calling the \code{rma()} or \code{meta()} functions (see examples below).
#' @param measure the measure of the effect: "SMD", "MD", "G", "OR" or "logOR, "RR" or "logRR", "HR" or "logHR", "IRR" or "logIRR".
#' If a an object of class \dQuote{rma} or \dQuote{meta} is used, the effect size should be either "SMD" or "OR". However, note that for \dQuote{rma} objects, a SMD is systematically assumed to be a G (to respect the naming used in the \pkg{metafor} package). For \dQuote{meta} objects, a SMD is assumed to be a G unless it is explicitly stated that this is not the case (i.e., using the \code{method.smd = "Cohen"} argument).
#' The effect size measure used can be indicated via the measure argument of the \code{esb.test()} function or directly when calling the \code{rma()} or \code{meta()} functions (see examples below).
#' @param method the method used to conduct the test. It must be \code{binom.test} or \code{chisq.test} (see details). The \code{umbrella()} function uses a \code{binom.test} method.
#' @param true_effect the best approximation of the true effect. It must be \code{"largest"} or a numeric value (see details).
#' @param seed an integer value used as an argument by the set.seed() function. Only used for measures "OR", "logOR, "RR", "logRR", "IRR" or "logIRR".
#'
#' @details The function starts by calculating whether each individual study has significant results (p < .05). Then, it estimates the statistical power of each individual study to detect an effect size equal to the best approximation of the true effect.
#' The \code{true_effect} argument can be used to select the method that will be applied to estimate the true effect.
#' * If \code{"largest"} is entered, the true effect size is assumed to be equal to the effect size of the largest study included in the meta-analysis.
#' * If a \code{numeric} value is entered, the true effect size is assumed to be equal to the value entered by the user (note that the value of ratios must be in their natural scale).
#'
#' Last, this function performs a statistical test to determine whether the observed number of statistically significant studies is higher than expected given the mean statistical power. The \code{method} argument can be used to select the test.
#' * If \code{"binom.test"} is entered, the function performs a binomial exact test of a simple null hypothesis about the probability of success. In this test, the studies with statistically significant results are considered as successes. The mean statistical power to detect the best approximation of the true effect is considered as the probability of success. The exact test is significant if the number of statistically significant studies is higher than what could be have been expected given the mean observed power.
#' * If \code{"chisq.test"} is entered, the function performs a chi-square test based on the number of studies with significant results, the number of studies with non-significant results and their associated probability of occurrence (i.e., the statistical power to detect the best approximation of the true effect). The chi-square test is significant if the number of statistically significant studies is higher than what could be have been expected given the observed power.
#'
#' @return
#' The dataset contains the following columns: \tabular{ll}{
#'  \code{method} \tab method used to conduct the test.\cr
#'  \tab \cr
#'  \code{p.value} \tab p-value for the test statistic.\cr
#'  \tab \cr
#'  \code{power} \tab the power of each individual study to detect the best\cr
#'  \tab approximation of the true effect (\code{true_effect}) at an alpha of .05.\cr
#'  \tab \cr
#'  \code{mean_power} \tab the mean power of all individual studies to detect the best\cr
#'  \tab approximation of the true effect (\code{true_effect}) at an alpha of .05.\cr
#'  \tab \cr
#'  \code{k} \tab the total number of studies.\cr
#'  \tab \cr
#'  \code{sig} \tab whether each individual study has statistically significant results.\cr
#'  \tab \cr
#'  \code{O} \tab the total number of studies with statistically significant results.\cr
#'  \tab \cr
#'  \code{E} \tab the total expected number of studies with statistically significant results.\cr
#'  \tab \cr
#'}
#'
#' @md
#'
#' @export esb.test
#'
#' @references Ioannidis, JPA., Munafo, MR., Fusar-Poli, P., Nosek, BA., & David, SP. (2014). Publication and other reporting biases in cognitive sciences: detection, prevalence, and prevention. \emph{Trends in Cognitive Sciences}, \bold{18}, 235-241.
#'
#' @examples
#' ### load a well-formatted dataframe with a single factor
#' df <- df.SMD[df.SMD$factor == "Surgical", ]
#'
#' ### perform an excess significance bias directly on this dataframe
#' esb <- esb.test(df, measure = "SMD", input = "dataframe")
#'
#' ### perform an excess significance bias using the umbrella function
#' esb.umbrella <- umbrella(df)[[1]]$esb
#'
#' ### perform an excess significance bias on a rma object
#' ### we convert the SMD into Hedges' g
#' G <- metaumbrella:::.estimate_g_from_d(df$value, df$n_cases, df$n_controls)
#' rma <- metafor::rma(yi = G$value, sei = G$se,
#'                     measure = "SMD",
#'                     ni = df$n_cases + df$n_controls,
#'                     data = df)
#'
#' esb.rma <- esb.test(rma, n_cases = df$n_cases, input = "rma")
#'
#' ### perform an excess significance bias on a meta object
#' meta <- meta::metagen(TE = G$value, seTE = G$se,
#'                       sm = "SMD",
#'                       n.e = df$n_cases,
#'                       n.c = df$n_controls,
#'                       data = df)
#'
#' esb.meta <- esb.test(meta, input = "meta")
#'
#' all.equal(esb$p.value, esb.umbrella$p.value, esb.rma$p.value, esb.meta$p.value)
esb.test = function (x, input = "dataframe", n_cases = NULL, n_controls = NULL, measure = NULL, method = "binom.test", true_effect = "largest", seed = NA) {
  # we check that, in case the input is a dataframe entered by user, it does not contain multiple factors
  if (length(unique(x$factor)) > 1) {
    stop("Only one factor can be assessed in the esb.test")
  } else if (!method %in% c("binom.test", "chisq.test")) {
    stop("The method argument must be either 'binom.test' or 'chisq.test'.)")
  }

  # we check that the input passed to the function is appropriate according to the input required
  #### RMA -------
  if (input == "rma") {

      if (!("rma" %in% class(x))) { stop("The object passed to esb.test should be a 'rma' object when 'input = rma'") }

      measure = ifelse(x$measure %in% c("SMD", "OR"),
                       x$measure,
                       ifelse(!is.null(measure),
                              measure,
                              stop("The measure should be indicated either when calling the 'rma' function or when calling the 'esb.test' function.")))

      x = .rma_to_umbrella_x(x, n_cases, n_controls, measure)

    #### meta --------
    } else if (input == "meta") {

      if (!("meta" %in% class(x))) { stop("The object passed to esb.test should be a 'meta' object when 'input = meta'") }

      measure = ifelse(x$sm %in% c("SMD", "OR"),
                       x$sm,
                       ifelse(!is.null(measure), measure, stop("The measure should be indicated either when calling the 'meta' function or when calling the 'esb.test' function.")))

      x = .meta_to_umbrella_x(x, n_cases, n_controls, measure)

    #### well-formatted dataset --------
    } else if (input == "dataframe") {

      if (is.null(measure)) { stop("The measure should be indicated when calling the 'esb.test' function.") }

      x_i_ok = attr(.check_data(x), "data")

      if (any(x_i_ok$duplicate == TRUE)) {
        x = .format_dataset(x_i_ok, mult.level = TRUE)
        measure = attr(x, "measure")
      } else {
        x = .format_dataset(x_i_ok)
        measure = attr(x, "measure")
      }

      for (i in which(x$measure == "G")) {
        G_i = x[i, "value"]
        se_g_i = x[i, "se"]
        x[i, "value"] = .estimate_d_from_g(g = G_i, n_cases = x[i, "n_cases"], n_controls = x[i, "n_controls"], se = se_g_i)$value
        x[i, "se"] = .estimate_d_from_g(g = G_i, n_cases = x[i, "n_cases"], n_controls = x[i, "n_controls"], se = se_g_i)$se
        x[i, "ci_lo"] = x[i, "value"] - x[i, "se"] * qt(0.975, x[i, "n_cases"] + x[i, "n_controls"] - 2)
        x[i, "ci_up"] = x[i, "value"] + x[i, "se"] * qt(0.975, x[i, "n_cases"] + x[i, "n_controls"] - 2)
        x[i, "measure"] = "SMD"
      }
      #### called from umbrella() --------
    } else if (input == "other") {

      for (i in which(x$measure == "G")) {
        G_i = x[i, "value"]
        se_g_i = x[i, "se"]
        x[i, "value"] = .estimate_d_from_g(g = G_i, n_cases = x[i, "n_cases"], n_controls = x[i, "n_controls"], se = se_g_i)$value
        x[i, "se"] = .estimate_d_from_g(g = G_i, n_cases = x[i, "n_cases"], n_controls = x[i, "n_controls"], se = se_g_i)$se
        x[i, "ci_lo"] = x[i, "value"] - x[i, "se"] * qt(0.975, x[i, "n_cases"]+ x[i, "n_controls"] - 2)
        x[i, "ci_up"] = x[i, "value"] + x[i, "se"] * qt(0.975, x[i, "n_cases"] + x[i, "n_controls"] - 2)
        x[i, "measure"] = "SMD"
      }
    }

  if (!(measure %in% c("SMD", "HR", "IRR", "OR", "RR"))) {
    stop("The measure should be one of 'SMD', 'HR', 'IRR', 'OR', 'RR'")
  }

  k = nrow(x)
  # Estimate p.values
  x$p.value = NA
  for (i in 1:k) {
    value_i = ifelse(measure %in% c("HR", "IRR", "OR", "RR"),
                     log(x$value[i]),
                     x$value[i])
    x$p.value[i] = ifelse(value_i == 0,
                          1,
                          .two_tail(ifelse(measure == "SMD",
                                           pt(value_i / x$se[i], x$n_cases[i] + x$n_controls[i] - 2),
                                           pnorm(value_i / x$se[i])
                          )))
  }
  x$signif = is.na(x$p.value) | x$p.value < 0.05 # is.na for NSUEs
  # Estimate statistical powers
  if (true_effect == "largest") {
    if (measure == "IRR") {
      true_value = .largest_irr(x, return = "value")
    } else if (measure %in% c("OR", "HR", "RR")) {
      true_value = .largest_or_rr_hr(x, return = "value")
    } else if (measure == "SMD") {
      true_value = .largest_smd(x, return = "value")
    }
  } else if (is.numeric(true_effect)) {
    true_value = true_effect
  }

  x$power = NA

  if (is.na(seed)) {
    withr::local_preserve_seed()
  } else {
    withr::local_seed(seed)
  }

  for (i in 1:k) {
    x$power[i] = switch (measure,
                         "SMD" = .power_d(n_cases = x$n_cases[i], n_controls = x$n_controls[i], true_d = true_value, se = x$se[i]),
                         "HR" = .power_hr(x[i, ], true_value),
                         "IRR" = .power_irr(x[i, ], true_value),
                         "OR" = .power_or(x[i, ], true_value),
                         "RR" = .power_rr(x[i, ], true_value),
                         NA
    )
  }
  # Conduct the test
  if (all(!is.na(x$power))) {
    esb = switch (method,
                  "binom.test" = {
                    observed = sum(x$signif)
                    expected_mean_power = mean(x$power)
                    test = binom.test(observed, k, expected_mean_power, alternative = "greater")
                    names(observed) = "stat. sign. studies"
                    names(k) = "total studies"
                    names(expected_mean_power) = "statistical power"
                    list(
                      # statistic = observed,
                      # parameter = k,
                      method = "Exact binomial test for excess significance bias",
                      p.value = test$p.value,
                      # null.value = expected_mean_power,
                      # alternative = test$alternative,
                      # test = test,
                      power = x$power,
                      mean_power = expected_mean_power,
                      k = k,
                      sig = x$signif,
                      O = observed,
                      E = expected_mean_power * k
                    )
                  },
                  "chisq.test" = {
                    observed = sum(x$signif)
                    expected_mean_power = mean(x$power)
                    test = suppressWarnings(prop.test(observed, k, p = expected_mean_power, alternative = "greater", correct = FALSE))
                    list(
                      method = "Chi-squared test for excess significance bias",
                      # statistic = test$statistic,
                      # parameter = test$parameter,
                      p.value = test$p.value,
                      # test = test,
                      power = x$power,
                      mean_power = expected_mean_power,
                      k = k,
                      sig = x$signif,
                      O = observed,
                      E = expected_mean_power * k
                    )
                  }
    )
  } else {
    warning("An unplanned error occured in the excess of significance bias test. Please contact us to obtain more information.")
    esb = list(
      p.value = NA,
      method = "Test for excess significance bias not conducted."
    )
  }
  class(esb) = "htest"
  return(esb)
}
