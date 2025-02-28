#' Conduct the calculations for an umbrella review
#'
#' This function performs various calculations needed for an umbrella review.
#'
#' @param x a well-formatted dataset.
#' @param method.var the estimator used to quantify the between-study variance in the random-effects meta-analysis. Default is the Restricted Likelihood Maximum ("REML") estimator. Alternatively, DerSimonian and Laird \code{"DL"}, Hartung-Knapp-Sidik-Jonkman \code{"hksj"} (applies a Hartung-Knapp-Sidik-Jonkman adjustment on the results of a \code{"DL"} estimator), maximum-likelihood \code{"ML"} or Paule-Mandel \code{"PM"} estimators can be used. A fixed-effect meta-analysis can be obtained by indicated the \code{method.var = "FE"} argument.
#' @param mult.level a logical variable indicating the presence of multiple effect sizes per study in at least one factor of the umbrella review. Default is \code{FALSE} (i.e., each study of all factors include only one effect size). If \code{mult.level = TRUE} is specified, the Borenstein's methods are used to generate only one effect size per study. See \code{\link{metaumbrella-package}} for more information.
#' @param r a correlation coefficient indicating the strength of the association between multiple outcomes (or time-points) within the same study. The \code{r} value is applied to all studies with a \code{"outcomes"} value in the \code{reverse_es} column that have no indication of correlation in the well-formatted dataset. Default is 0.5.
#' @param method.esb the method used to conduct the excess of statistical significance test. It must be \code{"PSST"}, \code{"TESS"} or \code{"TESSPSST"}  (see details). Default is \code{TESSPSST}.
#' @param true_effect the method to estimate the true effect in the test for excess significance. It must be \code{"largest"}, \code{"UWLS"}, \code{"pooled"} or a numeric value (see details). Default is \code{"UWLS"}.
#' @param pre_post_cor The value of the correlation coefficient between baseline and follow-up scores in pre-post studies. If your umbrella review includes pre-post controlled studies, you should indicate the mean pre-post correlation across groups. Only needed when using the SMC measure.
#' @param tau2 The tau2 value that should be used when using one of the \code{PSST}, \code{TESS} or \code{TESSPSST} methods (only if you want this value to be different from the one automatically estimated during the meta-analytic calculations).
#' @param max_asymmetry The percentage of assymetry tolerated in the 95% CI of the MD, OR and RR effect measures. Default is 10%. Any stronger asymmetry will stop the analysis.
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
#'  \code{overall_rob} \tab for the overall RoB, the proportion of participants in studies at low risk of bias (weighted mean based on meta-analytic weights).\cr
#'  \tab \cr
#'  \code{report_rob} \tab for the selective reporting bias, the proportion of participants in studies at low risk of bias (weighted mean based on meta-analytic weights).\cr
#'  \tab \cr
#'  \code{rob} \tab for each individual RoB, the proportion of participants in studies at low risk of bias (weighted mean based on meta-analytic weights).\cr
#'  \tab \cr
#'  \code{weights} \tab the weights assigned to each study in the meta-analysis.\cr
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
umbrella = function (x, method.var = "REML", mult.level = FALSE, r = 0.5, pre_post_cor = 0.8, method.esb = "TESSPSST", true_effect = "UWLS", tau2 = NULL, max_asymmetry = 10, seed = NA, verbose = TRUE) {

  y = list()

  if (!method.var %in% c("DL", "hksj", "REML", "PM", "ML", "FE")) {
    stop("The between-study variance estimator (argument method.var of the umbrella function) should be either 'PM', 'ML', 'DL', 'hksj', 'REML' or 'FE'.")
  }
  if (r > 1 | r < -1) {
    stop("The r argument of the umbrella function (the r value that will be applied to aggregate studies with multiple outcomes) must be within the range of [-1, 1].")
  }
  if (pre_post_cor > 1 | pre_post_cor < -1) {
    stop("The pre_post_cor argument of the umbrella function (the r value that will be applied to estimate the pre/post effect size) must be within the range of [-1, 1].")
  }
  # initial checkings ------
  checkings <- .check_data(x)

  x <- attr(checkings, "data")
  x_others <- attr(checkings, "others")
  x$pre_post_cor[is.na(x$pre_post_cor)] <- pre_post_cor
  x$r[is.na(x$r)] <- r
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
  # x=dat_test
  # x$mean_age = rnorm(nrow(x))
  # x$factor_name = sample(c("A", "B","C"), nrow(x), replace=TRUE)
  # method.var = "REML"; mult.level = TRUE; r = 0.5
  # method.esb = "TESSPSST"; true_effect = "UWLS"; pre_post_cor = NA;
  # seed = NA; verbose = TRUE; tau2= NULL
  # factor = unique(x$factor)[1]#"Cumulative trauma"##"Psychotic disorder" #  # factor = unique(dfwoN)
  # checkings <- .check_data(x); max_asymmetry = 10
  # x <- attr(checkings, "data")
  # x_others <- attr(checkings, "others")
  # list_author_concern = list_factor_concern = NULL
  # x$pre_post_cor[is.na(x$pre_post_cor)] <- pre_post_cor
  # x$r[is.na(x$r)] <- r

  for (factor in unique(x$factor)) {
    # print(factor)

    if (verbose) cat(paste("Analyzing factor:", factor, '\n'))

    x_i = x[which(x$factor == factor), ]

    measures = unique(x_i$measure)

    if (length(measures) == 1) {
      measure = measures
    } else if (any(c("SMD", "G", "SMC", "MD", "MC") %in% measures) &
               !any(c("IRR", "RR", "HR", "R", "Z") %in% measures)) {
      measure = "G"
    } else if (any(c("IRR") %in% measures) &
               any(c("SMD", "R", "Z") %in% measures)) {
      stop("'IRR' & 'SMD/R/Z' measures cannot be combined within the same factor")
    } else if (any(c("OR", "RR", "HR", "IRR") %in% measures) &
               !any(c("R", "Z") %in% measures)) {
      measure = "OR"
    } else if (all(measures %in% c("R", "Z"))) {
      measure = "Z"
    } else {
      stop("'R' & 'Z' cannot be combined with other effect size measures within the same factor, and 'IRR' cannot be combined with differences measures ('SMD', 'MD', ...)")
    }

    x_i_mlm <- .identify_multilevel(x_i, mult.level, verbose, r)
    REPEATED_STUDIES = attr(x_i_mlm, "REPEATED_STUDIES")
    # print(x_i_mlm[1,"measure"])
    x_i_hom = .format_hom(x_i_mlm)
    # x_i_hom = x_i_hom[x_i_hom$author == "dew", ]; x_i_hom$se
    # print(x_i_hom[1,"measure"])
    measure[measure %in% c("OR", "HR", "RR", "IRR")] <- paste0("log", measure)
    x_i_ci = .format_ci(x_i_hom, measure)
    x_i_value = .format_value(x_i_ci, measure)

    x_i_es = if (verbose) .format_effsize(x_i_value, measure, max_asymmetry) else suppressWarnings(.format_effsize(x_i_value, measure, max_asymmetry))
    x_i_format = x_i_es
    # return(x_i_es)
    if (REPEATED_STUDIES) {
      x_i_ok_full = x_i_format
      x_i_ok = .agg_mcv(x_i_format,
                        measure = measure,
                        r = r)
      rownames(x_i_ok) = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
      x_i_ok$unique_ID_study = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
      x_i_ok_full$factor = factor
      x_i_ok$factor = factor
    } else {
      x_i_ok_full = paste0("The dataset does not have a multivariate structure.")
      x_i_ok = x_i_format
      rownames(x_i_ok) = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
      x_i_ok$unique_ID_study = make.names(paste(x_i_ok$author, x_i_ok$year, x_i_ok$factor), unique = TRUE)
      x_i_ok$factor = factor
    }

    if (length(unique(rownames(x_i_ok))) != nrow(x_i_ok)) stop("Issue multilevels umbrella")


    attr(x_i_ok, "amstar") <- attr(x_i_format, "amstar")
    attr(x_i_ok, "REPEATED_STUDIES") <- attr(x_i_format, "REPEATED_STUDIES")
    attr(x_i_ok, "n_studies") <- length(unique(rownames(x_i_ok)))
    attr(x_i_ok, "data_mult") <- attr(x_i_format, "data_mult")
    attr(x_i_ok, "comparison_adjustment") <- attr(x_i_format, "comparison_adjustment")

    REPEATED_STUDIES <- attr(x_i_ok, "REPEATED_STUDIES")
    n_studies <- attr(x_i_ok, "n_studies")

    # create an object storing information on sample sizes
    n <- data.frame(studies = n_studies,
                    cases = .sum_na(x_i_ok$n_cases, na.rm = TRUE),
                    controls = .sum_na(x_i_ok$n_controls, na.rm = TRUE),
                    total_n = ifelse(measure %in% c("R", "Z"),
                                     .sum_na(x_i_ok$n_sample, na.rm = TRUE),
                                     .sum_na(
                                       c(.sum_na(x_i_ok$n_cases, na.rm = TRUE),
                                         .sum_na(x_i_ok$n_controls, na.rm = TRUE)),
                                       na.rm = TRUE)))

    rownames(n) = "n"

    .meta <- .meta_gen

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
      coef = x_i_ok$value
      se = x_i_ok$se
      z = coef / se
      p.value = ifelse(coef == 0, 1,
                       .two_tail(
                         ifelse(measure %in% c("SMD", "G", "SMC", "MD", "MC"),
                                pt(z, x$n_cases + x$n_controls - 2),
                                pnorm(z))))
      ci_lo = x_i_ok$ci_lo
      ci_up = x_i_ok$ci_up
      tau2_calc = i2 = qe = qe_p.value = pi_lo = pi_up = NA
      weights = 100
    } else if (method.var == "FE") {
      k = m$k
      coef = m$TE.common
      se = m$seTE.common
      z = m$zval.common
      p.value = m$pval.common
      ci_lo = m$lower.common
      ci_up = m$upper.common
      tau2_calc = NA
      i2 = m$I2 * 100
      qe = m$Q
      qe_p.value = m$pval.Q
      pi_lo = NA
      pi_up = NA
      weights = m$w.common
    } else {
      k = m$k
      coef = m$TE.random
      se = m$seTE.random
      z = m$zval.random
      p.value = m$pval.random
      ci_lo = m$lower.random
      ci_up = m$upper.random
      tau2_calc = m$tau^2
      i2 = m$I2 * 100
      qe = m$Q
      qe_p.value = m$pval.Q
      pi_lo = m$lower.predict#ifelse(n_studies >= 3, m$lower.predict, NA)
      pi_up = m$upper.predict#ifelse(n_studies >= 3, m$upper.predict, NA)
      weights = m$w.random
    }

    # Create summary datasets of the random-effects model
    ma_results = data.frame(value = coef, z, p.value, ci_lo, ci_up, pi_lo, pi_up)
    rownames(ma_results) = switch(as.character(measure),
                                  "MD"=, "SMD"=, "G" = "Bias-corrected SMD",
                                  "Z" =, "R" = "Fisher's Z",
                                  "MC"=, "SMC" = "Standardized mean change",
                                  "OR"=, "logOR" = "log (OR)",
                                  "RR"=, "logRR" = "log (RR)",
                                  "IRR"=, "logIRR" = "log (IRR)",
                                  "HR"=, "logHR" = "log (HR)")

    # Create summary datasets of the heterogeneity
    heterogeneity = data.frame(tau2 = tau2_calc, i2 = i2, qe = qe, p.value = qe_p.value)

    largest = .largest_overall(x_i_ok)

    rownames(largest) = switch(as.character(measure),
                               "G"=, "SMD" = "Bias-corrected SMD",
                               "Z" =, "R" = "Fisher's Z",
                               "SMC" = "Bias-corrected SMD (change from baseline)",
                               "MD" = "Mean difference",
                               "MC" = "Mean difference (change from baseline)",
                               "OR"=, "logOR" = "log (OR)",
                               "RR"=, "logRR" = "log (RR)",
                               "IRR"=, "logIRR" = "log (IRR)",
                               "HR"=, "logHR" = "log (HR)")

    # publication bias
    # only for meta-analyses with at least 3 studies
    if (n_studies < 2) {
      egger = data.frame(statistic = NA, p.value = NA)
    } else {
      mb = .egger_pb(value = x_i_ok$value, se = x_i_ok$se, measure = "non_ratio")
      egger = data.frame(statistic = mb$statistic, p.value = mb$p.value)
    }

    # excess significance bias
    if (true_effect == "largest") {
      true_value = "largest"
    } else if (true_effect == "UWLS") {
      true_value = "UWLS"
    } else if (true_effect == "pooled") {
      true_value = .as_numeric(ma_results$coef)
    } else if (is.numeric(true_effect)) {
      true_value = true_effect
    } else {
      stop("The input in the 'true_effect' argument should be 'pooled', 'UWLS', 'largest' or a numeric value. Please see the manual for more information.")
    }

    if (n_studies < 2) {
      esb = data.frame(p.value = NA)
    } else if (method.var == "FE" & method.esb %in% c('TESS', 'PSST', 'TESSPSST')) {
      if (verbose) {
        warning(paste0("The requested method.esb is ", method.esb, " while a fixed ",
                       "effect meta-analysis is also requested. The tau2 value  is recommended ",
                       "to be indicated (or a random-effects model should be used). For now, ",
                       "a tau2=0 has been assumed")
        )
      }
      esb = esb.test(x_i_ok, method.esb = method.esb,
                     measure = measure,
                     input = "other", true_effect = true_value,
                     seed = seed, tau2 = 0
      )

    } else {
      esb = esb.test(x_i_ok, method.esb = method.esb,
                     measure = measure,
                     input = "other", true_effect = true_value,
                     seed = seed, tau2 = ifelse(is.null(tau2), tau2_calc, tau2)
      )
    }
    # -------------
    # risk of bias
    # -------------
    x_i_ok$weights = .as_numeric(weights)

    perc_contradict = ifelse(
      ma_results$value >= 0,
      sum(x_i_ok$ci_up < 0) / nrow(x_i_ok),
      sum(x_i_ok$ci_lo >= 0) / nrow(x_i_ok))

    # if (REPEATED_STUDIES) {
      # rob_columns <- c("rob.recoded", "rob_report.recoded",
      #                  "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
      #                  "rob1_blind_outcome.recoded", "rob1_attrition.recoded", "rob1_report.recoded",
      #                  "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
      #                  "rob2_outcome.recoded", "rob2_report.recoded")
    #
    #   x_i_ok_full$ID = paste(x_i_ok_full$author, x_i_ok_full$year)
    #   x_i_ok$ID = paste(x_i_ok$author, x_i_ok$year)
    #   x_i_ok_full = merge(x_i_ok_full, x_i_ok[, c("ID", "weights")])
    #
    #   x_i_ok_full$rob.recoded[3:6] <- c(1, 0, 0, 1)
    #   x_i_ok_full$weights
    #   calculate_weighted_means <- function(df) {
    #     result <- sapply(rob_columns, function(col) {
    #       if(all(is.na(df[[col]])) || all(is.na(df$weights))) {
    #         return(NA)
    #       } else {
    #         return(weighted.mean(df[[col]], w = df$weights, na.rm = TRUE))
    #       }
    #     })
    #     return(result)
    #   }
    #
    #   # Use by() to apply the function to each author-year group
    #   result_by <- by(x_i_ok_full, list(x_i_ok_full$author, x_i_ok_full$year), calculate_weighted_means)
    #
    #   # Convert result to a data frame
    #   result_df <- as.data.frame(do.call(rbind, as.list(result_by)))
    #
    #   # Add author and year columns
    #   result_df$author <- rownames(result_df)
    #   result_df$author <- sub("^(.*)\\..*$", "\\1", result_df$author)
    #   result_df$year <- rownames(result_df)
    #   result_df$year <- sub("^.*\\.(.*)$", "\\1", result_df$year)
    #
    #   # Reorder columns to have author and year first
    #   result_df <- result_df[, c("author", "year", rob_columns)]
    #
    #   # Merge with original x_i_ok data
    #   final_result <- merge(x_i_ok, result_df, by = c("author", "year"), all.x = TRUE)
    #
    # }

    # Loop through each column
    for (col in c("rob.recoded", "rob_report.recoded",
                  "rob1_rand.recoded", "rob1_allocation.recoded", "rob1_blind_pers.recoded",
                  "rob1_blind_outcome.recoded", "rob1_attrition.recoded","rob1_report.recoded" ,
                  "rob2_rand.recoded", "rob2_deviation.recoded", "rob2_missing.recoded",
                  "rob2_outcome.recoded", "rob2_report.recoded")) {
      if (all(is.na(x_i_ok[, col]))) {
        x_i_ok[, paste0(gsub("recoded", "agg", col))] <- NA
      } else {
        x_i_ok[, col] <- as.numeric(x_i_ok[, col])
        x_i_ok[, paste0(gsub("recoded", "agg", col))] <-
          weighted.mean(x_i_ok[,col],
                        x_i_ok$weights, na.rm = TRUE) * 100
      }
    }

    # View(x_i_ok[, c("rob.agg",
    #                 "rob_rand.agg", "rob_deviation.agg", "rob_missing.agg",
    #                 "rob_outcome.agg", "rob_report.agg")])
    # if (all(is.na(x_i_ok$rob.recoded))) {
    #   riskofbias = NA
    # } else {
    #   x_i_ok$rob.recoded = .as_numeric(x_i_ok$rob.recoded)
    #   riskofbias = weighted.mean(x_i_ok$rob.recoded, x_i_ok$weights) * 100
    # }

    # AMSTAR
    amstar = unique(x_i_ok$amstar)

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

    ## attr
    ############## MD
    if (measure %in% c("MD", "MC") & nrow(x_i_ok) > 1) {
      if (REPEATED_STUDIES) {
        x_i_ok_md = .agg_mcv(attr(x_i_es, "x_i_es_bis"),
                             measure = measure,
                             r = r)
      } else {
        x_i_ok_md = attr(x_i_es, "x_i_es_bis")
      }

      m_md = .meta(x_i_ok_md, method.var)

      if (method.var == "FE") { #
        coef = m_md$TE.fixed
        ci_lo = m_md$lower.fixed
        ci_up = m_md$upper.fixed
      } else {
        coef = m_md$TE.random
        ci_lo = m_md$lower.random
        ci_up = m_md$upper.random
      }

      val_md = c(coef, ci_lo, ci_up)
    } else {
      val_md = c(NA,NA,NA)
      x_i_ok_md = NA
    }

    ##############
    rob_data = x_i_ok[, c("rob.agg",
                          "rob_report.agg",
                          "rob1_rand.agg",
                          "rob1_allocation.agg",
                          "rob1_blind_pers.agg",
                          "rob1_blind_outcome.agg",
                          "rob1_attrition.agg",
                          "rob1_report.agg",

                          "rob2_rand.agg", "rob2_deviation.agg", "rob2_missing.agg",
                          "rob2_outcome.agg", "rob2_report.agg")]
    weights_data = x_i_ok[,c(
      "author", "year", "factor", "weights", "n_cases", "n_controls")
    ]

    # Measure clean ratios
    measure <- gsub("log", "", measure)
    attr(x_i_ok, "measure") <- measure

    # FINAL
    y[[factor]] = list(
      factor = factor,
      measure = measure,
      x = x_i_ok,
      x_multi = attr(x_i_ok, "data_mult"),
      x_shared = attr(x_i_ok, "comparison_adjustment"),
      x_md = x_i_ok_md,
      n = n,
      method.var = method.var,
      ma_results = ma_results,
      largest = largest,
      heterogeneity = heterogeneity,
      egger = egger,
      esb = esb,
      indirectness = unique(x_i_ok$indirectness)[1],
      overall_rob = unique(rob_data$rob.agg)[1],
      report_rob = unique(rob_data$rob_report.agg)[1],
      perc_contradict = perc_contradict,
      rob = rob_data,
      weights = weights_data,
      amstar = amstar,
      jk = jk$p.value,
      pooled_md = val_md,
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
