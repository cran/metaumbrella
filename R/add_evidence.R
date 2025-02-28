#' Add evidence classes to \dQuote{umbrella} objects
#'
#' Add evidence classes to the factors included in an umbrella review.
#'
#' @param x an object of class \dQuote{umbrella}.
#' @param criteria the evidence criteria. It must be "GRADE", "Ioannidis" or "Personalized".
#' @param class_I a vector or list of threshold values required for reaching Class I in the Personalized criteria (see details below).
#' @param class_II a vector or list of threshold values required for reaching Class II in the Personalized criteria (see details below).
#' @param class_III a vector or list of threshold values required for reaching Class III in the Personalized criteria (see details below).
#' @param class_IV a vector or list of threshold values required for reaching Class IV in the Personalized criteria (see details below).
#' @param eq_range_or a vector of the bounds of equivalence ranges for OR/RR/HR/IRR (only required for GRADE) criteria.
#' @param eq_range_g a vector of the bounds of equivalence ranges for SMD (only required for GRADE) criteria.
#' @param verbose logical variable indicating whether text outputs and messages should be generated. We recommend turning this option to FALSE only after having carefully read all the generated messages.
#'
#' @details The \code{add.evidence()} function performs a stratification of evidence according to three criteria.
#'
#' ## \bold{"Ioannidis" classification}
#' This classification allows to stratify evidence according to the criteria described in Fusar-Poli & Radua (2018).
#'  This classification proposes to stratify evidence in five ordinal classes: "Class I", "Class II", "Class III", "Class IV", "Class ns".
#'  The criteria for each class are the following:
#' * \bold{Class I:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-6}, \eqn{I^2} < 0.5, 95% prediction interval excluding the null, p-value of the Egger test > .05 and p-value of the excess of statistical significance test > .05.
#' * \bold{Class II:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-6}, largest study with a statistically significant effect and class I criteria not met.
#' * \bold{Class III:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-3} and class I-II criteria not met.
#' * \bold{Class IV:} p-value of the meta-analysis < 0.05 and class I-III criteria not met.
#' * \bold{Class ns:} p-value of the meta-analysis >= 0.05.
#' To apply this classification with R and Z effect size measures, you should indicate both the 'n_sample' AND the 'n_cases'.
#'
#' ## \bold{"GRADE" classification}
#' This classification allows to stratify evidence according to four ordinal classes: "High", "Moderate", "Low", "Very low".
#' Importantly, this classification should not be taken as an equivalent to the subjective approach underlying the standard GRADE classification.
#' However, in line with the standard GRADE approach, this classification uses a downgrading procedure in which all factors start with a "High" evidence class that
#' could then be downgraded according to the 5 following criteria. Importantly, when the number of studies is low (k < 5), then the meta-analysis starts
#'  with a 'Moderate' rating to account for the difficulty of identifying heterogeneity and publication bias with such a limited number of studies.
#'
#'  All calculations are made automatically, but users should input in their dataset, i) the overall risk of bias of each study ('rob' column), ii) the risk of selective reporting ('rob1_report', or 'rob2_report' columns)
#'   and iii) the risk of indirectness ('indirectness' column). If this information is left empty, each criterion will be assumed to be at high risk.
#'
#' * \bold{Risk of Bias (Limitations):}
#'   \itemize{
#'     \item No downgrade: >=75% of participants included in low-risk studies
#'     \item One downgrade: 50%-75% of participants included in low-risk studies
#'     \item Two downgrades: <=50% of participants included in low-risk studies
#'   }
#'   The pooled percentage of participants is calculated as a weighted mean, with weights attributed to each study being equal to the weight each study receives in the meta-analysis.
#'
#' * \bold{Heterogeneity:}
#'   \itemize{
#'     \item Two downgrades: Substantial discrepancy between the 95% CI and 95% PI (e.g., bounds of the 95% CI and 95% PI not of the same sign and in different equivalence ranges).
#'     \item One downgrade: Small/moderate discrepancy between the 95% CI and 95% PI (e.g., bounds of the 95% CI and 95% PI of the same sign, but in different equivalence ranges).
#'   }
#'   When 95% PI is not reliably estimable, the assessment relies on the I² statistic and the percentage of studies with contradicting results:
#'   \itemize{
#'     \item Two downgrades: I² >= 50% and >=10% of studies with statistically significant results in the opposite direction compared to the pooled effect size
#'     \item One downgrade: I² >= 30% and >=10% of studies with statistically significant results in the opposite direction compared to the pooled effect size
#'   }
#'
#' * \bold{Indirectness:}
#'   The number of downgrades and the criteria are left to the user's discretion, as these factors vary significantly depending on the scope of the review.
#'   Examples of criteria may include heterogeneity in participants' age or undefined control groups:
#'   \itemize{
#'     \item No downgrade: No concerns regarding indirectness
#'     \item One downgrade: Serious concerns (e.g., "serious" indirectness)
#'     \item Two downgrades: Very serious concerns (e.g., "very serious" indirectness)
#'   }
#'
#' * \bold{Imprecision:}
#'   \itemize{
#'     \item Two downgrades: The 95% CI of the pooled effect size includes both null (SMD = 0; RR/OR = 1) and large (SMD >= 0.80; OR/RR >= 5) effects AND the meta-analysis does not have the sample size required to detect small effects (eSMD = 0.20) with 80% statistical power (n < 394 per arm)
#'     \item One downgrade: The 95% CI of the pooled effect size includes both null and large effects
#'     \item Two downgrades: The meta-analysis does not have the sample size required to detect moderate effects (eSMD = 0.50) with 80% statistical power (n < 64 per arm)
#'     \item One downgrade: The meta-analysis does not have the sample size required to detect small effects (eSMD = 0.20) with 80% statistical power (n < 394 per arm)
#'   }
#'
#' * \bold{Publication Bias:}
#'   \itemize{
#'     \item One downgrade: p-value of Egger's test < 0.10, OR excess significance bias p-value < 0.10, OR more than 50% of participants included in trials with high reporting bias
#'   }
#'
#' This classification is not available for R and Z effect size measures.
#'
#' The GRADE classification implementation in this package was developed through the collaborative efforts of:
#' Dr Corentin J. Gosling, Dr Miguel Garcia-Argibay, Prof Richard Delorme, Prof Marco Solmi,
#' Prof Andrea Cipriani, Prof Christoph U. Correll, Dr Cinzia Del Giovane, Prof Paolo Fusar-Poli, Prof Henrik Larsson, Edoardo Ostinelli,
#' Prof Jae Il Shin, Prof DongKeon Yon,
#' Prof Joaquim Radua, Prof John P. Ioannidis and Prof Samuele Cortese
#'
#' ## \bold{"Personalized" classification}
#' Because the "Ioannidis" and "GRADE" classifications do not necessarily provide a rating system that perfectly matches the requirements of your umbrella review, the \code{add.evidence()} function offers the possibility to use a "Personalized" criteria to stratify the evidence according to 13 criteria. This Personalized criteria proposes to stratify the evidence in 5 ordinal classes: "Class I", "Class II", "Class III", "Class IV" and "Class V". "Class I" is the highest class that could be achieved and "Class V" is the lowest.\cr
#' The overall class achieved by a factor is equal to the lowest class achieved by all the criteria used to stratify evidence. For example, if users choose to stratify the evidence according to 3 criteria (the p-value of the meta-analysis, the inconsistency, the publication bias), and that the classes achieved by these 3 criteria are respectively "Class I", "Class III" and "Class IV", the overall class reached by the factor will be "Class IV".\cr
#' To determine the class that should be assigned to a factor, users have to indicate - for each class - a vector/list of threshold values for all the criteria that are used to stratify the evidence.
#' A description of the criteria and their corresponding inputs is provided below:
#' 1. \code{n_studies}: a number of studies included in the meta-analysis. If the number of studies included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{studies}, the class for which this value is indicated can be reached.
#' 1. \code{total_n}: a total number of participants included in the meta-analysis. If the total number of participants included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{total_n}, the class for which this value is indicated can be reached.
#' 1. \code{n_cases}: a number of cases included in the meta-analysis. If the number of cases included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{cases}, the class for which this value is indicated can be reached.
#' 1. \code{p_value}: a p-value of the pooled effect size under the random-effects model. If the p-value of the pooled effect size is \bold{strictly inferior} to the threshold value indicated in \code{p.value}, the class for which this value is indicated can be reached.
#' 1. \code{I2}: an i-squared (\eqn{I^2}) value. If the \eqn{I^2} value of the meta-analysis is \bold{strictly inferior} to the threshold value indicated in \code{I2}, the class for which this value is indicated can be reached.
#' 1. \code{imprecision}: a SMD value that will be used to calculate the statistical power of the meta-analysis. If the number of participants included in the meta-analyses allows to obtain a statistical power \bold{strictly superior} to 80% for the SMD value indicated in \code{imprecision}, the class for which this value is indicated can be reached.
#' 1. \code{rob}: a percentage of participants included in studies \bold{at low risk of bias}. Note that the approach to determining whether a study is at low risk of bias is left to the user. If the percentage of participants included in studies at low risk of bias is \bold{strictly superior} to the threshold value indicated in \code{rob}, the class for which this value is indicated can be reached.
#' 1. \code{amstar}: an AMSTAR rating on the methodological quality of the meta-analysis. If the AMSTAR value of the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{amstar}, the class for which this value is indicated can be reached.
#' 1. \code{egger_p}: a p-value of an Egger's test for publication bias. If the p-value of the Egger's test is \bold{strictly superior} to the threshold value indicated in \code{egger_p}, the class for which this value is indicated can be reached.
#' 1. \code{esb_p}: a p-value of a test for excess of statistical significance bias (ESB). If the p-value  of the test is \bold{strictly superior} to the threshold value indicated in \code{esb_p}, the class for which this value is indicated can be reached.
#' 1. \code{JK_p}: the largest p-value obtained in the jackknife meta-analysis (JK). If the largest p-value obtained in the jackknife meta-analysis is \bold{strictly inferior} to the threshold value indicated in \code{JK_p}, the class for which this value is indicated can be reached.
#' 1. \code{pi}: a "notnull" value indicates that users request the 95% prediction interval of the meta-analysis to exclude the null value to achieve the class for which it is indicated.
#' 1. \code{largest_CI}: a "notnull" value indicates that users request the 95% confidence interval of the largest study included in the meta-analysis to exclude the null value to achieve the class for which it is indicated.
#'
#' @return
#' Return an object of class \dQuote{umbrella} with the evidence classes added.
#'
#' @export add.evidence
#'
#' @references Fusar-Poli, P., & Radua, J. (2018). Ten simple rules for conducting umbrella reviews. \emph{Evidence-Based Mental Health}, \bold{21}, 95-100.
#'
#' @seealso \code{\link{umbrella}()} for conducting an umbrella review.
#'
#' @md
#'
#' @examples
#' ### perform calculations required for an umbrella review
#' df <- subset(df.SMD, factor == "Surgical")
#' umb.full <- umbrella(df)
#'
#' ### stratify evidence according to the Ioannidis classification
#' evid_ioannidis <- add.evidence(umb.full, criteria = "Ioannidis")
#' summary(evid_ioannidis)
#'
#' ### stratify evidence according to the Personalized classification with
#' ### the number of studies and cases, the inconsistency as criteria.
#' ### - a class I can be reached if the number of studies is > 10, the number of cases is > 500 and
#' ###   the I2 is < 25%.
#' ### - a class II can be reached if the number of studies is > 5, the number of cases is > 400 and
#' ###   the I2 is < 50%.
#' ### - a class III can be reached if the number of cases is > 300 and the I2 is < 75%.
#' ### - a class IV can be reached if the number of cases is > 100.
#' ### - else, if the number of cases is <= 100, a class V is assigned.
#' evid_perso1 <- add.evidence(umb.full, criteria = "Personalized",
#'    class_I = c(n_studies = 10, n_cases = 500, I2 = 25),
#'    class_II = c(n_studies = 5, n_cases = 400, I2 = 50),
#'    class_III = c(n_cases = 300, I2 = 75),
#'    class_IV = c(n_cases = 100))
#' summary(evid_perso1)
add.evidence <- function (x,
            criteria = "Ioannidis",
            eq_range_or = c(0.80, 1.25),
            eq_range_g = c(-0.10, 0.10),
            class_I = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA, imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi = NA, largest_CI = NA),
            class_II = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA, imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi = NA, largest_CI = NA),
            class_III = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA, imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi = NA, largest_CI = NA),
            class_IV = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA, imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi = NA, largest_CI = NA),
            verbose = TRUE) {

  if (criteria %in% c("Personalized", "personalised", "personalized")) { criteria = "Personalised" }
  if (!inherits(x, "umbrella")) { stop("The 'x' argument must be an 'umbrella' object") }

    returned_evidence = switch(criteria,
                             Ioannidis = .add.evidence_Ioannidis(x, verbose),
                             Personalised = .add.evidence_personalised(
                               x, class_I, class_II, class_III, class_IV, verbose),
                             GRADE = .add.evidence_GRADE(x, eq_range_or = eq_range_or, eq_range_g = eq_range_g),
                             stop("'criteria' must be either 'Ioannidis', 'GRADE' or 'Personalized'")
    )
    return(returned_evidence)
  }

#' Remove evidence classes from an object of class \dQuote{umbrella}
#'
#' This function removes evidence classes previously created from an object of class \dQuote{umbrella}
#'
#' @param x an object of class \dQuote{umbrella}
#'
#' @return
#' Return an object of class \dQuote{umbrella} with the evidence classes dropped.
#'
#' @references Fusar-Poli, P., & Radua, J. (2018). Ten simple rules for conducting umbrella reviews. \emph{Evidence-Based Mental Health}, \bold{21}, 95-100.
#'
#' @export drop.evidence
#'
#' @md
#'
#' @seealso \code{\link{umbrella}()} for conducting an umbrella review.
#'
#' @examples
#' ### perform calculations required for an umbrella review
#' umb.full <- umbrella(df.SMD)
#'
#' ### stratify evidence according to the algorithmic GRADE criteria
#' evid_grade <- add.evidence(umb.full, criteria = "GRADE")
#' is.na(evid_grade$Pharmacological$evidence)
#'
#' evid_empty <- drop.evidence(evid_grade)
#' is.na(evid_empty$Pharmacological$evidence)
drop.evidence = function (x) {
  attr(x, "criteria") = NULL
  for (name in names(x)) {
    x[[name]]$evidence = NA
  }
  return(x)
}
