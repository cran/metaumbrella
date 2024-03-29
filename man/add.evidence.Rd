% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_evidence.R
\name{add.evidence}
\alias{add.evidence}
\title{Add evidence classes to \dQuote{umbrella} objects}
\usage{
add.evidence(
  x,
  criteria = "Ioannidis",
  class_I = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA,
    imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi =
    NA, largest_CI = NA),
  class_II = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA,
    imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi =
    NA, largest_CI = NA),
  class_III = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA,
    imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi =
    NA, largest_CI = NA),
  class_IV = c(n_studies = NA, total_n = NA, n_cases = NA, p_value = NA, I2 = NA,
    imprecision = NA, rob = NA, amstar = NA, egger_p = NA, esb_p = NA, JK_p = NA, pi =
    NA, largest_CI = NA),
  verbose = TRUE
)
}
\arguments{
\item{x}{an object of class \dQuote{umbrella}.}

\item{criteria}{the evidence criteria. It must be "GRADE", "Ioannidis" or "Personalized".}

\item{class_I}{a vector or list of threshold values required for reaching Class I in the Personalized criteria (see details below).}

\item{class_II}{a vector or list of threshold values required for reaching Class II in the Personalized criteria (see details below).}

\item{class_III}{a vector or list of threshold values required for reaching Class III in the Personalized criteria (see details below).}

\item{class_IV}{a vector or list of threshold values required for reaching Class IV in the Personalized criteria (see details below).}

\item{verbose}{logical variable indicating whether text outputs and messages should be generated. We recommend turning this option to FALSE only after having carefully read all the generated messages.}
}
\value{
Return an object of class \dQuote{umbrella} with the evidence classes added.
}
\description{
Add evidence classes to the factors included in an umbrella review.
}
\details{
The \code{add.evidence()} function performs a stratification of evidence according to three criteria.
\subsection{\bold{"Ioannidis" classification}}{

This classification allows to stratify evidence according to the criteria described in Fusar-Poli & Radua (2018).
This classification proposes to stratify evidence in five ordinal classes: "Class I", "Class II", "Class III", "Class IV", "Class ns".
The criteria for each class are the following:
\itemize{
\item \bold{Class I:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-6}, \eqn{I^2} < 0.5, 95\% prediction interval excluding the null, p-value of the Egger test > .05 and p-value of the excess of statistical significance test > .05.
\item \bold{Class II:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-6}, largest study with a statistically significant effect and class I criteria not met.
\item \bold{Class III:} number of cases > 1000, p-value of the meta-analysis < \eqn{10^-3} and class I-II criteria not met.
\item \bold{Class IV:} p-value of the meta-analysis < 0.05 and class I-III criteria not met.
\item \bold{Class ns:} p-value of the meta-analysis >= 0.05.
To apply this classification with R and Z effect size measures, you should indicate both the 'n_sample' AND the 'n_cases'.
}
}

\subsection{\bold{"GRADE" classification}}{

This classification allows to stratify evidence according to four ordinal classes: "High", "Moderate", "Low", "Very low".
Importantly, this classification should not be taken as an equivalent to the subjective approach underlying the standard GRADE classification
However, in line with the standard GRADE approach, this classification uses a downgrading procedure in which all factors start with a "High" evidence class that
could then be downgraded according to the following criteria:
\itemize{
\item \bold{Imprecision:} a total number of participants included in the meta-analysis giving a lower power than 0.8 to detect a SMD = 0.20 leads to a downgrading of 1 class.
A number of participants giving a lower power than 0.8 to detect a SMD = 0.50, leads to a downgrading of 2 classes.
\item \bold{Limitations:} a proportion of participants included in studies at low risk of bias inferior to 75\% leads to a downgrading of 1 class. A proportion inferior to 50\% leads to a downgrading of 2 classes.
\item \bold{Publication bias:} a p-value of an Egger's test < .10 leads to a downgrading of 1 class.
\item \bold{Inconsistency:} an \eqn{I^2} value >= 0.5 leads to a downgrading of 1 class.
This classification is not available for R and Z effect size measures.
}
}

\subsection{\bold{"Personalized" classification}}{

Because the "Ioannidis" and "GRADE" classifications do not necessarily provide a rating system that perfectly matches the requirements of your umbrella review, the \code{add.evidence()} function offers the possibility to use a "Personalized" criteria to stratify the evidence according to 13 criteria. This Personalized criteria proposes to stratify the evidence in 5 ordinal classes: "Class I", "Class II", "Class III", "Class IV" and "Class V". "Class I" is the highest class that could be achieved and "Class V" is the lowest.\cr
The overall class achieved by a factor is equal to the lowest class achieved by all the criteria used to stratify evidence. For example, if users choose to stratify the evidence according to 3 criteria (the p-value of the meta-analysis, the inconsistency, the publication bias), and that the classes achieved by these 3 criteria are respectively "Class I", "Class III" and "Class IV", the overall class reached by the factor will be "Class IV".\cr
To determine the class that should be assigned to a factor, users have to indicate - for each class - a vector/list of threshold values for all the criteria that are used to stratify the evidence.
A description of the criteria and their corresponding inputs is provided below:
\enumerate{
\item \code{n_studies}: a number of studies included in the meta-analysis. If the number of studies included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{studies}, the class for which this value is indicated can be reached.
\item \code{total_n}: a total number of participants included in the meta-analysis. If the total number of participants included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{total_n}, the class for which this value is indicated can be reached.
\item \code{n_cases}: a number of cases included in the meta-analysis. If the number of cases included in the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{cases}, the class for which this value is indicated can be reached.
\item \code{p_value}: a p-value of the pooled effect size under the random-effects model. If the p-value of the pooled effect size is \bold{strictly inferior} to the threshold value indicated in \code{p.value}, the class for which this value is indicated can be reached.
\item \code{I2}: an i-squared (\eqn{I^2}) value. If the \eqn{I^2} value of the meta-analysis is \bold{strictly inferior} to the threshold value indicated in \code{I2}, the class for which this value is indicated can be reached.
\item \code{imprecision}: a SMD value that will be used to calculate the statistical power of the meta-analysis. If the number of participants included in the meta-analyses allows to obtain a statistical power \bold{strictly superior} to 80\% for the SMD value indicated in \code{imprecision}, the class for which this value is indicated can be reached.
\item \code{rob}: a percentage of participants included in studies \bold{at low risk of bias}. Note that the approach to determining whether a study is at low risk of bias is left to the user. If the percentage of participants included in studies at low risk of bias is \bold{strictly superior} to the threshold value indicated in \code{rob}, the class for which this value is indicated can be reached.
\item \code{amstar}: an AMSTAR rating on the methodological quality of the meta-analysis. If the AMSTAR value of the meta-analysis is \bold{strictly superior} to the threshold value indicated in \code{amstar}, the class for which this value is indicated can be reached.
\item \code{egger_p}: a p-value of an Egger's test for publication bias. If the p-value of the Egger's test is \bold{strictly superior} to the threshold value indicated in \code{egger_p}, the class for which this value is indicated can be reached.
\item \code{esb_p}: a p-value of a test for excess of statistical significance bias (ESB). If the p-value  of the test is \bold{strictly superior} to the threshold value indicated in \code{esb_p}, the class for which this value is indicated can be reached.
\item \code{JK_p}: the largest p-value obtained in the jackknife meta-analysis (JK). If the largest p-value obtained in the jackknife meta-analysis is \bold{strictly inferior} to the threshold value indicated in \code{JK_p}, the class for which this value is indicated can be reached.
\item \code{pi}: a "notnull" value indicates that users request the 95\% prediction interval of the meta-analysis to exclude the null value to achieve the class for which it is indicated.
\item \code{largest_CI}: a "notnull" value indicates that users request the 95\% confidence interval of the largest study included in the meta-analysis to exclude the null value to achieve the class for which it is indicated.
}
}
}
\examples{
### perform calculations required for an umbrella review
df <- subset(df.SMD, factor == "Surgical")
umb.full <- umbrella(df)

### stratify evidence according to the Ioannidis classification
evid_ioannidis <- add.evidence(umb.full, criteria = "Ioannidis")
summary(evid_ioannidis)

### stratify evidence according to the Personalized classification with
### the number of studies and cases, the inconsistency as criteria.
### - a class I can be reached if the number of studies is > 10, the number of cases is > 500 and
###   the I2 is < 25\%.
### - a class II can be reached if the number of studies is > 5, the number of cases is > 400 and
###   the I2 is < 50\%.
### - a class III can be reached if the number of cases is > 300 and the I2 is < 75\%.
### - a class IV can be reached if the number of cases is > 100.
### - else, if the number of cases is <= 100, a class V is assigned.
evid_perso1 <- add.evidence(umb.full, criteria = "Personalized",
   class_I = c(n_studies = 10, n_cases = 500, I2 = 25),
   class_II = c(n_studies = 5, n_cases = 400, I2 = 50),
   class_III = c(n_cases = 300, I2 = 75),
   class_IV = c(n_cases = 100))
summary(evid_perso1)
}
\references{
Fusar-Poli, P., & Radua, J. (2018). Ten simple rules for conducting umbrella reviews. \emph{Evidence-Based Mental Health}, \bold{21}, 95-100.
}
\seealso{
\code{\link{umbrella}()} for conducting an umbrella review.
}
