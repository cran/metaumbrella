% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{df.train}
\alias{df.train}
\title{Training dataset}
\format{
The dataset contains the following variables: \tabular{ll}{
  \strong{comment} \tab comments on studies.\cr
  \tab \cr
  \strong{risk_factor} \tab name of the intervention studied.\cr
  \tab \cr
  \strong{author_study} \tab first study author of the individual studies.\cr
  \tab \cr
  \strong{year_publication_study} \tab year of publication of the individual studies.\cr
  \tab \cr
  \strong{type_of_effect_size} \tab type of effect size.\cr
  \tab \cr
  \strong{number_of_cases_exposed} \tab number of cases in the exposed group.\cr
  \tab \cr
  \strong{number_of_cases_non_exposed} \tab number of cases in the non-exposed group.\cr
  \tab \cr
  \strong{number_of_controls_exposed} \tab number of controls in the exposed group.\cr
  \tab \cr
  \strong{number_of_controls_non_exposed} \tab number of controls in the non-exposed group.\cr
  \tab \cr
  \strong{number_of_participants_exposed} \tab total number of participants in the exposed group.\cr
  \tab \cr
  \strong{number_of_participants_non_exposed} \tab total number of participants in the non-exposed group.\cr
  \tab \cr
  \strong{number_of_cases} \tab number of cases.\cr
  \tab \cr
  \strong{number_of_controls} \tab number of controls.\cr
  \tab \cr
  \strong{effect_size_value} \tab value of the effect size\cr
  \tab \cr
  \strong{low_bound_ci} \tab lower bound of the 95\% confidence interval.\cr
  \tab \cr
  \strong{up_bound_ci} \tab upper bound of the 95\% confidence interval.\cr
  \tab \cr
  \strong{time_disease_free} \tab total person-time at risk (sum of the person-time at risk in \cr
  \tab the exposed and non-exposed groups). \cr
  \tab \cr
  \strong{mean_of_intervention_group} \tab mean of the intervention group\cr
  \tab \cr
  \strong{mean_of_control_group} \tab mean of the control group\cr
  \tab \cr
  \strong{sd_of_intervention_group} \tab sd of the intervention group\cr
  \tab \cr
  \strong{sd_of_control_group} \tab sd of the control group\cr
}
}
\source{
No source, the data are entirely fictitious
}
\usage{
df.train
}
\description{
This is a non-formatted dataset that is used in a vignette to illustrate how obtaining a well-formatted dataset
with the help of the \code{view.errors.umbrella()} function.
}
\keyword{datasets}
