#' Meta-analyses exploring the efficacy of surgical and pharmacological interventions.
#'
#' Fictitious dataset of two meta-analyses of RCTs assessing the efficacy of surgical
#'  and pharmacological interventions on a numeric outcome.
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the intervention studied.\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (SMD).\cr
#'   \tab \cr
#'   \strong{value} \tab SMD value.\cr
#'   \tab \cr
#'   \strong{se} \tab standard error of the SMD.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{mean_cases} \tab means of patients in the experimental arm.\cr
#'   \tab \cr
#'   \strong{mean_controls} \tab means of patients in the control arm.\cr
#'   \tab \cr
#'   \strong{sd_cases} \tab standard deviations of patients in the experimental arm.\cr
#'   \tab \cr
#'   \strong{sd_controls} \tab standard deviations of patients in the control arm.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of patients in the experimental arm.\cr
#'   \tab \cr
#'   \strong{n_controls} \tab number of patients in the control arm.\cr
#'   \tab \cr
#'   \strong{rob} \tab risk of bias of the individual studies.\cr
#'   \tab \cr
#'   \strong{amstar} \tab AMSTAR score of the meta-analysis.\cr
#' }
#' @source No source, the data are entirely fictitious
"df.SMD"

#' Meta-analyses exploring a risk factor for neurodevelopmental disorders.
#'
#' Fictitious dataset of four meta-analyses of cross-sectional studies assessing a risk factor for neurodevelopmental disorders.
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the neurodevelopmental disorders on which the effect of the risk factor\cr
#'   \tab is studied\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (OR).\cr
#'   \tab \cr
#'   \strong{value} \tab OR value.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of cases (sum of the number of cases in the exposed\cr
#'   \tab and non-exposed groups).\cr
#'   \tab \cr
#'   \strong{n_controls} \tab number of controls (sum of the number of controls in the exposed\cr
#'   \tab and non-exposed groups).\cr
#'   \tab \cr
#'   \strong{n_exp} \tab number of participants in the exposed group (sum of the number of cases and\cr
#'   \tab controls in the exposed group).\cr
#'   \tab \cr
#'   \strong{n_nexp} \tab number of participants in the non-exposed group (sum of the number of cases\cr
#'   \tab and controls in the non-exposed group).\cr
#'   \tab \cr
#'   \strong{n_cases_exp} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_exp} \tab number of controls in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_cases_nexp} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_nexp} \tab number of controls in the non-exposed group.\cr
#' }
#' @source No source, the data are entirely fictitious
"df.OR"

#' Meta-analysis of RCTs assessing different dietary interventions on a binary outcome.
#'
#' Fictitious dataset including meta-analyses with dependent effect sizes.
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the intervention studied.\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (OR).\cr
#'   \tab \cr
#'   \strong{value} \tab OR value.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of cases (sum of the number of cases in the exposed and\cr
#'   \tab non-exposed groups).\cr
#'   \tab \cr
#'   \strong{n_controls} \tab number of controls (sum of the number of controls in the exposed\cr
#'   \tab and non-exposed groups).\cr
#'   \tab \cr
#'   \strong{n_cases_exp} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_exp} \tab number of controls in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_cases_nexp} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_nexp} \tab number of controls in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{multiple_es} \tab indicates the reason of the presence for multiple effect sizes\cr
#'   \tab (due to multiple groups or outcomes) per study.\cr
#' }
#' @source No source, the data are entirely fictitious
"df.OR.multi"

#' Meta-analysis of the adverse events of antidepressants.
#'
#' Fictitious dataset of a meta-analysis of cohort studies assessing the risks of adverse outcomes when taking selective serotonin reuptake inhibitors (SSRIs) therapy.
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the type of antidepressant studied.\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (RR).\cr
#'   \tab \cr
#'   \strong{value} \tab RR value.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases_exp} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_exp} \tab number of participants in the exposed group (sum of the number of cases and\cr
#'   \tab controls in the exposed group).\cr
#'   \tab \cr
#'   \strong{n_cases_nexp} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{n_nexp} \tab number of participants in the non-exposed group (sum of the number of cases\cr
#'   \tab and controls in the non-exposed group).\cr
#' }
#' @source No source, the data are entirely fictitious
"df.RR"

#' Meta-analysis exploring adverse events of smoking.
#'
#' Fictitious dataset of a meta-analysis of prospective cohorts assessing adverse effects of smoking on one binary outcome.
#'
#' @format The dataset contains the following variables:\tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the factor (only one factor is included).\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (IRR).\cr
#'   \tab \cr
#'   \strong{value} \tab IRR value.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of cases (sum of the number of cases in the exposed and non-exposed\cr
#'   \tab groups).\cr
#'   \tab \cr
#'   \strong{n_cases_exp} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_cases_nexp} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{time} \tab total person-time at risk (sum of the person-time at risk in the exposed and\cr
#'   \tab non-exposed groups).\cr
#'   \tab \cr
#'   \strong{time_exp} \tab person-time at risk in the exposed group.\cr
#'   \tab \cr
#'   \strong{time_nexp} \tab person-time at risk in the non-exposed group.\cr
#'}
#' @source No source, the data are entirely fictitious
"df.IRR"

#' Meta-analyses exploring the efficacy of several interventions on a binary outcome.
#'
#' Fictitious dataset of four meta-analyses of RCTs assessing the efficacy of yoga, aerobic training,
#' resistance training and mindfulness on a binary outcome
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the intervention studied.\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size (HR).\cr
#'   \tab \cr
#'   \strong{value} \tab HR value.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of cases.\cr
#'   \tab \cr
#'   \strong{n_controls} \tab number of controls.\cr
#'}
#' @source No source, the data are entirely fictitious
"df.HR"

#' Training dataset
#'
#' This is a non-formatted dataset that is used in a vignette to illustrate how obtaining a well-formatted dataset
#' with the help of the \code{view.errors.umbrella()} function.
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{comment} \tab comments on studies.\cr
#'   \tab \cr
#'   \strong{risk_factor} \tab name of the intervention studied.\cr
#'   \tab \cr
#'   \strong{author_study} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year_publication_study} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{type_of_effect_size} \tab type of effect size.\cr
#'   \tab \cr
#'   \strong{number_of_cases_exposed} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_cases_non_exposed} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_controls_exposed} \tab number of controls in the exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_controls_non_exposed} \tab number of controls in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_participants_exposed} \tab total number of participants in the exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_participants_non_exposed} \tab total number of participants in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{number_of_cases} \tab number of cases.\cr
#'   \tab \cr
#'   \strong{number_of_controls} \tab number of controls.\cr
#'   \tab \cr
#'   \strong{effect_size_value} \tab value of the effect size\cr
#'   \tab \cr
#'   \strong{low_bound_ci} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{up_bound_ci} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{time_disease_free} \tab total person-time at risk (sum of the person-time at risk in \cr
#'   \tab the exposed and non-exposed groups). \cr
#'   \tab \cr
#'   \strong{mean_of_intervention_group} \tab mean of the intervention group\cr
#'   \tab \cr
#'   \strong{mean_of_control_group} \tab mean of the control group\cr
#'   \tab \cr
#'   \strong{sd_of_intervention_group} \tab sd of the intervention group\cr
#'   \tab \cr
#'   \strong{sd_of_control_group} \tab sd of the control group\cr
#'}
#' @source No source, the data are entirely fictitious
"df.train"


#' Meta-analyses exploring the risk factors for posttraumatic stress disorder.
#'
#' Real dataset taken from Tortella-Feliu et al. (2019).
#'
#' @format The dataset contains the following variables: \tabular{ll}{
#'   \strong{meta_review} \tab name of the first author of the meta-analysis.\cr
#'   \tab \cr
#'   \strong{factor} \tab name of the risk factor.\cr
#'   \tab \cr
#'   \strong{author} \tab first study author of the individual studies.\cr
#'   \tab \cr
#'   \strong{year} \tab year of publication of the individual studies.\cr
#'   \tab \cr
#'   \strong{multiple_es} \tab indicates the reason of the presence of multiple effect sizes \cr
#'   \tab (due to multiple groups or outcomes) per study.\cr
#'   \tab \cr
#'   \strong{measure} \tab type of effect size.\cr
#'   \tab \cr
#'   \strong{value} \tab value of the effect size.\cr
#'   \tab \cr
#'   \strong{ci_lo} \tab lower bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{ci_up} \tab upper bound of the 95\% confidence interval.\cr
#'   \tab \cr
#'   \strong{n_cases} \tab number of cases.\cr
#'   \tab \cr
#'   \strong{n_controls} \tab number of controls.\cr
#'   \tab \cr
#'   \strong{n_exp} \tab number of participants in the exposed group (sum of the number of cases\cr
#'   \tab and controls in the exposed group).\cr
#'   \tab \cr
#'   \strong{n_nexp} \tab number of participants in the non-exposed group (sum of the number of cases\cr
#'   \tab and controls in the non-exposed group).\cr
#'   \tab \cr
#'   \strong{n_cases_exp} \tab number of cases in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_exp} \tab number of controls in the exposed group.\cr
#'   \tab \cr
#'   \strong{n_cases_nexp} \tab number of cases in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{n_controls_nexp} \tab number of controls in the non-exposed group.\cr
#'   \tab \cr
#'   \strong{mean_cases} \tab means of participants in the experimental arm.\cr
#'   \tab \cr
#'   \strong{sd_cases} \tab standard deviation of participants in the experimental arm.\cr
#'   \tab \cr
#'   \strong{mean_controls} \tab means of participants in the control arm.\cr
#'   \tab \cr
#'   \strong{sd_controls} \tab standard deviation of participants in the control arm.\cr
#'   \tab \cr
#'   \strong{amstar} \tab AMSTAR score of the meta-analysis\cr
#'   }
#' @source Tortella-Feliu, M. and Fullana, M.A., Perez-Vigil, A., Torres, X., Chamorro, J., and Littarelli, S.A., ..., & Radua, J. (2019). Risk Factors for Posttraumatic Stress Disorder: An Umbrella Review of Systematic Reviews and Meta-Analyses.
#' \emph{Neuroscience & Biobehavioral Reviews}, \bold{107}, 154--165.
"df.radua2019"
