## ----echo = FALSE, warning = FALSE, results = 'hide'--------------------------
library(metaumbrella)
library(DT)

## ----eval=FALSE---------------------------------------------------------------
#  # perform the calculations
#  umb <- umbrella(df.OR, verbose = FALSE)
#  
#  # plot the results
#  forest(umb)

## ----echo=FALSE, warning=FALSE, fig.width = 8, fig.height = 6-----------------
umb <- metaumbrella:::.quiet(umbrella(df.OR))
metaumbrella:::.quiet(forest(umb))

## ----eval=FALSE---------------------------------------------------------------
#  forest(umb,
#         measure = "OR", # display eOR instead of eG,
#         rightlab = "OR + 95% CI",
#         xlab = "Odds Ratio",
#         smlab = "Umbrella review of \nrisk factors for NDD" # title of the plot
#         )

## ----echo=FALSE, warning=FALSE, fig.width = 8, fig.height = 7-----------------
metaumbrella:::.quiet(forest(umb,
       measure = "OR", # display eOR instead of eG,
       rightlab = "OR + 95% CI",
       xlab = "Odds Ratio",
      smlab = "Umbrella review of \nrisk factors for NDD" # title of the plot
      )) 

## ----eval=FALSE---------------------------------------------------------------
#  forest(umb,
#         measure = "OR", # display eOR instead of eG,
#         rightlab = "OR + 95% CI",
#         xlab = "Odds Ratio",
#         weight.study = "same",
#         smlab = "Umbrella review of \nrisk factors for NDD" # title of the plot
#         )
#  

## ----echo=FALSE, warning=FALSE, fig.width = 8, fig.height = 7-----------------
metaumbrella:::.quiet(forest(umb,
       measure = "OR", # display eOR instead of eG,
       rightlab = "OR + 95% CI",
       xlab = "Odds Ratio",
       weight.study = "same",
       smlab = "Umbrella review of \nrisk factors for NDD" # title of the plot
       ) )

## ----eval=FALSE---------------------------------------------------------------
#  # perform the calculations
#  umb <- union.umbrella(umbrella(df.SMD), umbrella(df.HR))
#  
#  # stratify the evidence
#  strat.prso <- add.evidence(umb, criteria= "Personalized",
#                             class_I = c(n_studies = 10, total_n = 3000, egger_p = .10, esb_p = .05),
#                             class_II = c(n_studies = 10, total_n = 2000, egger_p = .10),
#                             class_III = c(n_studies = 10, total_n = 1000, egger_p = .10),
#                             class_IV = c(n_studies = 10, total_n = 500, egger_p = .10))
#  
#  # plot the results
#  forest(strat.prso)

## ----echo = FALSE, warning = FALSE, fig.width = 8, fig.height = 6-------------
umb <- metaumbrella:::.quiet(union.umbrella(umbrella(df.SMD), umbrella(df.HR)))
strat.prso <- metaumbrella:::.quiet(add.evidence(umb, criteria = "Personalized",
                           class_I = c(n_studies = 10, total_n = 3000, egger_p = .10, esb_p = .05),
                           class_II = c(n_studies = 10, total_n = 2000, egger_p = .10),
                           class_III = c(n_studies = 10, total_n = 1000, egger_p = .10),
                           class_IV = c(n_studies = 10, total_n = 500, egger_p = .10)))
metaumbrella:::.quiet(forest(strat.prso))

## ----eval=FALSE---------------------------------------------------------------
#  forest(strat.prso,
#         leftcols = c("Factor", "Class", "n_studies", "total_n", "tau2",
#                      "effect.ci"),
#         leftlabs = c("Factor", "Class", "n-studies", "n-sample", "tau²",
#                      "eSMD + 95% CI"),
#            rightcols = FALSE,
#      )
#  

## ----echo = FALSE, warning = FALSE, fig.width = 8, fig.height = 6-------------
metaumbrella:::.quiet(forest(
  strat.prso,
  leftcols = c("Factor", "Class", "n_studies", "total_n", "tau2",
                    "effect.ci"),
 leftlabs = c("Factor", "Class", "n-studies", "n-sample", "tau²",
                    "eSMD + 95% CI"),
          rightcols = FALSE,
    ))

## ----eval=FALSE---------------------------------------------------------------
#  # perform the calculations
#  umb <- union.umbrella(union.umbrella(
#          union.umbrella(union.umbrella(
#            umbrella(df.SMD), umbrella(df.OR)),
#            umbrella(df.RR)), umbrella(df.IRR)),
#          umbrella(df.OR.multi, mult.level = TRUE))
#  
#  strat.prso <- add.evidence(umb, criteria = "Ioannidis")
#  
#  forest(strat.prso)

## ----echo=FALSE, warning=FALSE, fig.width = 8, fig.height = 8-----------------
umb <- metaumbrella:::.quiet(union.umbrella(union.umbrella(
        union.umbrella(
          union.umbrella(umbrella(df.SMD), umbrella(df.OR)),
          umbrella(df.RR)), umbrella(df.IRR)), umbrella(df.OR.multi, mult.level = TRUE)))

strat.prso <- metaumbrella:::.quiet(add.evidence(umb, criteria = "Ioannidis"))

metaumbrella:::.quiet(forest(strat.prso))

## ----eval=FALSE---------------------------------------------------------------
#  forest(strat.prso,
#         layout = "RevMan5")

## ----echo = FALSE, warning = FALSE, fig.width = 8, fig.height = 6-------------
metaumbrella:::.quiet(forest(strat.prso,
       layout = "RevMan5"))

## ----eval=FALSE---------------------------------------------------------------
#  forest(strat.prso,
#         layout = "RevMan5",
#         subgroup = "Class",
#         subgroup.name = "Class")

## ----echo = FALSE, warning = FALSE, fig.width = 8, fig.height = 6-------------
metaumbrella:::.quiet(forest(strat.prso,
       layout = "RevMan5",
       subgroup = "Class",
       subgroup.name = "Class"))

