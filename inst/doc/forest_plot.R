## ---- echo = FALSE, warning = FALSE, results = 'hide'-------------------------
library(metaumbrella)
library(DT)

## ---- eval=FALSE--------------------------------------------------------------
#  # perform the calculations
#  umb <- umbrella(df.OR)
#  
#  # plot the results
#  forest(umb)

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 6----------------
umb <- metaumbrella:::.quiet(umbrella(df.OR))
metaumbrella:::.quiet(forest(umb))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(umb,
#         measure = "eOR", # display eOR instead of eG
#         main_value = "OR [95% CI]", # header of the column displaying the effect sizes
#         main_x_axis = "Odds Ratio (OR)", # title of the x axis
#         main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders" # title of the plot
#         )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 7----------------
metaumbrella:::.quiet(forest(umb,
                      measure = "eOR", # display eOR instead of eG
 main_value = "OR [95% CI]", # header of the column displaying the effect sizes
                main_x_axis = "Odds Ratio (OR)", # title of the x axis
                main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders" # title of the plot
                )) 

## ---- eval=FALSE--------------------------------------------------------------
#  forest(umb,
#         measure = "eOR",
#         main_value = "OR [95% CI]",
#         main_x_axis = "Odds Ratio (OR)",
#         main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders",
#         cex_title = 1.1, # size of the title, default set as 1.4
#  )
#  

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 7----------------
metaumbrella:::.quiet(forest(umb,
                measure = "eOR",
                main_value = "OR [95% CI]", 
                main_x_axis = "Odds Ratio (OR)",
                main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders",
                cex_title = 1.1, # size of the title
))


## ---- eval=FALSE--------------------------------------------------------------
#  forest(umb,
#         measure = "eOR",
#         main_value = "OR [95% CI]",
#         main_x_axis = "Odds Ratio (OR)",
#         main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders",
#         cex_title = 1.1,
#         log_cex_dots = TRUE # the natural log of the index of precision is used to scale the size of the dots
#  )
#  

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 7----------------
metaumbrella:::.quiet(forest(umb,
                measure = "eOR",
                main_value = "OR [95% CI]",
                main_x_axis = "Odds Ratio (OR)", 
                main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders", 
                cex_title = 1.1, 
                log_cex_dots = TRUE
))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(umb,
#         measure = "eOR",
#         main_value = "OR [95% CI]",
#                  main_x_axis = "Odds Ratio (OR)",
#                  main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders",
#                  cex_title = 1.1,
#                  fix_size_dots = 10 # fix the size of the dots
#  )
#  

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 7----------------
metaumbrella:::.quiet(forest(umb,
                measure = "eOR",
                main_value = "OR [95% CI]", 
                main_x_axis = "Odds Ratio (OR)", 
                main_title = "Umbrella review of risk factors for\n neurodevelopmental disorders",
                cex_title = 1.1, 
                fix_size_dots = 10
))

## ---- eval=FALSE--------------------------------------------------------------
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

## ---- echo = FALSE, warning = FALSE, fig.width = 7, fig.height = 6------------
umb <- metaumbrella:::.quiet(union.umbrella(umbrella(df.SMD), umbrella(df.HR)))
strat.prso <- metaumbrella:::.quiet(add.evidence(umb, criteria = "Personalized",
                           class_I = c(n_studies = 10, total_n = 3000, egger_p = .10, esb_p = .05),
                           class_II = c(n_studies = 10, total_n = 2000, egger_p = .10),
                           class_III = c(n_studies = 10, total_n = 1000, egger_p = .10),
                           class_IV = c(n_studies = 10, total_n = 500, egger_p = .10)))
metaumbrella:::.quiet(forest(strat.prso))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(strat.prso,
#         xlim = c(-8, 4), # this increases the size of the left area of the plot
#         xlim_factor = -6 # this moves the factors column to the left
#         )

## ---- echo = FALSE, warning = FALSE, fig.width = 7, fig.height = 6------------
metaumbrella:::.quiet(forest(strat.prso,
                xlim = c(-8, 4), # this increases the size of the left area of the plot
                xlim_factor = -6 # this moves the factor column to the left
))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(strat.prso,
#                  xlim = c(-8, 4),
#                  xlim_factor = -6,
#                  xlim_value = -3.5 # this moves the value column to the left
#                  )

## ---- echo = FALSE, warning = FALSE, fig.width = 7, fig.height = 6------------
metaumbrella:::.quiet(forest(strat.prso,
                xlim = c(-8, 4), # this increases the size of the left area of the plot
                xlim_factor = -6, # this moves the factors column to the left
                xlim_value = -3.5
)) 

## ---- eval = FALSE------------------------------------------------------------
#  forest(strat.prso,
#                  xlim = c(-8, 4),
#                  xlim_factor = -6,
#                  xlim_value = -3.5,
#                  pos_value = "center" # change the alignment of the value column
#                  )

## ---- echo = FALSE, fig.width = 7, fig.height = 6-----------------------------
metaumbrella:::.quiet(forest(strat.prso,
                xlim = c(-8, 4), 
                xlim_factor = -6, 
                xlim_value = -3.5,
                pos_value = "center"
))

## ---- eval = FALSE------------------------------------------------------------
#  forest(strat.prso,
#                  xlim = c(-8, 4),
#                  xlim_factor = -6,
#                  xlim_value = -3.5,
#                  pos_value = "center",
#                  pos_text = "center" # change the alignment of the factor column
#                  )

## ---- echo = FALSE, fig.width = 7, fig.height = 6-----------------------------
metaumbrella:::.quiet(forest(strat.prso,
                xlim = c(-8, 4), 
                xlim_factor = -6, 
                xlim_value = -3.5,
                pos_value = "center",
                pos_text = "center"
))

## ---- eval = FALSE------------------------------------------------------------
#  forest(strat.prso,
#                  xlim = c(-6.8, 2), # adjust the x-limits of the plot.
#                  xlim_factor = -6,
#                  xlim_value = -3.5,
#                  pos_value = "center",
#                  pos_text = "center"
#                  )

## ---- echo = FALSE, fig.width = 7, fig.height = 6-----------------------------
metaumbrella:::.quiet(forest(strat.prso,
                xlim = c(-6.8, 2), 
                xlim_factor = -6, 
                xlim_value = -3.5,
                pos_value = "center",
                pos_text = "center"
))

## ---- eval=FALSE--------------------------------------------------------------
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

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
umb <- metaumbrella:::.quiet(union.umbrella(union.umbrella(
        union.umbrella(
          union.umbrella(umbrella(df.SMD), umbrella(df.OR)),
          umbrella(df.RR)), umbrella(df.IRR)), umbrella(df.OR.multi, mult.level = TRUE)))

strat.prso <- metaumbrella:::.quiet(add.evidence(umb, criteria = "Ioannidis"))

metaumbrella:::.quiet(forest(strat.prso))

## -----------------------------------------------------------------------------
list_authors <- lapply(list(df.SMD, df.OR, df.RR, df.IRR, df.HR, df.OR.multi),
     function(x) cbind(author = unique(x$meta_review),
                       factor = unique(x$factor)))

authors <- do.call(rbind, list_authors)
authors

## ---- eval = FALSE------------------------------------------------------------
#  order_factors_plot <- forest(strat.prso)
#  
#  # this allows to obtain the order of the factors in the plot
#  order_factors <- order_factors_plot$factor$factor
#  
#  # now that we have the correct order, we simply have to reorganize the dataset according to this information
#  authors_ordered <- authors[match(order_factors, authors[, "factor"]), ]
#  authors_ordered

## ---- fig.show='hide', echo=FALSE, warning=FALSE------------------------------
order_factors_plot <- metaumbrella:::.quiet(forest(strat.prso))

order_factors <- order_factors_plot$factor$factor
order_factors

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
authors_ordered <- authors[match(order_factors, authors[, "factor"]), ]
authors_ordered

## ---- eval=FALSE--------------------------------------------------------------
#  # we create the variable displaying the number of studies and total sample size
#  sum <- summary(strat.prso)
#  
#  # as you can see, the order of the summary function and of the forest function is identical. We do not need to reorder this information.
#  stud_total_n = paste0(sum$n_cases)
#  
#  forest(strat.prso,
#         add_columns = data.frame(authors_ordered[, "author"], stud_total_n), # new columns
#         main_add_columns = c("Authors", "Studies\n(sample size)"), # name of the new columns
#         xlim_add_columns = c(-7.5, -5), # position of the new columns
#         xlim = c(-10, 4.5) # increase the width of the plot on the left side
#  )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
# we create the variable displaying the number of studies and total sample size 
sum <- summary(strat.prso)
# as you can see, the order of the summary function and of the forest function is identical. We do not need to reorder this information.
stud_total_n = sum$n_cases

metaumbrella:::.quiet(forest(strat.prso,
                add_columns = data.frame(authors_ordered[, "author"], stud_total_n), # new columns
                main_add_columns = c("Authors", "Studies\n(sample size)"), # name of the columns
                xlim_add_columns = c(-7.5, -5), # position of the new columns
                xlim = c(-10, 5), # increase the width of the plot on the left side
))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(strat.prso,
#                  add_columns = data.frame(authors_ordered[, "author"], stud_total_n),
#                  main_add_columns = c("Authors", "Studies\n(sample size)"),
#                  xlim_add_columns = c(-7.5, -5),
#                  xlim = c(-10, 5),
#                  pos_text = "center" # center the factors columns and the two columns added to the plot
#  )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
metaumbrella:::.quiet(forest(strat.prso,
                add_columns = data.frame(authors_ordered[, "author"], stud_total_n), 
                main_add_columns = c("Authors", "Cases"), 
                xlim_add_columns = c(-7.5, -5),
                xlim = c(-10, 5), # increase the width of the plot on the left side
                pos_text = "center"
                
))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(strat.prso,
#                  add_columns = data.frame(authors_ordered[, "author"], stud_total_n),
#                  main_add_columns = c("Authors", "Studies\n(sample size)"),
#                  xlim_add_columns = c(-9.5, -7), # adjust the x-axis position of the new columns
#                  xlim = c(-10, 5),
#                  pos_text = "center",
#                  xlim_factor = -4 # adjust the x-axis position of the factor column
#  )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
metaumbrella:::.quiet(forest(strat.prso,
                add_columns = data.frame(authors_ordered[, "author"], stud_total_n), # new columns
                main_add_columns = c("Authors", "Studies\n(sample size)"), # name of the columns
                xlim_add_columns = c(-9.5, -7), # position of the new columns
                xlim = c(-10, 5), # increase the width of the plot on the left side
                pos_text = "center", # center the factors columns and the two columns added to the plot
                xlim_factor = -4 # moves the factor column to the left
                
))

## ---- eval=FALSE--------------------------------------------------------------
#  forest(strat.prso,
#                  add_columns = data.frame(authors_ordered[, "author"], stud_total_n),
#                  main_add_columns = c("Authors", "Studies\n(sample size)"),
#                  xlim_add_columns = c(-9.5, -7),
#                  xlim = c(-10, 5),
#                  pos_text = "center",
#                  xlim_factor = -4,
#                  print.classes = c("I", "II", "III", "IV") # request to print only classes I to IV
#  
#  )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
metaumbrella:::.quiet(forest(strat.prso,
                add_columns = data.frame(authors_ordered[, "author"], stud_total_n), # new columns
                main_add_columns = c("Authors", "Studies\n(sample size)"), # name of the columns
                xlim_add_columns = c(-9.5, -7), # position of the new columns
                xlim = c(-10, 5), # increase the width of the plot on the left side
                pos_text = "center", # center the factors columns and the two columns added to the plot
                xlim_factor = -4, # moves the factor column to the left
                print.classes = c("I", "II", "III", "IV")
                
)) 

## ---- eval = FALSE------------------------------------------------------------
#  forest(strat.prso,
#                  add_columns = data.frame(authors_ordered[, "author"], stud_total_n),
#                  main_add_columns = c("Authors", "Studies\n(sample size)"),
#                  xlim_add_columns = c(-9.5, -7),
#                  xlim = c(-10, 5),
#                  pos_text = "center",
#                  xlim_factor = -4,
#                  ylim_correction_text = -.10,
#                  print.classes = c("I", "II", "III", "IV"),
#                  cex_text = 0.8, # reduce the size of the text of the factor and new columns
#                  cex_value = 0.8,# reduce the size of the text of the value column
#                  cex_text_header = 0.8, # reduce the size of the header of the factor and new columns
#                  cex_value_header = 0.8 # reduce the size of the header of the value column
#  
#  )

## ---- echo=FALSE, warning=FALSE, fig.width = 7, fig.height = 8----------------
metaumbrella:::.quiet(forest(strat.prso,
                add_columns = data.frame(authors_ordered[, "author"], stud_total_n), # new columns
                main_add_columns = c("Authors", "Studies\n(sample size)"), # name of the columns
                xlim_add_columns = c(-9.5, -7), # position of the new columns
                xlim = c(-10, 5), # increase the width of the plot on the left side
                pos_text = "center", # center the factors columns and the two columns added to the plot
                xlim_factor = -4, # moves the factor column to the left
                print.classes = c("I", "II", "III", "IV"),
                cex_text = 0.8,
                cex_value = 0.8,
                cex_text_header = 0.8,
                cex_value_header = 0.8

)) 

