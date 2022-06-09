## ---- echo = FALSE, warning = FALSE, results = 'hide'-------------------------
library(metaumbrella)
library(DT)

## ---- eval=FALSE--------------------------------------------------------------
#  umb <- umbrella(df.SMD)

## ---- eval=FALSE--------------------------------------------------------------
#  umb <- umbrella(df.SMD)
#  strat.io <- add.evidence(umb.SMD, criteria = "Ioannidis")

## ---- eval=FALSE--------------------------------------------------------------
#  umb <- umbrella(df.SMD)
#  strat.grd <- add.evidence(umb.SMD, criteria = "GRADE")

## ---- eval=FALSE--------------------------------------------------------------
#  umb <- umbrella(df.SMD)
#  strat.prso <- add.evidence(umb, criteria = "Personalized",
#                           class_I = c(total_n = 600, I2 = 25, rob = 75),
#                           class_II = c(total_n = 400, I2 = 50, rob = 50),
#                           class_III = c(total_n = 200, I2 = 75, rob = 25),
#                           class_IV = c(total_n = 100))

## ---- eval=FALSE--------------------------------------------------------------
#  umb <- umbrella(df.SMD)
#  strat.io <- add.evidence(umb.SMD, criteria = "Ioannidis")
#  forest(strat.io)

## ---- eval=FALSE--------------------------------------------------------------
#  df.OR

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
DT::datatable(df.OR, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  umb.OR <- umbrella(df.OR)
#  summary(umb.OR)

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
umb.OR <- metaumbrella:::.quiet(umbrella(df.OR))

DT::datatable(summary(umb.OR), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  strat.io <- add.evidence(umb.OR, criteria = "Ioannidis")
#  summary(strat.io)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
strat.io <- metaumbrella:::.quiet(add.evidence(umb.OR, criteria = "Ioannidis"))

DT::datatable(summary(strat.io), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval = FALSE------------------------------------------------------------
#  forest(strat.io,
#         measure = "eOR",
#         main_title = "umbrella review of risk factors \nfor neurodevelopmental disorders")

## ---- fig.width = 7, fig.height = 7.2, echo=FALSE, warning=FALSE--------------
metaumbrella:::.quiet(forest(strat.io,
       measure = "eOR",
       main_title = "umbrella review of risk factors \nfor neurodevelopmental disorders"))

## ---- eval=FALSE--------------------------------------------------------------
#  df.RR

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
DT::datatable(df.RR, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 30,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  umb.RR <- umbrella(df.RR)
#  summary(umb.RR)

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
umb.RR <- metaumbrella:::.quiet(umbrella(df.RR))

DT::datatable(summary(umb.RR), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 2,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  strat.grade <- add.evidence(umb.RR, criteria = "GRADE")
#  summary(strat.grade)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
strat.grade <- metaumbrella:::.quiet(add.evidence(umb.RR, criteria = "GRADE"))

DT::datatable(summary(strat.grade), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ----eval = FALSE-------------------------------------------------------------
#  forest(strat.grade,
#         measure = "eOR",
#         main_title = "umbrella review of adverse events\n of SSRI treatment.")

## ----echo = FALSE, fig.width = 7, fig.height = 7------------------------------
metaumbrella:::.quiet(forest(strat.grade,
       measure = "eOR",
       main_title = "umbrella review of adverse events\n of SSRI treatment."))

## ---- eval=FALSE--------------------------------------------------------------
#  df.SMD

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
DT::datatable(df.SMD, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 50,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  umb.SMD <- umbrella(df.SMD)
#  summary(umb.SMD)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
umb.SMD <- metaumbrella:::.quiet(umbrella(df.SMD))

DT::datatable(summary(umb.SMD), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  strat.pers1 <- add.evidence(umb.SMD, criteria = "Personalized",
#                              class_I = c(n_cases = 800),
#                              class_II = c(n_cases = 500),
#                              class_III = c(n_cases = 200),
#                              class_IV = c(n_cases = 100))

## ---- eval=FALSE--------------------------------------------------------------
#  strat.pers1 <- add.evidence(umb.SMD, criteria = "Personalized",
#                              class_I = c(n_cases = 800, esb_p = .10),
#                              class_II = c(n_cases = 500, esb_p = .05),
#                              class_III = c(n_cases = 200, esb_p = .01),
#                              class_IV = c(n_cases = 100))

## ---- eval = FALSE------------------------------------------------------------
#  strat.pers1 <- add.evidence(umb.SMD, criteria = "Personalized",
#                              class_I = c(n_cases = 800, esb_p = .10, rob = 80),
#                              class_II = c(n_cases = 500, esb_p = .05, rob = 65),
#                              class_III = c(n_cases = 200, esb_p = .01, rob = 50),
#                              class_IV = c(n_cases = 100, rob = 35))

## ---- eval=FALSE--------------------------------------------------------------
#  summary(strat.pers1)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
strat.pers1 <- metaumbrella:::.quiet(add.evidence(umb.SMD, criteria = "Personalized",
                            class_I = c(n_cases = 800, esb_p = .10, rob = 80),
                            class_II = c(n_cases = 500, esb_p = .05, rob = 65),
                            class_III = c(n_cases = 200, esb_p = .01, rob = 50),
                            class_IV = c(n_cases = 100, rob = 35)))

DT::datatable(summary(strat.pers1), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ----eval = FALSE-------------------------------------------------------------
#  forest(strat.pers1,
#         measure = "eG",
#         main_title = "Umbrella review of pharmacological and surgical\n treatments on a numeric outcome.")

## ---- echo = FALSE, fig.width = 7, fig.height = 7.2---------------------------
metaumbrella:::.quiet(forest(strat.pers1,
       measure = "eG",
       main_title = "Umbrella review of pharmacological and surgical\n treatments on a numeric outcome."))

## ---- eval=FALSE--------------------------------------------------------------
#  df.OR.multi

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
DT::datatable(df.OR.multi, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering=FALSE,
    scrollY = "300px", 
    pageLength = 40,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ---- eval=FALSE--------------------------------------------------------------
#  df.OR.multi$r <- NA # we initialize the r column in the dataset
#  df.OR.multi[df.OR.multi$author == "Godebu", ]$r <- .30 # we indicate a mean correlation of .30 for the study of Godebu
#  
#  # option 1: we specify - via the r argument of the umbrella function - that all studies with multiple outcomes
#  # but no r values in the dataset are assigned with a correlation of .60.
#  umb.OR.multi_1 <- umbrella(df.OR.multi, mult.level = TRUE, r = 0.6)
#  
#  # option 2: we manually specify - via the r argument of the dataset - the correlation for other studies
#  df.OR.multi[df.OR.multi$multiple_es == "outcomes" &
#                !is.na(df.OR.multi$multiple_es) &
#                !df.OR.multi$author %in% c("Godebu"), ]$r <- .60
#  # you no longer have to specify the r value in the umbrella function as it is already specified for all studies in the dataset
#  umb.OR.multi_2 <- umbrella(df.OR.multi, mult.level = TRUE)
#  
#  # as usual, you can obtain results of the calculations using the summary command
#  summary(umb.OR.multi_2)
#  
#  # check: you can check results are equal regardless of the method used
#  all(summary(umb.OR.multi_1) == summary(umb.OR.multi_2), na.rm = TRUE)
#  

## ---- echo=FALSE, message=FALSE-----------------------------------------------
df.OR.multi$r <- NA # we initialize the r column in the dataset
df.OR.multi[df.OR.multi$author == "Godebu", ]$r <- .30 # we indicate a mean correlation of .30 for the study of Godebu

# option 1: we specify - via the r argument of the umbrella function - that all studies with multiple outcomes
# but no r values in the dataset are assigned with a correlation of .60.
umb.OR.multi_1 <- metaumbrella:::.quiet(umbrella(df.OR.multi, mult.level = TRUE, r = 0.6))

# option 2: we manually specify - via the r argument of the dataset - the correlation for other studies
df.OR.multi[df.OR.multi$multiple_es == "outcomes" &
                !is.na(df.OR.multi$multiple_es) &
                !df.OR.multi$author %in% c("Godebu"), ]$r <- .60
# you no longer have to specify the r value in the umbrella function as it is already specified for all studies in the dataset

# you no longer have to specify the r value in the umbrella function as it is already specified for all studies in the dataset
umb.OR.multi_2 <- metaumbrella:::.quiet(umbrella(df.OR.multi, mult.level = TRUE))


DT::datatable(summary(umb.OR.multi_2), options = list(
    scrollX = TRUE,
    dom = c('t'),
    ordering = FALSE,
    pageLength = 5,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))


paste0("all(summary(umb.OR.multi_1) == summary(umb.OR.multi_2), na.rm = TRUE) returns " , all(summary(umb.OR.multi_1) == summary(umb.OR.multi_2), na.rm = TRUE))

## ---- eval = FALSE------------------------------------------------------------
#  strat.pers2 <- add.evidence(umb.OR.multi_1, criteria = "Personalized",
#                              class_I = c(I2 = 20),
#                              class_II = c(I2 = 40),
#                              class_III = c(I2 = 60),
#                              class_IV = c(I2 = 80))

## ---- eval = FALSE------------------------------------------------------------
#  strat.pers2 <- add.evidence(umb.OR.multi_1, criteria = "Personalized",
#                              class_I = c(I2 = 20, egger_p = .10),
#                              class_II = c(I2 = 40, egger_p = .10),
#                              class_III = c(I2 = 60, egger_p = .05),
#                              class_IV = c(I2 = 80, egger_p = .05))

## ---- eval = FALSE------------------------------------------------------------
#  strat.pers2 <- add.evidence(umb.OR.multi_1, criteria = "Personalized",
#                              class_I = c(I2 = 20, egger_p = .10, largest_CI = "notnull"),
#                              class_II = c(I2 = 40, egger_p = .10, largest_CI = "notnull"),
#                              class_III = c(I2 = 60, egger_p = .05, largest_CI = "notnull"),
#                              class_IV = c(I2 = 80, egger_p = .05))
#  

## ---- eval = FALSE------------------------------------------------------------
#  strat.pers2 <- add.evidence(umb.OR.multi_1, criteria = "Personalized",
#                              class_I = c(I2 = 20, egger_p = .10, largest_CI = "notnull", imprecision = 0.2),
#                              class_II = c(I2 = 40, egger_p = .10, largest_CI = "notnull", imprecision = 0.4),
#                              class_III = c(I2 = 60, egger_p = .05, largest_CI = "notnull", imprecision = 0.6),
#                              class_IV = c(I2 = 80, egger_p = .05, imprecision = 0.8))

## ---- eval=FALSE--------------------------------------------------------------
#  summary(strat.pers2)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
strat.pers2 <- metaumbrella:::.quiet(add.evidence(umb.OR.multi_1, criteria = "Personalized",
                            class_I = c(I2 = 20, egger_p = .10, largest_CI = "notnull", imprecision = 0.2),
                            class_II = c(I2 = 40, egger_p = .10, largest_CI = "notnull", imprecision = 0.4),
                            class_III = c(I2 = 60, egger_p = .05, largest_CI = "notnull", imprecision = 0.6),
                            class_IV = c(I2 = 80, egger_p = .05, imprecision = 0.8)))

DT::datatable(summary(strat.pers2), options = list(
    scrollX = TRUE,
    scrollY = "600px", 
    dom = c('t'),
    ordering = FALSE,
    columnDefs = list(
                  list(width = '110px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## ----eval = FALSE-------------------------------------------------------------
#  forest(strat.pers2, measure = "eOR")

## ---- echo = FALSE, fig.width = 7, fig.height = 7.2---------------------------
metaumbrella:::.quiet(forest(strat.pers2,  measure = "eOR"))

