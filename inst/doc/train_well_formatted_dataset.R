## ---- echo = FALSE------------------------------------------------------------
library(metaumbrella)
library(DT)

## ---- eval = FALSE------------------------------------------------------------
#  df.train

## ---- echo = FALSE------------------------------------------------------------
DT::datatable(df.train, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))


## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## -----------------------------------------------------------------------------
# rename columns
names(df.train)[names(df.train) == "risk_factor"] <- "factor"
names(df.train)[names(df.train) == "author_study"] <- "author"
names(df.train)[names(df.train) == "year_publication_study"] <- "year"
names(df.train)[names(df.train) == "type_of_effect_size"] <- "measure"

df.train$meta_review[df.train$factor %in% c("risk_factor_1", "risk_factor_2", "risk_factor_3")] <- "Smith (2020)"
df.train$meta_review[df.train$factor %in% c("risk_factor_4")] <- "Jones (2018)"
df.train$meta_review[df.train$factor %in% c("risk_factor_5")] <- "De Martino (2015)"


## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## ---- echo = FALSE------------------------------------------------------------
d <- view.errors.umbrella(df.train, return = "data")
DT::datatable(d, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## -----------------------------------------------------------------------------
df.train[is.na(df.train$measure), ]$measure <- "SMD"

## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## ---- echo = FALSE------------------------------------------------------------
d <- view.errors.umbrella(df.train, return = "data")
DT::datatable(d, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## -----------------------------------------------------------------------------
names(df.train)[names(df.train) == "number_of_cases_exposed"] <- "n_cases_exp"
names(df.train)[names(df.train) == "number_of_cases_non_exposed"] <- "n_cases_nexp" 
names(df.train)[names(df.train) == "number_of_controls_exposed"] <- "n_controls_exp" 
names(df.train)[names(df.train) == "number_of_controls_non_exposed"] <- "n_controls_nexp" 

names(df.train)[names(df.train) == "number_of_participants_exposed"] <- "n_exp" 
names(df.train)[names(df.train) == "number_of_participants_non_exposed"] <- "n_nexp"

names(df.train)[names(df.train) == "number_of_cases"] <- "n_cases" 
names(df.train)[names(df.train) == "number_of_controls"] <- "n_controls" 

## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## ---- echo = FALSE------------------------------------------------------------
d <- view.errors.umbrella(df.train, return = "data")
DT::datatable(d[d$column_errors != "", ], options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## -----------------------------------------------------------------------------
names(df.train)[names(df.train) == "effect_size_value"] <- "value"
names(df.train)[names(df.train) == "low_bound_ci"] <- "ci_lo" 
names(df.train)[names(df.train) == "up_bound_ci"] <- "ci_up" 
names(df.train)[names(df.train) == "time_disease_free"] <- "time" 

## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## ---- echo = FALSE------------------------------------------------------------
d <- view.errors.umbrella(df.train, return = "data")
DT::datatable(d, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## -----------------------------------------------------------------------------
names(df.train)[names(df.train) == "mean_of_intervention_group"] <- "mean_cases"
names(df.train)[names(df.train) == "mean_of_control_group"] <- "mean_controls" 
names(df.train)[names(df.train) == "sd_of_intervention_group"] <- "sd_cases" 
names(df.train)[names(df.train) == "sd_of_control_group"] <- "sd_controls" 

## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## -----------------------------------------------------------------------------
df.train$multiple_es <- df.train$r <- NA

df.train[which(duplicated(paste(df.train$author, df.train$year)) | duplicated(paste(df.train$author, df.train$year), fromLast = TRUE)), ]$multiple_es <- "outcomes"

df.train[which(duplicated(paste(df.train$author, df.train$year)) | duplicated(paste(df.train$author, df.train$year), fromLast = TRUE)), ]$r <- .60

## -----------------------------------------------------------------------------
errors <- view.errors.umbrella(df.train)

## -----------------------------------------------------------------------------
umb <- umbrella(df.train, mult.level = TRUE, method.var = "REML")

## ---- eval = FALSE------------------------------------------------------------
#  forest(umb)

## ---- echo = FALSE, fig.height=7, fig.width=7---------------------------------
metaumbrella:::.quiet(forest(umb))

## ---- eval = FALSE------------------------------------------------------------
#  df.train

## ---- echo = FALSE------------------------------------------------------------
DT::datatable(df.train, options = list(  
    scrollX = TRUE,
    dom = c('pt'),
    ordering = FALSE,
    scrollY = "300px", 
    pageLength = 100,
    columnDefs = list(
                  list(width = '130px',
                       targets = "_all"),
                  list(className = 'dt-center', 
                                     targets = "_all"))))

## -----------------------------------------------------------------------------
df.train$reverse_es <- NA

df.train[df.train$factor %in% c("risk_factor_1", "risk_factor_3"), ]$reverse_es <- "reverse"

## ---- eval = FALSE------------------------------------------------------------
#  umb <- umbrella(df.train, mult.level = TRUE)
#  forest(umb)

## ---- echo = FALSE, fig.height=7, fig.width=7---------------------------------
umb <- metaumbrella:::.quiet(umbrella(df.train, mult.level = TRUE))
metaumbrella:::.quiet(forest(umb))

## -----------------------------------------------------------------------------
df.train$shared_nexp <- NA
df.train$shared_nexp[22:23] <- "el-Neman"

## ---- eval = FALSE------------------------------------------------------------
#  umb <- umbrella(df.train, mult.level = TRUE)
#  evid <- add.evidence(umb, "GRADE")
#  forest(umb)

## ---- echo = FALSE, fig.height=7, fig.width=7---------------------------------
umb <- metaumbrella:::.quiet(umbrella(df.train, mult.level = TRUE))
metaumbrella:::.quiet(forest(umb))

