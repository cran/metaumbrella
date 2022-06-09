tol_large = 1e-10

test_that(".check_data correctly detects input type", {
  a <- 1:10
  expect_error(.check_data(a), "The object passed to the umbrella function is not a data.frame object.", fixed = TRUE)
})

test_that(".check_data correctly detects multiple paper columns", {
  df <- df.SMD[1:2,]
  df$study <- paste0(df$author, "_", df$year)
  expect_message(message(attr(.check_data(df), "message")), "There are both a column called 'study' and a column called 'author' or 'year'.", fixed = TRUE)
})

test_that(".check_data correctly detects missing required columns 1", {
  df <- df.SMD[1:2,]
  df <- subset(df.SMD, select = -c(meta_review, factor, author, year, measure))
  expect_message(message(attr(.check_data(df), "message")),
                 "The following required variables are missing:", fixed = TRUE)
})
test_that(".check_data correctly detects missing required columns 2", {
  df <- df.SMD[1:2,]
  df <- subset(df.SMD, select = -c(n_cases, n_controls))
  expect_message(message(attr(.check_data(df), "message")),
                 "The following required variables are missing:", fixed = TRUE)
})
test_that(".check_data correctly detects wrong numeric inputs 1", {
  df <- df.SMD[1:2,]
  df$value[1] <- "4.32,3"
  expect_message(message(attr(.check_data(df), "message")), "A numeric cell in the column 'value'", fixed = TRUE)
})

test_that(".check_data correctly detects wrong numeric inputs 2", {
  df <- df.SMD[1:2,]
  df$n_cases[1] <- "?"
  df$n_controls[1] <- "!!"
  check <- grepl("The dataframe contains non-numeric characters while this is expected. Please check inputs.",
                 attr(suppressWarnings(.check_data(df)), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly removes rows with empty authors/study column", {
  df <- df.SMD[1:10, ]
  df$author[6:9] <- NA
  expect_message(message(attr(.check_data(df), "message")), "Some rows of the original dataset have been removed because they do not include any value in 'author' column: rows = ", fixed = TRUE)
  expect_equal(c(1:5, 10), attr(.check_data(df), "data")$row_index)
})

test_that(".fix_measure_name correctly rename measures", {
  df <- df.SMD[1:9, ]
  df$measure <- c("d", "Cohen d", "hr", "irr", "or", "odds ratio", "rr", "g", "md")

  actual_measure = attr(.check_data(df), "data")$measure

  expected_measure <- c("SMD", "SMD", "HR",  "IRR", "OR",
                        "OR",  "RR",  "G",   "MD")
  expect_equal(actual_measure, expected_measure)
})

test_that(".check_data correctly identify wrong measure names", {
  df <- df.SMD
  df$measure[3] <- "a"

  dferror <- attr(.check_data(df), "data")
  check <- grepl("Measure ' a ' not supported",
                 attr(.check_data(df), "message"), fixed = TRUE)
  check2 <- grepl("Measure ' a ' not supported", dferror[dferror$row_index == 3,]$column_errors,
                  fixed = TRUE)

  expect_equal(check, TRUE)
  expect_equal(check2, TRUE)
})

test_that(".check_data correctly identify missing measures", {
  df <- df.SMD
  df$measure[3] <- NA
  dferror <- attr(.check_data(df), "data")

  check <- grepl("Measure cannot be empty or NA.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  check2 <- grepl("Wrong measure: measure cannot be empty or NA. //", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)


  expect_equal(check, TRUE)
  expect_equal(check2, TRUE)
})

test_that(".check_data correctly identify wrong values", {
  df <- df.OR
  df$value[3] <- - 2
  df$ci_lo[3] <- - 4
  df$ci_up[3] <- 0
  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("Wrong value", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Wrong 'ci_lo'", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  verif3 <- grepl("Wrong 'ci_up'", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Wrong value. Values should be positive for 'OR', 'RR', 'HR' and 'IRR'.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(verif3, TRUE)
})

test_that(".check_data correctly identify wrong ci_lo values", {
  df <- df.SMD
  df$value[3] <- 2
  df$ci_lo[3] <- 3
  df$ci_up[3] <- 4
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Wrong 'ci_lo', should be lower than 'value' and should be > 0 with OR RR HR IRR.", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Lower bound of the 95% CI should be lower than 'value' and should not be negative or null with 'OR', 'RR', 'IRR' and 'HR' measures.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify wrong ci_up values", {
  df <- df.SMD
  df$value[3] <- 2
  df$ci_lo[3] <- 1
  df$ci_up[3] <- 1
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Wrong 'ci_up', should be higher than 'value' and should be > 0 with OR RR HR IRR", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Upper bound of the 95% CI should be higher than 'value' and should not be negative or null with 'OR', 'RR', 'IRR' and 'HR' measures.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify wrong rob values", {
  df <- df.SMD
  df$rob[1:4] <- c("Low", "High", "Moderate", "Unclear")
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Wrong risk of bias (rob) value. Should be 'high', 'unclear' or 'low'", dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Wrong risk of bias: should be either high/unclear/low",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify upper case rob values", {
  df <- df.SMD
  df$rob[1:3] <- c("Low", "High", "Unclear")
  dferror <- attr(.check_data(df), "data")

  check <- grepl("Your dataset is well formatted",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify wrong amstar values", {
  df <- df.SMD
  df$amstar[3] <- -3
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Wrong AMSTAR value. Should be a positive number.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Wrong AMSTAR value. Should be a positive number.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify wrong multiple_es values", {
  df <- df.SMD
  df$multiple_es[3] <- "multivariate"
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Wrong identifier for studies with multiple effect sizes:",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Wrong identifier for studies with multiple effect sizes",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify wrong reverse_es values", {
  df <- df.SMD
  df$reverse_es <- NA; df$reverse_es[3] <- "reversal"
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("identifier for studies in which the direction of the effect size must be reversed. Should be 'reverse'.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("Wrong identifier for studies in which the direction of the effect size must be reversed. Should be 'reverse'.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
})

test_that(".check_data correctly identify wrong sample size inputs: SMD", {
  df1 <- df.SMD
  df1$n_cases[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  verif1 <- grepl("the number of cases/controls is mandatory",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("the number of cases and controls is mandatory",
                 attr(.check_data(df1), "message"), fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)

  df2 <- df.SMD
  df2$n_controls[3] <- NA
  dferror2 <- attr(.check_data(df2), "data")
  verif2 <- grepl("the number of cases/controls is mandatory",
                  dferror2[dferror2$row_index == 3,]$column_errors, fixed = TRUE)
  check2 <- grepl("the number of cases and controls is mandatory",
                  attr(.check_data(df2), "message"), fixed = TRUE)

  expect_equal(verif2, TRUE)
  expect_equal(check2, TRUE)
})

test_that(".check_data correctly identify wrong sample size inputs: HR", {
  df1 <- df.HR
  df1$n_cases[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  verif1 <- grepl("For HR measure, the number of cases is mandatory",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("For HR measure, the number of cases is mandatory",
                  attr(.check_data(df1), "message"), fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)
})

test_that(".check_data correctly identify wrong sample size inputs: IRR", {
  df1 <- df.IRR
  df1$n_cases_exp[3] <- NA
  df1$n_cases[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  verif1 <- grepl("For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both.",
                  attr(.check_data(df1), "message"), fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)

  df2 <- df.IRR
  df2$n_cases_nexp[3] <- NA
  df2$n_cases[3] <- NA
  dferror2 <- attr(.check_data(df2), "data")
  verif2 <- grepl("For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both",
                  dferror2[dferror2$row_index == 3,]$column_errors, fixed = TRUE)
  check2 <- grepl("For IRR measure, only one group between (n_cases), and (n_cases_exp, n_cases_nexp) can be empty, not both.",
                  attr(.check_data(df2), "message"), fixed = TRUE)

  expect_equal(verif2, TRUE)
  expect_equal(check2, TRUE)

  df3 <- df.IRR
  df3$n_cases <- NA
  dferror3 <- attr(.check_data(df3), "data")
  check3 <- grepl("Your dataset is well formatted.",
                  attr(.check_data(dferror3), "message"), fixed = TRUE)

  expect_equal(check3, TRUE)

  df4 <- df.IRR
  df4$n_cases_exp <- NA
  df4$n_cases_nexp <- NA
  dferror4 <- attr(.check_data(df4), "data")
  check4 <- grepl("Your dataset is well formatted.",
                  attr(.check_data(dferror4), "message"), fixed = TRUE)

  expect_equal(check4, TRUE)
})

test_that(".check_data correctly identify wrong sample size inputs: OR", {
  df1 <- df.OR
  df1$n_cases[3] <- NA; #df1$n_controls[3] <- NA
  df1$n_exp[3] <- NA;   df1$n_nexp[3] <- NA
  df1$n_cases_exp[3] <- NA; df1$n_cases_nexp[3] <- NA
  df1$n_controls_exp[3] <- NA;  df1$n_controls_nexp[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  verif1 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated.",
                  attr(.check_data(df1), "message"), fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)

  df2 <- df.OR
  df2$n_controls[3] <- NA
  df2$n_exp[3] <- NA;   df2$n_nexp[3] <- NA
  df2$n_cases_exp[3] <- NA; df2$n_cases_nexp[3] <- NA
  df2$n_controls_exp[3] <- NA;  df2$n_controls_nexp[3] <- NA
  dferror2 <- attr(.check_data(df2), "data")
  verif2 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror2[dferror2$row_index == 3,]$column_errors, fixed = TRUE)
  check2 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df2), "message"), fixed = TRUE)

  expect_equal(verif2, TRUE)
  expect_equal(check2, TRUE)

  df3 <- df.OR
  df3$n_cases[3] <- NA; df3$n_controls[3] <- NA
  df3$n_exp[3] <- NA;   #df3$n_nexp[3] <- NA
  df3$n_cases_exp[3] <- NA; df3$n_cases_nexp[3] <- NA
  df3$n_controls_exp[3] <- NA;  df3$n_controls_nexp[3] <- NA
  dferror3 <- attr(.check_data(df3), "data")
  verif3 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror3[dferror3$row_index == 3,]$column_errors, fixed = TRUE)
  check3 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df3), "message"), fixed = TRUE)

  expect_equal(verif3, TRUE)
  expect_equal(check3, TRUE)


  df4 <- df.OR
  df4$n_cases[3] <- NA; df4$n_controls[3] <- NA
  df4$n_nexp[3] <- NA # df4$n_exp[3] <- NA;
  df4$n_cases_exp[3] <- NA; df4$n_cases_nexp[3] <- NA
  df4$n_controls_exp[3] <- NA;  df4$n_controls_nexp[3] <- NA
  dferror4 <- attr(.check_data(df4), "data")
  verif4 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror4[dferror4$row_index == 3,]$column_errors, fixed = TRUE)
  check4 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df4), "message"), fixed = TRUE)

  expect_equal(verif4, TRUE)
  expect_equal(check4, TRUE)

  df5 <- df.OR
  df5$n_cases[3] <- NA; df5$n_controls[3] <- NA
  df5$n_exp[3] <- NA;   df5$n_nexp[3] <- NA
  df5$n_cases_exp[3] <- NA; #df5$n_cases_nexp[3] <- NA
  df5$n_controls_exp[3] <- NA;  df5$n_controls_nexp[3] <- NA
  dferror5 <- attr(.check_data(df5), "data")
  verif5 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror5[dferror5$row_index == 3,]$column_errors, fixed = TRUE)
  check5 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df5), "message"), fixed = TRUE)

  expect_equal(verif5, TRUE)
  expect_equal(check5, TRUE)

  df6 <- df.OR
  df6$n_cases[3] <- NA; df6$n_controls[3] <- NA
  df6$n_exp[3] <- NA;   df6$n_nexp[3] <- NA
  df6$n_cases_nexp[3] <- NA #df6$n_cases_exp[3] <- NA;
  df6$n_controls_exp[3] <- NA;  df6$n_controls_nexp[3] <- NA
  dferror6 <- attr(.check_data(df6), "data")
  verif6 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror6[dferror6$row_index == 3,]$column_errors, fixed = TRUE)
  check6 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df6), "message"), fixed = TRUE)

  expect_equal(verif6, TRUE)
  expect_equal(check6, TRUE)

  df7 <- df.OR
  df7$n_cases[3] <- NA; df7$n_controls[3] <- NA
  df7$n_exp[3] <- NA;   df7$n_nexp[3] <- NA
  df7$n_cases_exp[3] <- NA; df7$n_cases_nexp[3] <- NA
  df7$n_controls_exp[3] <- NA;  # df7$n_controls_nexp[3] <- NA
  dferror7 <- attr(.check_data(df7), "data")
  verif7 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror7[dferror7$row_index == 3,]$column_errors, fixed = TRUE)
  check7 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df7), "message"), fixed = TRUE)

  expect_equal(verif7, TRUE)
  expect_equal(check7, TRUE)

  df8 <- df.OR
  df8$n_cases[3] <- NA; df8$n_controls[3] <- NA
  df8$n_exp[3] <- NA;   df8$n_nexp[3] <- NA
  df8$n_cases_exp[3] <- NA; df8$n_cases_nexp[3] <- NA
  df8$n_controls_nexp[3] <- NA
  dferror8 <- attr(.check_data(df8), "data")
  verif8 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror8[dferror8$row_index == 3,]$column_errors, fixed = TRUE)
  check8 <- grepl("For OR measure, one group between (n_cases, n_controls) / (n_exp, n_nexp) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df8), "message"), fixed = TRUE)

  expect_equal(verif8, TRUE)
  expect_equal(check8, TRUE)

  df9 <- df.OR
  df9$n_cases[3] <- NA; df9$n_controls[3] <- NA
  dferror9 <- attr(.check_data(df9), "data")
  check9 <- grepl("Your dataset is well formatted.",
                  attr(.check_data(dferror9), "message"), fixed = TRUE)

  expect_equal(check9, TRUE)

  df10 <- df.OR
  df10$n_exp[3] <- NA;   df10$n_nexp[3] <- NA
  dferror10 <- attr(.check_data(df10), "data")
  check10 <- grepl("Your dataset is well formatted.",
                  attr(.check_data(dferror10), "message"), fixed = TRUE)

  expect_equal(check10, TRUE)

  df11 <- df.OR
  df11$n_cases_exp[3] <- NA; df11$n_cases_nexp[3] <- NA
  df11$n_controls_exp[3] <- NA;  df11$n_controls_nexp[3] <- NA
  dferror11 <- attr(.check_data(df11), "data")
  check11 <- grepl("Your dataset is well formatted.",
                  attr(.check_data(dferror11), "message"), fixed = TRUE)

  expect_equal(check11, TRUE)
})

test_that(".check_data correctly identify wrong sample size inputs: RR", {
  df1 <- df.RR
  df1$n_cases_exp[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  verif1 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df1), "message"), fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)


  df2 <- df.RR
  df2$n_cases_nexp[3] <- NA
  dferror2 <- attr(.check_data(df2), "data")
  verif2 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror2[dferror2$row_index == 3,]$column_errors, fixed = TRUE)
  check2 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df2), "message"), fixed = TRUE)

  expect_equal(verif2, TRUE)
  expect_equal(check2, TRUE)



  df3 <- df.RR
  df3$n_exp[3] <- NA; df3$n_controls[3] <- NA
  dferror3 <- attr(.check_data(df3), "data")
  verif3 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror3[dferror3$row_index == 3,]$column_errors, fixed = TRUE)
  check3 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df3), "message"), fixed = TRUE)

  expect_equal(verif3, TRUE)
  expect_equal(check3, TRUE)

  df4 <- df.RR
  df4$n_nexp[3] <- NA
  dferror4 <- attr(.check_data(df4), "data")
  verif4 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  dferror4[dferror4$row_index == 3,]$column_errors, fixed = TRUE)
  check4 <- grepl("For RR measure, one group between (n_cases, n_controls) / (n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp) has to be indicated",
                  attr(.check_data(df4), "message"), fixed = TRUE)

  expect_equal(verif4, TRUE)
  expect_equal(check4, TRUE)

  df7 <- df.RR
  df7$n_cases[3] <- NA;
  dferror7 <- attr(.check_data(df7), "data")
  check7 <- grepl("Your dataset is well formatted.",
                   attr(.check_data(dferror7), "message"), fixed = TRUE)

  expect_equal(check7, TRUE)

})




test_that(".check_data correctly identify non symmetric CI", {
  df1 <- df.SMD
  df2 <- df.OR
  df1$ci_lo[20] <- 1.05
  df2$ci_lo[20] <- 2.2

  dferror1 <- attr(.check_data(df1), "data")
  dferror2 <- attr(.check_data(df2), "data")
  verif1 <- grepl("Non-symmetric confidence interval",
                  dferror1[dferror1$row_index == 20,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Non-symmetric confidence interval",
                  dferror2[dferror2$row_index == 20,]$column_errors, fixed = TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)

  check1 <- grepl("The confidence interval is not symmetric around the effect size",
                 attr(.check_data(df1), "message"), fixed = TRUE)
  check2 <- grepl("The confidence interval is not symmetric around the effect size",
                 attr(.check_data(df2), "message"), fixed = TRUE)
  expect_equal(check1, TRUE)
  expect_equal(check2, TRUE)
})

test_that(".check_data correctly identify missing information for applying shared_nexp / shared_controls", {
  df1 <- subset(df.OR, select = -c(n_exp, n_nexp, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  df1$shared_nexp <- c(1,1,1,2:(nrow(df1)-2))
  df2 <- subset(df.OR, select = -c(n_cases, n_controls, n_cases_exp, n_cases_nexp, n_controls_exp, n_controls_nexp))
  df2$shared_controls <- c(1,1,1,2:(nrow(df2)-2))
  df3 <- subset(df.OR, select = -c(n_cases, n_controls, n_exp, n_nexp))
  df3$shared_controls <- c(1,1,1,2:(nrow(df3)-2))
  df4 <- subset(df.OR, select = -c(n_cases, n_controls, n_exp, n_nexp))
  df4$shared_nexp <- c(1,1,1,2:(nrow(df4)-2))

  dferror1 <- attr(.check_data(df1), "data")
  dferror2 <- attr(.check_data(df2), "data")
  dferror3 <- attr(.check_data(df3), "data")
  dferror4 <- attr(.check_data(df4), "data")

  check1 <- grepl("The number of participants in the non-exposed group(s) (resp. in the control group(s)) is mandatory when 'shared_nexp' (resp. 'shared_control') is indicated.",
                 attr(.check_data(df1), "message"), fixed = TRUE)

  check2 <- grepl("The number of participants in the non-exposed group(s) (resp. in the control group(s)) is mandatory when 'shared_nexp' (resp. 'shared_control') is indicated.",
                 attr(.check_data(df2), "message"), fixed = TRUE)

  check3 <- grepl("Your dataset is well formatted",
                 attr(.check_data(df3), "message"), fixed = TRUE)

  check4 <- grepl("Your dataset is well formatted",
                 attr(.check_data(df4), "message"), fixed = TRUE)

  expect_equal(check1, TRUE)
  expect_equal(check2, TRUE)
  expect_equal(check3, TRUE)
  expect_equal(check4, TRUE)

  verif1 <- grepl("Missing n_controls / n_nexp while shared_controls / shared_nexp is indicated.",
                  unique(dferror1$column_errors), fixed = TRUE)
  verif2 <- grepl("Missing n_controls / n_nexp while shared_controls / shared_nexp is indicated.",
                  unique(dferror2$column_errors), fixed = TRUE)
  verif3 <- is.na(unique(dferror3$column_errors))
  verif4 <- is.na(unique(dferror4$column_errors))

  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(verif3, TRUE)
  expect_equal(verif4, TRUE)
})

test_that(".check_data correctly identify both shared_nexp/n_controls entry", {
  df <- df.SMD
  df$shared_controls <- c(1,1,1,2:(nrow(df)-2))
  df$shared_nexp <- c(1,1,1,2:(nrow(df)-2))

  dferror <- attr(.check_data(df), "data")

  verif <- grepl("The 'shared_nexp' and 'shared_controls' columns cannot be indicated for the same study.",
                 unique(dferror$column_errors), fixed = TRUE)

  expect_equal(verif, TRUE)
})

test_that(".check_data correctly prevents users to apply a shared corrections with specific measures", {
  df1 <- df.IRR
  df2 <- df.RR
  df3 <- df.HR
  df4 <- df.HR
  df5 <- df.SMD
  df6 <- df.SMD; df6$measure <- "MD"
  df7 <- df.SMD; df6$measure <- "G"
  df1$shared_controls <- c(1,1,1,2:(nrow(df1)-2))
  df2$shared_controls <- c(1,1,1,2:(nrow(df2)-2))
  df3$shared_controls <- c(1,1,1,2:(nrow(df3)-2))
  df4$shared_nexp <- c(1,1,1,2:(nrow(df4)-2))
  df5$shared_nexp <- c(1,1,1,2:(nrow(df5)-2))
  df6$shared_nexp <- c(1,1,1,2:(nrow(df6)-2))
  df7$shared_nexp <- c(1,1,1,2:(nrow(df7)-2))
  dferror1 <- attr(.check_data(df1), "data")
  dferror2 <- attr(.check_data(df2), "data")
  dferror3 <- attr(.check_data(df3), "data")
  dferror4 <- attr(.check_data(df4), "data")
  dferror5 <- attr(.check_data(df5), "data")
  dferror6 <- attr(.check_data(df6), "data")
  dferror7 <- attr(.check_data(df7), "data")

  verif1 <- grepl("The 'shared_controls' column cannot be indicated with a 'RR' or 'IRR' measure",
                  unique(dferror1$column_errors), fixed = TRUE)
  verif2 <- grepl("The 'shared_controls' column cannot be indicated with a 'RR' or 'IRR' measure",
                  unique(dferror2$column_errors), fixed = TRUE)
  verif3 <- grepl("The 'shared_nexp' / 'shared_controls' columns cannot be indicated with a 'HR', 'R' or 'Z' measures",
                  unique(dferror3$column_errors), fixed = TRUE)
  verif4 <- grepl("The 'shared_nexp' / 'shared_controls' columns cannot be indicated with a 'HR', 'R' or 'Z' measures",
                  unique(dferror4$column_errors), fixed = TRUE)
  verif5 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                  unique(dferror5$column_errors), fixed = TRUE)
  verif6 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                  unique(dferror6$column_errors), fixed = TRUE)
  verif7 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                  unique(dferror7$column_errors), fixed = TRUE)

  check1 <- grepl("The 'shared_controls' column is only supported for the SMD, SMC, MD, G or OR measure",
                  attr(.check_data(df1), "message"), fixed = TRUE)
  check2 <- grepl("The 'shared_controls' column is only supported for the SMD, SMC, MD, G or OR measure",
                  attr(.check_data(df2), "message"), fixed = TRUE)
  check3 <- grepl("The 'shared_nexp' and 'shared_controls' columns are not supported in combination with the HR, R and Z measures",
                  attr(.check_data(df3), "message"), fixed = TRUE)
  check4 <- grepl("The 'shared_nexp' and 'shared_controls' columns are not supported in combination with the HR, R and Z measures",
                  attr(.check_data(df4), "message"), fixed = TRUE)

  check5 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                  attr(.check_data(df5), "message"), fixed = TRUE)
  check6 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                  attr(.check_data(df6), "message"), fixed = TRUE)
  check7 <- grepl("The 'shared_nexp' column cannot be indicated with a 'SMD', 'SMC', 'MD', or 'G' measure",
                 attr(.check_data(df7), "message"), fixed = TRUE)

  expect_equal(check1, TRUE)
  expect_equal(check2, TRUE)
  expect_equal(check3, TRUE)
  expect_equal(check4, TRUE)
  expect_equal(check5, TRUE)
  expect_equal(check6, TRUE)
  expect_equal(check7, TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(verif3, TRUE)
  expect_equal(verif4, TRUE)
  expect_equal(verif5, TRUE)
  expect_equal(verif6, TRUE)
  expect_equal(verif7, TRUE)
})


test_that(".check_data correctly identify not indicated multiple entry", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("Study with same factor, author and year. If it is not an erroneous repeated entry, specify whether the multiple effect sizes come from multiple 'groups' or 'outcomes' in the 'multiple_es' column",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Study with same factor, author and year. If it is not an erroneous repeated entry, specify whether the multiple effect sizes come from multiple 'groups' or 'outcomes' in the 'multiple_es' column",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  check <- grepl("Some repeated studies (author and year) in the same factor do not have any 'multiple_es' value.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify indicated multiple entries", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31, 33)] <- "groups"

  dferror1 <- attr(.check_data(df), "data")
  check1 <- grepl("Your dataset is well formatted",
                 attr(.check_data(df), "message"), fixed = TRUE)
  verif1 <- all(is.na(dferror1$column_errors))
  expect_equal(verif1, TRUE)
  expect_equal(check1, TRUE)

  df$multiple_es[c(31,33)] <- "outcomes"
  dferror2 <- attr(.check_data(df), "data")
  check2 <- grepl("Your dataset is well formatted",
                  attr(.check_data(df), "message"), fixed = TRUE)
  verif2 <- all(is.na(dferror2$column_errors))
  expect_equal(verif2, TRUE)
  expect_equal(check2, TRUE)
})

test_that(".check_data correctly identify multiple 'multiple_es' values per study", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31)] <- "groups"
  df$multiple_es[c(33)] <- "outcomes"

  dferror <- attr(.check_data(df), "data")
  check <- grepl("Study with several multiple_es values. Please, specify only one of either 'groups' or 'outcomes'",
                  attr(.check_data(df), "message"), fixed = TRUE)

  verif1 <- grepl("Study with several multiple_es values. Please, specify only one of either 'groups' or 'outcomes'",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Study with several multiple_es values. Please, specify only one of either 'groups' or 'outcomes'",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify multiple 'r' values per study", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31)] <- "outcomes"
  df$multiple_es[c(33)] <- "outcomes"
  df$r <- NA; df$r[33] <- 0.5; df$r[31] <- 0.4;

  dferror <- attr(.check_data(df), "data")
  check <- grepl("Study with several r values. Please, specify only one unique value per study.",
                 attr(.check_data(df), "message"), fixed = TRUE)

  verif1 <- grepl("Study with several r values. Please, specify only one unique value per study.",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Study with several r values. Please, specify only one unique value per study.",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly detects multiple 'reverse_es' values per study", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31, 33)] <- "groups"
  df$reverse_es <- NA
  df$reverse_es[c(31)] <- "reverse"

  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("Some but not all effect sizes are reversed for this factor. Check this is what you want",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Some but not all effect sizes are reversed for this factor. Check this is what you want",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  check <- grepl("Some but not all effect sizes are reversed for a factor. Check this is what you want (this is only a warning, not an error)",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
})

test_that(".check_data correctly identify multiple 'shared_controls' values per multivariate study", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31)] <- "groups"
  df$multiple_es[c(33)] <- "groups"
  df$shared_controls <- 1:nrow(df)

  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("Study associated with multiple 'shared_controls' or 'shared_nexp' values. There should be an unique value.",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Study associated with multiple 'shared_controls' or 'shared_nexp' values. There should be an unique value.",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  check <- grepl("A study is associated with multiple 'shared_controls' or 'shared_nexp' values. There should be an unique value per study",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify varying amstar values", {
  df <- df.SMD
  df$amstar[3] <- 2
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Values of amstar should not differ within a factor. Please insert a unique value for each factor.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)

  check <- grepl("AMSTAR values should be constant within a factor.",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data correctly identify varying reverse values", {
  df <- df.SMD
  df$reverse_es <- "reverse"
  df$reverse_es[3] <- NA
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Some but not all effect sizes are reversed for this factor. Check this is what you want.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  check <- grepl("Some but not all effect sizes are reversed for a factor. Check this is what you want (this is only a warning, not an error)",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
  expect_equal(verif, TRUE)
})


test_that(".check_data correctly identify shared_control groups with another study", {
  df <- df.SMD
  df$author[33] <- df$author[31]
  df$year[33] <- df$year[31]
  df$multiple_es <- NA
  df$multiple_es[c(31)] <- "groups"
  df$multiple_es[c(33)] <- "groups"
  df$shared_controls <- 1:nrow(df)
  df$shared_controls[c(30,31,33)] <- 40
  df$shared_controls[c(1:25)] <- NA

  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study.",
                  dferror[dferror$row_index == 30,]$column_errors, fixed = TRUE)
  verif2 <- grepl("It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study.",
                  dferror[dferror$row_index == 31,]$column_errors, fixed = TRUE)
  verif3 <- grepl("It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study.",
                  dferror[dferror$row_index == 33,]$column_errors, fixed = TRUE)

  check <- grepl("It is not currently possible to have a study with multiple independent subgroups that shares a control group with another study",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
  expect_equal(verif3, TRUE)
})

test_that(".check_data correctly converts sample sizes", {
  df1 <- subset(df.OR, select = -c(n_cases, n_controls, n_exp, n_nexp))
  dfcheck <- attr(.check_data(df1), "data")
  dfcheck <- dfcheck[order(dfcheck$row_index), ]
  expect_equal(dfcheck$n_cases, df.OR$n_cases)
  expect_equal(dfcheck$n_controls, df.OR$n_controls)
  expect_equal(dfcheck$n_exp, df.OR$n_exp)
  expect_equal(dfcheck$n_nexp, df.OR$n_nexp)

  df2 <- subset(df.IRR, select = -c(time))
  dfcheck <- attr(.check_data(df2), "data")
  dfcheck <- dfcheck[order(dfcheck$row_index), ]
  expect_equal(dfcheck$time, df.IRR$time)
})

test_that(".check_data correctly estimates value from CI", {
  df1 <- df.SMD
  df2 <- df.OR
  df1$value <- NA
  df2$value <- NA

  dfcheck1 <- attr(.check_data(df1), "data")
  dfcheck1 <- dfcheck1[order(dfcheck1$row_index), ]
  dfcheck2 <- attr(.check_data(df2), "data")
  dfcheck2 <- dfcheck2[order(dfcheck2$row_index), ]

  verif1 <- all(df.SMD$value == dfcheck1$value)
  verif2 <- all(df.OR$value == dfcheck2$value)

  expect_equal(df.SMD$value, dfcheck1$value, tolerance = tol_large)
  expect_equal(df.OR$value, dfcheck2$value, tolerance = tol_large)
})

test_that(".check_data correctly estimates CI from value+ CI", {
  df1 <- df.SMD
  df2 <- df.OR
  df1$ci_lo <- NA
  df2$ci_up <- NA

  dfcheck1 <- attr(.check_data(df1), "data")
  dfcheck1 <- dfcheck1[order(dfcheck1$row_index), ]
  dfcheck2 <- attr(.check_data(df2), "data")
  dfcheck2 <- dfcheck2[order(dfcheck2$row_index), ]

  expect_equal(df.SMD$ci_lo, dfcheck1$ci_lo, tolerance = tol_large)
  expect_equal(df.OR$ci_up, dfcheck2$ci_up, tolerance = tol_large)
})

test_that(".check_data detects missing information for SMD: 1", {
  df <- df.SMD
  df$value[3] <- df$ci_lo[3] <- df$mean_cases[3] <- NA
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Unknown value of the SMD / means and SD.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  check <- grepl("SMD measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for SMD: 2", {
  df <- df.SMD
  df$value[3] <- df$se[3] <- df$mean_cases[3] <- NA
  check <- grepl("Your dataset is well formatted",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for SMD: 2", {
  df <- df.SMD
  df$ci_up[3] <- df$ci_lo[3] <- df$se[3] <- df$mean_cases[3] <- NA
  check <- grepl("Your dataset is well formatted",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for MD", {
  df <- df.SMD
  df$measure = "MD"
  df$value[3] <- df$sd_controls[3] <- df$ci_up[3] <- NA
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Unknown value + (confidence interval / se / var) of the MD.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  check <- grepl("MD measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})


test_that(".check_data detects missing information for MD: 2", {
  df <- df.SMD
  df$measure = "MD"
  df$ci_up[3] <- df$ci_lo[3] <- df$se[3] <- df$mean_cases[3] <- NA
  check <- grepl("MD measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for MD: 3", {
  df <- df.SMD
  df$measure = "MD"
  df$ci_up[3] <- df$ci_lo[3] <- df$mean_cases[3] <- NA
  check <- grepl("Your dataset is well formatted",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for HR", {
  df <- df.HR
  df$ci_lo[3] <- df$ci_up[3] <- NA
  dferror <- attr(.check_data(df), "data")
  verif <- grepl("Unknown value + (confidence interval / se / var) of the HR.",
                 dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  check <- grepl("HR measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(verif, TRUE)
  expect_equal(check, TRUE)
})

test_that(".check_data detects missing information for HR2", {
  df <- df.HR
  df$var <- 5
  df$ci_lo[3] <- df$ci_up[3] <- NA
  check <- grepl("Your dataset is well",
                 attr(.check_data(df), "message"), fixed = TRUE)
  expect_equal(check, TRUE)
})


test_that(".check_data detects missing CI and (n_exp|n_nexp|n_cases_exp|n_cases_nexp|n_controls_exp|n_controls_nexp) for RR IRR", {
  # RR
  df <- subset(df.RR, select = -c(n_exp, n_nexp))
  df$ci_lo[3] <- df$ci_up[3] <- NA
  dferror <- attr(.check_data(df), "data")
  verif1 <- grepl("Unknown value + (confidence interval / se / var) of the RR or unknown 2x2 table.",
                  dferror[dferror$row_index == 3,]$column_errors, fixed = TRUE)
  check <- grepl("RR measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df), "message"), fixed = TRUE)

  expect_equal(check, TRUE)
  expect_equal(verif1, TRUE)

  # IRR
  df <- subset(df.IRR, select = -c(time))
  df1 <- df2 <- df
  df1$ci_lo[3] <- df1$ci_up[3] <- df1$time_exp[3] <- NA
  df2$ci_lo[3] <- df2$ci_up[3] <- df2$time_nexp[3] <- NA
  dferror1 <- attr(.check_data(df1), "data")
  dferror2 <- attr(.check_data(df2), "data")
  verif1 <- grepl("Unknown (value + (ci/se/var) + n_cases + time) OR number of cases and time for both exp and non-exp groups.",
                  dferror1[dferror1$row_index == 3,]$column_errors, fixed = TRUE)
  verif2 <- grepl("Unknown (value + (ci/se/var) + n_cases + time) OR number of cases and time for both exp and non-exp groups.",
                  dferror2[dferror2$row_index == 3,]$column_errors, fixed = TRUE)
  check1 <- grepl("IRR measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df1), "message"), fixed = TRUE)
  check2 <- grepl("IRR measure is not associated with sufficient information to run the umbrella review",
                 attr(.check_data(df2), "message"), fixed = TRUE)

  expect_equal(check1, TRUE)
  expect_equal(check2, TRUE)
  expect_equal(verif1, TRUE)
  expect_equal(verif2, TRUE)
})

