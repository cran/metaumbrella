test_that("summary works for SMD measure", {
  umb <- .quiet(umbrella(df.SMD))
  sum <- summary.umbrella(umb)
  expect_equal(sum$n_studies, c(umb[[1]]$n$studies, umb[[2]]$n$studies))
  expect_equal(sum$n_cases, c(umb[[1]]$n$cases, umb[[2]]$n$cases))
  expect_equal(sum$n_controls, c(umb[[1]]$n$controls, umb[[2]]$n$controls))
  expect_equal(sum$value_CI,
               c(paste0("[", round(umb[[1]]$ma_results$ci_lo, 3), ", ", round(umb[[1]]$ma_results$ci_up, 3),  "]"),
               paste0("[", round(umb[[2]]$ma_results$ci_lo, 3), ", ", round(umb[[2]]$ma_results$ci_up, 3),  "]")))
})

test_that("summary works for OR measure", {
  umb <- .quiet(umbrella(df.OR))
  sum <- summary.umbrella(umb)
  expect_equal(sum$n_studies, c(umb[[1]]$n$studies, umb[[2]]$n$studies, umb[[3]]$n$studies, umb[[4]]$n$studies))
  expect_equal(sum$n_cases, c(umb[[1]]$n$cases, umb[[2]]$n$cases, umb[[3]]$n$cases, umb[[4]]$n$cases))
  expect_equal(sum$n_controls, c(umb[[1]]$n$controls, umb[[2]]$n$controls, umb[[3]]$n$controls, umb[[4]]$n$controls))
  expect_equal(sum$value_CI[1], paste0("[", round(exp(umb[[1]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[1]]$ma_results$ci_up), 3),  "]"))
  expect_equal(sum$value_CI[2], paste0("[", round(exp(umb[[2]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[2]]$ma_results$ci_up), 3),  "]"))
  expect_equal(sum$value_CI[3], paste0("[", round(exp(umb[[3]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[3]]$ma_results$ci_up), 3),  "]"))
  expect_equal(sum$value_CI[4], paste0("[", round(exp(umb[[4]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[4]]$ma_results$ci_up), 3),  "]"))
})

test_that("summary works for RR measure", {
  umb <- .quiet(umbrella(df.RR))
  sum <- summary.umbrella(umb)
  expect_equal(sum$n_studies, c(umb[[1]]$n$studies))
  expect_equal(sum$n_cases, c(umb[[1]]$n$cases))
  expect_equal(sum$n_controls, c(umb[[1]]$n$controls))
  expect_equal(sum$value_CI, paste0("[", round(exp(umb[[1]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[1]]$ma_results$ci_up), 3),  "]"))
})

test_that("summary works for HR measure", {
  umb <- .quiet(umbrella(df.HR))
  sum <- summary.umbrella(umb)
  expect_equal(sum$n_studies, c(umb[[1]]$n$studies, umb[[2]]$n$studies, umb[[3]]$n$studies, umb[[4]]$n$studies))
  expect_equal(sum$n_cases, c(umb[[1]]$n$cases, umb[[2]]$n$cases, umb[[3]]$n$cases, umb[[4]]$n$cases))
  expect_equal(sum$n_controls, c(umb[[1]]$n$controls, umb[[2]]$n$controls, umb[[3]]$n$controls, umb[[4]]$n$controls))
  expect_equal(sum$value_CI,
               c(paste0("[", round(exp(umb[[1]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[1]]$ma_results$ci_up), 3),  "]"),
                 paste0("[", round(exp(umb[[2]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[2]]$ma_results$ci_up), 3),  "]"),
                 paste0("[", round(exp(umb[[3]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[3]]$ma_results$ci_up), 3),  "]"),
                 paste0("[", round(exp(umb[[4]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[4]]$ma_results$ci_up), 3),  "]")))
})

test_that("summary works for IRR measure", {
  umb <- .quiet(umbrella(df.IRR))
  sum <- summary.umbrella(umb)
  expect_equal(sum$n_studies, c(umb[[1]]$n$studies))
  expect_equal(sum$n_cases, c(umb[[1]]$n$cases))
  expect_equal(sum$n_controls, c(umb[[1]]$n$controls))
  expect_equal(sum$value_CI, paste0("[", round(exp(umb[[1]]$ma_results$ci_lo), 3), ", ", round(exp(umb[[1]]$ma_results$ci_up), 3),  "]"))
})

