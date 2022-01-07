# Multiple groups / outcomes: effect size estimates
test_that("agg.data correctly aggregates studies with multiple effect size estimates: SMD", {
  df <- df.SMD;   df$factor <- "Pharma"; df$amstar <- 7

  df$author[c(2,3, 5,6,7, 15,16,17)] <- c(df$author[1], df$author[1],
                                          df$author[4], df$author[4], df$author[4],
                                          df$author[14], df$author[14], df$author[14])

  df$year[c(2,3, 5,6,7, 15,16,17)] <- c(df$year[1], df$year[1],
                                        df$year[4], df$year[4], df$year[4],
                                        df$year[14], df$year[14], df$year[14])

  df$multiple_es <- NA ; df$multiple_es[c(1,2,3, 4,5,6,7)] <- "groups"
  df$multiple_es[c(14,15,16,17)] <- "outcomes"

  df.format.transit <- .quiet(.format_dataset(attr(.check_data(df), "data"), mult.level = TRUE))
  df.format <- attr(df.format.transit, "data_mult")

  df.agg.umb <- .agg_data(df.format, r = 0.5, measure = "SMD")

  df.agg.umb <- df.agg.umb[order(df.agg.umb$author, df.agg.umb$year), ]

  df.agg.mfr <- metafor::escalc(yi = value, sei = se, data = df.format)

  df.agg.subgroups <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "groups"),
                                                cluster = author,
                                                struct = "ID",
                                                weighted = TRUE)

  df.agg.outcomes <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "outcomes"),
                                               cluster = author,
                                               struct = "CS",
                                               weighted = FALSE,
                                               rho = 0.5)
  df.mfr <- rbind(subset(df.agg.mfr, is.na(multiple_es)),
                  df.agg.subgroups,
                  df.agg.outcomes)

  df.mfr <- df.mfr[order(df.mfr$author, df.mfr$year), ]

  expect_equal(df.agg.umb$value, as.numeric(as.character(df.mfr$yi)), tolerance = 1e-9)
  expect_equal(df.agg.umb$se, sqrt(df.mfr$vi), tolerance = 1e-9)
})


test_that("agg.data correctly aggregates studies with multiple effect size estimates: OR", {
  df <- df.OR.multi

  df.format.transit <- .quiet(.format_dataset(attr(.check_data(df), "data"), mult.level = TRUE))

  df.format <- attr(df.format.transit, "data_mult")

  df.agg.umb <- .agg_data(df.format, r = 0.5, measure = "OR")

  df.agg.umb <- df.agg.umb[order(df.agg.umb$author, df.agg.umb$year), ]

  df.agg.mfr <- metafor::escalc(yi = log(value), sei = se, data = df.format)

  df.agg.subgroups <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "groups"),
                                                cluster = paste0(author, year),
                                                struct = "ID",
                                                weighted = TRUE)

  df.agg.outcomes <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "outcomes"),
                                               cluster = paste0(author, year),
                                               struct = "CS",
                                               weighted = FALSE,
                                               rho = 0.5)

  df.mfr <- rbind(subset(df.agg.mfr, is.na(multiple_es)),
                  df.agg.subgroups,
                  df.agg.outcomes)

  df.mfr <- df.mfr[order(df.mfr$author, df.mfr$year), ]

  expect_equal(df.agg.umb$value, exp(as.numeric(as.character(df.mfr$yi))), tolerance = 1e-15)
  expect_equal(df.agg.umb$se, sqrt(df.mfr$vi), tolerance = 1e-15)
})

test_that("agg.data correctly aggregates studies with multiple effect size estimates: RR", {
  df <- df.OR.multi
  df$measure <- "RR"
  df.format.transit <- .quiet(.format_dataset(attr(.check_data(df), "data"), mult.level = TRUE))
  df.format <- attr(df.format.transit, "data_mult")

  df.agg.umb <- .agg_data(df.format, r = 0.5, measure = "RR")

  df.agg.umb <- df.agg.umb[order(df.agg.umb$author, df.agg.umb$year), ]

  df.agg.mfr <- metafor::escalc(yi = log(value), sei = se, data = df.format)

  df.agg.subgroups <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "groups"),
                                                cluster = author,
                                                struct = "ID",
                                                weighted = TRUE)

  df.agg.outcomes <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "outcomes"),
                                               cluster = author,
                                               struct = "CS",
                                               weighted = FALSE,
                                               rho = 0.5)
  df.mfr <- rbind(subset(df.agg.mfr, is.na(multiple_es)),
                  df.agg.subgroups,
                  df.agg.outcomes)

  df.mfr <- df.mfr[order(df.mfr$author, df.mfr$year), ]

  expect_equal(df.agg.umb$value, exp(as.numeric(as.character(df.mfr$yi))), tolerance = 1e-15)
  expect_equal(df.agg.umb$se, sqrt(df.mfr$vi), tolerance = 1e-15)
})

test_that("agg.data correctly aggregates studies with multiple effect size estimates: IRR", {
  df <- df.IRR
  df$measure <- "IRR"
  df$author[c(2,3, 5,6,7, 15,16)] <- c(df$author[1], df$author[1],
                                          df$author[4], df$author[4], df$author[4],
                                          df$author[14], df$author[14])

  df$year[c(2,3, 5,6,7, 15,16)] <- c(df$year[1], df$year[1],
                                        df$year[4], df$year[4], df$year[4],
                                        df$year[14], df$year[14])

  df$multiple_es <- NA ; df$multiple_es[c(1,2,3, 4,5,6,7)] <- "groups"
  df$multiple_es[c(14,15,16)] <- "outcomes"


  df.format.transit <- .quiet(.format_dataset(attr(.check_data(df), "data"), mult.level = TRUE))
  df.format <- attr(df.format.transit, "data_mult")

  df.agg.umb <- .agg_data(df.format, r = 0.5, measure = "IRR")

  df.agg.umb <- df.agg.umb[order(df.agg.umb$author, df.agg.umb$year), ]

  df.agg.mfr <- metafor::escalc(yi = log(value), sei = se, data = df.format)

  df.agg.subgroups <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "groups"),
                                                cluster = author,
                                                struct = "ID",
                                                weighted = TRUE)

  df.agg.outcomes <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "outcomes"),
                                               cluster = author,
                                               struct = "CS",
                                               weighted = FALSE,
                                               rho = 0.5)
  df.mfr <- rbind(subset(df.agg.mfr, is.na(multiple_es)),
                  df.agg.subgroups,
                  df.agg.outcomes)

  df.mfr <- df.mfr[order(df.mfr$author, df.mfr$year), ]

  expect_equal(df.agg.umb$value, exp(as.numeric(as.character(df.mfr$yi))), tolerance = 1e-14)
  expect_equal(df.agg.umb$se, sqrt(df.mfr$vi), tolerance = 1e-15)
})

test_that("agg.data correctly aggregates studies with multiple effect size estimates: HR", {
  df <- df.OR.multi
  df$measure <- "HR"
df$n_cases = with(df, n_cases_exp + n_cases_nexp)
df$n_controls = with(df, n_controls_exp + n_controls_nexp)
  df.format.transit <- .quiet(.format_dataset(attr(.check_data(df), "data"), mult.level = TRUE))
  df.format <- attr(df.format.transit, "data_mult")

  df.agg.umb <- .agg_data(df.format, r = 0.5, measure = "HR")

  df.agg.umb <- df.agg.umb[order(df.agg.umb$author, df.agg.umb$year), ]

  df.agg.mfr <- metafor::escalc(yi = log(value), sei = se, data = df.format)

  df.agg.subgroups <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "groups"),
                                                cluster = author,
                                                struct = "ID",
                                                weighted = TRUE)

  df.agg.outcomes <- metafor::aggregate.escalc(subset(df.agg.mfr, multiple_es == "outcomes"),
                                               cluster = author,
                                               struct = "CS",
                                               weighted = FALSE,
                                               rho = 0.5)
  df.mfr <- rbind(subset(df.agg.mfr, is.na(multiple_es)),
                  df.agg.subgroups,
                  df.agg.outcomes)

  df.mfr <- df.mfr[order(df.mfr$author, df.mfr$year), ]

  expect_equal(df.agg.umb$value, exp(as.numeric(as.character(df.mfr$yi))), tolerance = 1e-15)
  expect_equal(df.agg.umb$se, sqrt(df.mfr$vi), tolerance = 1e-15)
})

# SHARED_CONTROLS

# CORRECTION FACTOR - simple level
test_that(".shared_adjustment_mod provides adequate sample size correction for standard situations", {
  df <- df.SMD[1:5,]

  df$shared_controls <- c(1,2,2,1,1)

  adj <- .shared_adjustment_mod(shared = df$shared_controls, author = df$author, year = df$year)

  expect_equal(adj, c(1/3, 1/2, 1/2, 1/3, 1/3), tolerance = 1e-15)
})

# CORRECTION FACTOR - multi level
test_that(".shared_adjustment_mod provides adequate sample size correction for multilevel situations", {
  df <- df.SMD[1:5,]

  df$author[c(4)] <- c(df$author[1])

  df$year[c(4)] <- c(df$year[1])

  df$shared_controls <- c(1,2,2,1,1)
  adj <- .shared_adjustment_mod(shared = df$shared_controls, author = df$author, year = df$year)
  expect_equal(adj, c(1/2, 1/2, 1/2, 1/2, 1/2), tolerance = 1e-15)

  df$shared_controls <- c(NA,2,2,NA,NA)
  adj <- .shared_adjustment_mod(shared = df$shared_controls, author = df$author, year = df$year)
  expect_equal(adj, c(1, 1/2, 1/2, 1, 1), tolerance = 1e-15)
})

# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple level - SMD
test_that(".shared_adjustment_mod provides adequate sample size and effect size correction for STANDARD situations SMD", {
  df <- df.SMD[1:5,]

  df$shared_controls <- c(1,2,2,3,4)


  umb <- .quiet(umbrella(df))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_raw, df$n_controls)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)


  # adjusted values
  tmp2 <- .estimate_d_from_means(n_cases = df$n_cases[2], n_controls = df$n_controls[2] / 2,
                                  mean_cases = df$mean_cases[2], sd_cases = df$sd_cases[2],
                                  mean_controls = df$mean_controls[2], sd_controls = df$sd_controls[2])

  tmp3 <- .estimate_d_from_means(n_cases = df$n_cases[3], n_controls = df$n_controls[3] / 2,
                                  mean_cases = df$mean_cases[3], sd_cases = df$sd_cases[3],
                                  mean_controls = df$mean_controls[3], sd_controls = df$sd_controls[3])

  tmp2G <- data.frame(value = .estimate_g_from_d(tmp2$value, df$n_cases[2], df$n_controls[2] / 2, tmp2$se)$value,
                      se =    .estimate_g_from_d(tmp2$value, df$n_cases[2], df$n_controls[2] / 2, tmp2$se)$se)

  tmp3G <- data.frame(value = .estimate_g_from_d(tmp3$value, df$n_cases[3], df$n_controls[3] / 2, tmp3$se)$value,
                      se =    .estimate_g_from_d(tmp3$value, df$n_cases[3], df$n_controls[3] / 2, tmp3$se)$se)

  ci_loG1 <- tmp2G$value - tmp2G$se * qt(0.975, (df$n_cases[2]) + (df$n_controls[2] / 2) - 2)
  ci_loG2 <- tmp3G$value - tmp3G$se * qt(0.975, (df$n_cases[3]) + (df$n_controls[3] / 2) - 2)

  ci_upG1 <- tmp2G$value + tmp2G$se * qt(0.975, (df$n_cases[2]) + (df$n_controls[2] / 2) - 2)
  ci_upG2 <- tmp3G$value + tmp3G$se * qt(0.975, (df$n_cases[3]) + (df$n_controls[3] / 2) - 2)

  expect_equal(df_adj[2:3, ]$n_cases_raw, df[2:3, ]$n_cases)
  expect_equal(df_adj[2:3, ]$n_controls_adj, df[2:3, ]$n_controls / 2, tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_adj, df_adj$n_controls_adj), umb$Pharmacological$n$cases_and_controls, tolerance = 1e-6)

  expect_equal(df_adj$ci_lo_adj[2], ci_loG1, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj[2], ci_upG1, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_adj[3], ci_loG2, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj[3], ci_upG2, tolerance = 1e-6)
})
# SAMPLE SIZE and EFFECT SIZE ESTIMATE - multilevel - SMD
test_that(".shared_adjustment_mod provides adequate sample size and effect size correction for MULTILEVEL situations SMD", {
  df <- df.SMD[1:5,]

  df$shared_controls <- c(1,2,2,1,1)

  df$author[c(4)] <- c(df$author[1])

  df$year[c(4)] <- c(df$year[1])

  df$multiple_es <- NA ; df$multiple_es[c(1,4)] <- "outcomes"
  adj <- .shared_adjustment_mod(shared = df$shared_controls, author = df$author, year = df$year)

  umb <- .quiet(umbrella(df, mult.level = TRUE))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_raw, df$n_controls)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  tmp1 <- .estimate_d_from_means(n_cases = df$n_cases[1], n_controls = df$n_controls[1] / 2,
                                 mean_cases = df$mean_cases[1], sd_cases = df$sd_cases[1],
                                 mean_controls = df$mean_controls[1], sd_controls = df$sd_controls[1])
  tmp4 <- .estimate_d_from_means(n_cases = df$n_cases[4], n_controls = df$n_controls[4] / 2,
                                mean_cases = df$mean_cases[4], sd_cases = df$sd_cases[4],
                                mean_controls = df$mean_controls[4], sd_controls = df$sd_controls[4])

  tmpG1 <- data.frame(value = .estimate_g_from_d(tmp1$value, df$n_cases[1], df$n_controls[1] / 2, tmp1$se)$value,
                      se = .estimate_g_from_d(tmp1$value, df$n_cases[1], df$n_controls[1] / 2, tmp1$se)$se)

  ci_loG1 <- tmpG1$value - tmpG1$se * qt(0.975, df$n_cases[1] + df$n_controls[1] / 2 - 2)
  ci_upG1 <- tmpG1$value + tmpG1$se * qt(0.975, df$n_cases[1] + df$n_controls[1] / 2 - 2)


  tmpG4 <- data.frame(value = .estimate_g_from_d(tmp4$value, df$n_cases[4], df$n_controls[4] / 2, tmp4$se)$value,
                      se = .estimate_g_from_d(tmp4$value, df$n_cases[4], df$n_controls[4] / 2, tmp4$se)$se)

  ci_loG4 <- tmpG4$value - tmpG4$se * qt(0.975, df$n_cases[4] + df$n_controls[4] / 2 - 2)
  ci_upG4 <- tmpG4$value + tmpG4$se * qt(0.975, df$n_cases[4] + df$n_controls[4] / 2 - 2)

  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_adj, df$n_controls / 2, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_adj[1], ci_loG1, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj[1], ci_upG1, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_adj[4], ci_loG4, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj[4], ci_upG4, tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_raw[2:5], df_adj$n_controls_adj[2:5]), umb$Pharmacological$n$cases_and_controls, tolerance = 1e-6)

})
# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple level - OR - shared_controls
test_that("shared_controls provides adequate sample size and effect size correction for STANDARD situations OR", {
  df <- df.OR[1:5,]

  df$shared_controls <- c(1,2,2,3,4)

  umb <- .quiet(umbrella(df))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_raw, df$n_controls)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  tmp2 <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp[2], n_cases_nexp = df$n_cases_nexp[2],
                                 n_controls_exp = df$n_controls_exp[2] / 2, n_controls_nexp = df$n_controls_nexp[2] / 2)
  tmp3 <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp[3], n_cases_nexp = df$n_cases_nexp[3],
                                 n_controls_exp = df$n_controls_exp[3] / 2, n_controls_nexp = df$n_controls_nexp[3] / 2)

  ci_lo_2 <- tmp2$value / exp(qnorm(0.975) * tmp2$se)
  ci_up_2 <- tmp2$value * exp(qnorm(0.975) * tmp2$se)
  ci_lo_3 <- tmp3$value / exp(qnorm(0.975) * tmp3$se)
  ci_up_3 <- tmp3$value * exp(qnorm(0.975) * tmp3$se)

  expect_equal(df_adj[2:3, ]$n_cases_raw, df[2:3, ]$n_cases)
  expect_equal(df_adj[2:3, ]$n_controls_adj, df[2:3, ]$n_controls / 2, tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_raw, df_adj$n_controls_adj), umb$ADHD$n$cases_and_controls, tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$value_adj, c(df$value[2], df$value[3]), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_lo_adj, c(ci_lo_2, ci_lo_3), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_up_adj, c(ci_up_2, ci_up_3), tolerance = 1e-6)
})

# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple level - OR - shared_nexp
test_that("shared_nexp provides adequate sample size and effect size correction for STANDARD situations OR", {
  df <- df.OR[1:5,]

  df$shared_nexp <- c(1,2,2,3,4)

  umb <- .quiet(umbrella(df))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_exp_raw, df$n_cases_exp)
  expect_equal(df_adj$n_controls_exp_raw, df$n_controls_exp)
  expect_equal(df_adj$n_cases_nexp_raw, df$n_cases_nexp)
  expect_equal(df_adj$n_controls_nexp_raw, df$n_controls_nexp)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  tmp2 <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp[2], n_cases_nexp = df$n_cases_nexp[2] / 2,
                              n_controls_exp = df$n_controls_exp[2], n_controls_nexp = df$n_controls_nexp[2] / 2)
  tmp3 <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp[3], n_cases_nexp = df$n_cases_nexp[3] / 2,
                              n_controls_exp = df$n_controls_exp[3], n_controls_nexp = df$n_controls_nexp[3] / 2)

  ci_lo_2 <- tmp2$value / exp(qnorm(0.975) * tmp2$se)
  ci_up_2 <- tmp2$value * exp(qnorm(0.975) * tmp2$se)
  ci_lo_3 <- tmp3$value / exp(qnorm(0.975) * tmp3$se)
  ci_up_3 <- tmp3$value * exp(qnorm(0.975) * tmp3$se)

  expect_equal(df_adj[2:3, ]$n_nexp_adj, df[2:3, ]$n_nexp/2)
  expect_equal(df_adj[2:3, ]$value_adj, c(df$value[2], df$value[3]), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_lo_adj, c(ci_lo_2, ci_lo_3), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_up_adj, c(ci_up_2, ci_up_3), tolerance = 1e-6)
})

# SAMPLE SIZE and EFFECT SIZE ESTIMATE - multilevel - OR - shared_nexp
test_that("shared_controls provides adequate sample size and effect size correction for multilevel situations OR", {
  df <- df.OR[1:5,]

  df$shared_controls <- c(1,2,2,1,1)

  df$author[c(4)] <- c(df$author[1])

  df$year[c(4)] <- c(df$year[1])

  df$multiple_es <- NA ; df$multiple_es[c(1,4)] <- "outcomes"

  adj <- .shared_adjustment_mod(shared = df$shared_controls, author = df$author, year = df$year)

  umb <- .quiet(umbrella(df, mult.level = TRUE))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_raw, df$n_controls)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  se = NA
  # adjusted values
  se <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp,
                            n_cases_nexp = df$n_cases_nexp,
                            n_controls_exp = df$n_controls_exp / 2,
                            n_controls_nexp = df$n_controls_nexp / 2)$se

  ci_lo <- df$value / exp(qnorm(0.975) * se)
  ci_up <- df$value * exp(qnorm(0.975) * se)

  expect_equal(df_adj$n_cases_raw, df$n_cases)
  expect_equal(df_adj$n_controls_adj, df$n_controls / 2, tolerance = 1e-6)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_adj, ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj, ci_up, tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_adj[c(1:3,5)], df_adj$n_controls_adj[c(1:3,5)]), umb$ADHD$n$cases_and_controls, tolerance = 1e-6)
})

# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple MULTILEVEL - OR - shared_nexp
test_that("shared_nexp provides adequate sample size and effect size correction for MULTILEVEL situations OR", {
  df <- df.OR[1:5,]

  df$shared_nexp <- c(1,2,2,1,1)

  df$author[c(4)] <- c(df$author[1])

  df$year[c(4)] <- c(df$year[1])

  df$multiple_es <- NA ; df$multiple_es[c(1,4)] <- "outcomes"

  umb <- .quiet(umbrella(df, mult.level = TRUE))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_exp_raw, df$n_cases_exp)
  expect_equal(df_adj$n_controls_exp_raw, df$n_controls_exp)
  expect_equal(df_adj$n_cases_nexp_raw, df$n_cases_nexp)
  expect_equal(df_adj$n_controls_nexp_raw, df$n_controls_nexp)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  se <- .estimate_or_from_n(n_cases_exp = df$n_cases_exp, n_cases_nexp = df$n_cases_nexp / 2,
                              n_controls_exp = df$n_controls_exp, n_controls_nexp = df$n_controls_nexp / 2)$se

  ci_lo <- df$value / exp(qnorm(0.975) * se)
  ci_up <- df$value * exp(qnorm(0.975) * se)

  expect_equal(df_adj$n_cases_nexp_adj, df$n_cases_nexp / 2)
  expect_equal(df_adj$n_controls_nexp_adj, df$n_controls_nexp / 2)

  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_adj, ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_adj, ci_up, tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_adj[c(1:3,5)], df_adj$n_controls_adj[c(1:3,5)]), umb$ADHD$n$cases_and_controls, tolerance = 1e-6)
})


# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple level - RR
test_that("shared_nexp provides adequate sample size and effect size correction for STANDARD situations RR", {

  df <- df.RR[1:5,]

  df$shared_nexp <- c(1,2,2,3,4)

  umb <- .quiet(umbrella(df))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_exp_raw, df$n_cases_exp)
  expect_equal(df_adj$n_exp_raw, df$n_exp)
  expect_equal(df_adj$n_cases_nexp_raw, df$n_cases_nexp)
  expect_equal(df_adj$n_nexp_raw, df$n_nexp)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  tmp2 <- .estimate_rr_from_n(n_cases_exp = df$n_cases_exp[2], n_cases_nexp = df$n_cases_nexp[2] / 2,
                              n_exp = df$n_exp[2], n_nexp = df$n_nexp[2] / 2)
  tmp3 <- .estimate_rr_from_n(n_cases_exp = df$n_cases_exp[3], n_cases_nexp = df$n_cases_nexp [3] / 2,
                              n_exp = df$n_exp[3], n_nexp = df$n_nexp[3] / 2)

  ci_lo_2 <- tmp2$value / exp(qnorm(0.975) * tmp2$se)
  ci_up_2 <- tmp2$value * exp(qnorm(0.975) * tmp2$se)
  ci_lo_3 <- tmp3$value / exp(qnorm(0.975) * tmp3$se)
  ci_up_3 <- tmp3$value * exp(qnorm(0.975) * tmp3$se)

  expect_equal(df_adj[2:3, ]$n_cases_exp_adj, df[2:3, ]$n_cases_exp)
  expect_equal(df_adj[2:3, ]$n_exp_adj, df[2:3, ]$n_exp)
  expect_equal(df_adj[2:3, ]$n_cases_nexp_adj, df[2:3, ]$n_cases_nexp / 2)
  expect_equal(df_adj[2:3, ]$n_nexp_adj, df[2:3, ]$n_nexp / 2)
  #
  expect_equal(df_adj[2:3, ]$value_adj, c(df$value[2], df$value[3]), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_lo_adj, c(ci_lo_2, ci_lo_3), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_up_adj, c(ci_up_2, ci_up_3), tolerance = 1e-6)
})



# SAMPLE SIZE and EFFECT SIZE ESTIMATE - simple level - IRR
test_that("shared_nexp provides adequate sample size and effect size correction for STANDARD situations RR", {

  df <- df.IRR[1:5,]

  df$shared_nexp <- c(1,2,2,3,4)

  umb <- .quiet(umbrella(df))

  df_adj <- umb[[1]]$x_shared

  # not adjusted values should be equal to original ones
  expect_equal(df_adj$n_cases_exp_raw, df$n_cases_exp)
  expect_equal(df_adj$time_exp_raw, df$time_exp)
  expect_equal(df_adj$value_raw, df$value, tolerance = 1e-6)
  expect_equal(df_adj$ci_lo_raw, df$ci_lo, tolerance = 1e-6)
  expect_equal(df_adj$ci_up_raw, df$ci_up, tolerance = 1e-6)

  # adjusted values
  tmp2 <- .estimate_irr_from_n(n_cases_exp = df$n_cases_exp[2],
                               time_exp = df$time_exp[2],
                               n_cases_nexp = df$n_cases_nexp[2] / 2,
                               time_nexp = df$time_nexp[2] / 2)
  tmp3 <- .estimate_irr_from_n(n_cases_exp = df$n_cases_exp[3],
                               time_exp = df$time_exp[3],
                               n_cases_nexp = df$n_cases_nexp[3] / 2,
                               time_nexp = df$time_nexp[3] / 2)

  ci_lo_2 <- tmp2$value / exp(qnorm(0.975) * tmp2$se)
  ci_up_2 <- tmp2$value * exp(qnorm(0.975) * tmp2$se)
  ci_lo_3 <- tmp3$value / exp(qnorm(0.975) * tmp3$se)
  ci_up_3 <- tmp3$value * exp(qnorm(0.975) * tmp3$se)

  expect_equal(df_adj[2:3, ]$n_cases_exp_adj, df[2:3, ]$n_cases_exp)
  expect_equal(df_adj[2:3, ]$n_cases_nexp_adj, df[2:3, ]$n_cases_nexp / 2)

  expect_equal(df_adj[2:3, ]$value_adj, c(df$value[2], df$value[3]), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_lo_adj, c(ci_lo_2, ci_lo_3), tolerance = 1e-6)
  expect_equal(df_adj[2:3, ]$ci_up_adj, c(ci_up_2, ci_up_3), tolerance = 1e-6)
  expect_equal(sum(df_adj$n_cases_adj), umb$Smoking$n$cases_and_controls, tolerance = 1e-6)
})
