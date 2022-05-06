#' Obtain the two tailed p-value from the percentile rank
#'
#' @param p.value a p-value
#'
#' @noRd
.two_tail = function (p.value) {
  return(1 - 2 * abs(p.value - 0.5))
}

#' Improve the confidence interval estimation
#'
#' @param value value
#' @param ci_lo 95% ci lo
#' @param ci_up 95% ci up
#' @param uselog whether we want to apply the natural log
#'
#' @noRd
.improve_ci = function (value, ci_lo, ci_up, uselog) {

  if (!is.na(ci_lo) && (ci_lo == 0 || abs(ci_lo) == Inf)) {
    ci_lo = NA
  } else if (!is.na(ci_up) && ((abs(ci_up) == Inf || ci_up == 0))) {
    ci_up = NA
  }
  are_missing = is.na(value) + is.na(ci_lo) + is.na(ci_up)
  if (are_missing > 1) {
    # If two or three missing, returns NA
    return(data.frame(value = NA))
  }
  me = NA
  if (are_missing == 0) {
    # "unrounds"
    # Find round digits
    round_digits = 0
    while (round(value, round_digits) != value) {
      round_digits = round_digits + 1
    }
    while (round(ci_lo, round_digits) != ci_lo) {
      round_digits = round_digits + 1
    }
    while (round(ci_up, round_digits) != ci_up) {
      round_digits = round_digits + 1
    }
    # Estimate real parameters, in two steps
    if (uselog) {
      par = c(log(value), (log(ci_lo) + log(ci_up)) / 2)

      o = optim(par, function (mean_me, value, ci_lo, ci_up) {
        mean = mean_me[1]
        me = mean_me[2]
        2 * (value - exp(mean))^2 +
          (ci_lo - exp(mean - me))^2 +
          (ci_up - exp(mean + me))^2
      }, gr = NULL, value, ci_lo, ci_up)


      o = optim(o$par, function (mean_me, value, ci_lo, ci_up, round_digits) {
        mean = mean_me[1]
        me = mean_me[2]
        2 * (value - round(exp(mean), round_digits))^2 +
          (ci_lo - round(exp(mean - me), round_digits))^2 +
          (ci_up - round(exp(mean + me), round_digits))^2
      }, gr = NULL, value, ci_lo, ci_up, round_digits)

    } else {
      par = c(value, (ci_lo + ci_up) / 2)
      o = optim(par, function (mean_me, value, ci_lo, ci_up) {
        mean = mean_me[1]
        me   = mean_me[2]
        2 * (value - mean)^2 +
          (ci_lo - (mean - me))^2 +
          (ci_up - (mean + me))^2
      }, gr = NULL, value, ci_lo, ci_up)
      o = optim(o$par, function (mean_me, value, ci_lo, ci_up, round_digits) {
        mean = mean_me[1]
        me   = mean_me[2]
        2 * (value - round(mean, round_digits))^2 +
          (ci_lo - round(mean - me, round_digits))^2 +
          (ci_up - round(mean + me, round_digits))^2
      }, gr = NULL, value, ci_lo, ci_up, round_digits)
    }
    mean = o$par[1]
    me = o$par[2]
  } else {
    # If one missing, simply estimates it
    if (uselog) {
      if (is.na(value)) {
        mean = (log(ci_lo) + log(ci_up)) / 2
        me = (log(ci_up) - log(ci_lo)) / 2
      } else {
        mean = log(value)
        if (is.na(ci_lo)) {
          me = log(ci_up) - mean
        } else {
          me = mean - log(ci_lo)
        }
      }
    } else {
      if (is.na(value)) {
        mean = (ci_lo + ci_up) / 2
        me = (ci_up - ci_lo) / 2
      } else {
        mean = value
        if (is.na(ci_lo)) {
          me = ci_up - value
        } else {
          me = value - ci_lo
        }
      }
    }
  }

  out = data.frame(value = mean, ci_lo = mean - me, ci_up = mean + me)
  if (uselog) {
    out = exp(out)
  }
  return(out)
}

#' Extract critical information from a meta object (needed for esb.test)
#'
#' @param x an rma object
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param measure measure used OR or SMD
#'
#' @noRd
.meta_to_umbrella_x = function (x, n_cases, n_controls, measure) {

  if (.true.na.null(n_cases) & .true.na.null(x$n.e) & .true.na.null(x$event.e)) {
    stop("The number of cases should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_cases' argument)")
  }
  if (.true.na.null(n_controls) & .true.na.null(x$n.c) & .true.na.null(x$event.c)) {
    stop("The number of controls should be indicated. You can do it when calling the meta function or when calling the esb.test function (via 'n_controls' argument)")
  }
  if (!is.null(measure)) {
    if (measure == "") {
      stop("The effect size measure should be indicated when calling the esb.test function from a meta object (via the 'sm' argument in the meta function or via the 'measure' argument of the esb.test function).")
    } else if (!(measure %in% c("SMD", "G", "OR"))) {
    stop("The esb.test function can be called from a 'meta' object only with 'G', 'SMD' or 'OR' as effect size measures.")
    }
  } else {
    stop("The effect size measure should be indicated when calling the esb.test function from a meta object (via the 'sm' argument in the meta function or via the 'measure' argument of the esb.test function).")
  }

  switch (measure,
          "SMD" =,
          "G" = {
            n_cas <- if(!is.null(n_cases)) {
              n_cases
            } else {
              x$n.e
            }
            n_cont <- if(!is.null(n_controls)) {
              n_controls
            } else {
              x$n.c
            }
            j = .d_j(n_cas + n_cont - 2)

            if (!is.null(x$method.smd)) {
              if (x$method.smd == "Hedges") {
              value <- x$TE / j
              se <- sqrt(1 / n_cas + 1 / n_cont)
              } else {
                value <- x$TE
                se <- sqrt(1 / n_cas + 1 / n_cont)
              }
            } else {
              value <- x$TE / j
              se <- sqrt(1 / n_cas + 1 / n_cont)
            }

            sum_N = apply(cbind(n_cas, n_cont), 1, sum)

            returned_df = data.frame(
              value = value,
              se = se,
              n_cases = n_cas,
              n_controls = n_cont,
              sum_N = sum_N,
              ci_lo = NA,
              ci_up = NA,
              reverse_es = NA
            )
          }, "OR" = {
            n_cas <- if(!is.null(n_cases)) {
              n_cases
            } else {
              x$event.e + x$event.c
            }
            n_cont <- if(!is.null(n_controls)) {
              n_controls
            } else {
              (x$n.e + x$n.c) - (x$event.e + x$event.c)
            }
            n_tot = data.frame(n_cases_exp = NA, n_cases_nexp = NA, n_controls_exp = NA, n_controls_nexp = NA)

            if (measure == "OR") { # for future updates
              for (i in 1:length(n_cas)) {
                n_tot[i,] <- .estimate_n_from_or_and_n_cases(or = exp(x$TE[i]), var = x$seTE[i]^2, n_cases = n_cas[i], n_controls = n_cont[i])
              }
            }
            sum_N = apply(cbind(n_cas, n_cont), 1, sum)

            returned_df = data.frame(
              n_tot,
              n_cases = n_cas,
              n_controls = n_cont,
              n_exp = n_tot$n_cases_exp + n_tot$n_controls_exp,
              n_nexp = n_tot$n_cases_nexp + n_tot$n_controls_nexp,
              value = exp(x$TE),
              se = x$seTE,
              sum_N = sum_N,
              ci_lo = NA,
              ci_up = NA,
              reverse_es = NA
            )
          }
  )
  return(returned_df)
}

#' Extract critical information from an rma object (needed for esb.test)
#'
#' @param x an rma object
#' @param n_cases number of cases
#' @param n_controls number of controls
#' @param measure measure used OR or SMD
#'
#' @noRd
.rma_to_umbrella_x = function (x, n_cases, n_controls, measure) {

  if (.true.na.null(n_cases) & .true.na.null(n_controls)) {
    stop("The number of cases or the number of controls should be indicated when calling the esb.test function from an 'rma' object. You can do it via the 'n_cases' or 'n_controls' arguments of the esb.test function.")
  }

  if (.true.na.null(n_controls) & .true.na.null(x$ni)) {
    stop("The number of controls should be indicated when calling the esb.test function from an 'rma' object without the 'ni' argument")
  }

  if (.true.na.null(n_cases) & .true.na.null(x$ni)) {
    stop("The number of cases should be indicated when calling the esb.test function from an 'rma' object without the 'ni' argument")
  }

  if (!is.null(measure)) {
    if (measure == "GEN") {
      stop("The effect size measure should be indicated when calling the esb.test function from a rma object (via the 'measure' argument in the rma function or via the 'measure' argument of the esb.test function).")
    } else if (!(measure %in% c("SMD", "G", "OR"))) {
    stop("The esb.test function can be called from an 'rma' or 'meta' object only with 'G', 'SMD' or 'OR' as effect size measure")
      }
    } else {
      stop("The effect size measure should be indicated when calling the esb.test function from a rma object (via the 'measure' argument in the rma function or via the 'measure' argument of the esb.test function).")
      }

  n_cas <- if(!is.null(n_cases)) {
    n_cases
  } else if (!is.null(x$ni) & !is.null(n_controls)){
    x$ni - n_controls
  }

  n_cont <- if(!is.null(n_controls)) {
    n_controls
  } else if (!is.null(x$ni) & !is.null(n_cases)){
    x$ni - n_cases
  }
  sum_N = apply(cbind(n_cas, n_cont), 1, sum)
  switch (measure,
          "SMD" = {
            j = .d_j(n_cas + n_cont - 2)
            returned_df = data.frame(
              value = .as_numeric(x$yi) / j,
              se = sqrt(1 / n_cas + 1 / n_cont),
              n_cases = n_cas,
              n_controls = n_cont,
              sum_N = sum_N,
              ci_lo = NA,
              ci_up = NA,
              reverse_es = NA
            )
          }, "OR" = {
            n_tot = data.frame(n_cases_exp = NA, n_cases_nexp = NA, n_controls_exp = NA, n_controls_nexp = NA)
              for (i in 1:length(n_cas)) {
                n_tot[i,] <- .estimate_n_from_or_and_n_cases(or = exp(x$yi[i]), var = x$vi[i], n_cases = n_cas[i], n_controls = n_cont[i])
              }
            returned_df = data.frame(
              n_tot,
              n_cases = n_cas,
              n_controls = n_cont,
              n_exp = n_tot$n_cases_exp + n_tot$n_controls_exp,
              n_nexp = n_tot$n_cases_nexp + n_tot$n_controls_nexp,
              value = as.numeric(as.character(exp(x$yi))),
              se = sqrt(as.numeric(as.character(x$vi))),
              sum_N = sum_N,
              ci_lo = NA,
              ci_up = NA,
              reverse_es = NA
            )
          }
  )
  return(returned_df)
}

#' convert an object to a numeric format
#'
#' @param num an object
#'
#' @noRd
.as_numeric = function(num){
  return(suppressWarnings(as.numeric(as.character(num))))
}


#' function selecting the largest study with IRR measure
#'
#' @param x a well formatted dataset
#'
#' @noRd
.largest_irr <- function(x, return = "ci") {
  largest_index_transit = which(x$time == max(x$time))
  # if there is equality in the time we take the study with the lowest ES
  if (length(largest_index_transit) > 1) {
    largest_index = which.min(abs(log(x[largest_index_transit, ]$value)))
      largest = data.frame(ci_lo = x[largest_index_transit, ]$ci_lo[largest_index],
                           ci_up = x[largest_index_transit, ]$ci_up[largest_index],
                           value = x[largest_index_transit, ]$value[largest_index])
  } else {
    largest_index = largest_index_transit
    # there is only one maximum value for time
      largest = data.frame(ci_lo = x[largest_index,]$ci_lo,
                           ci_up = x[largest_index,]$ci_up,
                           value = x[largest_index,]$value)
  }
  if (return == "ci") {
    dat <- largest[, c("ci_lo", "ci_up")]
  } else if (return == "value") {
    dat <- .as_numeric(subset(largest, select = -c(ci_lo, ci_up)))
  } else if (return == "nrow") {
    dat = largest_index
  }
  return(dat)
}

#' function selecting the largest study with OR RR HR measure
#'
#' @param x a well formatted dataset
#'
#' @noRd
.largest_or_rr_hr <- function(x, return = "ci") {
  largest_index_transit = which(x$sum_N == max(x$sum_N))
  if (length(largest_index_transit) > 1) {
    largest_index = which.min(abs(log(x[largest_index_transit,]$value)))
    # if there is equality in the sum_N we take the study with the lowest ES
    largest = data.frame(ci_lo = x[largest_index_transit,]$ci_lo[largest_index],
                         ci_up = x[largest_index_transit,]$ci_up[largest_index],
                         value = x[largest_index_transit,]$value[largest_index])
  } else {
    # there is only one maximum value for sum_N
    largest_index = largest_index_transit
    largest = data.frame(ci_lo = x[largest_index,]$ci_lo,
                           ci_up = x[largest_index,]$ci_up,
                           value = x[largest_index,]$value)
  }
  if (return == "ci") {
    dat <- largest[, c("ci_lo", "ci_up")]
  } else if (return == "value") {
    dat <- .as_numeric(subset(largest, select = -c(ci_lo, ci_up)))
  } else if (return == "nrow") {
    dat = largest_index
  }
  return(dat)
}

#' function selecting the largest study with smd measure
#'
#' @param x a well formatted dataset
#'
#' @noRd
.largest_smd <- function(x, return = "ci") {
  largest_index_transit = which(x$sum_N == max(x$sum_N))
  if (length(largest_index_transit) > 1) {
    largest_index = which.min(abs(x[largest_index_transit,]$value))
    # if there is equality in the sum_N we take the study with the lowest ES
    largest = data.frame(ci_lo = x[largest_index_transit, ]$ci_lo[largest_index],
                         ci_up = x[largest_index_transit, ]$ci_up[largest_index],
                         value = x[largest_index_transit, ]$value[largest_index],
                         n_cases = x[largest_index_transit, ]$n_cases[largest_index],
                         n_controls = x[largest_index_transit, ]$n_controls[largest_index],
                         se = x[largest_index_transit, ]$se[largest_index])
  } else {
    # there is only one maximum value for sum_N
    largest_index = largest_index_transit
    largest = data.frame(ci_lo = x[largest_index, ]$ci_lo,
                         ci_up = x[largest_index, ]$ci_up,
                         value = x[largest_index, ]$value,
                         n_cases = x[largest_index, ]$n_cases,
                         n_controls = x[largest_index, ]$n_controls,
                         se = x[largest_index, ]$se)
  }
  if (return == "ci") {
    dat = largest[, c("ci_lo", "ci_up")]
  } else if (return == "value") {
    dat = .as_numeric(largest$value)
  } else if (return == "nrow") {
    dat = largest_index
  }
  return(dat)
}

#' function selecting the largest study with Z measure
#'
#' @param x a well formatted dataset
#'
#' @noRd
.largest_z <- function(x, return = "ci") {

  largest_index_transit = which(x$sum_N == max(x$sum_N))

  if (length(largest_index_transit) > 1) {
    largest_index = which.min(abs(x[largest_index_transit,]$value))

    # if there is equality in the sum_N we take the study with the lowest ES
    largest = data.frame(ci_lo = x[largest_index_transit, ]$ci_lo[largest_index],
                         ci_up = x[largest_index_transit, ]$ci_up[largest_index],
                         value = x[largest_index_transit, ]$value[largest_index],
                         n_sample = x[largest_index_transit, ]$n_sample[largest_index],
                         se = x[largest_index_transit, ]$se[largest_index])
  } else {
    # there is only one maximum value for sum_N
    largest_index = largest_index_transit
    largest = data.frame(ci_lo = x[largest_index, ]$ci_lo,
                         ci_up = x[largest_index, ]$ci_up,
                         value = x[largest_index, ]$value,
                         n_sample = x[largest_index, ]$n_sample,
                         se = x[largest_index, ]$se)
  }
  if (return == "ci") {
    dat = largest[, c("ci_lo", "ci_up")]
  } else if (return == "value") {
    dat = .as_numeric(largest$value)
  } else if (return == "nrow") {
    dat = largest_index
  }
  return(dat)
}

#' Assess whether x is missing or null
#'
#' @param x an object
#'
#' @noRd
.true.na.null <- function(x){
  return(any(is.na(x)) || any(is.null(x)))
}

#' convert a RR to an OR
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_RR_to_OR <- function(x){

  for (i in which(x[, "measure"] == "RR")) {
    x_raw_i = x[i, ]

    # We convert RR into an OR
    # USERS REPORT: RR value + CI/se + n_cases/controls
    if (is.na(x_raw_i$n_cases_exp) || is.na(x_raw_i$n_controls_exp) ||
        is.na(x_raw_i$n_cases_nexp) || is.na(x_raw_i$n_controls_nexp)) {

      if (is.na(x_raw_i$se)) {
        tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)
        value_i = tmp$value
        se_i = (log(tmp$ci_up) - log(tmp$ci_lo)) / (2 * qnorm(0.975))
      } else {
        value_i = x_raw_i$value
        se_i = x_raw_i$se
      }

      tmp2 = .estimate_n_from_rr(value_i, se_i^2, x_raw_i$n_cases, x_raw_i$n_controls)

      x[i, "n_cases_exp"] = tmp2$n_cases_exp
      x[i, "n_cases_nexp"] = tmp2$n_cases_nexp
      x[i, "n_controls_exp"] = tmp2$n_exp - tmp2$n_cases_exp
      x[i, "n_controls_nexp"] = tmp2$n_nexp - tmp2$n_cases_nexp
      x[i, "situation"] = paste0(x_raw_i$situation, "RR_2x2")

      # USERS REPORT: 2x2 table
    } else {
      # all values are set to NA as they will be re-estimated later
      x[i, "value"] = NA
      x[i, "se"] = NA
      x[i, "ci_lo"] = NA
      x[i, "ci_up"] = NA
    }
    x[i, "measure"] = "OR"
  }
  return(x)
}

#' convert a HR to an OR
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_HR_to_OR <- function(x){

  for (i in which(x[, "measure"] == "HR")) {
    x[i, "measure"] = "OR"
    x[i, "situation"] = gsub("HR", "OR", as.character(x[i, "situation"]))
  }

  return(x)
}

#' convert a OR to a SMD
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_OR_to_SMD <- function(x) {

  for (i in which(x[, "measure"] == "OR")) {

    x_raw_i = x[i, ]

    if (is.na(x_raw_i$n_cases_exp) | is.na(x_raw_i$n_cases_nexp) |
        is.na(x_raw_i$n_controls_exp) | is.na(x_raw_i$n_controls_nexp)) {

      # users report OR + SE/VAR
      if (!is.na(x_raw_i$se)) {
        tmp = data.frame(value = x_raw_i$value,
                         ci_lo = x_raw_i$value / exp(qnorm(0.975) * x_raw_i$se),
                         ci_up = x_raw_i$value * exp(qnorm(0.975) * x_raw_i$se))

        # users report OR + CI
      } else if (is.na(x_raw_i$se)) {
        tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, TRUE)
      }

      # users report OR + n_cases/controls
      if (is.na(tmp$value)) {
        tmp2 = .estimate_se_from_or(or = x_raw_i$value, n_cases = x_raw_i$n_cases, n_controls = x_raw_i$n_controls)
        tmp_n = .estimate_n_from_or_and_n_cases(tmp2$val, tmp2$var, n_cases = x_raw_i$n_cases, n_controls = x_raw_i$n_controls)
        x[i,"n_cases_exp"] = tmp_n$n_cases_exp
        x[i, "n_cases_nexp"] = tmp_n$n_cases_nexp
        x[i, "n_controls_exp"] = tmp_n$n_controls_exp
        x[i, "n_controls_nexp"] = tmp_n$n_controls_nexp

        x[i, "value"] = .or_to_d(tmp2$value)
        x[i, "ci_lo"] = NA
        x[i, "ci_up"] = NA
        x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES")
      } else {
        x[i, "value"] = .or_to_d(tmp$value)
        x[i, "ci_lo"] = .or_to_d(tmp$ci_lo)
        x[i, "ci_up"] = .or_to_d(tmp$ci_up)
        x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_CI")
      }
      # users report 2x2 table
    } else {
      tmp = .estimate_or_from_n(x[i, "n_cases_exp"], x[i, "n_cases_nexp"], x[i, "n_controls_exp"], x[i, "n_controls_nexp"])
      x[i, "value"] = .or_to_d(tmp$value)
      x[i, "ci_lo"] = .or_to_d(tmp$value / exp(qnorm(0.975) * tmp$se))
      x[i, "ci_up"] = .or_to_d(tmp$value * exp(qnorm(0.975) * tmp$se))
      x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_CI")
    }

    x[i, "measure"] = "SMD"
  }

  return(x)
}

#' convert a SMD to a OR
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_SMD_to_OR <- function(x) {

  for (i in which(x[, "measure"] == "SMD")) {

    x_raw_i = x[i, ]

    if (is.na(x_raw_i$mean_cases) | is.na(x_raw_i$mean_controls) |
        is.na(x_raw_i$sd_cases) | is.na(x_raw_i$sd_controls)) {

      # users report SMD + SE/VAR
      if (!is.na(x_raw_i$se)) {
        tmp = data.frame(value = x_raw_i$value,
                         ci_lo = x_raw_i$value - x_raw_i$se * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2),
                         ci_up = x_raw_i$value + x_raw_i$se * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2))
        # users report SMD + CI
      } else if (is.na(x_raw_i$se)) {
        tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)
      }

      # users report SMD + n_cases/controls
      if (is.na(tmp$value)) {
        x[i, "value"] = .d_to_or(x_raw_i$value)
        x[i, "ci_lo"] = NA
        x[i, "ci_up"] = NA
        x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_cases_controls")
      } else {
        x[i, "value"] = .d_to_or(tmp$value)
        x[i, "ci_lo"] = .d_to_or(tmp$ci_lo)
        x[i, "ci_up"] = .d_to_or(tmp$ci_up)
        x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_CI_cases_controls")
      }
      # users report means/SD
    } else {
      tmp = .estimate_d_from_means(x[i, "n_cases"], x[i, "n_controls"],
                                   x[i, "mean_cases"], x[i, "sd_cases"],
                                   x[i, "mean_controls"], x[i, "sd_controls"])
      x[i, "value"] = .d_to_or(tmp$value)
      x[i, "ci_lo"] = .d_to_or(tmp$ci_lo)
      x[i, "ci_up"] = .d_to_or(tmp$ci_up)
      x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_CI_cases_controls")
    }

    x[i, "measure"] = "OR"
  }

  return(x)
}

#' convert a Z to a SMD
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_Z_to_SMD <- function(x){

  for (i in which(x[, "measure"] == "Z")) {

    x_raw_i = x[i, ]

    # users report z + se
    if (!is.na(x_raw_i$se)) {
      x_raw_i$ci_lo = (x_raw_i$value - qnorm(0.975) * x_raw_i$se)
      x_raw_i$ci_up = (x_raw_i$value + qnorm(0.975) * x_raw_i$se)
      ci_lo = .z_to_r(x_raw_i$ci_lo)
      ci_up = .z_to_r(x_raw_i$ci_up)
      se_r = (ci_up - ci_lo) / (2 * qnorm(0.975))
      r = .z_to_r(x_raw_i$value)
      # users report z + 95% CI
    } else if (!is.na(x_raw_i$ci_lo) & x_raw_i$ci_up) {
      tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)
      ci_lo = .z_to_r(tmp$ci_lo)
      ci_up = .z_to_r(tmp$ci_up)
      se_r = (ci_up - ci_lo) / (2 * qnorm(0.975))
      r = .z_to_r(tmp$value)
    # users report z
    } else {
      x_raw_i$se = sqrt(1 / (x_raw_i$n_sample - 3))
      x_raw_i$ci_lo = (x_raw_i$value - qnorm(0.975) * x_raw_i$se)
      x_raw_i$ci_up = (x_raw_i$value + qnorm(0.975) * x_raw_i$se)

      ci_lo = .z_to_r(x_raw_i$ci_lo)
      ci_up = .z_to_r(x_raw_i$ci_up)
      se_r = (ci_up - ci_lo) / (2 * qnorm(0.975))
      r = .z_to_r(x_raw_i$value)
    }

    x[i, "value"] = .r_to_d(r)
    # x[i, "se"] = sqrt(4 * (se_r^2) / ((1 - r^2)^3))
    # x[i, "ci_lo"] = (x[i, "value"] - qnorm(0.975) * x[i, "se"])
    # x[i, "ci_up"] = (x[i, "value"] + qnorm(0.975) * x[i, "se"])
    # x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_SE_CI")
    x[i, "ci_lo"] = .z_to_r(ci_lo)
    x[i, "ci_up"] = .z_to_r(ci_up)
    x[i, "n_cases"] = round(x[i, "n_sample"] / 2)
    x[i, "n_controls"] = round(x[i, "n_controls"] / 2)
    x[i, "situation"] = paste0(as.character(x[i, "situation"]), "ES_CI")
    x[i, "measure"] = "SMD"
  }
  return(x)
}

#' convert a SMC to an SMD
#'
#' @param x a well formatted dataframe
#'
#' @noRd
.convert_SMC_to_SMD <- function(x, pre_post_cor){

  for (i in which(x[, "measure"] == "SMC")) {

    x_raw_i = x[i, ]

    if (!is.na(x_raw_i$mean_change_cases) & !is.na(x_raw_i$mean_change_controls) &
        !is.na(x_raw_i$sd_change_cases) & !is.na(x_raw_i$sd_change_controls)) {
        tmp = .estimate_smc_change(n_cases = x_raw_i$n_cases, n_controls = x_raw_i$n_controls,
                                   mean_change_cases = x_raw_i$mean_change_cases, sd_change_cases = x_raw_i$sd_change_cases,
                                   mean_change_controls = x_raw_i$mean_change_controls, sd_change_controls = x_raw_i$sd_change_controls)
        value_i = tmp$value
        se_i = tmp$se
        ci_lo_i = value_i - se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
        ci_up_i = value_i + se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
    } else if (!is.na(x_raw_i$value) & !is.na(x_raw_i$se)) {
        value_i = x_raw_i$value
        se_i = x_raw_i$se
        ci_lo_i = value_i - se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
        ci_up_i = value_i + se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
    } else if (!is.na(x_raw_i$value) & !is.na(x_raw_i$ci_lo) & !is.na(x_raw_i$ci_up)) {
        tmp = .improve_ci(x_raw_i$value, x_raw_i$ci_lo, x_raw_i$ci_up, FALSE)
        value_i = tmp$value
        ci_lo_i = value_i - se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
        ci_up_i = value_i + se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
        se_i = (ci_up_i - ci_lo_i) / (2 * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2))
     } else {
        if (is.na(x_raw_i$pre_post_cor) & is.na(pre_post_cor) & is.na(x_raw_i$value) & is.na(x_raw_i$se) & is.na(x_raw_i$ci_lo) & is.na(x_raw_i$ci_up)) {
          pre_post_cor_est_cases = (x$sd_pre_cases^2 * x$sd_cases^2 - x$sd_change_cases) / (2 * x$sd_pre_cases^2 * x$sd_cases^2)
          pre_post_cor_est_controls = (x$sd_pre_controls^2 * x$sd_controls^2 - x$sd_change_controls) / (2 * x$sd_pre_controls^2 * x$sd_controls^2)

          if (any(!is.na(pre_post_cor_est_cases) | !is.na(pre_post_cor_est_controls))) {
            row = which(!is.na(pre_post_cor_est_cases) | !is.na(pre_post_cor_est_controls))
            pre_post_cor_est = apply(cbind(pre_post_cor_est_cases, pre_post_cor_est_controls), 1, mean, na.rm = TRUE)
            weights = 1 / ((x$n_cases + x$n_controls)^2)
            pre_post_cor = sum(weights[row] * pre_post_cor_est[row]) / sum(weights[row])
            warning(paste0("The pre/post correlation was calculated using values indicated in studies: ", paste(paste0(x$author[row], " (", x$year[row], ")"), collapse = " / ")))
          }

          cor_i = ifelse(!is.na(x_raw_i$pre_post_cor), x_raw_i$pre_post_cor,
                         ifelse(!is.na(pre_post_cor), pre_post_cor,
                                stop("The pre/post correlation should be indicated when using the 'SMC' measure.")))

          tmp = .estimate_smc_raw(n_cases = x_raw_i$n_cases, n_controls = x_raw_i$n_controls,
                                  mean_pre_cases = x_raw_i$mean_pre_cases, mean_cases = x_raw_i$mean_cases,
                                  sd_pre_cases = x_raw_i$sd_pre_cases, sd_cases = x_raw_i$sd_cases,
                                  mean_pre_controls = x_raw_i$mean_pre_controls, mean_controls = x_raw_i$mean_controls,
                                  sd_pre_controls = x_raw_i$sd_pre_controls, sd_controls = x_raw_i$sd_controls,
                                  cor = cor_i)
          value_i = tmp$value
          se_i = tmp$se
          ci_lo_i = value_i - se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
          ci_up_i = value_i + se_i * qt(0.975, x_raw_i$n_cases + x_raw_i$n_controls - 2)
        }
     }

    x[i, "measure"] = "SMD"
    x[i, "value"] = value_i
    x[i, "se"] = se_i
    x[i, "ci_lo"] = ci_lo_i
    x[i, "ci_up"] = ci_up_i
    x[i, "situation"] = gsub("SMC", "SMD", as.character(x[i, "situation"]))
    x[i, "situation"] = gsub("mean/SD_", "", as.character(x[i, "situation"]))
  }
  return(x)
}

#' Function used in tests to avoid errors generated by messages
#'
#' @param x an object producing an unwanted message
#'
#' @noRd
.quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  suppressMessages(invisible(force(x)))
}

#' @importFrom grDevices dev.off pdf
#' @importFrom graphics lines plot.new plot.window strwidth text points
#' @importFrom stats prop.test aggregate.data.frame binom.test lm na.omit optim optimize pnorm pt qnorm qt quantile rbinom rpois weighted.mean
#' @importFrom utils browseURL write.csv
NULL

utils::globalVariables(c("duplicate", "multiple_es", "shared_controls", "aggregate",
                         "shared_nexp", ".get_file_extension", "check_sensitivity", "tk_choose.dir",
                         "tk_choose.files", "tk_select.list", "excel_sheets", ".read.excel", ".get_filename_without_extension",
                         ".write_errors_file",
                         ".largest_or_rr_hr", ".largest_smd", ".largest_irr",
                         "ci_lo", "ci_up"))
