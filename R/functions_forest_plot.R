#' Forest plots for objects of class \dQuote{umbrella}
#'
#' Draw a forest plot of the factors included in an umbrella review.
#'
#' @param x an object of class \dQuote{umbrella}
#' @param ... additional arguments that can be passed to this function
#'
#' @export forest
#'
#' @md
#'
#' @details
#' For now, this function simply applies the \code{\link{forest.umbrella}()} function on an object of class \dQuote{umbrella}.
#'
#' @return
#' In addition to the plot, the function returns a list including:
#' \itemize{
#'   \item a dataset with the factors, their class and their effect size. Particularly useful when adding a column via the 'add_columns' argument to obtain the ordering of the factors in the plot. See the vignette dedicated to the forest plots for a concrete example.
#'   \item the optimal width and height of the plot, useful when calling the function \code{pdf()} or \code{png()}.
#'}
#'
#' @seealso
#' \code{\link{forest.umbrella}()}
#'
#' @examples
#' forest(umbrella(df.SMD))
forest <- function (x, ...) {
  UseMethod("forest")
}

#' Forest plots for \dQuote{umbrella} objects
#'
#' Draw a forest plot of the factors included in an umbrella review.
#' The basic version of this plot contains three parts:
#'  * a column displaying the name of the factors (referred to as the \code{'factor'} column).
#'  * a graphical representation of the pooled effect sizes and 95% CI of the factors.
#'  * a column displaying the numeric values of the pooled effect size and 95% CI of the factors (referred to as the \code{'value'} column).
#'
#' @param x an object of class \dQuote{umbrella}.
#' @param measure type of effect size used in the plot. Default is equivalent Hedges' g (\code{"eG"}). Alternatively, equivalent Odds Ratio (\code{"eOR"}) can be used.
#' @param main_title the title of the plot
#' @param main_value a title for the header of the 'value' column.
#' @param main_x_axis title of the x-axis
#' @param max.value the maximum value that will be plotted on the x-axis.
#' Must be strictly superior to 1 when equivalent odds ratio (eOR) measure is used, and strictly superior to 0 when SMD measure is used.
#' Note that minimum value will be automatically set to the inverse of \code{max.value} for eOR measure and to \code{- max.value} for SMD measure.
#' @param print.classes a vector of classes. Only factors reaching these classes will be displayed on the plot.
#' These classes must be:
#' - "I", "II", "III", "IV" and/or "ns" for the "Ioannidis" classification
#' - "High", "Moderate", "Weak" and/or "Very weak" for the "GRADE" classification
#' - "I", "II", "III", "IV", and/or "V" for the "Personalized" classification
#' @param col_sig a vector of two colors. Statistically significant factors are displayed using these colors:
#' - Protective factors (i.e., with an eOR < 1 or an SMD < 0) are displayed with the first color
#' - Risk factors (i.e., with an eOR >= 1 or an SMD >= 0) will be displayed with the second color.
#' - Non-significant factors are displayed with the color indicated in the 'col_dots' argument.
#' @param log_cex_dots a logical value indicating whether the logarithm of the size of the dot should be used (should be TRUE or FALSE). When set as TRUE, this argument allows to reduce the disparities in the size of the dots between factors.
#' @param fix_size_dots a unique positive numeric value that is used to fix the size of all dots
#' @param xlim a vector containing the limits of the x-axis (x1, x2) of the plot.
#' @param xlim_main_title position of the title of the plot on the x-axis.
#' @param xlim_value position of the 'value' column on the x-axis.
#' @param xlim_factor position of the 'factor' column on the x-axis.
#' @param ylim_correction_value when the position of columns is modified (e.g., "right-align" versus "center"), the height of the columns can be slighlty modified. This correction value allows to adjust the height of the 'value' column.
#' @param ylim_correction_text when the position of columns is modified (e.g., "right-align" versus "center"), the height of the columns can be slighlty modified. This correction value allows to adjust the height of the 'factor' column and to any new column added to the plot.
#' @param cex_title numeric value indicating the amount by which title text should be scaled relative to the default.
#' @param cex_text_header numeric value indicating the amount by which the text of the header of the 'factor' column and of any column added to the plot should be scaled relative to the default.
#' @param cex_text numeric value indicating the amount by which the text of the 'factor' column and of any column added to the plot should be scaled relative to the default.
#' @param cex_value_header numeric value indicating the amount by which the text of the header of the 'value' column should be scaled relative to the default.
#' @param cex_value numeric value indicating the amount by which the text of the 'value' column should be scaled relative to the default.
#' @param cex_x_axis numeric value indicating the amount by which the text of the x-axis title should be scaled relative to the default.
#' @param cex_x_axis_value numeric value indicating the amount by which the text of the x-axis values should be scaled relative to the default.
#' @param cex_dots numeric value indicating the amount by which dot size should be scaled relative to the default.
#' @param col_title color of the font of the title.
#' @param col_text_header color of the font of the header of the 'factor' column and to any column added to the plot.
#' @param col_text color of the font of the 'factor' column and of any column added to the plot.
#' @param col_value_header color of the header of the 'value' column.
#' @param col_value color of the font of the 'value' column.
#' @param col_x_axis color of the font of the x-axis.
#' @param col_dots color of the dots.
#' @param col_lines color of the lines.
#' @param pos_value type of centering of the column displaying the values. Must be either "left-align", "center", or "right-align".
#' @param pos_text type of centering of the 'factor' column. Must be either "left-align", "center", or "right-align".
#' @param add_columns a vector/dataframe containing the columns that should be added to the plot. The number of rows of the columns added to the plot should be equal to the number of factors.
#' @param main_add_columns a vector containing the name of the header of the new columns added to the plot (default is the name of the columns in the dataset).
#' @param xlim_add_columns position of the new columns added on x-axis. Should be a numeric vector containing the exact same number values as the number of columns added. Mandatory when columns are added to the plot.
#' @param x_lim_adj a numeric value that can be used to adjust the size of the window of the plot on the x-axis.
#' @param y_lim_adj a numeric value that can be used to adjust the size of the window of the plot on the y-axis.
#' @param x_axis_adj a numeric value that can be used to adjust the height of the space between the x-axis and the title of the x-axis.
#' @param ... additional arguments that can be passed to the forest.umbrella function
#'
#' @details
#' The function allows to have a visualization of the results of an umbrella review.
#' Various parameters, such as the type of effect size displayed, the restriction to some classes or the color of the dots, allows to simplify the visualization.
#'
#' @return
#' In addition to the plot, the function returns a list including:
#' \itemize{
#'   \item a dataset with the factors, their class and their effect size. Particularly useful when adding a column via the 'add_columns' argument to obtain the ordering of the factors in the plot. See the vignette dedicated to the forest plots for a concrete example.
#'   \item the optimal width and height of the plot, useful when calling the function \code{pdf()} or \code{png()}.
#'}
#'
#' @exportS3Method
#'
#' @export forest.umbrella
#'
#' @md
#'
#' @references
#' Tortella-Feliu, M. and Fullana, M.A., Perez-Vigil, A., Torres, X., Chamorro, J., and Littarelli, S.A., ..., & Radua, J. (2019). Risk Factors for Posttraumatic Stress Disorder: An Umbrella Review of Systematic Reviews and Meta-Analyses.
#' \emph{Neuroscience & Biobehavioral Reviews}, \bold{107}, 154--165.
#'
#' @examples
#' \donttest{
#' ### perform an umbrella review of various datasets
#' umbs <- union.umbrella(union.umbrella(union.umbrella(union.umbrella(
#' umbrella(df.OR), umbrella(df.SMD)), umbrella(df.RR)), umbrella(df.IRR)), umbrella(df.HR))
#'
#' ### stratify the evidence according to some Personalized criteria
#' umbs.crit <- add.evidence(umbs, criteria = "Personalized",
#'   class_I = c(n_cases = 1000, p_value = 1e-6, esb_p = 0.10, egger_p = 0.10,
#'   I2 = 0.25, pi = "notnull"),
#'   class_II = c(n_cases = 1000, p_value = 1e-6, largest_CI = "notnull"),
#'   class_III = c(n_cases = 1000, p_value = 1e-3),
#'   class_IV = c(p_value = 5e-2))
#' sum.umbs <- summary.umbrella(umbs.crit)
#'
#' ### generate a forest plot of each factor included in the umbrella review
#' forest.umbrella(umbs.crit, max.value = 7,
#'                 main_title = "Plot of the umbrella review")
#'}
forest.umbrella <- function (x,
                             measure = "eG",
                             main_title = NA,
                             main_value = NA,
                             main_x_axis = NA,
                             max.value = NULL,
                             print.classes = NULL,
                             col_sig = c("#252525", "#252525"),
                             log_cex_dots = FALSE,
                             fix_size_dots = NA,
                             xlim = NULL,
                             xlim_main_title = 0,
                             xlim_value = 2.1,
                             xlim_factor = -2.1,
                             ylim_correction_value = 0,
                             ylim_correction_text = 0,
                             cex_title = 1.4,
                             cex_text_header = 1,
                             cex_text = 0.9,
                             cex_value_header = 1,
                             cex_value = 0.9,
                             cex_x_axis = 1.1,
                             cex_x_axis_value = 0.8,
                             cex_dots = 1.2,
                             col_title = "#1D1D1D",
                             col_text_header = "#252525",
                             col_text = "#252525",
                             col_value_header = "#252525",
                             col_value = "#252525",
                             col_x_axis = "#252525",
                             col_dots = "#252525",
                             col_lines = "#252525",
                             pos_value = "left-align",
                             pos_text = "right-align",
                             add_columns = NULL,
                             main_add_columns = NA,
                             xlim_add_columns = NA,
                             x_lim_adj = 0,
                             y_lim_adj = 0,
                             x_axis_adj = 0,
                             ...) {

  if (!inherits(x, "umbrella")) { stop("The 'x' argument must be an 'umbrella' object") }

  if (!measure %in% c("SMD", "eG", "OR", "eOR")) {
    stop("The 'measure' argument must be either 'eOR' or 'eG'")
  } else if (measure =="SMD") {
    measure <- "eG"
  } else if (measure == "OR") {
    measure <- "eOR"
  }

  if (pos_value == "right-align") {
    pos_value = 2
    pos_value_ylim_cor = 0
  } else if (pos_value == "left-align") {
    pos_value = 4
    pos_value_ylim_cor = 0
  } else if (pos_value == "center") {
    pos_value = 3
    pos_value_ylim_cor = -0.45
  } else {
    stop("The 'pos_value' must be either 'left-align', 'right-align' or 'center'.")
  }
  if (pos_text == "right-align") {
    pos_text = 2
    pos_text_ylim_cor = 0
  } else if (pos_text == "left-align") {
    pos_text = 4
    pos_text_ylim_cor = 0
  } else if (pos_text == "center") {
    pos_text = 3
    pos_text_ylim_cor = -0.45
  } else {
    stop("The 'pos_text' must be either 'left-align', 'right-align' or 'center'.")
  }

  criteria = attr(x,"criteria")
  y <- NULL
  for (name in names(x)) {
    x_i <- x[[name]]

    if (is.null(criteria) || is.null(print.classes) || x_i$evidence %in% print.classes) {
      y_i <- x_i$random$value
      ci_lo_i <- x_i$random$ci_lo
      ci_up_i <- x_i$random$ci_up
      if (x_i$measure == "eOR" && measure == "eG") {
        y_i <- .or_to_d(exp(y_i));
        ci_lo_i <- .or_to_d(exp(ci_lo_i));
        ci_up_i <- .or_to_d(exp(ci_up_i));
      } else if (x_i$measure == "eG" && measure == "eOR") {
        y_i <- log(.d_to_or(y_i));
        ci_lo_i <- log(.d_to_or(ci_lo_i));
        ci_up_i <- log(.d_to_or(ci_up_i));
      }
      y_i <- data.frame(y = y_i,
                        ci_lo = ci_lo_i,
                        ci_up = ci_up_i
      )
      if (!is.null(criteria)) {
        if (criteria == "GRADE") {

          class <- switch(x_i$evidence,
                          "High" = 1,
                          "Moderate" = 2,
                          "Weak" = 3,
                          4
          )
        } else if (criteria == "Ioannidis") {
          class <- switch(x_i$evidence,
                          "I" = 1,
                          "II" = 2,
                          "III" = 3,
                          "IV" = 4,
                          5
          )
          # modification
        } else if (criteria == "Personalised") {
          class <- switch(x_i$evidence,
                          "I" = 1,
                          "II" = 2,
                          "III" = 3,
                          "IV" = 4,
                          5
          )
        }
        y_i$class = class
      }
      rownames(y_i) <- name
      y <- rbind(y, y_i)
    }
  }
  if (is.null(y)) {
    warning("No factors to plot")
    return(invisible(list(optimal.width = NA, optimal.height = NA)));
  }

  n.stud <- nrow(y);
  y <- y[order(abs(y$y), decreasing = TRUE),]

  if (!is.null(criteria)) {
    CLASS <- y$class
    CLASS <- sort(.as_numeric(CLASS))
    LEN <- ifelse(length(CLASS) == 1, 1, length(CLASS) - 1)
    warn <- ifelse(length(CLASS) == 1, "warning", "ok")

    if (warn != "warning") {
      for (i in 1:LEN) {
        if (!(CLASS[i + 1] == CLASS[i] |
              CLASS[i + 1] == CLASS[i] + 1)) {
          CLASS[which(CLASS == CLASS[i + 1])] = CLASS[i] + 1
        }
      }
    }

    if (min(CLASS) != 1) {
      delta = 1 - min(CLASS)
      CLASS = CLASS + delta
    }

    y <- y[order(y$class),]
    n.classes <- length(unique(y$class))
    pos.y.value <- n.stud + 1 - 1:n.stud + n.classes - CLASS + ylim_correction_value + pos_value_ylim_cor #c(1,2,3,3,4,4)
    pos.y.text <- n.stud + 1 - 1:n.stud + n.classes - CLASS + ylim_correction_text + pos_text_ylim_cor #c(1,2,3,3,4,4)
  } else {
    n.classes <- 0
    pos.y.value <- n.stud + 1 - 1:n.stud + ylim_correction_value + pos_value_ylim_cor
    pos.y.text <- n.stud + 1 - 1:n.stud + ylim_correction_text + pos_text_ylim_cor
  }
  #name of the factors to plot
  labels <- rownames(y)
  # size of points
  if (is.na(fix_size_dots)) {
    lwd <- 1 / (y$ci_up - y$ci_lo);
    if(length(lwd) > 1) {
      lwd <- sqrt(30 + 150 * (lwd - min(lwd)) / (max(lwd) - min(lwd))) * cex_dots
    } else {
      if (lwd < 10) {
        lwd <- 10
      }
    }
    if (log_cex_dots) { lwd <- log(lwd) * 4 }
  } else {
    lwd <- rep(fix_size_dots, nrow(y))
  }
  if (measure == "eG") {
    value.text <- paste0(format(round(y$y, 2), nsmall = 2), " [",
                         format(round(y$ci_lo, 2), nsmall = 2), ", ",
                         format(round(y$ci_up, 2), nsmall = 2), "]")
  } else {
    value.text <- paste0(format(round(exp(y$y), 2), nsmall = 2), " [",
                         format(round(exp(y$ci_lo), 2), nsmall = 2), ", ",
                         format(round(exp(y$ci_up), 2), nsmall = 2), "]")

  }

  if (is.null(max.value)) {
    if (measure == "eG") {
      est.max.value <- max(-quantile(y$ci_lo, 0.05), -y$y, y$y, quantile(y$ci_up, 0.95))
      max.value <- ceiling(est.max.value);
    } else {
      est.max.value <- exp(max(-quantile(y$ci_lo, 0.05), -y$y, y$y, quantile(y$ci_up, 0.95)))
      power <- 10^(-floor(log10(abs(est.max.value))));
      max.value <- log(ceiling(est.max.value * power) / power);
    }
  } else {
    if (measure == "eOR") {
      max.value <- log(max.value)
    }
  }
  ################################
  ######## Start the plot ########
  ################################
  plot.new();
  if (is.null(xlim)) {
    xlim <- c(x_lim_adj -2.5 - max(strwidth(labels, units = "inches")),
              max(strwidth(value.text, units = "inches")) + 2.5 - x_lim_adj);
  }
  ylim <- c(-2.2 + y_lim_adj, n.stud + n.classes + 2 - y_lim_adj);
  plot.window(xlim = xlim, ylim = ylim, ...);

  # Plot the axes:
  # y axis
  lines(x = c(0, 0), y = c(n.classes + n.stud + 0.5, 0), col = "#5D5D5D", lty = 1);
  # x axis
  lines(x = c(-2, 2), y = rep(0, 2), col = "#5D5D5D", lty = 1);
  # seq x axis
  for (pos.x in -2:2) {
    lines(rep(pos.x, 2), c(0, -0.2), col = "#5D5D5D", lty = 1);
    if (measure == "eG") {
      text(pos.x, -0.2, round((pos.x) / 2 * max.value, 2), pos = 1,
           col = "#5D5D5D", cex = cex_x_axis_value);
    } else {
      text(pos.x, -0.2, round(exp((pos.x) / 2 * max.value), 2), pos = 1,
           col = "#5D5D5D", cex = cex_x_axis_value);
    }
  }


  #### TITLE
  if (!is.na(main_title)) {
    text(x = xlim_main_title, y = n.classes + n.stud + 2, paste0(main_title),
         col = col_title, font = 1, family = "sans", cex = cex_title)
  }

  #### X-AXIS: name of the measure
  if (measure == "eG") {
    if (is.na(main_x_axis)) { main_x_axis <- "Equivalent Hedges's g (eG)" }
    text(x = 0, y = -1.7 + x_axis_adj, paste0(main_x_axis),
         col = col_x_axis, font = 2, family = "sans", cex = cex_x_axis)
  } else {
    if (is.na(main_x_axis)) { main_x_axis <- "Equivalent Odds Ratio (eOR)" }
    text(x = 0, y = -1.7 + x_axis_adj, paste0(main_x_axis),
         col = col_x_axis, font = 2, family = "sans", cex = cex_x_axis)
  }
  #### HEADER for criteria
  if (!is.null(criteria)) {
    if (criteria == "GRADE") {
      if (any(y$class == 1)) {
        text(x = xlim_factor, y = - 0.05 + n.classes + n.stud + ylim_correction_text + pos_text_ylim_cor, "GRADE 4\n(high)", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 2)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 1) + any(y$class == 3) + any(y$class == 4) + 1 + ylim_correction_text + pos_text_ylim_cor, "GRADE 3\n(moderate)", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 3)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 2) + any(y$class == 4) + 1 + ylim_correction_text + pos_text_ylim_cor, "GRADE 2\n(weak)", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 4)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 3) + 1 + ylim_correction_text + pos_text_ylim_cor, "GRADE 1\n(very weak)", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
    } else if (criteria == "Ioannidis") {
      if (any(y$class == 1)) {
        text(x = xlim_factor, y = - 0.05 + n.classes + n.stud + ylim_correction_text + pos_text_ylim_cor, "Class I", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 2)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 1) + any(y$class == 3) + any(y$class == 4) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class II", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 3)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 2) + any(y$class == 4) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class III", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 4)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 3) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class IV", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 5)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 4) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class ns", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
    } else if (criteria == "Personalised") {
      if (any(y$class == 1)) {
        text(x = xlim_factor, y = - 0.05 + n.classes + n.stud + ylim_correction_text + pos_text_ylim_cor, "Class I", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 2)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 1) + any(y$class == 3) + any(y$class == 4) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class II", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 3)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 2) + any(y$class == 4) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class III", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 4)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 3) + any(y$class == 5) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class IV", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
      if (any(y$class == 5)) {
        text(x = xlim_factor, y = - 0.05 + sum(y$class > 4) + 1 + ylim_correction_text + pos_text_ylim_cor, "Class V", pos = pos_text, font = 2, col = "#252525", family = "sans", cex = cex_value_header)
      }
    }
  }

  ############################################################
  ######## plot summary estimates for each factor ############
  ############################################################
  for (i in 1:n.stud) {
    pos.y.value_i <- pos.y.value[i]
    pos.y.text_i <- pos.y.text[i]
    y_i <- y$y[i]
    ci_lo_i <- y$ci_lo[i]
    ci_up_i <- y$ci_up[i]

    if (any(is.na(col_sig))) { col_sig <- col_dots}
    col2_i <- ifelse(ci_lo_i > 0, col_sig[2], ifelse(ci_up_i < 0, col_sig[1], col_dots));

    # plot value
    if (y_i < max.value) {
      lines(x = rep(y_i / max.value * 2, 2), y = rep(pos.y.value_i, 2) - pos_value_ylim_cor - ylim_correction_value,
            lwd = lwd[i], col = col2_i);
    }

    if (ci_lo_i < max.value) {
      lines(x = c(max(ci_lo_i / max.value * 2, -2),
                  min(ci_up_i / max.value * 2, 2)),
            y = rep(pos.y.value_i, 2) - pos_value_ylim_cor - ylim_correction_value, lend = 2, col = col2_i);
      if (ci_lo_i > -max.value) {
        lines(x = rep(ci_lo_i / max.value * 2, 2),
              y = pos.y.value_i + c(0.0, 0.0) - pos_value_ylim_cor - ylim_correction_value,
              lend = 2, col = col2_i);
      }
      if (ci_up_i < max.value) {
        lines(x = rep(ci_up_i / max.value * 2, 2),
              y = pos.y.value_i + c(0.0, 0.0) - pos_value_ylim_cor - ylim_correction_value,
              lend = 2, col = col2_i);
      }
    }
    text(x = xlim_factor, y = pos.y.text_i, labels[i], pos = pos_text, col = col_text, family = "sans", cex = cex_text);
    text(x = xlim_value, y = pos.y.value_i, value.text[i], pos = pos_value, col = col_value, family = "sans", cex = cex_value);
  }

  if(is.null(criteria)) { text(xlim_factor, max(pos.y.text) + 1, "Factors", pos = pos_text, col = col_text_header, font = 2, family = "sans", cex = cex_text_header * 1.025);}

  if (measure == "eG") {
    if (is.na(main_value)) { main_value = "eG [95% CI]"}
    text(x = xlim_value, y = max(pos.y.value) + 1 , paste0(main_value), pos = pos_value, col = col_value_header, font = 2, family = "sans", cex = cex_value_header);
  } else {
    if (is.na(main_value)) { main_value = "eOR [95% CI]"}
    text(x = xlim_value, y = max(pos.y.value) + 1, paste0(main_value), pos = pos_value, col = col_value_header, font = 2, family = "sans", cex = cex_value_header);
  }

  base_pos = 0
  if (!is.null(add_columns)) {
    if (is.vector(add_columns)) { add_columns <- data.frame(add_columns) }
    if (length(xlim_add_columns) < ncol(add_columns)) {
      stop("The 'xlim_add_columns' argument contains less values than the number of columns of the dataset in 'add_columns'.")
    }
    if (any(is.na(main_add_columns))) { main_add_columns <- substr(paste0(colnames(add_columns)), start = 1, stop = 7)}
    for (col in colnames(add_columns)) {
      base_pos = base_pos + 1
      for (i in 1:n.stud) {
        pos.y.text_i <- pos.y.text[i]
        text(x = xlim_add_columns[base_pos], y = pos.y.text_i, add_columns[i, col], pos = pos_text, col = col_text, font = 1, family = "sans", cex = cex_text)
      }
      text(x = xlim_add_columns[base_pos], y = max(pos.y.text) + 1, paste0(main_add_columns[base_pos]), pos = pos_text, col = col_text_header, font = 2, family = "sans", cex = cex_text_header)
    }
  }

  y <- cbind(y, factor = row.names(y))
  width <- round(diff(xlim));
  height <- round(diff(ylim) / 3);
  cat("\n");
  cat("Use pdf(filename, width, height) before calling forest to save it.\n");
  cat("The optimal width and height of this plot is ~", width, " x ~",
      height, " inches.\n", sep = "");
  cat("\n");
  return(list(factor = data.frame(y),
              size = list(optimal.width = width, optimal.height = height)))
}


