#' Forest plots for objects of class \dQuote{umbrella} or \dQuote{data.frame}
#'
#' Draw a forest plot of the factors included in an umbrella review.
#'
#' @param x an object of class \dQuote{umbrella} or \dQuote{data.frame}
#' @param ... additional arguments that can be passed to this function
#'
#' @export forest
#'
#' @md
#'
#' @details
#' For now, this function simply applies the \code{\link{forest.umbrella}()} function.
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


#' Forest plots for \dQuote{data.frame} objects
#'
#' Draw a forest plot of the factors included in an umbrella review.
#' This function is now a wrapper around the amazing forest.meta function designed by
#' Guido Schwarzer and Gerta Rucker.
#' You can thus add any other arguments available in the forest.meta function but not detailed here.
#'
#' @param x an object of class \dQuote{umbrella} or a or \dQuote{data.frame} object returned by the \code{umbrella()} or \code{add.evidence()} functions.
#' @param measure type of effect size used in the plot ("eG", "eOR", "raw" or "auto"). Default is the most frequently used effect size measure.
#' @param layout type of layout of the plot ("meta", "JAMA" or "RevMan5").
#' @param leftcols vector of columns contained in the object passed to the \code{x} argument, used to specify columns which are printed on the left side of the forest plot.
#' @param leftlabs vector of column names used to specify column names printed on the left side of the forest plot.
#' @param rightcols vector of columns contained in the object passed to the \code{x} argument, used to specify columns which are printed on the right side of the forest plot.
#' @param rightlabs vector of column names used to specify column names printed on the left side of the forest plot.
#' @param digits number of digits to display
#' @param smlab character string describing the title of the plot
#' @param xlab character string describing the x-axis title
#' @param type.study the shape used to depict the pooled effect size (must be either "square", "diamond", or "predict")
#' @param print.classes a vector of classes. Only factors reaching these classes will be displayed on the plot. These classes must be "I", "II", "III", "IV" and/or "ns" for the "Ioannidis" classification, or "High", "Moderate", "Weak" and/or "Very weak" for the "GRADE" classification, or "I", "II", "III", "IV", and/or "V" for the "Personalized" classification
#' @param subgroup a character variable indicating the name of the column that should be used as header for creating subgroups between the factors
#' @param subgroup.name a character variable displayed just before each modality of the subgroup variable
#' @param col.square The colour for squares reflecting study's weight in the meta-analysis.
#' @param col.study The colour for individual study results and confidence limits.
#' @param col.square.lines The colour for the outer lines of squares reflecting study weight in the meta-analysis.
#' @param fontsize The size of text (in points)
#' @param spacing A numeric variable determining line spacing in a forest plot.
#' @param squaresize A numeric variable used to increase or decrease the size of squares in the forest plot.
#' @param ... additional arguments that can be passed to the forest.umbrella function
#'
#' @details
#' The function allows to have a visualization of the results of an umbrella review.
#' Various parameters, such as the type of effect size displayed, the restriction to some classes or the color of the dots, allows to simplify the visualization.
#'
#' @return
#' Return a forest plot of the pooled effect sizes, along with additional information
#'
#' @exportS3Method
#'
#' @keywords internal
#'
#' @export forest.data.frame
#'
#' @noMd
forest.data.frame <- function (x,
                               layout = "meta",
                               measure = "auto",
                               leftcols = NULL,
                               leftlabs = NULL,
                               rightcols = NULL,
                               rightlabs = NULL,
                               digits = 2,
                               smlab = "",
                               xlab = NULL,
                               type.study = "square",
                               print.classes = FALSE,
                               subgroup = NULL,
                               subgroup.name = "",
                               col.square = "gray",
                               col.study = "black",
                               col.square.lines = "black",
                               fontsize = 12,
                               spacing = 1,
                               squaresize = 0.8 / spacing,
                               ...) {

  if (!class(x) %in% c("umbrella", "data.frame")) { stop("The 'x' argument must be an 'umbrella' object") }

  if (inherits(x, "umbrella")) {
    dat = .quiet(summary(x, het_max = TRUE))
  } else {
    dat = x
  }
  if ("I2" %in% colnames(dat)) dat$I2 = dat$I2/100

  if (!measure %in% c("SMD", "eSMD", "eG", "OR", "eOR", "raw", "auto")) {
    stop("The 'measure' argument must be either 'auto', 'eOR', 'eG' or 'raw'")
  } else if (measure =="SMD" | measure == "eSMD" | measure == "G") {
    measure <- "eG"
  } else if (measure == "OR") {
    measure <- "eOR"
  }

  if (measure == "auto") {
    len.G = sum(dat$measure == "G")
    len.OR = sum(dat$measure %in% c("OR", "RR", "HR", "IRR"))
    len.other = sum(dat$measure %in% c("R", "MD"))
    measure = c("eG", "eOR", "raw")[which.max(c(len.G, len.OR, len.other))]
  }
  if (measure %in% c("eG")) {
    dat$es = dat$eG
    dat$ci_lo = te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$eG_CI))))
    dat$ci_up = te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$eG_CI))))
  } else if (measure == "eOR") {
    dat$es = log(dat$eOR)
    te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$eOR_CI))))
    te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$eOR_CI))))
    dat$ci_lo = log(te_lo)
    dat$ci_up = log(te_up)
  } else if (measure %in% c("raw")) {
    dat$es = dat$value
    dat$ci_lo = te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$value_CI))))
    dat$ci_up = te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$value_CI))))
  }

  if (!all(print.classes == FALSE)) {
    if ("Class" %in% colnames(dat)) {
      dat = subset(dat, dat$Class %in% print.classes)
    }
  }

  if (!is.null(subgroup)) {
    res = meta::metagen(TE = es,
                        lower = ci_lo, upper = ci_up,
                        digits = digits,
                        sm = ifelse(measure == "eOR", "OR", "SMD"),
                        data = dat,
                        # split
                        subgroup = dat[, subgroup])
  } else {
    res = meta::metagen(TE = es,
                        lower = ci_lo, upper = ci_up,
                        # digits = digits,
                        sm = ifelse(measure == "eOR", "OR", "SMD"),
                        data = dat)
  }


  if (is.null(leftcols) & is.null(leftlabs)) {
    leftcols = c("Factor", "n_studies", "n_cases", "I2")
    leftlabs = c("Factor", "n-studies", "n-cases",  "I2")
    if ("Class" %in% colnames(dat)) {
      if (is.null(subgroup)) {
        leftcols = append(leftcols, "Class")
        leftlabs = append(leftlabs, "Class")
      } else {
        if (subgroup != "Class") {
          leftcols = append(leftcols, "Class")
          leftlabs = append(leftlabs, "Class")
        }
      }
    }
    if (measure == "raw") {
      leftcols = append(leftcols, "measure", 1)
      leftlabs = append(leftlabs, "Measure", 1)
    }

  }
  if (is.null(rightcols) & is.null(rightlabs)) {
    rightcols = c("effect.ci")
    rightlabs = ifelse(measure == "eOR",
                       "eOR + 95% CI",
                       ifelse(measure == "eG",
                              "eG + 95% CI",
                              "ES + 95% CI"))
  }

  xlab = ifelse(is.null(xlab),
                ifelse(measure == "eOR",
                       "Equivalent Odds Ratio (eOR)",
                       ifelse(measure == "eG",
                              "Equivalent\n Standardized Mean Difference (eG)",
                              "Effect size value")), xlab)

  suppressWarnings(meta::forest.meta(
    res,
    digits = digits,
    layout = layout,
    type.study = type.study,
    common = FALSE,
    random = FALSE,
    overall = FALSE,
    subgroup = ifelse(is.null(subgroup), FALSE, TRUE),
    subgroup.name = subgroup.name,
    smlab = smlab,
    xlab = xlab,
    leftcols = leftcols,
    leftlabs = leftlabs,
    rightcols = rightcols,
    rightlabs = rightlabs,
    hetstat = FALSE,
    col.study = col.study,
    col.square = col.square,
    col.square.lines = col.square.lines,
    fontsize = fontsize,
    spacing = spacing,
    squaresize = squaresize,
    backtransf = ifelse(measure == "eOR", TRUE, FALSE),
    ...))

}
#' Forest plots for \dQuote{umbrella} objects
#'
#' Draw a forest plot of the factors included in an umbrella review.
#' This function is now a wrapper around the amazing forest.meta function designed by
#' Guido Schwarzer and Gerta Rucker.
#' You can thus add any other arguments available in the forest.meta function but not detailed here.
#'
#' @param x an object of class \dQuote{umbrella} or a or \dQuote{data.frame} object returned by the \code{umbrella()} or \code{add.evidence()} functions.
#' @param measure type of effect size used in the plot ("eG", "eOR", "raw" or "auto"). Default is the most frequently used effect size measure.
#' @param layout type of layout of the plot ("meta", "JAMA" or "RevMan5").
#' @param leftcols vector of columns contained in the object passed to the \code{x} argument, used to specify columns which are printed on the left side of the forest plot.
#' @param leftlabs vector of column names used to specify column names printed on the left side of the forest plot.
#' @param rightcols vector of columns contained in the object passed to the \code{x} argument, used to specify columns which are printed on the right side of the forest plot.
#' @param rightlabs vector of column names used to specify column names printed on the left side of the forest plot.
#' @param digits number of digits to display
#' @param smlab character string describing the title of the plot
#' @param xlab character string describing the x-axis title
#' @param type.study the shape used to depict the pooled effect size (must be either "square", "diamond", or "predict")
#' @param print.classes a vector of classes. Only factors reaching these classes will be displayed on the plot. These classes must be "I", "II", "III", "IV" and/or "ns" for the "Ioannidis" classification, or "High", "Moderate", "Weak" and/or "Very weak" for the "GRADE" classification, or "I", "II", "III", "IV", and/or "V" for the "Personalized" classification
#' @param subgroup a character variable indicating the name of the column that should be used as header for creating subgroups between the factors
#' @param subgroup.name a character variable displayed just before each modality of the subgroup variable
#' @param col.square The colour for squares reflecting study's weight in the meta-analysis.
#' @param col.study The colour for individual study results and confidence limits.
#' @param col.square.lines The colour for the outer lines of squares reflecting study weight in the meta-analysis.
#' @param fontsize The size of text (in points)
#' @param spacing A numeric variable determining line spacing in a forest plot.
#' @param squaresize A numeric variable used to increase or decrease the size of squares in the forest plot.
#' @param ... additional arguments that can be passed to the forest.umbrella function
#'
#' @details
#' The function allows to have a visualization of the results of an umbrella review.
#' Various parameters, such as the type of effect size displayed, the restriction to some classes or the color of the dots, allows to simplify the visualization.
#'
#' @return
#' Return a forest plot of the pooled effect sizes, along with additional information
#'
#' @exportS3Method
#'
#' @export forest.umbrella
#'
#' @md
#'
#' @references
#' Balduzzi S, Rucker G, Schwarzer G (2019). How to perform a meta-analysis with R: a practical tutorial. \emph{Evidence-Based Mental Health}, 153–160.
#'
#' @examples
#' ### perform an umbrella review
#' umb <- umbrella(df.OR)
#'
#' ### generate a forest plot of each factor included in the umbrella review
#' forest(umb)
forest.umbrella <- function (x,
                             layout = "meta",
                             measure = "auto",
                             leftcols = NULL,
                             leftlabs = NULL,
                             rightcols = NULL,
                             rightlabs = NULL,
                             digits = 2,
                             smlab = "",
                             xlab = NULL,
                             type.study = "square",
                             print.classes = FALSE,
                             subgroup = NULL,
                             subgroup.name = "",
                             col.square = "gray",
                             col.study = "black",
                             col.square.lines = "black",
                             fontsize = 12,
                             spacing = 1,
                             squaresize = 0.8 / spacing,
                             ...) {

  if (!class(x) %in% c("umbrella", "data.frame")) { stop("The 'x' argument must be an 'umbrella' object") }

  if (inherits(x, "umbrella")) {
    dat = .quiet(summary(x, het_max = TRUE))
  } else {
    dat = x
  }
  if ("I2" %in% colnames(dat)) dat$I2 = dat$I2/100

  if (!measure %in% c("SMD", "eSMD", "eG", "OR", "eOR", "raw", "auto")) {
    stop("The 'measure' argument must be either 'auto', 'eOR', 'eG' or 'raw'")
  } else if (measure =="SMD" | measure == "eSMD" | measure == "G") {
    measure <- "eG"
  } else if (measure == "OR") {
    measure <- "eOR"
  }

  if (measure == "auto") {
    len.G = sum(dat$measure == "G")
    len.OR = sum(dat$measure %in% c("OR", "RR", "HR", "IRR"))
    len.other = sum(dat$measure %in% c("R", "MD"))
    measure = c("eG", "eOR", "raw")[which.max(c(len.G, len.OR, len.other))]
  }
  if (measure %in% c("eG")) {
    dat$es = dat$eG
    dat$ci_lo = te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$eG_CI))))
    dat$ci_up = te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$eG_CI))))
  } else if (measure == "eOR") {
    dat$es = log(dat$eOR)
    te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$eOR_CI))))
    te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$eOR_CI))))
    dat$ci_lo = log(te_lo)
    dat$ci_up = log(te_up)
  } else if (measure %in% c("raw")) {
    dat$es = dat$value
    dat$ci_lo = te_lo = as.numeric(as.character(gsub("\\[", "", gsub(",.*", "", dat$value_CI))))
    dat$ci_up = te_up = as.numeric(as.character(gsub("\\]", "", gsub(".*, ", "", dat$value_CI))))
  }

  if (!all(print.classes == FALSE)) {
    if ("Class" %in% colnames(dat)) {
      dat = subset(dat, dat$Class %in% print.classes)
    }
  }

  if (!is.null(subgroup)) {
    res = meta::metagen(TE = es,
                        lower = ci_lo, upper = ci_up,
                        digits = digits,
                        sm = ifelse(measure == "eOR", "OR", "SMD"),
                        data = dat,
                        # split
                        subgroup = dat[, subgroup])
  } else {
    res = meta::metagen(TE = es,
                        lower = ci_lo, upper = ci_up,
                        # digits = digits,
                        sm = ifelse(measure == "eOR", "OR", "SMD"),
                        data = dat)
  }


  if (is.null(leftcols) & is.null(leftlabs)) {
    leftcols = c("Factor", "n_studies", "n_cases", "I2")
    leftlabs = c("Factor", "n-studies", "n-cases",  "I2")
    if ("Class" %in% colnames(dat)) {
      if (is.null(subgroup)) {
        leftcols = append(leftcols, "Class")
        leftlabs = append(leftlabs, "Class")
      } else {
        if (subgroup != "Class") {
          leftcols = append(leftcols, "Class")
          leftlabs = append(leftlabs, "Class")
        }
      }
    }
      if (measure == "raw") {
        leftcols = append(leftcols, "measure", 1)
        leftlabs = append(leftlabs, "Measure", 1)
      }

  }
  if (is.null(rightcols) & is.null(rightlabs)) {
    rightcols = c("effect.ci")
    rightlabs = ifelse(measure == "eOR",
                       "eOR + 95% CI",
                       ifelse(measure == "eG",
                              "eG + 95% CI",
                              "ES + 95% CI"))
  }

  xlab = ifelse(is.null(xlab),
                ifelse(measure == "eOR",
                  "Equivalent Odds Ratio (eOR)",
                  ifelse(measure == "eG",
                         "Equivalent\n Standardized Mean Difference (eG)",
                         "Effect size value")), xlab)

  suppressWarnings(meta::forest.meta(
    res,
    digits = digits,
    layout = layout,
    type.study = type.study,
    common = FALSE,
    random = FALSE,
    overall = FALSE,
    subgroup = ifelse(is.null(subgroup), FALSE, TRUE),
    subgroup.name = subgroup.name,
    smlab = smlab,
    xlab = xlab,
    leftcols = leftcols,
    leftlabs = leftlabs,
    rightcols = rightcols,
    rightlabs = rightlabs,
    hetstat = FALSE,
    col.study = col.study,
    col.square = col.square,
    col.square.lines = col.square.lines,
    fontsize = fontsize,
    spacing = spacing,
    squaresize = squaresize,
    backtransf = ifelse(measure == "eOR", TRUE, FALSE),
    ...))
}

# x = sumumb
# layout = "meta"
# measure = "eOR"
# leftcols = leftlabs = rightcols = rightlabs = NULL
# digits = 2
# xlim = "auto"
# sort = "default"
# smlab = ""
# xlab = ""
# type.study = "square"
# subgroup = NULL
# subgroup.name = ""
# col.study = "gray"
# col.square = "gray"
# col.square.lines = "black"
# fontsize = 12
# spacing = 1
# squaresize = 0.8 / spacing
# just = if (layout == "JAMA") "left" else "right"
# just.studlab = "left"
# just.addcols = "center"
# just.addcols.left = "center"
# just.addcols.right = "center"
#
