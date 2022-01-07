#' Automatically conduct an umbrella review and export results outside of R
#'
#' The \code{umbrella.xls} function allows to automatically perform an umbrella review and to export results outside of R.
#'
#' @param input_file path indicating the folder where the dataset (stored in an excel file) is located.
#' @param sheet the number of the excel sheet where the data are located.
#' @param output_path path indicating the folder where the results to be generated.
#' @param output_name name of the files created by the function.
#' @param factors_to_analyze the name(s) of factors to analyze. Can be either a character string indicating the name of the factor to analyze or a vector indicating the names of the factors to analyze. By default, all the factors are analyzed.
#' @param evidence criteria used to stratify evidence. Must be either "Ioannidis" or "GRADE".
#' @param plot_results indicate whether a forest plot of the results is returned. Must be either TRUE or FALSE.
#'
#' @details
#' This function implicitly calls the \code{view.errors.umbrella()} to check the formatting of the data,
#' the \code{umbrella()} function to perform calculations, the \code{add.evidence()} function to stratify evidence (limited to "Ioannidis" and "GRADE" classifications for now)
#'  and the \code{forest()} function to generate a visual representation of the results.
#'
#' @return
#' The \code{umbrella.xls()} function returns several elements including
#' \tabular{ll}{
#'  \code{csv} \tab a csv file containing the results of the umbrella review.\cr
#'  \tab \cr
#'  \code{pdf} \tab a pdf file containing the plot of the results (only if requested, i.e., the \code{plot_results}\cr
#'  \tab argument is set as TRUE).\cr
#'  \tab \cr
#'  \code{html} \tab an HTML file containing the results of the umbrella review.\cr
#' }
#'
#' @seealso
#' \code{\link{umbrella}()}\cr
#' \code{\link{add.evidence}()}\cr
#' \code{\link{summary.umbrella}()}
#'
#' @export umbrella.xls
#'
#' @md
#'
#' @examples
#' if(interactive()){
#' ### perform an umbrella review according to the GRADE criteria.
#' ### the umbrella review is restricted to the factor "Pharmacological"
#' ### Note that the df.SMD should be stored under a .xls or .xlsx format
#' umbrella.xls(input_file = file.choose(),
#'              sheet = 1,
#'              output_path = choose.dir(),
#'              output_name = "Pharmacological_Ioannidis",
#'              factors_to_analyze = c("Pharmacological"),
#'              evidence = "GRADE",
#'              plot_results = TRUE)
#'}
umbrella.xls <- function(input_file = NULL, sheet = 1, output_path = "", output_name = "",
                         factors_to_analyze = "", evidence = NULL, plot_results = FALSE) {

  # some settings that could be included to the function in future updates --
  discard_column = ""
  sensitivity_analysis_variable = NULL
  sensitivity_analysis_value = NULL

  # GUI - simple
  if (is.null(input_file)) {
    Filters <- matrix(c("Excel 2013", ".xlsx",
                        "Excel", ".xls",
                        "All files", "*"),
                      3, 2, byrow = TRUE)
    input_file = tk_choose.files(filters = Filters)
    if (is.na(input_file)) {
      stop("No input file selected. Nothing to do")
    }
    if (!(.get_file_extension(input_file) %in% c('xlsx','xls'))) {
      stop("Wrong file type. Input file should be an excel (.xlsx or .xls).")
    }
    sheets_excel = excel_sheets( input_file )
    sheet_selected <- tk_select.list(sheets_excel, title="Choose a sheet:", preselect = sheets_excel[1])
    sheet <- match(sheet_selected,sheets_excel)
    print(paste("File selected: ", input_file, "Sheet: ", sheet_selected))
    if(!file.exists(input_file)) {
      stop("File does not exist! Aborting process.\n")
    }
    ## OUTPUT
    output_path = tk_choose.dir(default = dirname(input_file), "Choose where you want to save the output")
    if (is.na(output_path)) {
      stop("No output folder selected. Nothing to do\n")
    }
  }
  # define output folder
  if (!is.null(input_file) && output_path == "") {
    output_path <- dirname(input_file)
  }
  if (output_name == "") {
    output_name <- strsplit(x=basename(input_file),split='[.]')[[1]][1]
  }

  # read excel
  cat("Reading the excel file and checking possible errors...\n")
  x <- .read.excel(path_xls = input_file, sheet = sheet)

  if (!is.null(sensitivity_analysis_variable)) {
    x <- x[x[,sensitivity_analysis_variable] %in% sensitivity_analysis_value,]
    output_tag <- paste0("_", sensitivity_analysis_variable, "_", sensitivity_analysis_value)
  } else {
    output_tag <- ""
  }

  if (factors_to_analyze != "") {
    x = x[x$factor %in% factors_to_analyze, ]
  }
  cat("Performing the umbrella analysis...\n")

  X2 = .check_data(x)

  if(attr(X2, "status") != "NO ERRORS OR WARNINGS") {

    x_out <- attr(X2, "data")

    returned_error <- .write_errors_file(x = x_out, save_errors = sprintf('%s/%s%s', output_path, output_name, output_tag), X2 = X2)
    if (attr(returned_error, "status") == "ERRORS") {
      class(returned_error) <- c("umbrella", class(returned_error))
      attr(returned_error, "save_errors") <- sprintf('%s/%s%s', output_path, output_name, output_tag)
      message(attr(X2, "message"))
      return(returned_error)
    }
  }

  X = umbrella(x)

  if(attr(X2,'status') != 'ERRORS' && !is.null(evidence)) {

    X_Ioannidis = add.evidence(X, evidence)

    umbrella_df = summary(X_Ioannidis)

    input_filename = basename(.get_filename_without_extension(input_file))
    out_res_path = paste0(output_path, "/", output_name, output_tag, ".csv")
    table_html_name = paste0(dirname(out_res_path),"/",output_name, output_tag, ".html")
    print(xtable::xtable(umbrella_df), type="html", file=table_html_name)
    browseURL(table_html_name)

    write.csv(umbrella_df, out_res_path, row.names = TRUE)
    if (attr(X2,"status") != 'NO ERRORS OR WARNINGS') {
      cat(sprintf("%s %s", attr(X2, "status"), attr(X2, "message")))
      cat(sprintf(" Errors and partial results saved in\n: %s \n", dirname(out_res_path)))
    } else {
      cat(paste("\n\nResults saved to: ", out_res_path, "\n"))
    }

    if(plot_results) {
      if (grepl("Ioannidis", evidence)){
        height <- max(sum(umbrella_df$Class %in% c('I','II','III')), 7)
        pdf_file <- paste0(output_path,"/", input_filename, output_tag, "_Ioannidis.pdf")
        pdf(pdf_file,
            width = 7, height = height)
        forest.umbrella(X_Ioannidis, main_title = input_filename)
      } else if (evidence == "GRADE") {
        height <- max(sum(umbrella_df$Class %in% c('High', 'Moderate','Weak', 'Very weak')),7)
        pdf_file <- paste0(output_path, "/", input_filename, output_tag, "_GRADE.pdf")
        pdf(pdf_file,
            width = 7, height = height)
        forest.umbrella(X_Ioannidis, main_title = input_filename)

      }
      dev.off()
      browseURL(pdf_file)
    }
  }
  attr(X, "status") <- attr(X2,"status")
  attr(X, "message") <- attr(X2, "message")
  return(X)
}

#' Load an excel to an umbrella object
#'
#' @param path_xls a path to a xls file
#' @param sheet the sheet in which the data are stored
#' @param factor_columns identify factor column
#'
#' @noRd
.load_umbrella_xls = function(path_xls, sheet=1, factor_columns="factor") {
  # load an excel to an umbrella object
  if(!(.get_file_extension(path_xls) %in% c('xlsx','xls'))){ stop("Wrong file type. Input file should be an excel (.xlsx or .xls).") }
  x = readxl::read_excel(path_xls, sheet=sheet)
  x = .check_data(x)
  if (any(x$column_type_errors=="ERRORS")) {
    stop(paste(
      '\n\n#################################\nERROR: Please fix the errors before continuing. Check file',
      paste0("./errors_to_fix.csv"),
      "\n#################################\n\n")
    )
  }
  if(length(factor_columns)>1) {
    aux_factor = ""
    for (i in 1:length(factor_columns)) {
      aux_factor = paste(aux_factor, x[[which(colnames(x) == factor_columns[i])]])
    }
    x$factor = trimws(aux_factor)
  }
  other_expected_cols=c("factor","measure","n_studies","n_cases", "n_controls",
                        "p.value","value","ci_lo","ci_up","pi_lo","pi_up",
                        "fixed_value","fixed_p",
                        "largest_ci_lo","largest_ci_up",
                        "i2","tau2","heterogeneity_p","heterogeneity_q",
                        "egger_p","egger_bias",
                        "esb_obs","esb_exp","esb_p",
                        "jk_value","jk_p",
                        "evidence_criteria","evidence_value")
  for(col_exp in other_expected_cols) {
    if (! col_exp %in% colnames(x)) x[,col_exp] <- NA
  }
  obj = list()
  for (i in 1:length(x$factor)) {

    if(x$sign[i] != "NA" && !is.na(x$sign[i])) {
      if (x$sign[i]=='decr'&& !is.na(x$value[i])) x$value[i] <- -abs(suppressWarnings(as.numeric(x$value[i])))
      else if (x$sign[i]=='incr' && !is.na(x$value[i])) x$value[i] <- abs(suppressWarnings(as.numeric(x$value[i])))
    }
    x$measure[i] = .fix_measure_name(x$measure[i])
    print(x$measure[i])
    if (!x$measure[i] %in% c("SMD","MD","OR")) {
      x$value[i] = NA
      x$ci_lo[i] = NA
      x$ci_up[i] = NA
    }
    if (toupper(x$measure[i]) == "MD" && !is.na(as.numeric(x$value[i]))) {
      tmp = .improve_ci(as.numeric(x$value[i]), as.numeric(x$ci_lo[i]), as.numeric(x$ci_up[i]), FALSE)
      tmp = .estimate_d_from_md(tmp$value, tmp$ci_lo, tmp$ci_up, as.numeric(x$n_cases[i]), as.numeric(x$n_controls[i]))
      x$value[i] = tmp$value
      x$ci_lo[i] = NA
      x$ci_up[i] = NA
      x$measure[i] = "SMD"
    }
    if(toupper(x$measure[i]) != "SMD") {
      x$value[i] = log(as.numeric(x$value[i]))
      x$ci_lo[i] = log(as.numeric(x$ci_lo[i]))
      x$ci_up[i] = log(as.numeric(x$ci_up[i]))
    }
    n_cases_tmp = x$n_cases[i]
    obj[[x$factor[i]]] = list(
      measure = x$measure[i],
      data = NULL,
      n = data.frame(studies = .as_numeric(x$n_studies[i]), cases = ifelse(is.na(as.numeric(n_cases_tmp)),n_cases_tmp,as.numeric(n_cases_tmp)), controls = .as_numeric(x$n_controls[i])),
      random = data.frame(p.value = x$p.value[i], value = .as_numeric(x$value[i]), ci_lo = .as_numeric(x$ci_lo[i]), ci_up = .as_numeric(x$ci_up[i]), pi_lo = .as_numeric(x$pi_lo[i]), pi_up = .as_numeric(x$pi_up[i])),
      fixed = data.frame(value = .as_numeric(x$fixed_value[i]), p.value = .as_numeric(x$fixed_p[i])),
      largest = data.frame(ci_lo = .as_numeric(x$largest_ci_lo[i]), ci_up = .as_numeric(x$largest_ci_up[i])),
      heterogeneity = data.frame(i2 = .as_numeric(x$i2[i]), tau2 = x$tau2[i], p.value = .as_numeric(x$heterogeneity_p[i]), q = .as_numeric(x$heterogeneity_q[i])),
      egger = data.frame(p.value = .as_numeric(x$egger_p[i]), statistic = x$egger_bias[i]),
      esb = structure(list(observed = x$esb_obs[i], expected = x$esb_exp[i], p.value = .as_numeric(x$esb_p[i])), class = "htest"),
      jk = data.frame(value = .as_numeric(x$jk_value[i]), p.value = .as_numeric(x$jk_p[i])),
      evidence = x$evidence_value[i]
    )
  }
  attr(obj,"criteria") = x$evidence_criteria[1]
  class(obj) = "umbrella"
  obj
}

#' Reads an excel file
#'
#' @param path_xls a path to a xls file
#' @param sheet the sheet in which the data are stored
#'
#' @noRd
.read.excel <- function(path_xls, sheet=1) {

  if (!file.exists(path_xls)) {
    out <- data.frame()
    attr(out, "status") <- "ERRORS"
    attr(out, "message") <- "The input file does not exist."
    return(out)
  }
  if (!(.get_file_extension(path_xls) %in% c('xlsx','xls'))) {
    stop("Wrong file type. Input file should be an excel (.xlsx or .xls).")
  }

  x = as.data.frame(readxl::read_excel(path_xls, sheet=sheet))
  x$row_index = rep(1:nrow(x))
  attr(x, "status") <- "OK"
  colnames(x) <- tolower(colnames(x))

  # remove empty rows
  x = x[which(rowSums(is.na(x)) != ncol(x)),]

  # check whether discard column exists and if it contains any value != NA
  # if (discard_column %in% colnames(x) && !all(is.na(x[,discard_column]))) {
  #   x = x[-which(x[,discard_column] == TRUE | x[,discard_column]==1),]
  # }

  x
}

#' write an excel file with the errors found in the original data
#'
#' @param x dataset
#' @param save_errors argument to save errors
#'
#' @noRd
.write_errors_file <- function(x, save_errors="", X2) {
  # ERROR = ifelse(attr(x,'status')=='ERROR', TRUE, FALSE)
  if (all(x$column_type_errors == "")) { # && !ERROR
    cat('Done\n')
    attr(x, "message") <- "Done"
  } else {
    cat('Check warning/error messages.\n')
    if (any(x$column_type_errors=="ERROR")) {
      message('ERROR: Please fix the errors before continuing.')
      attr(x, "status") = 'ERRORS'
      if(save_errors!=""){
        writexl::write_xlsx(x, paste0(save_errors, '_errors.xlsx'))
        attr(x, "message") <- sprintf(attr(X2, "message"))
      }
    } else if (any(x$column_type_errors == "WARNING")) {
      message('WARNING: Please check the warnings before continuing.')
      attr(x, 'status') <- "WARNINGS"
      if (save_errors != "") {
        writexl::write_xlsx(x, paste0(save_errors,'_warnings.xlsx'))
        attr(x, "message") <- sprintf(attr(X2, "message"))
      }
    }
  }
  return(x)
}

#' Extract file extension
#'
#' @param x a file
#'
#' @noRd
.get_file_extension <- function(x) {
  filename = basename(x)
  return(substr(filename, regexpr("\\.[^\\.]*$", filename) + 1, nchar(filename)))
}

#' Extract file name without extension
#'
#' @param x a file
#'
#' @noRd
.get_filename_without_extension <- function(x) {
  return(sub("\\.[^\\.]*$", "", basename(x)))
}
