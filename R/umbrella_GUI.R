#' Graphical User Interface for conducting an umbrella review
#'
#' The \code{umbrella.gui} function allows to perform an umbrella review in R with a graphical user interface and to generate results outside of R.
#'
#' @details
#' This function implicitly calls the \code{view.errors.umbrella()} to check the formatting of the data,
#' the \code{umbrella()} function to perform calculations, the \code{add.evidence()} function to stratify evidence (limited to "Ioannidis" and "GRADE" classifications for now)
#'  and the \code{forest()} function to generate a visual representation of the results.
#'
#' To use this function:
#' \itemize{
#'   \item The first step is to type \code{umbrella.gui()} in the console. This will open a pop-up window.
#'   \item The second step is to load a well-formatted dataset (stored in an excel file) by clicking on the button at the right of the "Excel file" label. An exploratory window will open, allowing to select the file that should be uploaded.
#'   \item The third step is to select the folder where the results will be exported. Again, this can be achieved by clicking on the button at the right of the "Output folder" label, and selecting the appropriate folder thanks to the exploratory window.
#'   \item Last, the classification that should be used for stratifying the evidence has to be selected. For now, "Ioannidis" and "GRADE" classifications are available.
#'   }
#'
#' @return
#' The \code{umbrella.gui()} function returns several elements including \tabular{ll}{
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
#' @export umbrella.gui
#'
#' @md
#'
#' @examples
#' if(interactive()){
#' ### open the GUI
#' umbrella.gui()
#' }
umbrella.gui <- function() {

  selectFile <- function() {
    Filters <- matrix(c("Excel 2013", ".xlsx",
                        "Excel", ".xls",
                        "All files", "*"),
                      3, 2, byrow = TRUE) # this will filter results when searching a file when using GUI
    path_xls_str = tcltk::tk_choose.files(filters = Filters)
    if (is.na(path_xls_str)) {
      error_msg = "No input file selected. Nothing to do"
      tcltk::tk_messageBox(type="ok", message = "No input file selected. Nothing to do.", icon = "error")
      stop("No input file selected. Nothing to do")
    }
    if (!(.get_file_extension(path_xls_str) %in% c('xlsx','xls'))) {
      error_msg = "No input file selected. Nothing to do"
      tcltk::tk_messageBox(type="ok", message = "No input file selected. Nothing to do.", default = stop(error_msg))
      stop("Wrong file type. Input file should be an excel (.xlsx or .xls).")
    }
    sheets_excel = readxl::excel_sheets( path_xls_str )
    sheet_selected <- tcltk::tk_select.list(sheets_excel, title="Choose a sheet:", preselect = sheets_excel[1])
    tcltk::tclvalue(sheet) <- sheet_selected
    print(paste("File selected: ", path_xls_str, "Sheet: ", sheet_selected))
    if(!file.exists(path_xls_str)) {
      stop("File does not exist! Aborting process.\n")
    }
    tcltk::tclvalue(path_xls) <- path_xls_str
  }
  selectOutput <- function() {
    output_path = tcltk::tk_choose.dir(default = dirname(tcltk::tclvalue(path_xls)), "Choose where you want to save the output")

    if (is.na(output_path)) {
      stop("No output folder selected. Nothing to do\n")
    }
    tcltk::tclvalue(output_folder) <- output_path
  }

  calculate_umbrella <- function() {
    cat("Performing the umbrella analysis...\n")
    tryCatch({
      tcltk::tclvalue(run_lbl) <- 'Calculating...'
      input_filename = sprintf("%s/%s",tcltk::tclvalue(output_folder), sub('\\..*$', '', basename(tcltk::tclvalue(path_xls))))

      X = umbrella.xls(input_file = tcltk::tclvalue(path_xls), sheet = tcltk::tclvalue(sheet), output_path = tcltk::tclvalue(output_folder),
                       # discard_column = "",
                       # sensitivity_analysis_variable = NULL,
                       # sensitivity_analysis_value = NULL,
                       evidence = tcltk::tclvalue(criteria_selected), plot_results = TRUE)
      if (attr(X, 'status') == 'WARNINGS') {
        tcltk::tk_messageBox(icon = 'warning', message = paste(attr(X, 'message')))
      } else if (attr(X, 'status') == 'ERRORS') {
        tcltk::tk_messageBox(icon = 'error', message = paste(attr(X, 'message')))
      }
    }, finally = {
      tcltk::tclvalue(run_lbl) <- 'Run'
    }
    )
  }
  sensitivity_analysis <- function() {
    print(tcltk::tclvalue(check_sensitivity))
    if(tcltk::tclvalue(check_sensitivity)=="1"){
      tl <- tcltk::tklabel(win, text="Evidence criteria2")
      tcb <- tcltk::ttkcombobox(win, values=criteria, textvariable=criteria_selected, state="readonly", width=30)
      fufu<-tcltk::tkgrid(tl, tcb, pady=10, padx=10)
    } else {
      tcltk::tkgrid.remove(tl, tcb)
    }
  }



  # variables
  path_xls = tcltk::tclVar("Choose file...")
  output_folder = tcltk::tclVar("Choose folder...")
  sheet = tcltk::tclVar("")
  X = tcltk::tclVar("")
  table_html_name = tcltk::tclVar("")
  umbrella_save_name = tcltk::tclVar("")
  results_save_name = tcltk::tclVar("")

  # create top level window
  win<-tcltk::tktoplevel()
  tcltk::tkfocus(win)
  tcltk::tkwm.title(win, "umbrella_gui")

  # input entry
  xls.entry <- tcltk::tkentry(win, textvariable=path_xls, width=30)
  # output folder entry
  output.entry <- tcltk::tkentry(win, textvariable=output_folder, width=30)
  # run button
  run_lbl <- tcltk::tclVar('Run')
  calculate.btn <- tcltk::tkbutton(win, textvariable=run_lbl, command=calculate_umbrella, height=2, width=30)
  # select file button
  selectFile.btn <- tcltk::tkbutton(win, text="...", command=selectFile)
  # select criteria selector
  selectOutput.btn <- tcltk::tkbutton(win, text="...", command=selectOutput)
  criteria=c('Ioannidis','GRADE')
  criteria_selected=tcltk::tclVar('Choose criteria...')
  ################ for future updates #######################
  # checker
  # check_sensitivity = tclVar(FALSE)
  # sensitivity_checker <- tcltk::tkcheckbutton(win, text="Sensitivity analysis", variable = check_sensitivity, command=sensitivity_analysis)
  ###################################################
  # grid definition
  tcltk::tkgrid(tcltk::tklabel(win, text="Please select the excel file with the data to include in the umbrella review, \nthe output folder where I will write the results, and the criteria to grade the evidence."), columnspan=3, padx=10, pady=10)
  tcltk::tkgrid(tcltk::tklabel(win, text="Excel file"), xls.entry, selectFile.btn, pady=10, padx=10)
  tcltk::tkgrid(tcltk::tklabel(win, text="Output folder"), output.entry, selectOutput.btn, pady=10, padx=10)
  tcltk::tkgrid(tcltk::tklabel(win, text="Evidence criteria"), tcltk::ttkcombobox(win, values=criteria, textvariable=criteria_selected, state="readonly", width=30), pady=10, padx=10)
  tcltk::tkgrid(tcltk::tklabel(win, text=''), calculate.btn, padx=10, pady=10, columnspan=1)
  #tcltk::tkgrid(tcltk::tklabel(win, text=""), sensitivity_checker)
}
