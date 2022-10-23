#' Load data through clipboard on Windows or macOS
#'
#' @details Extract data from the clipboard into a data.frame on Windows or macOS
#' @param delim Delimiter to use (tab is the default)
#' @param text Text input to convert to table
#' @param suppress Suppress warnings
#' @seealso See the \code{\link{save_clip}}
#' @export
load_clip <- function(delim = "\t", text, suppress = TRUE) {
  sw <- if (suppress) suppressWarnings else function(x) x
  sw(
    try({
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        dataset <- read.table(
          "clipboard", header = TRUE, sep = delim,
          comment.char = "", fill = TRUE, as.is = TRUE,
          check.names = FALSE
        )
      } else if (os_type == "Darwin") {
        dataset <- read.table(
          pipe("pbpaste"), header = TRUE, sep = delim,
          comment.char = "", fill = TRUE, as.is = TRUE,
          check.names = FALSE
        )
      } else if (os_type == "Linux") {
        if (missing(text) || radiant.data::is_empty(text)) {
          message("Loading data through clipboard is currently only supported on Windows and macOS")
          return(invisible())
        } else {
          dataset <- read.table(
            text = text, header = TRUE, sep = delim,
            comment.char = "", fill = TRUE, as.is = TRUE,
            check.names = FALSE
          )
        }
      }
      as.data.frame(dataset, check.names = FALSE, stringsAsFactors = FALSE) %>%
        radiant.data::to_fct()
    }, silent = TRUE)
  )
}

#' Save data to clipboard on Windows or macOS
#'
#' @details Save a data.frame or tibble to the clipboard on Windows or macOS
#' @param dataset Dataset to save to clipboard
#' @seealso See the \code{\link{load_clip}}
#' @export
save_clip <- function(dataset) {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    write.table(dataset, "clipboard-10000", sep = "\t", row.names = FALSE)
  } else if (os_type == "Darwin") {
    write.table(dataset, file = pipe("pbcopy"), sep = "\t", row.names = FALSE)
  } else if (os_type == "Linux") {
    message("Saving data to clipboard is currently only supported on Windows and macOS.\nSave data to csv for use in a spreadsheet")
  }
  invisible()
}

#' Ensure column names are valid
#'
#' @details Remove symbols, trailing and leading spaces, and convert to valid R column names. Opinionated version of \code{\link{make.names}}
#' @param x Data.frame or vector of (column) names
#' @param lower Set letters to lower case (TRUE or FALSE)
#' @examples
#' fix_names(c(" var-name ", "$amount spent", "100"))
#' @export
fix_names <- function(x, lower = FALSE) {
  isdf <- is.data.frame(x)
  cn <- if (isdf) colnames(x) else x
  cn <- gsub("(^\\s+|\\s+$)", "", cn) %>%
    gsub("\\s+", "_", .) %>%
    gsub("[[:punct:]]", "_", .) %>%
    gsub("^[[:punct:]]", "", .) %>%
    make.names(unique = TRUE) %>%
    gsub("\\.{2,}", ".", .) %>%
    gsub("_{2,}", "_", .) %>%
    make.names(unique = TRUE) %>% ## used twice to make sure names are still unique
    {if (lower) tolower(.) else .}
  if (isdf) stats::setNames(x, cn) else cn
}
#' #' Send files to CKAN Server
#' #'
#' #' @details Sends radiant state file to a specific CKAN server (igenomed.stanford.edu)
#' #' @param author author
#' #' @param authemail author email
#' #' @param title_ckan title
#' #' @export
#' saveckan <- function(author, authemail, title_ckan){
#'   download_handler <- function(id, label = "", fun = id, fn, type = "csv", caption = "Save to csv",
#'                                class = "", ic = "download", btn = "link", onclick = "function() none;", ...) {
#'     ## create observer
#'     shinyFiles::shinyFileSave(input, id, roots = sf_volumes, session = session)
#'
#'     ## create renderUI
#'     if (btn == "link") {
#'       output[[paste0("ui_", id)]] <- renderUI({
#'         if (is.function(fn)) fn <- fn()
#'         if (is.function(type)) type <- type()
#'         shinyFiles::shinySaveLink(
#'           id, label, caption,
#'           filename = fn, filetype = type,
#'           class = "alignright", icon = icon(ic, verify_fa = FALSE), onclick = onclick
#'         )
#'       })
#'     } else {
#'       output[[paste0("ui_", id)]] <- renderUI({
#'         if (is.function(fn)) fn <- fn()
#'         if (is.function(type)) type <- type()
#'         shinyFiles::shinySaveButton(
#'           id, label, caption,
#'           filename = fn, filetype = type,
#'           class = class, icon = icon("download", verify_fa = FALSE), onclick = onclick
#'         )
#'       })
#'     }
#'
#'     observeEvent(input[[id]], {
#'       if (is.integer(input[[id]])) {
#'         return()
#'       }
#'       path <- shinyFiles::parseSavePath(sf_volumes, input[[id]])
#'       if (!inherits(path, "try-error") && !radiant.data::is_empty(path$datapath)) {
#'         fun(path$datapath, ...)
#'       }
#'     })
#'   }
#'   saveState_CKAN <- function(filename) {
#'     withProgress(
#'       message = "Preparing radiant state file", value = 1,
#'       isolate({
#'         LiveInputs <- toList(input)
#'         r_state[names(LiveInputs)] <- LiveInputs
#'         r_data <- active2list(r_data)
#'         r_info <- toList(r_info)
#'         save(r_state, r_data, r_info, file = filename)
#'       })
#'     )
#'   }
#'   state_file<- download_handler(
#'     id = "state_ckan",
#'     label = "Save",
#'     fun = saveState_CKAN,
#'     fn = function() state_name_dlh() %>% sans_ext(),
#'     type = function() {
#'       state_name_dlh() %>%
#'         {
#'           if (grepl("\\.state\\.rda", .)) "state.rda" else tools::file_ext(.)
#'         }
#'     },
#'     btn = "button",
#'     caption = "Save radiant state file"
#'   )
#'   key1="cd92b0b4-8606-49d3-9a3e-23587790ffe3"
#'   ckanr_setup(url = "https://igenomed.stanford.edu/", key = key1)
#'   ckan_file<-package_create(title=title_ckan, author = author, author_email = authemail, owner_org="Radiant State", name="Radiant State")
#'   resource_create(package_id = ckan_file$id, upload = state_file, rcurl = "https://igenomed.stanford.edu/", description = "Radiant State File, download to load file")
#' }
