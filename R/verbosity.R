#################################################################
##    set_verbosity
#################################################################
#' Set the verbosity level.
#'
#' This function sets the verbosity level which controls the amount of
#' information that is printed to the console by the \code{\link{print_msg}}
#' function. The verbosity level can be set to any non-negative integer,
#' with higher values indicating more detailed output. By default, the
#' verbosity level is set to 1.
#'
#' @param verbosity_value A non-negative integer indicating the verbosity level to be set.
#'
#' @return NULL, invisibly.
#'
#' @examples
#' # Set verbosity level to 2
#' set_verbosity(2)
#'
#' # Set verbosity level to 0
#' set_verbosity(0)
#'
#' # 0 : No message
#' # 1 : Display only INFO type message
#' # 2 : Display both INFO and DEBUG type message
#' @export
#' @importFrom cli cli_abort
#' @importFrom  stats setNames
set_verbosity <- function(verbosity_value) {
  
  if (!is.numeric(verbosity_value) ||
      length(verbosity_value) != 1 ||
      is.na(verbosity_value) ||
      verbosity_value < 0 ||
      verbosity_value %% 1 != 0) {
    cli::cli_abort(
      "{.arg verbosity_value} must be a non-negative integer."
    )
  }
  
  opt_name <- "denlabutils_verbosity"
  
  options(
    stats::setNames(list(verbosity_value), opt_name)
  )
  
  invisible(NULL)
}


#################################################################
##    get_verbosity()
#################################################################
#' Get the current verbosity level.
#'
#' This function gets the verbosity level which controls the amount of
#' information that is printed to the console by the \code{\link{print_msg}}
#' function.
#'
#'
#' @return A numeric value representing the verbosity level.
#'
#' @export
#'
#' @examples
#' get_verbosity()
#'
get_verbosity <- function() {
  
  opt_name <- "denlabutils_verbosity"
  
  if (is.null(getOption(opt_name))) {
    set_verbosity(1)
  }
  
  getOption(opt_name)
}


#################################################################
##    print_msg
#################################################################
#' Print a message based on the level of verbosity.
#'
#' @param ... The message components to be printed (pasted together).
#' @param msg_type The type of message, one of "INFO", "DEBUG", "WARNING",
#'   "STOP", or "ERROR" (ERROR is translated to STOP).
#'
#' @return NULL, invisibly.
#'
#' @examples
#' # Set verbosity level to 1
#' library(denlabutils) 
#' set_verbosity(1)
#' print_msg("Hello world!", msg_type = "INFO")
#' set_verbosity(2)
#' print_msg("Debugging message", msg_type = "DEBUG")
#' set_verbosity(0)
#' print_msg("Hello world!", msg_type = "INFO")
#' print_msg("Debugging message", msg_type = "DEBUG")
#' @export
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_alert_danger
print_msg <- function(...,
                      msg_type = c("INFO", "DEBUG", "WARNING", "STOP", "ERROR")) {
  
  msg_type <- match.arg(msg_type)
  msg <- paste(..., collapse = " ")
  verbosity <- get_verbosity()
  
  if (msg_type == "DEBUG") {
    
    if (verbosity > 1) {
      cli::cli_alert_info(
        paste0("|-- DEBUG: ", msg)
      )
    }
    
  } else if (msg_type == "INFO") {
    
    if (verbosity > 0) {
      cli::cli_alert_info(paste0("|-- ", msg))
    }
    
  } else if (msg_type == "WARNING") {
    
    cli::cli_alert_warning(paste0("|-- ", msg))
    
  } else if (msg_type %in% c("STOP", "ERROR")) {
    
    cli::cli_alert_danger(paste0("|-- ", msg))
    stop()

  }
  
  invisible(NULL)
}
