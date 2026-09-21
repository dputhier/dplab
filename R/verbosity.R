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
#' @param date A character string accepted by [format()] for displaying the
#'   current time before each message, for example `"%X"`. When `NULL` (the
#'   default), messages are printed without a date/time prefix.
#'
#' @return NULL, invisibly.
#'
#' @examples
#' # Set verbosity level to 2
#' set_verbosity(2)
#'
#' # Display the current time before messages
#' set_verbosity(1, date = "%X")
#'
#' # Set verbosity level to 0
#' set_verbosity(0)
#'
#' # 0 : No message
#' # 1 : Display only INFO type message
#' # 2 : Display both INFO and DEBUG type message
#' @export
#' @importFrom  stats setNames
set_verbosity <- function(verbosity_value, date = NULL) {
  
  check_this_var(verbosity_value, type = "int")
  check_this_var(date, type = "char", null_accepted = TRUE)
  if (verbosity_value < 0) {
    print_msg(
      "verbosity_value must be a non-negative integer.",
      msg_type = "STOP"
    )
  }
  
  verbosity_option <- "dplab_verbosity"
  date_option <- "dplab_date_format"
  
  options(
    stats::setNames(
      list(verbosity_value, date),
      c(verbosity_option, date_option)
    )
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
  
  opt_name <- "dplab_verbosity"
  
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
#' library(dplab) 
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
  date_format <- getOption("dplab_date_format")
  date_prefix <- if (is.null(date_format)) {
    ""
  } else {
    paste0(format(Sys.time(), date_format), " : ")
  }
  
  if (msg_type == "DEBUG") {
    
    if (verbosity > 1) {
      cli::cli_alert_info(
        paste0("|-- ", date_prefix, "DEBUG: ", msg)
      )
    }
    
  } else if (msg_type == "INFO") {
    
    if (verbosity > 0) {
      cli::cli_alert_info(paste0("|-- ", date_prefix, msg))
    }
    
  } else if (msg_type == "WARNING") {
    
    cli::cli_alert_warning(paste0("|-- ", date_prefix, msg))
    
  } else if (msg_type %in% c("STOP", "ERROR")) {
    
    cli::cli_alert_danger(paste0("|-- ", date_prefix, msg))
    stop()

  }
  
  invisible(NULL)
}
