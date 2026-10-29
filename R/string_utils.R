#################################################################
##    Random string generation functions
#################################################################
#' @title Generate a random string of letters and numbers
#' @description
#' This function generates a random string of 10 characters, consisting
#' of 3 uppercase letters, 4 digits, and 3 lowercase letters.
#'
#' @return A character string of length 10, consisting of random letters
#'   and numbers.
#'
#' @export
#'
#' @examples
#' create_rand_str()
create_rand_str <- function() {
  
  v <- c(
    sample(LETTERS, 3, replace = TRUE),
    sample(0:9, 4, replace = TRUE),
    sample(letters, 3, replace = TRUE)
  )
  
  paste0(sample(v), collapse = "")
}


#' @title Generate a random string of letters and numbers (alias)
#' @description
#' Alias for \code{\link{create_rand_str}}. Generates a random string
#' of 10 characters.
#'
#' @return A character string of length 10, consisting of random letters
#'   and numbers.
#'
#' @export
#'
#' @examples
#' rand_string()
rand_string <- function() {
  create_rand_str()
}


#' Truncate Axis Labels
#'
#' Create a label function that truncates strings after a specified
#' number of characters and appends an ellipsis (`"..."`).
#' Optionally replace newline and carriage return characters with spaces.
#'
#' @param n Positive integer specifying the maximum number of characters
#'   to retain before appending the ellipsis.
#' @param remove_newlines Logical. If `TRUE` (default), replace newline
#'   (`"\\n"`) and carriage return (`"\\r"`) characters with spaces.
#'
#' @return A function accepting a character vector and returning a
#'   character vector with long strings truncated.
#'
#' @examples
#' labeller <- truncate_label(10)
#' labeller(c("short", "a much longer label", "line one\nline two"))
#' labeller(c("short", "a much longer label", NA, NA, "line one\nline two"))
#'
#' @export
truncate_label <- function(n, remove_newlines = TRUE) {
  if (length(n) != 1L || !is.numeric(n) ||
      is.na(n) || !is.finite(n) || n < 1 || n != floor(n)) {
    print_msg("`n` must be a single positive integer.", msg_type = "STOP")
  }
  
  if (length(remove_newlines) != 1L ||
      !is.logical(remove_newlines) || is.na(remove_newlines)) {
    print_msg("`remove_newlines` must be TRUE or FALSE.", msg_type = "STOP")
  }
  
  function(x) {
    x <- as.character(x)
    
    if (remove_newlines) {
      x <- gsub("[\r\n]", " ", x)
    }
    
    idx <- !is.na(x) & nchar(x) > n
    x[idx] <- paste0(substr(x[idx], 1L, n), "...")
    x
  }
}
