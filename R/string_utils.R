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
