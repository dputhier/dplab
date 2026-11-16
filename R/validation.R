# -------------------------------------------------------------------------
# check_this_var is intended to check variable type      ------------------
# -------------------------------------------------------------------------
#' Check whether an object is a named list
#'
#' Returns `TRUE` when `x` is an empty list or a list with present, non-empty,
#' unique names. `NULL` can optionally be accepted.
#'
#' @param x Object to check.
#' @param null_accepted Whether `NULL` is accepted.
#' @return A logical scalar.
#' @examples
#' is_named_list(list(first = 1, second = 2))
#' is_named_list(list())
#' is_named_list(NULL, null_accepted = TRUE)
#' @export
is_named_list <- function(x, null_accepted = FALSE) {
  if (is.null(x)) return(null_accepted)
  if (!is.list(x)) return(FALSE)
  if (!length(x)) return(TRUE)

  !is.null(names(x)) &&
    all(names(x) != "") &&
    !anyDuplicated(names(x))
}

# -------------------------------------------------------------------------
# check_this_var is intended to check variable type      ------------------
# -------------------------------------------------------------------------
#' Check variable type.
#'
#' This function checks the type of a variable and raises an error if it does not match the expected type.
#'
#' @param x The variable to be checked.
#' @param type The expected type. Choose from "char", "num", "bool", or "int".
#' @param x_name The name of the variable as a string.
#' @param null_accepted Whether NULL values are accepted.
#' @param empty_accepted Whether empty strings are accepted (only relevant for character type).
#' @param calling_function The name of the calling function (automatically obtained if envnames is available).
#' @examples
#' \dontrun{
#' # Example checking a character variable
#' check_this_var(x = "hello", type = "char")
#'
#' # Example checking a numeric variable
#' check_this_var(x = 42, type = "num")
#' }
#'
#' @export
check_this_var <- function(x,
                           type = c("char", "num", "bool", "int"),
                           x_name = deparse(substitute(x)),
                           null_accepted = FALSE,
                           empty_accepted=FALSE,
                           calling_function = NULL) {
  
  # -----------------------------------------------------------------------
  # Calling function
  # -----------------------------------------------------------------------
  
  if (is.null(calling_function)) {
    if (requireNamespace("envnames", quietly = TRUE)) {
      calling_function <- envnames::get_fun_name()
    } else {
      calling_function <- "unknown"
    }
  }
  
  fun_info <- paste0("(", calling_function, ")")
  
  # -----------------------------------------------------------------------
  # Expected type
  # -----------------------------------------------------------------------
  
  type <- match.arg(type)
  
  # -----------------------------------------------------------------------
  # NULL
  # -----------------------------------------------------------------------
  
  if (is.null(x)) {
    if (!null_accepted) {
      print_msg(
        x_name, " should not be NULL.",
        fun_info,
        msg_type = "STOP"
      )
    }
    
    return(invisible(NULL))
  }
  
  # -----------------------------------------------------------------------
  # Length
  # -----------------------------------------------------------------------
  
  if (length(x) != 1L) {
    print_msg(
      x_name, " should be of length 1.",
      fun_info,
      msg_type = "STOP"
    )
  }
  
  # -----------------------------------------------------------------------
  # Type
  # -----------------------------------------------------------------------
  
  if (type == "char") {
    
    if (!is.character(x)) {
      print_msg(
        x_name, " should be a character.",
        fun_info,
        msg_type = "STOP"
      )
    }
    
  } else if (type == "num") {
    
    if (!is.numeric(x)) {
      print_msg(
        x_name, " should be numeric.",
        fun_info,
        msg_type = "STOP"
      )
    }
    
  } else if (type == "int") {
    
    if (!is.numeric(x) || !is.finite(x) || x %% 1 != 0) {
      print_msg(
        x_name, " should be an integer.",
        fun_info,
        msg_type = "STOP"
      )
    }
    
  } else if (type == "bool") {
    
    if (!is.logical(x)) {
      print_msg(
        x_name, " should be logical.",
        fun_info,
        msg_type = "STOP"
      )
    }
  }
  
  # -----------------------------------------------------------------------
  # Missing / invalid values
  # -----------------------------------------------------------------------
  
  if (is.numeric(x)) {
    
    if (is.nan(x)) {
      print_msg(
        x_name, " should not be NaN.",
        fun_info,
        msg_type = "STOP"
      )
    }
    
    if (is.infinite(x)) {
      print_msg(
        x_name, " should not be infinite.",
        fun_info,
        msg_type = "STOP"
      )
    }
  }
  
  if (is.na(x)) {
    print_msg(
      x_name, " should not be NA.",
      fun_info,
      msg_type = "STOP"
    )
  }
  
  if (is.character(x)) {
    if(x == ""){
      if(!empty_accepted)
        print_msg(
          x_name, " should not be an empty string.",
          fun_info,
          msg_type = "STOP"
        )
    }
  
  }
  
  invisible(NULL)
}

#' Check whether a numeric value is within a range
#'
#' This function checks that a numeric scalar is between `min` and `max`,
#' including the boundary values.
#'
#' @param x The numeric value to check.
#' @param min The inclusive lower bound.
#' @param max The inclusive upper bound.
#' @param x_name The name of the variable to use in error messages.
#' @return `NULL` invisibly.
#'
#' @examples
#' check_range(0.5, min = 0, max = 1)
#' @export
check_range <- function(x,
                        min,
                        max,
                        x_name = deparse(substitute(x))) {
  check_this_var(
    x,
    type = "num",
    x_name = x_name,
    calling_function = "check_range"
  )
  check_this_var(
    min,
    type = "num",
    x_name = "min",
    calling_function = "check_range"
  )
  check_this_var(
    max,
    type = "num",
    x_name = "max",
    calling_function = "check_range"
  )

  if (min > max) {
    print_msg(
      "'min' must be less than or equal to 'max'.",
      msg_type = "STOP"
    )
  }

  if (x < min || x > max) {
    print_msg(
      x_name,
      " is not between ",
      min,
      " and ",
      max,
      ".",
      msg_type = "STOP"
    )
  }

  invisible(NULL)
}


# -------------------------------------------------------------------------
# Check file existence and optionally create directory   ------------------
# -------------------------------------------------------------------------
#' Check file existence and optionally create directory
#'
#' This function checks whether a file exists and, depending on the mode, performs additional checks.
#' If the mode is set to "write", it checks whether the file exists. If it exists and force is FALSE,
#' an error is raised. If force is TRUE, the existing file is overwritten or created if it doesn't exist.
#' If the mode is set to "read", it checks whether the file exists. If it doesn't, an error is raised.
#'
#' @param file_path The path to the file to be checked. If NULL, an error is raised.
#' @param mode The mode of operation. Choose from "write" or "read".
#' @param force Logical indicating whether to force the operation (default is FALSE).
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Example with a temporary file for writing
#' temp_file_write <- tempfile(fileext = ".txt")
#' check_this_file(temp_file_write, mode = "write")
#'
#' # Example creating a file with file.create() and then checking it for writing
#' temp_file_create <- tempfile(fileext = ".txt")
#' file.create(temp_file_create)
#' check_this_file(temp_file_create, mode = "write", force = TRUE)
#'
#' # Example checking a non-existing file for reading
#' temp_file_read <- tempfile(fileext = ".txt")
#' check_this_file(temp_file_read, mode = "read")
#' }
#'
#' @export
check_this_file <- function(file_path, mode = c("write", "read"), force = FALSE) {
  
  check_this_var(x = file_path, type = "char")
  check_this_var(x = force, type = "bool")
  
  mode <- match.arg(mode)
  
  if (mode == "write") {
    if (file.exists(file_path)) {
      if (!force) {
        stop(paste("Error: File", file_path, "already exists. Use force = TRUE to overwrite.", sep = " "))
      } else {
        unlink(file_path)
        out <- file.create(file_path)
      }
    } else {
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        print_msg("Creating directory:", dir_path)
        out <- dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
      print_msg("Creating file:", file_path)
      out <- file.create(file_path)
    }
  } else {
    if (!file.exists(file_path)) {
      print_msg("File", file_path, "does not exist.", msg_type = "STOP")
    }
  }
}

# -------------------------------------------------------------------------
# Remove NA, NaN, infinite values and duplicates from a vector   ----------
# -------------------------------------------------------------------------
#' @title Remove NA, NaN, infinite values and duplicates from a vector
#' @description
#' This function removes NA values, NaN values, infinite values (Inf/-Inf),
#' and duplicates from a vector. It reports (via \code{print_msg}) whether
#' any of these were found and removed.
#' @param x A vector to clean.
#' @param label A label used in the messages to identify the vector
#' being cleaned (e.g. the variable name).
#' @return The input vector, with NA, NaN, infinite values and duplicates
#' removed.
#' @examples
#' clean_var(c(1, 2, 2, NA, 3))
#' clean_var(c(1, NaN, Inf, -Inf, 2))
#' clean_var(c("a", "b", "a"), label = "my_set")
#' @export
clean_var <- function(x, label = "variable") {
  
  is_num <- is.numeric(x)
  
  has_na <- anyNA(x)
  has_nan <- is_num && any(is.nan(x))
  has_inf <- is_num && any(is.infinite(x))
  has_dup <- anyDuplicated(x) > 0
  
  if (has_na && !has_nan) {
    print_msg(label, ": NA value(s) were removed.", msg_type = "DEBUG")
  }
  
  if (has_nan) {
    print_msg(label, ": NaN value(s) were removed.", msg_type = "DEBUG")
  }
  
  if (has_inf) {
    print_msg(label, ": infinite values were removed.", msg_type = "DEBUG")
  }
  
  if (has_dup) {
    print_msg(label, ": duplicated values were removed.", msg_type = "DEBUG")
  }
  
  # Remove NA (this also removes NaN).
  x <- x[!is.na(x)]
  
  # Remove infinite values
  if (is_num) {
    x <- x[!is.infinite(x)]
  }
  
  # Remove duplicates.
  x <- unique(x)
  
  x
}
