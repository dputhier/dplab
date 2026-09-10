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
                           calling_function = NULL) {
  
  if (is.null(calling_function)) {
    if (requireNamespace("envnames", quietly = TRUE)) {
      calling_function <- envnames::get_fun_name()
    } else {
      calling_function <- "unknown"
    }
  }
  
  fun_info <- paste0("(", calling_function, ")")
  
  type <- match.arg(type)
  
  if(length(x) > 1)
    print_msg(x_name, " should be of length 1.", msg_type = "STOP")
  
  if(is.null(x)){
    if(!null_accepted)
      print_msg(x_name, " should not be NULL.", msg_type = "STOP")
  }else if(is.nan(x)){
    print_msg(x_name, " should not be nan", msg_type = "STOP")
  }else if(is.infinite(x)){
    print_msg(x_name, "should not be infinite", msg_type = "STOP")
  }else if(is.na(x)){
    print_msg(x_name, " should not be NA", msg_type = "STOP")
  }else if(is.character(x) && x == ""){
    print_msg(x_name, "should not be an empty string.", msg_type = "STOP")
  }
  
  if(!is.null(x)){
    if (type == "char") {
      if (!is.character(x))
        print_msg(x_name, " should be a character", fun_info, msg_type = "STOP")
    } else if (type == "int") {
      if(is.numeric(x)){
        if(!x %% 1 == 0){
          print_msg(x_name, " should be an integer", fun_info, msg_type = "STOP")
        }
      }else{
        print_msg(x_name, " should be an integer", fun_info, msg_type = "STOP")
      }
      
    } else if (type == "num") {
      if (!is.numeric(x))
        print_msg(x_name, " should be a numeric", fun_info, msg_type = "STOP")
    } else if (type == "bool") {
      if (!is.logical(x))
        print_msg(x_name, " should be a logical", fun_info, msg_type = "STOP")
    }else {
      print_msg(x_name, " has unknown format...", fun_info, msg_type = "STOP")
    }
  }
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
    print_msg(label, ": NA value(s) found and removed.", msg_type = "INFO")
  }
  
  if (has_nan) {
    print_msg(label, ": NaN value(s) found and removed.", msg_type = "INFO")
  }
  
  if (has_inf) {
    print_msg(label, ": infinite value(s) found and removed.", msg_type = "INFO")
  }
  
  if (has_dup) {
    print_msg(label, ": duplicate value(s) found and removed.", msg_type = "INFO")
  }
  
  # Remove NA (this also removes NaN).
  x <- x[!is.na(x)]
  
  # MODIF: Remove infinite values
  if (is_num) {
    x <- x[!is.infinite(x)]
  }
  
  # Remove duplicates.
  x <- unique(x)
  
  x
}
