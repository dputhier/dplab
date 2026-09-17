#################################################################
##    mkdir_p
#################################################################
#' @title Create a directory (including parents) if it doesn't exist
#' @description
#' Creates a directory recursively if it doesn't already exist. 
#' Similar to `mkdir -p` in Unix. Uses \code{\link{print_msg}} to inform the user.
#' set_verbosity() can be used to control the verbosity of messages.
#'
#' @param path The path to the directory to create (character string).
#'
#' @return NULL, invisibly. Creates the directory as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a directory and its parents if they don't exist
#' # print a msg using print_msg() (controlled by set_verbosity())
#' mkdir_p("/tmp/dplab")
#' 
#' # Call again - will not error if already exists
#' mkdir_p("/tmp/dplab")
#' 
#' # Set verbosity level to see DEBUG messages
#' set_verbosity(2)
#' mkdir_p("/tmp/dplab/subdir")
#' mkdir_p("/tmp/dplab/subdir")
#' }
mkdir_p <- function(path) {
  
  check_this_var(path, type = "char")
  
  if (dir.exists(path)) {
    print_msg("Directory already exists:", path,
              msg_type = "DEBUG")
  } else {
    tryCatch({
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      print_msg("Created directory:", path,
                msg_type = "INFO")
    }, error = function(e) {
      print_msg("Failed to create directory:", path, "-", e$message,
                msg_type = "STOP")
    })
  }
  
  invisible(NULL)
}

#################################################################
##    make_tmp_file
#################################################################
#' @title Create a temporary file with automatic cleanup
#' @description
#' Creates a temporary file that will be automatically removed upon exit.
#' All files created with this function are stored in the option 
#' \code{<package_name>_temp_files} and will be removed when R exits or
#' when the user manually triggers cleanup.
#'
#' @param prefix A prefix for the temporary file. The package name will be 
#'   prepended to this prefix. Defaults to "tmp".
#' @param pkgname The package name to use in the filename. Defaults to "dplab".
#' @param suffix A suffix for the temporary file (e.g., ".txt", ".csv"). 
#'   Defaults to "" (no suffix).
#' @param cleanup_on_exit Logical indicating whether to store the file path in the 
#'   temporary file list for automatic cleanup. Defaults to TRUE.
#' @param dir The directory where the temporary file should be created. 
#'   Defaults to \code{tempdir()}.
#'
#' @return A character string containing the path to the created temporary file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a temporary file with default settings
#' tmp_file <- make_tmp_file(). The set_verbosity() function 
#' can be used to control the verbosity of messages.
#' 
#' # Create a temporary CSV file
#' tmp_csv <- make_tmp_file(prefix = "data", suffix = ".csv")
#' 
#' # Create a temporary file without automatic cleanup
#' tmp_manual <- make_tmp_file(cleanup_on_exit = FALSE)
#' }
make_tmp_file <- function(prefix = "tmp",
                          pkgname = "dplab",
                          suffix = "",
                          cleanup_on_exit = TRUE,
                          dir = tempdir()) {
  
  check_this_var(prefix, type = "char", empty_accepted = TRUE)
  check_this_var(pkgname, type = "char")
  check_this_var(suffix, type = "char", empty_accepted = TRUE)
  check_this_var(cleanup_on_exit, type = "bool")
  check_this_var(dir, type = "char", empty_accepted = TRUE)
  
  if (!dir.exists(dir)) {
    print_msg(paste0("Directory does not exist: ", dir), msg_type = "STOP")
  }
  
  # Create filename pattern: pkgname_prefix_randomstring_suffix
  file_pattern <- paste0(pkgname, "_", prefix, "_")
  
  # Create temporary file
  tmp_file <- tempfile(pattern = file_pattern, tmpdir = dir, fileext = suffix)
  print_msg("Creating temporary file: ", tmp_file, msg_type = "INFO")
  
  # Create the file
  file.create(tmp_file)
  
  # Store in option list if requested
  if (cleanup_on_exit) {

    opt_name <- paste0(pkgname, "_temp_files")
    print_msg("Option name for temporary files: ", opt_name, msg_type = "DEBUG")
    current_files <- getOption(opt_name, default = character(0))
    print_msg("The number of current temporary files stored in options: ", length(current_files), msg_type = "DEBUG")   
    
    print_msg("Setting up cleanup on exit.", msg_type = "DEBUG")
    options(stats::setNames(list(c(current_files, tmp_file)), opt_name))
    
    # Set up cleanup on exit if not already done
    cleanup_opt_name <- paste0(pkgname, "_cleanup_registered")
    print_msg("Option name for cleanup registration: ", cleanup_opt_name, msg_type = "DEBUG")
    
    if (is.null(getOption(cleanup_opt_name))) {
      reg.finalizer(
        environment(),
        function(e) {
          files_to_remove <- getOption(opt_name, default = character(0))
          if (length(files_to_remove) > 0) {
            file.remove(files_to_remove[file.exists(files_to_remove)])
          }
        },
        onexit = TRUE
      )
      options(stats::setNames(list(TRUE), cleanup_opt_name))
    }
  }
  
  return(tmp_file)
}


#################################################################
##    count_lines
#################################################################
#' @title Count the number of lines in a file
#' @description
#' Counts the number of lines in a text file reading the
#' file in chunks to limit memory usage.
#'
#' @param afile Path to the file (character string) or a connection object.
#'
#' @return An integer representing the number of lines read.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Count lines in a file
#' n_lines <- count_lines("path/to/file.txt")
#' print(n_lines)
#'
#' # Use with a connection
#' con <- file("path/to/file.txt", "r")
#' n_lines <- count_lines(con)
#' close(con)
#' }
count_lines <- function(afile) {
  
  # Handle connection objects
  if (inherits(afile, "connection")) {
    con <- afile
    close_con <- FALSE
  } else {
    
    # Handle file path
    check_this_var(afile, type = "char")
    
    if (!file.exists(afile)) {
      stop("File does not exist: ", afile)
    }
    
    con <- file(afile, open = "r")
    close_con <- TRUE
  }
  
  if (close_con) {
    on.exit(close(con), add = TRUE)
  }
  
  n_lines <- 0L
  
  repeat {
    chunk <- readLines(
      con,
      n = 10000L,
      warn = FALSE
    )
    
    if (!length(chunk)) {
      break
    }
    
    n_lines <- n_lines + length(chunk)
  }
  
  n_lines
}

