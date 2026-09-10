#################################################################
##    mkdir_p
#################################################################
#' @title Create a directory (including parents) if it doesn't exist
#' @description
#' Creates a directory recursively if it doesn't already exist. 
#' Similar to `mkdir -p` in Unix. Uses \code{\link{print_msg}} to inform the user.
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
#' mkdir_p("/tmp/denlabutils")
#' 
#' # Call again - will not error if already exists
#' mkdir_p("/tmp/denlabutils")
#' 
#' # Set verbosity level to see DEBUG messages
#' set_verbosity(2)
#' mkdir_p("/tmp/denlabutils/subdir")
#' mkdir_p("/tmp/denlabutils/subdir")
#' }
mkdir_p <- function(path) {
  
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    print_msg("Invalid path specification.",
              msg_type = "STOP")
  }
  
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
#' @param pkgname The package name to use in the filename. Defaults to "denlabutils".
#' @param suffix A suffix for the temporary file (e.g., ".txt", ".csv"). 
#'   Defaults to "" (no suffix).
#' @param store Logical indicating whether to store the file path in the 
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
#' tmp_file <- make_tmp_file()
#' 
#' # Create a temporary CSV file
#' tmp_csv <- make_tmp_file(prefix = "data", suffix = ".csv")
#' 
#' # Create a temporary file without automatic cleanup
#' tmp_manual <- make_tmp_file(store = FALSE)
#' }
make_tmp_file <- function(prefix = "tmp",
                          pkgname = "denlabutils",
                          suffix = "",
                          store = TRUE,
                          dir = tempdir()) {
  
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("prefix must be a single character string")
  }
  
  if (!is.character(pkgname) || length(pkgname) != 1) {
    stop("pkgname must be a single character string")
  }
  
  if (!is.character(suffix) || length(suffix) != 1) {
    stop("suffix must be a single character string")
  }
  
  if (!is.logical(store) || length(store) != 1) {
    stop("store must be a single logical value")
  }
  
  if (!is.character(dir) || length(dir) != 1) {
    stop("dir must be a single character string")
  }
  
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }
  
  # Create filename pattern: pkgname_prefix_randomstring_suffix
  file_pattern <- paste0(pkgname, "_", prefix, "_")
  
  # Create temporary file
  tmp_file <- tempfile(pattern = file_pattern, tmpdir = dir, fileext = suffix)
  
  # Create the file
  file.create(tmp_file)
  
  # Store in option list if requested
  if (store) {
    opt_name <- paste0(pkgname, "_temp_files")
    current_files <- getOption(opt_name, default = character(0))
    options(stats::setNames(list(c(current_files, tmp_file)), opt_name))
    
    # Set up cleanup on exit if not already done
    cleanup_opt_name <- paste0(pkgname, "_cleanup_registered")
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
#' Counts the number of lines in a text file efficiently.
#'
#' @param afile Path to the file (character string) or a connection object.
#'
#' @return An integer representing the number of lines in the file.
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
    lines <- readLines(afile, warn = FALSE)
    return(length(lines))
  }
  
  # Handle file path
  if (!is.character(afile) || length(afile) != 1) {
    stop("afile must be a file path (character string) or a connection object")
  }
  
  if (!file.exists(afile)) {
    stop("File does not exist: ", afile)
  }
  
  # Efficient line counting using readLines with connection
  con <- file(afile, "r")
  on.exit(close(con))
  
  n_lines <- 0
  while (length(chunk <- readLines(con, n = 10000, warn = FALSE)) > 0) {
    n_lines <- n_lines + length(chunk)
  }
  
  return(n_lines)
}
