# -------------------------------------------------------------------------
# Show available methods --------------------------------------------------
# -------------------------------------------------------------------------
#' @title List the methods for an S4 class
#' @description
#' List the methods available for an S4 class in a specified environment
#' or package.
#'
#' @param class The name of the S4 class (character string) or an object
#'   of that class. If \code{NULL}, methods for all classes are shown.
#' @param where The environment or package where the class is defined.
#'   Defaults to \code{".GlobalEnv"}. If a package name is supplied,
#'   it is automatically converted to \code{"package:<package>"}.
#'
#' @return A character vector containing the available method names.
#'
#' @importFrom methods showMethods
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{
#' # Define an example S4 class in the global environment
#' methods::setClass(
#'   "ExampleClass",
#'   slots = list(
#'     name = "character",
#'     values = "numeric"
#'   )
#' )
#'
#' # Define an example S4 method
#' methods::setMethod(
#'   "show",
#'   "ExampleClass",
#'   function(object) {
#'     cat("ExampleClass:", object@name, "\n")
#'   }
#' )
#'
#' # List methods for the class
#' show_methods("ExampleClass")
#'
#' # Alternatively, pass an object
#' example_object <- methods::new(
#'   "ExampleClass",
#'   name = "test",
#'   values = c(1, 2, 3)
#' )
#'
#' show_methods(example_object)
#' }
#'
#' @export
show_methods <- function(class = NULL,
                         where = ".GlobalEnv") {
  
  # -----------------------------------------------------------------------
  # Validate where
  # -----------------------------------------------------------------------
  check_this_var(where, type = "char")
  
  if (where != ".GlobalEnv" &&
      !grepl("^package:", where)) {
    where <- paste0("package:", where)
  }
  
  # -----------------------------------------------------------------------
  # Handle `class`
  # -----------------------------------------------------------------------
  if (is.null(class)) {
    return(character(0))
  }
  
  # If an object was supplied, extract its class
  if (!is.character(class)) {
    class <- class(class)[1]
  }
  
  # Validate class name
  if (length(class) != 1 || !nzchar(class)) {
    print_msg(
      "Invalid class specification. Using default 'ClusterSet'.",
      msg_type = "WARNING"
    )
    class <- "ClusterSet"
  }
  
  # -----------------------------------------------------------------------
  # Retrieve methods
  # -----------------------------------------------------------------------
  class_method <- utils::capture.output(
    methods::showMethods(
      class = class,
      where = where
    )
  )
  
  # Extract function names
  class_method <- class_method[
    grepl("Function:", class_method, fixed = TRUE)
  ]
  
  class_method <- sub(
    "^.*Function:\\s*([^ ]+).*",
    "\\1",
    class_method
  )
  
  unique(class_method)
}



# -------------------------------------------------------------------------
# Reload the package     --------------------------------------------------
# -------------------------------------------------------------------------
#' @title Reload a package (used for development).
#' @description
#' Detach and reload a package. Useful during package development to test changes
#' without restarting R. By default, reloads "dplab".
#'
#' @param package_name The name of the package to reload. Defaults to "dplab".
#'
#' @return NULL, invisibly. Prints messages about the reload process.
#'
#' @examples
#' \dontrun{
#' # Reload dplab
#' reload_pac()
#'
#' }
#'
#' @export
reload_pac <- function(package_name = "dplab") {
  
  check_this_var(package_name, type = "char")
  package_string <- paste0("package:", package_name)
  
  if (package_string %in% search()) {
    tryCatch({
      detach(package_string, unload = TRUE, character.only = TRUE)
      print_msg("Package", package_name, "detached successfully.")
    }, error = function(e) {
      print_msg("Error detaching package", package_name, ":", e$message,
                msg_type = "WARNING")
    })
  } else {
    print_msg("Package", package_name, "is not currently loaded.",
              msg_type = "INFO")
  }
  
  tryCatch({
    library(package_name, character.only = TRUE)
    print_msg("Package", package_name, "reloaded successfully.")
  }, error = function(e) {
    print_msg("Error loading package", package_name, ":", e$message,
              msg_type = "STOP")
  })
  
  invisible(NULL)
}
