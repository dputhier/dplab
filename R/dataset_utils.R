#' Download an example dataset
#'
#' Downloads an `.rda` example dataset hosted on Zenodo into
#' `file.path(path.expand("~"), ".dplab", "datasets")` and returns its local
#' path. A dataset already present in that directory is not downloaded again.
#'
#' @param dataset A Zenodo dataset identifier in the form
#'   `"<record-id>/files/<object-name>"`.
#' @param timeout Timeout value in seconds.
#' @param quiet Whether to suppress download progress output.
#'
#' @return The path to the cached `.rda` file.
#'
#' @section Available datasets:
#' \describe{
#'   \item{`22745973/files/ENCFF119BYM_H3K36me3_chr1.rda`}{A dataframe containing H3K36me3 
#'   ChIP-seq peaks coordinates on chromosome 1.}
#'   \item{`22705530/files/pbmc3k_medium`}{A Seurat object derived from the
#'   10x Genomics PBMC 3k dataset.}
#'   \item{`22705530/files/pbmc3k_medium_clusters_enrich`}{A ClusterSet object
#'   derived from the 10x Genomics PBMC 3k dataset, with functional enrichment
#'   results.}
#'   \item{`22705530/files/pbmc3k_medium_clusters`}{A ClusterSet object derived
#'   from the 10x Genomics PBMC 3k dataset, without functional enrichment
#'   results.}
#'   \item{`22705530/files/pbmc3k_medium_clusters_enrich_sub`}{A ClusterSet
#'   object derived from the 10x Genomics PBMC 3k dataset, with a subset of the
#'   functional enrichment results.}
#'   \item{`22705530/files/lymph_node_tiny_2.rda`}{A Seurat object derived from
#'   the 10x Genomics Visium CytAssist Spatial Gene Expression Library, Human
#'   Lymph Node dataset.}
#'   \item{`22705530/files/lymph_node_tiny_clusters_2`}{A ClusterSet object
#'   derived from the 10x Genomics Visium CytAssist Spatial Gene Expression
#'   Library, Human Lymph Node dataset.}
#' }
#'
#' These datasets are available in `~/.dplab/datasets` after download.
#'
#' @examples
#' \dontrun{
#' path <- get_example("22745973/files/ENCFF119BYM_H3K36me3_chr1")
#' load_example(path)
#' }
#'
#' @export
get_example <- function(dataset, 
                        timeout = 1200, 
                        quiet = TRUE) {
  
  # Check arguments
  check_this_var(dataset, type = "char")
  
  check_this_var(timeout, type = "num")
  
  if (timeout <= 0) {
    print_msg("timeout must be a positive number.",
              msg_type = "STOP")
  }
  
  check_this_var(quiet, type = "bool")
  
  # Get dataset path
  dataset <- sub("^https?://zenodo.org/records?/", "", dataset)
  
  if (!grepl("^[0-9]+/files/[^/]+$", dataset)) {
    print_msg(
      "dataset must have the form <record-id>/files/<object-name>.",
      msg_type = "STOP"
    )
  }
  
  dataset_name <- sub(".*/", "", dataset)
  dataset_name <- sub("\\.rda$", "", dataset_name, ignore.case = TRUE)

  dataset_path <- file.path(
    path.expand("~/.dplab/datasets"),
    paste0(dataset_name, ".rda")
  )
  
  if (file.exists(dataset_path)) {
    return(dataset_path)
  }
  
  # Create dataset directory
  dataset_dir <- dirname(dataset_path)
  
  if (!dir.exists(dataset_dir)) {
    print_msg("Creating dataset directory at ", dataset_dir,
              msg_type = "INFO")
    
    if (!dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)) {
      print_msg("Unable to create dataset directory at ", dataset_dir,
                msg_type = "STOP")
    }
  }
  
  # Set timeout
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, timeout))
  
  # Download dataset
  url <- paste0("https://zenodo.org/records/", dataset)
  if (!grepl("\\.rda$", dataset, ignore.case = TRUE)) {
    url <- paste0(url, ".rda")
  }
  
  temporary_file <- tempfile(
    pattern = paste0(dataset_name, "-"),
    tmpdir = dataset_dir,
    fileext = ".rda"
  )
  on.exit(unlink(temporary_file), add = TRUE)
  
  print_msg("Downloading dataset ", url, " from Zenodo...",
            msg_type = "INFO")
  
  if (utils::download.file(
    url, destfile = temporary_file, quiet = quiet
  ) != 0L) {
    print_msg("Unable to download the dataset from ", url,
              msg_type = "STOP")
  }
  
  if (!file.rename(temporary_file, dataset_path)) {
    print_msg("Unable to save the downloaded dataset to ", dataset_path,
              msg_type = "STOP")
  }
  
  dataset_path
}



#' Load an example dataset
#'
#' Loads an example dataset downloaded with [get_example()] into an environment.
#'
#' @param path Path returned by [get_example()].
#' @param envir The environment into which the dataset is loaded.
#'
#' @return Invisibly, a character vector of the names of objects loaded.
#'
#' @examples
#' \dontrun{
#' set_verbosity(3)
#' load_example(get_example("22745973/files/ENCFF119BYM_H3K36me3_chr1"))
#' }
#'
#' @export
load_example <- function(path, envir = parent.frame()) {
  check_this_var(path, type = "char")
  if (!file.exists(path)) {
    print_msg("Dataset file does not exist at", path, msg_type = "STOP")
  }
  if (!is.environment(envir)) {
    print_msg("envir must be an environment.", msg_type = "STOP")
  }

  print_msg("Loading dataset from ", path, msg_type = "DEBUG")
  invisible(load(path, envir = envir))
}
