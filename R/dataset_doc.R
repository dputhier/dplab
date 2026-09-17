#---------------------------------------
# The clustermole_subset dataset
#---------------------------------------

#' @title The clustermole_subset dataset
#' @description
#'  Few markers from the clustermole package This dataset is used for testing purposes.
#'
#' @docType data
#' @keywords datasets
#' @name clustermole_subset
#' @usage data(clustermole_subset)
#' @format A list of size 28.
#'
#' @note
#' This dataset was produced using the clustermole package:
#' m <- clustermole_markers(species = "hs")
#' clustermole_subset <- split(m$gene[m$organ == "Blood" & m$species == "Human"],
#'                             m$celltype[m$organ == "Blood" & m$species == "Human"])
#'
#' @examples
#' data(clustermole_subset)
#'
'clustermole_subset'

#---------------------------------------
# The ident_pbmc3k_medium dataset
#---------------------------------------

#' @title The ident_pbmc3k_medium dataset
#' @description
#'  Cell identity (ident) from the pbmc3k_medium dataset. This dataset is used for testing purposes.
#'
#' @docType data
#' @keywords datasets
#' @name ident_pbmc3k_medium
#' @usage data(ident_pbmc3k_medium)
#' @format A named vector of size 361.
#' @examples
#' data(ident_pbmc3k_medium)
#'
'ident_pbmc3k_medium'
