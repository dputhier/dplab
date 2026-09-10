#################################################################
##    make_cmp_string
#################################################################
#' @title Generate string specifying comparison samples
#' @description
#' A helper function to generate a string of format "A1,A2,A3 B1,B2,B3" specifying sample names
#' belonging to each group under comparison. This is useful for constructing comparison lists for
#' differential expression analysis or other group comparisons.
#'
#' @param sample_names Character vector of sample names (e.g. rownames of a sample metadata data.frame).
#' @param group_c Logical vector of same length as sample_names indicating the group 1 (control) samples
#'   for differential expression analysis according to this comparison.
#' @param group_t Logical vector of same length as sample_names indicating the group 2 (treatment) samples
#'   for differential expression analysis according to this comparison.
#'
#' @return A string with two groups of comma-separated sample names, separated by a space
#'   (e.g. "A1,A2,A3 B1,B2,B3")
#'
#' @examples
#' sample_metadata <- data.frame(
#'   "Condition" = c("DM", "DM", "DM", "PI", "PI", "PI"),
#'   "Rep" = c("1", "2", "3", "1", "2", "3"),
#'   row.names = c("DM1", "DM2", "DM3", "PI1", "PI2", "PI3")
#' )
#'
#' make_cmp_string(
#'   sample_names = rownames(sample_metadata),
#'   group_c = (sample_metadata$Condition == "DM"),
#'   group_t = (sample_metadata$Condition == "PI")
#' )
#'
#' @export
make_cmp_string <- function(sample_names, group_c, group_t) {
  
  stopifnot(
    "group_c is different length than sample_names" = 
      length(sample_names) == length(group_c)
  )
  
  stopifnot(
    "group_t is different length than sample_names" = 
      length(sample_names) == length(group_t)
  )
  
  stopifnot(
    "group_c is not a boolean" = inherits(group_c, "logical")
  )
  
  stopifnot(
    "group_t is not a boolean" = inherits(group_t, "logical")
  )
  
  return(
    paste(
      paste(sample_names[group_c], collapse = ","), 
      paste(sample_names[group_t], collapse = ",")
    )
  )
}
