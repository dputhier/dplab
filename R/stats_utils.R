#################################################################
##    print_stat
#################################################################
#' @title Debugging statistics (vector, matrix or dataframe)
#' @description
#' Mostly a debugging function that will print some summary
#' statistics about a numeric vector, matrix or dataframe.
#'
#' @param msg The message to users.
#' @param data The vector (numeric) for which the stats are to be produced.
#' @param msg_type The type of message, one of "INFO", "DEBUG", or "WARNING".
#' @param round_val Round the values in its first argument to the specified number
#'   of decimal. Set argument to -1 for no rounding.
#' @return NULL, invisibly.
#'
#' @examples
#' print_stat("My data", 1:10, msg_type = "INFO")
#' set_verbosity(2)
#' print_stat("My data", matrix(rnorm(10), nc = 2), msg_type = "DEBUG")
#' set_verbosity(0)
#' print_stat("My data", matrix(rnorm(10), nc = 2), msg_type = "DEBUG")
#' @export
print_stat <- function(msg,
                       data,
                       round_val = 2,
                       msg_type = c("DEBUG", "WARNING", "INFO")) {
  
  msg_type <- match.arg(msg_type)
  
  if (inherits(data, "data.frame")) {
    data <- as.matrix(data)
  }
  
  data <- as.vector(data)
  
  if (!is.numeric(data)) {
    
    print_msg(
      "Can't print stats from non numeric object",
      msg_type = "WARNING"
    )
    
    stats <- "No Statistics"
    
  } else {
    
    stats <- summary(data)
    names(stats) <- c("Min", "Q1", "Med", "Mean", "Q3", "Max")
    
    check_this_var(round_val, type = "num")
    
    if (round_val >= 0) {
      stats <- round(stats, round_val)
    }
    
    stats <- paste(
      names(stats),
      stats,
      sep = ":",
      collapse = " "
    )
  }
  
  print_msg(
    paste0(msg, ": ", stats),
    msg_type = msg_type
  )
  
  invisible(NULL)
}


# -------------------------------------------------------------------------
# control_list()   --------------------------------------------------------
# -------------------------------------------------------------------------

#' @title Generate a Control Gene List with Similar Distribution
#' @description
#' This function generates a control gene list with a distribution of expression values similar to the provided gene list.
#'
#' @param expression_value A named vector containing all gene expression values and their associated gene names.
#' @param gene_list A character vector of gene names for which a control gene list made of different genes with similar distribution is to be found.
#' @return A character vector of control gene names with similar expression distribution to the input \code{gene_list}.
#' @details The function works by calculating the absolute differences between the expression values of the genes in \code{gene_list}
#' and those of all other genes in the \code{expression_value} vector. For each gene in the \code{gene_list}, the gene with the
#' smallest difference in expression value from the remaining genes is selected. This process ensures that the selected control genes
#' have expression values that are closely matched to the target genes, thereby maintaining a similar distribution of expression values
#' in the control list. Ensuring the distribution are the same (e.g histogram, boxplot...) is important as the function may fail depending on the way the
#' gene list is sampled from the global distribution.
#' @examples
#' # Continuous values
#' set.seed(1)
#' a <- sort(c(rnorm(100, mean=0), rnorm(100, mean=4)))
#' names(a) <- paste0("gene_", 1:length(a))
#' b <- names(a[sample(1:100, size = 10, replace = FALSE)])
#' control <- control_list(a, b)
#' boxplot(list(a, a[b], a[control]))
#' all(!b %in% control)
#'
#' # Discrete values
#' set.seed(1)
#' a <- c(rpois(100, 10), rpois(100, 30))
#' names(a) <- paste0("gene_", 1:length(a))
#' b <- names(a[sample(50:130, size = 10, replace = FALSE)])
#' control <- control_list(a, b)
#' boxplot(list(a, a[b], a[control]))
#' all(!b %in% control)
#' @importFrom stats ks.test
#' @export
control_list <- function(expression_value = NULL,
                         gene_list = NULL) {
  
  if (!is.numeric(expression_value) ||
      is.null(names(expression_value))) {
    print_msg(
      "expression_value must be a named numeric vector.",
      msg_type = "STOP"
    )
  }
  
  if(length(unique(names(expression_value))) != length(names(expression_value)))
    print_msg("The provided vector of expression values contains duplicate names.",
              msg_type = "STOP")
  
  if (anyNA(expression_value)) {
    print_msg(
      "expression_value contains missing values (NA).",
      msg_type = "STOP"
    )
  }
  
  if (!is.character(gene_list)) {
    print_msg(
      "gene_list must be a character vector.",
      msg_type = "STOP"
    )
  }
  
  if(length(unique(gene_list)) != length(gene_list))
    print_msg("The provided gene list contains duplicates.",
              msg_type = "STOP")
  
  if(!all(sapply(gene_list, "nchar") > 0))
    print_msg("Some gene names are empty.",
              msg_type = "STOP")
  
  if(!all(sapply(names(expression_value), "nchar") > 0))
    print_msg("Some gene names are empty.",
              msg_type = "STOP")
  
  if(!all(gene_list %in% names(expression_value)))
    print_msg("Some genes from gene_list are not part of expression_value...",
              msg_type = "STOP")
  
  other_genes <- names(expression_value[!names(expression_value) %in% gene_list])
  
  if(length(other_genes) < length(gene_list))
    print_msg("The number of remaining genes is not sufficient to find a matched distribution...",
              msg_type = "STOP")
  
  diff <- abs(outer(expression_value[gene_list],
                    expression_value[other_genes], "-"))
  
  f <- integer()
  g <- character()
  
  for(i in 1:nrow(diff)){
    if(length(f) > 0){
      if(length(diff[i, -f]) == 0)
        print_msg("Not enough remaining expression values.",
                  msg_type = "STOP")
      
      tmp <- which(diff[i, -f] == min(diff[i, -f]))
      
      if(length(tmp) > 1)
        tmp <- tmp[sample(length(tmp))][1]
      
      g[i] <- colnames(diff[i, -f, drop=FALSE])[tmp]
      f[i] <- which(colnames(diff) == g[i])
      
    }else{
      
      f[i] <- which(diff[i, ] == min(diff[i, ]))[1]
      g[i] <- colnames(diff)[f[i]]
      
    }
  }
  
  
  ks <- ks.test(expression_value[gene_list], expression_value[g])$p.value
  
  print_msg("Kolmogorov-Smirnov test p-value :",  ks)
  
  return(g)
}
