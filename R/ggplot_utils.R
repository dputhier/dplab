# -------------------------------------------------------------------------
# denlab_gg_theme             ---------------------------------------------
# -------------------------------------------------------------------------

#' Custom ggplot2 Theming for DenLab Packages
#'
#' This function defines a custom theme for ggplot2 plots with specific adjustments
#' to the legend key dimensions, legend text, axis titles, and strip text sizes.
#'
#' @return A ggplot2 theme object with predefined settings for legend, axis, and strip text.
#'
#' @details
#' The \code{denlab_gg_theme} function customizes the appearance of ggplot2 plots.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   denlab_gg_theme()
#' }
#'
#' @importFrom ggplot2 theme element_text unit
#' @export
denlab_gg_theme <- function(){
  
  gg_theme <- ggplot2::theme(
    legend.key.width = ggplot2::unit(0.10, "in"),
    legend.key.height = ggplot2::unit(0.10, "in"),
    legend.text = ggplot2::element_text(size = 8),
    legend.title = ggplot2::element_text(size = 8),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_text(size = 8),
    strip.text.x = ggplot2::element_text(size = 7),
    strip.text.y = ggplot2::element_text(size = 7))
  
  return(gg_theme)
  
}
