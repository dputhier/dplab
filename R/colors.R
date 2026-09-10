#################################################################
##    Gradient color palettes
#################################################################

.gradient_palettes <- list(
  
  Je1 = c(
    "#27408B", "#3A5FCD", "#3288BD", "#66C2A5",
    "#ABDDA4", "#E6F598", "#FEE08B", "#FDAE61",
    "#F46D43", "#D53E4F", "#8B2323"
  ),
  
  Seurat_Like = c(
    "#5D50A3", "#9FD7A4", "#FBFDBA", "#FEB163", "#A80B44"
  ),
  
  Ju1 = c(
    "#A9D6E5", "#2166AC", "#000000", "#B2182B", "#FFCA3A"
  ),
  
  De1 = c(
    "#d73027", "#fc8d59", "#fee090",
    "#e0f3f8", "#91bfdb", "#253494"
  ),
  
  De2 = c(
    "#FFF7FB", "#ECE2F0", "#D0D1E6", "#A6BDDB",
    "#67A9CF", "#3690C0", "#02818A", "#016450"
  ),
  
  De3 = c(
    "#1A1835", "#15464E", "#2B6F39", "#757B33",
    "#C17A70", "#D490C6", "#C3C1F2", "#CFEBEF"
  ),
  
  De4 = c(
    "#0000FF", "#00FFFF", "#80FF80", "#FFFF00", "#FF0000"
  ),
  
  De5 = c(
    "#0000AA", "#0000FF", "#00FFFF", "#80FF80",
    "#FFFF00", "#FF0000", "#AA0000"
  ),
  
  De6 = c(
    "#4575b4", "#74add1", "#abd9e9", "#e0f3f8",
    "#fee090", "#fdae61", "#f46d43", "#d73027"
  ),
  
  De7 = c(
    "#67001f", "#b2182b", "#d6604d", "#f4a582",
    "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de",
    "#4393c3", "#2166ac", "#053061"
  ),
  
  De8 = c(
    "#2b83ba", "#abdda4", "#fdae61", "#d7191c"
  ),
  
  De9 = c(
    "#0000BF", "#0000FF", "#0080FF", "#00FFFF",
    "#40FFBF", "#80FF80", "#BFFF40", "#FFFF00",
    "#FF8000", "#FF0000", "#BF0000"
  ),
  
  Magma = c(
    "#ffdb00", "#ffa904", "#ee7b06", "#a12424", "#400b0b"
  ),
  
  viridis = c(
    "#fde725", "#5ec962", "#21918c", "#3b528b", "#440154"
  ),
  
  magma2 = c(
    "#fcfdbf", "#fc8961", "#b73779", "#51127c", "#000004"
  ),
  
  plasma = c(
    "#f0f921", "#f89540", "#cc4778", "#7e03a8", "#0d0887"
  )
)


#' @title Generate a vector of colors for a gradient
#'
#' @description
#' This function generates a vector of colors for a gradient, given
#' a specified palette name.
#'
#' @param palette A character string specifying the palette to use.
#'
#' @return A character vector of color codes.
#'
#' @examples
#' colors_for_gradient()
#' colors_for_gradient(palette = "Ju1")
#'
#' @export
colors_for_gradient <- function(palette = "Je1") {
  
  palette <- match.arg(
    palette,
    choices = names(.gradient_palettes)
  )
  
  .gradient_palettes[[palette]]
}


#################################################################
##    Discrete color palettes
#################################################################

#' @title Generate a discrete color palette
#'
#' @description
#' This function generates a vector of colors for a discrete variable,
#' given a specified palette name.
#'
#' @param n An integer specifying the number of colors to generate.
#' @param palette A character string specifying the palette to use.
#'
#' @return A character vector of color codes.
#'
#' @importFrom grDevices hcl colorRampPalette
#'
#' @examples
#' discrete_palette()
#' discrete_palette(n = 20, palette = "ggplot")
#'
#' @export
discrete_palette <- function(
    n = 10,
    palette = c("Ju1", "De1", "ggplot")
) {
  
  if (!is.numeric(n) ||
      length(n) != 1 ||
      is.na(n) ||
      n < 1 ||
      n %% 1 != 0) {
    
    cli::cli_abort(
      "{.arg n} must be a positive integer."
    )
  }
  
  n <- as.integer(n)
  
  palette <- match.arg(palette)
  
  if (palette == "Ju1") {
    
    palette <- grDevices::colorRampPalette(
      c(
        "#9F1717", "#AE5B11", "#C48D00", "#517416",
        "#115C8A", "#584178", "#9D1C70", "#E96767",
        "#EC9342", "#FFCA3A", "#8AC926", "#4DADE8",
        "#9579B9", "#E25CB4", "#DB2020", "#DA7316",
        "#F0AE00", "#6D9D1E", "#1882C0", "#71529A",
        "#D02494", "#EF9292", "#F2B57D", "#FFDA77",
        "#B6E36A", "#7BC4EE", "#AD98C9", "#EA8AC9"
      )
    )(n)
    
  } else if (palette == "De1") {
    
    palette <- grDevices::colorRampPalette(
      c("#808080", "#EA8AC9", "#9F1717")
    )(n)
    
  } else {
    
    hues <- seq(15, 375, length.out = n + 1)
    
    palette <- grDevices::hcl(
      h = hues,
      l = 65,
      c = 100
    )[seq_len(n)]
  }
  
  names(palette) <- seq_len(n)
  
  palette
}
