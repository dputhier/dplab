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
#' @details
#' Palettes with a fixed set of colors (e.g. `"Okabe"`, 
#' `"Pal10"`) return their original colors when `n` is small enough, and
#' are interpolated otherwise. Palettes `"Ju1"`, `"Ju2"` and `"De1"` are always 
#' interpolated.
#'
#' Colorblind-friendly palettes: `"Okabe"`, `"Tol_Bright"`,
#' `"Tol_Muted"`, `"Tol_Vibrant"`, `"Tol_HC"`.
#'
#' `"Dynamic"`, `"Zissou"` and `"viridis"` rely on
#' [grDevices::hcl.colors()] (R >= 3.6.0). `"Gray"` is a gray ramp, safe for
#' black and white printing.
#'
#' @param n An integer specifying the number of colors to generate.
#' @param palette A character string specifying the palette to use.
#'
#' @return A named character vector of color codes.
#'
#' @importFrom grDevices hcl colorRampPalette gray.colors hcl.colors
#'
#' @examples
#' discrete_palette()
#' discrete_palette(n = 20, palette = "ggplot")
#' discrete_palette(n = 5, palette = "Tol_Muted")
#' discrete_palette(n = 5, palette = "Tol_HC")
#' 
#' @references
#' Okabe M, Ito K (2008). Color Universal Design (CUD): how to make figures
#' and presentations that are friendly to colorblind people.
#' <https://jfly.uni-koeln.de/color/>
#'
#' Paul Tol's Notes. Colour schemes and templates.
#' <https://sronpersonalpages.nl/~pault/>
#'
#' @export
discrete_palette <- function(
    n = 10,
    palette = c(
      "Ju1",
      "Ju2",
      "De1",
      "Okabe",
      "Tol_Bright",
      "Tol_Muted",
      "Tol_Vibrant",
      "Tol_HC",
      "Pal10",
      "Gray",
      "Dynamic",
      "Zissou",
      "viridis",
      "ggplot"
    )
) {
  
  check_this_var(n, type = "int")
  
  if (n < 1) {
    print_msg(
      "n must be a positive integer.",
      msg_type = "STOP"
    )
  }
  
  n <- as.integer(n)
  palette <- match.arg(palette)
  
  
  # ---------------------------------------------------------------------------
  # Palettes defined by a set of base colors
  # ---------------------------------------------------------------------------
  
  base_palettes <- list(
    
    # Always interpolated (see 'always_ramp' below)
    Ju1 = c(
      "#9F1717", "#AE5B11", "#C48D00", "#517416",
      "#115C8A", "#584178", "#9D1C70", "#E96767",
      "#EC9342", "#FFCA3A", "#8AC926", "#4DADE8",
      "#9579B9", "#E25CB4", "#DB2020", "#DA7316",
      "#F0AE00", "#6D9D1E", "#1882C0", "#71529A",
      "#D02494", "#EF9292", "#F2B57D", "#FFDA77",
      "#B6E36A", "#7BC4EE", "#AD98C9", "#EA8AC9"
    ),
    
    Ju2 = c(
      "#264653", "#2A9D8F", "#E9C46A", "#F4A261",
      "#E76F51", "#457B9D", "#6A4C93", "#BC4749",
      "#386641", "#F28482", "#577590", "#90BE6D"
    ),
    
    De1 = c("#808080", "#EA8AC9", "#9F1717"),
    
    # Colorblind-friendly
    Okabe = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#000000"
    ),
    
    Tol_Bright = c(
      "#4477AA", "#66CCEE", "#228833", "#CCBB44",
      "#EE6677", "#AA3377", "#BBBBBB"
    ),
    
    Tol_HC = c(
      "#BBBBBB",  "#004488", "#DDAA33", "#BB5566",
      "#000000"
      ),
    
    Tol_Muted = c(
      "#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE",
      "#882255", "#44AA99", "#999933", "#AA4499", "#BBBBBB"
    ),
    
    Tol_Vibrant = c(
      "#EE7733", "#0077BB", "#33BBEE", "#EE3377",
      "#CC3311", "#009988", "#BBBBBB"
    ),
    
    # Other qualitative palettes
    Pal10 = c(
      "#557EAA", "#E58A2F", "#D85B5E", "#70AEA9", "#5A9E50",
      "#E2C34F", "#A2769A", "#EE969F", "#936F5B", "#AAA8A6"
    )
  )
  
  # Palettes that are always interpolated, whatever n is
  always_ramp <- c("Ju1", "Ju2", "De1")
  
  # Palettes for which interpolation loses the colorblind-friendly property
  colorblind_safe <- c(
    "Okabe", "Tol_Bright", "Tol_Muted", "Tol_Vibrant", "Tol_HC"
  )
  
  
  # ---------------------------------------------------------------------------
  # Build the palette
  # ---------------------------------------------------------------------------
  
  if (palette %in% names(base_palettes)) {
    
    base_colors <- base_palettes[[palette]]
    
    if (palette %in% always_ramp) {
      
      colors <- grDevices::colorRampPalette(base_colors)(n)
      
    } else if (n <= length(base_colors)) {
      
      colors <- base_colors[seq_len(n)]
      
    } else {
      
      msg <- paste0(
        "Palette '", palette, "' only has ", length(base_colors),
        " colors: colors are interpolated to reach n = ", n,
        ", so some may be hard to distinguish."
      )
      
      if (palette %in% colorblind_safe) {
        msg <- paste0(
          msg,
          " The colorblind-friendly property of this palette is also lost."
        )
      }
      
      print_msg(msg, msg_type = "INFO")
      
      colors <- grDevices::colorRampPalette(base_colors)(n)
    }
    
  } else if (palette == "Gray") {
    
    # Gray ramp, safe for black and white printing
    colors <- grDevices::gray.colors(
      n,
      start = 0.15,
      end = 0.85
    )
    
  } else if (palette == "Dynamic") {
    
    colors <- grDevices::hcl.colors(n, "Dynamic")
    
  } else if (palette == "Zissou") {
    
    colors <- grDevices::hcl.colors(n, "Zissou 1")
    
  } else if (palette == "viridis") {
    
    colors <- grDevices::hcl.colors(n, "viridis")
    
  } else if (palette == "ggplot") {
    
    hues <- seq(
      15,
      375,
      length.out = n + 1
    )
    
    colors <- grDevices::hcl(
      h = hues,
      l = 65,
      c = 70
    )[seq_len(n)]
  }
  
  
  # ---------------------------------------------------------------------------
  # Names
  # ---------------------------------------------------------------------------
  
  names(colors) <- seq_len(n)
  
  colors
}
