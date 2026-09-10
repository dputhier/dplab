# Tests for color functions: colors_for_gradient, discrete_palette

set_verbosity(0)

test_that("Check colors_for_gradient()", {
  expect_equal(colors_for_gradient(palette = "Seurat_Like"),
               c("#5D50A3", "#9FD7A4", "#FBFDBA", "#FEB163", "#A80B44"))
  
  expect_equal(colors_for_gradient(palette = "Ju1"),
               c("#A9D6E5", "#2166AC", "#000000", "#B2182B", "#FFCA3A"))
  
  expect_equal(colors_for_gradient(palette = "Je1"),
               c("#27408B", "#3A5FCD", "#3288BD", "#66C2A5","#ABDDA4", "#E6F598","#FEE08B", "#FDAE61","#F46D43","#D53E4F","#8B2323"))
})


test_that("Check discrete_palette()", {
  expect_equal(length(discrete_palette(n = 10)), 10)
  
  expect_equal(unname(discrete_palette(n = 50)), c(
    "#9F1717", "#A73C13", "#B0600F", "#BC7B05", "#AC8704", "#6D7A10",
    "#3D6C39", "#1A5F79", "#2D5082", "#554278", "#7B2E73", "#A1206F",
    "#CB496A", "#E96E60", "#EB864C", "#F1A13F", "#FBBF3B", "#D4C932",
    "#93C927", "#6DBB81", "#4EABE7", "#768FCD", "#9E75B8", "#C865B5",
    "#E04E92", "#DC2D41", "#DA3B1C", "#DA6817", "#E38C0C", "#EFAC00",
    "#AAA40F", "#669A2B", "#378B84", "#2879B9", "#595EA4", "#8C4498",
    "#C02B94", "#DC4E93", "#ED8B92", "#F0A387", "#F2B67C", "#F9CA79",
    "#F4DB75", "#CCE06D", "#A7DB8A", "#87CAD3", "#8CB4E1", "#A79CCC",
    "#C891C9", "#EA8AC9"
  ))
})
