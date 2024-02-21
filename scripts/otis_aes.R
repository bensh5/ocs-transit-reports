library(RColorBrewer)
library(scico)
library(scales)

# City/OTIS color palette -------------------------------------------------

pal_phila <- c(
  dark_ben_franklin  = "#0f4d90",
  bell_yellow        = "#f3c613",
  flyers_orange      = "#f99300",
  kelly_drive_green  = "#58c04d",
  love_park_red      = "#cc3000",
  ben_franklin_blue  = "#2176d2",
  light_ben_franklin = "#96c9ff",
  electric_blue      = "#25cef7",
  light_bell         = "#ffefa2",
  light_red          = "#fed0d0",
  light_kelly_drive  = "#b9f2b1",
  light_blue         = "#DAEDFE",
  phanatic_green     = "#3a833c",
  pride_purple       = "#9400c6",
  black              = "#000000",
  dark_gray          = "#444444",
  medium_gray        = "#a1a1a1",
  sidewalk           = "#cfcfcf"
)

pal_connect <- c(
  connect_green   = "#00af41",
  connect_blue    = "#0f4d90",
  connect_yellow  = "#f3c613",
  connect_red     = "#ed552b",
  connect_teal    = "#45c1c2",
  connect_gray    = "#6d6e71"
  )

#' Title
#'
#' @param ... 
#' @param palette 
#'
#' @return
#' @export
#'
#' @examples
get_colors <- function(..., palette = pal_phila) {
  colors <- c(...)
  
  if (is.null(colors))
    return (palette)
  palette[colors]
}

pal_phila_list <- list(
  main = get_colors("ben_franklin_blue",
                    "bell_yellow",
                    "pride_purple",
                    "kelly_drive_green",
                    "love_park_red",
                    "medium_gray",
                    "pride_purple",
                    "ben_franklin_blue"),
  dark = get_colors("dark_ben_franklin", "phanatic_green", "love_park_red",
                    "pride_purple", "dark_gray"),
  light = get_colors("light_blue", "light_bell", "light_red", "light_kelly_drive"),
  blues = get_colors("dark_ben_franklin", "ben_franklin_blue", "light_ben_franklin"),
  grays = get_colors("black", "dark_gray", "medium_gray", "sidewalk")
)

#' scale_color_phl Color scale
#' 
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 3)
#' p + scale_color_phl()
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Petal.Length)) +
#'   geom_point(size = 3)
#' p + scale_color_phl("blues", discrete = F)
#' p + scale_color_phl("blues", discrete = F, reverse = T)
#'
scale_color_phl <- function(...,
                            palette = "main",
                            discrete = T,
                            reverse = F,
                            pal_list = pal_phila_list,
                            named = F) {
  
  pal <- pal_phila_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_color_manual(..., values = pal)
  } else {
    scale_color_gradientn(..., colors = pal)
  }
}

#' scale_color_phl Fill scale
#' 
#' @examples
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#'
#' p + scale_fill_phl()
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#' p + scale_fill_phl("blues", discrete = F)
#' p + scale_fill_phl("blues", discrete = F, reverse = T)
#'
scale_fill_phl <- function(...,
                           palette = "main",
                           discrete = T,
                           reverse = F,
                           pal_list = pal_phila_list,
                           named = F) {
  
  pal <- pal_phila_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_fill_manual(..., values = pal)
  } else {
    scale_fill_gradientn(..., colors = pal)
  }
}

# OTIS ggplot theme -------------------------------------------------------


theme_otis <- function(..., 
                       base_size = 12, 
                       title_align = "panel", 
                       legend_pos = "right") {
  font <- c("Montserrat","Open Sans")
  theme_minimal() %+replace%
    theme(
      axis.ticks = element_line(color = "grey92"),
      legend.position = legend_pos,
      title = element_text(
        size = rel(1.25),
        face = 'plain',
        family = font[1],
      ),
      text = element_text(
        size = base_size,
        family = font[2],
        color = "#444444"
      ),
      strip.text = element_text(
        size = base_size,
        family = font[1],
        hjust = 0
      ),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#FFFFFF', color = '#FFFFFF'),
      plot.title.position = title_align,
      plot.caption.position = "plot",
      plot.margin = margin(12,24,12,24),
      plot.title = element_text(
        hjust = 1,
        margin = margin(0,0,6,0)
      ),
      plot.subtitle = element_text(
        size = base_size,
        family = font[2],
        hjust = 1,
        margin = margin(0,0,12,0)
      ),
      plot.caption = element_text(
        size = rel(0.5),
        family = font[2],
        hjust = 1
      ),
      legend.title = element_text(
        size = base_size
      ),
      axis.title = element_text(
        size = rel(0.75),
        vjust = -1
      ),
      axis.text = element_text(
        size = rel(0.75),
        margin = margin(0,0,3,0)
      )
    )
}
