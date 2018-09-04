#' Plot both pdfs and pngs in one go.
#' 
#' @importFrom ggplot2 ggsave
#' @importFrom here here
#'
#' @param plot A ggplot2 plot object.
#' @param filename filename (without extenstion!)
#' @param ... Other arguments passed to \code{ggsave()}.
#' 
#' @export
ggsave2 <- function(plot, filename, ...) {

  if (!dir.exists(here("output", "figs"))) {
    dir.create(here("output", "figs"))
  }

  if (!dir.exists(here("output", "figs", "png"))) {
    dir.create(here("output", "figs", "png"))
  }


  ggsave(
    plot = plot,
    filename = paste0(
      here("output", "figs"),
      "/", filename, ".pdf"
    ),
    ...
  )
  
  ggsave(
    plot = plot,
    filename = paste0(
      here("output", "figs", "png"),
      "/", filename, ".png"
    ),
    ...
  )
  
  invisible(plot)
}

#' Project theme for ggplot2.
#' 
#' @importFrom ggplot2 theme element_text element_line margin element_blank
#' @importFrom grid unit
#'
#' @param ... Other arguments passed to \code{theme()}.
#' 
#' @export
theme_m <- function(...) {
  theme(
    text = element_text(size = 12,
                        colour = "black"),
    axis.text = element_text(size = 10,
                             colour = "black"),
    axis.title = element_text(size = 11,
                              colour = "black"),
    axis.line = element_line(),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(1, "mm"),
    plot.margin = margin(1, 5, 1, 1, "mm"),
    panel.spacing.x = unit(7.5, "mm"),
    panel.spacing.y = unit(2.5, "mm"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10,
                              face = "bold"),
    ## strip.text.y = element_text(angle = 0, face = "bold.italic"),
    strip.text.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.key.height = unit(4, "mm"),
    legend.title.align = .125,
    legend.margin = margin(),
    ...
  )
}
