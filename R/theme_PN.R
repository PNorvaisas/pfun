#' Theme Publication
#'
#' @param base_size Base size
#' @param base_family Base family
#' @keywords ggtheme, Publication
#' @export
#' @examples
#'

theme_PN<-function(base_size = par()$ps, base_family = par()$family,grid=FALSE) {
  faces <- c("plain", "bold", "italic", "bold.italic")
  half_line <- base_size/2
  #%+replace%
  thm <- ggthemes::theme_foundation(base_size=base_size) +
    theme(line = element_line(colour = par()$fg,
                              size = 0.5,
                              lineend = par()$lend,
                              linetype = par()$lty), 
  rect = element_rect(fill = par()$bg,
                      colour = par()$fg, 
                      size = 0.5,
                      linetype = par()$lty
  ),
  text=element_text(),
  axis.title = element_text(size = rel(7/6),
                            colour = par()$col.lab,
                            face = "bold"
  ), 
  axis.text = element_text(size = base_size,#rel(par()$cex.axis), 
                           colour = par()$col.axis,
                           face = "bold"
  ), 
  axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2, b = 0.8 * half_line/2)),
  axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2, l = 0.8 * half_line/2)),
  axis.ticks = element_line(colour = par()$fg), 
  legend.title = element_text(size=rel(7/6),
                              colour = par()$fg,
                              face="bold"),
  legend.text = element_text(size=base_size,
                             colour = par()$fg
  ), 
  legend.spacing = unit(5, "pt"),
  legend.key = element_rect(colour = NA), 
  panel.background = element_rect(fill = NA,
                                  colour = par()$col),
  panel.grid = element_blank(), 
  plot.background = element_rect(colour = NA),
  plot.title = element_text(face = "bold",
                            size = rel(7/6),
                            hjust = 0.5),
  strip.text = element_text(size = rel(7/6),
                            face = "bold",
                            colour = par()$col.sub
  ), 
  strip.background = element_rect(colour="#f0f0f0",
                                  fill="#f0f0f0"))
  las <- par()$las
  if (las == 0) {
    thm <- thm + theme(axis.title.x = element_text(angle = 0), 
                       axis.title.y = element_text(angle = 90))
  }
  else if (las == 1) {
    thm <- thm + theme(axis.title.x = element_text(angle = 0), 
                       axis.title.y = element_text(angle = 0))
  }
  else if (las == 2) {
    thm <- thm + theme(axis.title.x = element_text(angle = 90), 
                       axis.title.y = element_text(angle = 0))
  }
  else if (las == 3) {
    thm <- thm + theme(axis.title.x = element_text(angle = 90), 
                       axis.title.y = element_text(angle = 90))
  }
  if (grid) {
    thm <- thm + theme(panel.grid.major = element_line(colour="#f0f0f0"),
                       panel.grid.minor = element_blank())
  }
  if (par()$xaxt == "n") {
    thm <- thm + theme(axis.line.x = element_blank(),
                       axis.text.x = element_blank(), 
                       axis.ticks.x = element_blank())
  }
  if (par()$yaxt == "n") {
    thm <- thm + theme(axis.line.y = element_blank(),
                       axis.text.y = element_blank(), 
                       axis.ticks.y = element_blank())
  }
  thm
}

