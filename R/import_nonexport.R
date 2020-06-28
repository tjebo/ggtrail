#' Imported functions from other non-exported namespaces
#' @description from scales
#' @author Hadley Wickham
pal_name <- function (palette, type)
{
  if (is.character(palette)) {
    if (!palette %in% unlist(brewer)) {
      warning("Unknown palette ", palette)
      palette <- "Greens"
    }
    return(palette)
  }
  type <- match.arg(type, c("div", "qual", "seq"))
  brewer[[type]][palette]
}


brewer <- list(
  div = c(
    "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
    "Spectral"
  ),
  qual = c(
    "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
    "Set2", "Set3"
  ),
  seq = c(
    "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
    "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
    "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
  )
)

brewermax <- c(BrBG = 11, PiYG = 11, PRGn = 11, PuOr = 11, RdBu = 11, RdGy = 11,
  RdYlBu = 11, RdYlGn = 11, Spectral = 11, Accent = 8, Dark2 = 8,
  Paired = 12, Pastel1 = 9, Pastel2 = 8, Set1 = 9, Set2 = 8, Set3 = 12,
  Blues = 9, BuGn = 9, BuPu = 9, GnBu = 9, Greens = 9, Greys = 9,
  Oranges = 9, OrRd = 9, PuBu = 9, PuBuGn = 9, PuRd = 9, Purples = 9,
  RdPu = 9, Reds = 9, YlGn = 9, YlGnBu = 9, YlOrBr = 9, YlOrRd = 9
)

