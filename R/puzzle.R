#' Demonstrate the chessboard with labeling points.
#'
#' @return The chessboard
#' @export
#'
#' @examples
#' puzzle()
puzzle <- function() {
  gf <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_label(data = cba, ggplot2::aes(x + 0.02, y + 0.05, label = xy),
                        color = "white", fill = "red", size = 5) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  ls <- vector("list")
  ls[[1]] <- list(df, gf)
  ls
}
