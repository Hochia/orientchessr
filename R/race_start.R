#' Race start
#'
#' @return A setup chessboard
#' @export
#'
#' @examples
#' race_start()
race_start <- function() {

  gf <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_point(data = cp, ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
    ggplot2::geom_text(data = cp, ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
    ggplot2::scale_color_manual(values = c("black", "red"), guide = FALSE) +
    ggplot2::scale_fill_manual(values = c("white", "white"), guide = FALSE) +
    ggplot2::theme_void()

  ls <- list(dplyr::tibble(name = character(), before = character(), after = character()),
             dplyr::tibble(name = cp$name,
                           at = stringr::str_c(cp$x, cp$y, sep = ",")),
             gf)
  ls
}
