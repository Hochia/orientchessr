#' Animate the record
#'
#' @param data list
#' @param speed numeric, control the speed of the animation
#' @param loop logical, control the loop of the animation
#'
#' @return Play the record
#' @export
#'
#' @examples
#' initiate_chessboard() %>%
#'   add_one("JU", 1, 1) %>%
#'   add_one("MA", 2, 1) %>%
#'   add_one("PAO", 7, 1) %>%
#'   animate_chessboard()
animate_chessboard <- function(data, speed = 1, loop = FALSE) {
  if (!is.data.frame(data)) {
    df <- data[[length(data)]][[1]]
  }
  else {
    df <- data
  }
  df <- df %>%
    dplyr::mutate(row = dplyr::row_number(),
                  p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                        TRUE ~ FALSE))

  anim <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_point(data = df, ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
    ggplot2::geom_text(data = df, ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::scale_fill_manual(values = c("white", "white")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    gganimate::transition_manual(data = data, row, cumulative = TRUE)

  gganimate::animate(anim,
                     renderer = gganimate::gifski_renderer(loop = loop),
                     nframes = nrow(df),
                     duration = (length(df) + 1) / (5 * speed))
}
