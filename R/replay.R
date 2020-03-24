#' Replay the game
#'
#' @param data list
#' @param speed numeric, control the speed of the animation
#' @param loop logical, control the loop of the animation
#'
#' @return Replay the game
#' @export
#'
#' @examples
#' race_start() %>%
#'   player(c(2,1), c(3, 3)) %>%
#'   player(c(8,8), c(5, 8)) %>%
#'   player(c(2,3), c(2, 10)) %>%
#'   replay()
replay <- function(data, speed = 1, loop = TRUE) {

  df <- data[[1]]
  df2 <- dplyr::tibble(name = cp$name,
                       at = stringr::str_c(cp$x, cp$y, sep = ","),
                       state = TRUE,
                       step = 0)
  df4 <- df2

  for(i in 1:length(df)) {
    # check all pieces postion and state
    # save them as a data frame
    # add step count to the data frame
    # add_row for each loop
    # after the loop, all steps had been documented
    # the data frame is ready for animation
    df3 <- df2 %>%
      dplyr::mutate(state = dplyr::case_when(at == df[[3]][i] ~ FALSE,
                                             TRUE ~ TRUE),
                    at = dplyr::case_when(at == df[[2]][i] ~ df[[3]][i],
                                          TRUE ~ at),
                    step = i) %>%
      dplyr::filter(state == TRUE)

    df4 <- dplyr::bind_rows(df4, df3)
    df2 <- df3
  }

  df4 <- df4 %>%
    tidyr::separate(at, c("x", "y"), sep = ",", convert = TRUE) %>%
    dplyr::mutate(p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                 TRUE ~ FALSE))

  anim <- ggplot2::ggplot(df4) +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_point(ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
    ggplot2::geom_text(ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::scale_fill_manual(values = c("white", "white")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    gganimate::transition_manual(step)

  gganimate::animate(anim,
                     renderer = gganimate::gifski_renderer(loop = loop),
                     nframes = nrow(df) + 1,
                     duration = (length(df) + 1) / speed)
}
