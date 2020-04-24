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
  df1 <- data[[1]]
  df2 <- data[[2]]

  n <- nrow(df1)
  nm <- df1[[1]]
  bf <- df1[[2]]
  af <- df1[[3]]

  li <- list()
  li[[n+1]] <- df2

  for(i in n:1) {
    temp1 <- df1[i,]
    temp2 <- df2
    check <- df2[[1]] == nm[i] & df2[[2]] == af[i]
    if(sum(check) == 1) {
      df2[[2]][check] <- df1[[2]][i]
    } else {
      df2[[2]][max(which(check))] <- df1[[2]][i]
    }
  }

  df2 <- dplyr::mutate(df2, state = TRUE, step = 1)
  li[[1]] <- df2
  for(i in 1:n) {
    bf <- df1[[2]][i]
    af <- df1[[3]][i]

    df2 <- df2 %>%
      dplyr::mutate(
        state = dplyr::case_when(
          (at == af & state == TRUE) | state == FALSE ~ FALSE,
          TRUE ~ TRUE),
        at = dplyr::case_when(
          at == bf & state == TRUE ~ af,
          TRUE ~ at),
      ) %>%
      dplyr::mutate(step = i + 1)

    li[[i + 1]] <- df2
  }

  df3 <- dplyr::bind_rows(!!!li) %>%
    dplyr::filter(state == TRUE) %>%
    tidyr::separate(at, c("x", "y"), sep = ",", convert = TRUE) %>%
    dplyr::mutate(p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                        TRUE ~ FALSE))

  anim <- ggplot2::ggplot(df3) +
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
                     nframes = n + 1,
                     duration = (n + 1) / speed)
}
