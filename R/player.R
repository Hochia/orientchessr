#' Player input when race start
#'
#' @param data list
#' @param before numeric vector of length 2
#' @param after numeric vector of length 2
#'
#' @return The record, pieces information, and the graph
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' race_start() %>%
#'   player(c(2,1), c(3, 3)) %>%
#'   player(c(8,8), c(5, 8)) %>%
#'   player(c(2,3), c(2, 10))
player <- function(data, before, after) {

  bf <- paste0(before, collapse = ",")
  af <- paste0(after, collapse = ",")

  df <- data[[2]] %>%
    dplyr::mutate(
      state = dplyr::case_when(
        (at == af & state == TRUE) | state == FALSE ~ FALSE,
        TRUE ~ TRUE),
      at = dplyr::case_when(
        at == bf & state == TRUE ~ af,
        TRUE ~ at),
    ) %>%
    dplyr::select(name, at, state)

  data[[1]] <- data[[1]] %>%
    dplyr::add_row(
      name = dplyr::filter(df, at == af, state == TRUE)$name, # be carful to choose only one piece
      before = bf,
      after = af
    )

  xy <- unlist(stringr::str_split(df$at, ","))

  df2 <- df %>%
    dplyr::mutate(
      p1 = dplyr::case_when(name %in% ccname ~ TRUE, TRUE ~ FALSE),
      x = as.integer(xy[1:length(xy) %% 2 == 1]),
      y = as.integer(xy[1:length(xy) %% 2 == 0])
    ) %>%
    dplyr::filter(state == TRUE)

  gf <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
    ggplot2::geom_text(data = df2, ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::scale_fill_manual(values = c("white", "white")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  ls <- list(data[[1]], df, gf)
  ls
}
