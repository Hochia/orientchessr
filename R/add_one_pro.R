#' Input the data one by one, record the input process and each graph
#'
#' @param data list
#' @param name character, name of the chess
#' @param x numeric, x-axis
#' @param y numeric, y-axis
#'
#' @return Add one chess to the data and show the graphs
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' initiate_chessboard() %>%
#'   add_one_pro("MA", 5, 1)
add_one_pro <- function(data, name, x, y) {

  if (!is.data.frame(data)) {
    df <- data[[length(data)]][[1]]
  }
  else {
    df <- data
  }

  df <- df %>%
    dplyr::add_row(name = name,x = x, y = y) %>%
    dplyr::filter(name != "name")

  df2 <- df %>%
    dplyr::mutate(p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                        TRUE ~ FALSE))

  # store the data and image
  ls <- vector("list")
  for (i in 1:nrow(df)) {
    # cancel the label placing chesses
    cba <- cba %>%
      dplyr::anti_join(df[1:i,])

    gf <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
      ggplot2::geom_label(data = cba, ggplot2::aes(x + 0.02, y + 0.05, label = xy), color = "white", fill = "red", size = 5) +
      ggplot2::geom_point(data = df2[1:i,], ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
      ggplot2::geom_text(data = df2[1:i,], ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
      ggplot2::scale_color_manual(values = c("black", "red")) +
      ggplot2::scale_fill_manual(values = c("white", "white")) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    ls[[i]] <- list(df[1:i,], gf)
  }
  ls
}
