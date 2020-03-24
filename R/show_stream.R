#' Clean the data label of axes in each graph
#'
#' @param data dataframe or list
#'
#' @return Show the records
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' initiate_chessboard() %>%
#'   add_one("PAO", 5, 1) %>%
#'   add_one("JU", 6, 3) %>%
#'   show_stream()
#'
#' pname <- c("BING", "ZU", "SHI", "SHI", "XIANG", "PAO")
#' px <- c(1, 1, 4, 6, 3, 2)
#' py <- c(4, 7, 1, 1, 1, 3)
#'
#' initiate_chessboard() %>%
#'   add_one(pname, px, py) %>%
#'   add_one("PAO", 5, 1) %>%
#'   show_stream()
show_stream <- function(data) {

  if (!is.data.frame(data)) {
    df <- data[[length(data)]][[1]]
  }
  else {
    df <- data
  }

  df2 <- df %>%
    dplyr::mutate(p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                        TRUE ~ FALSE))

  ls <- vector("list")
  for (i in 1:nrow(df)) {

    gf <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
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
