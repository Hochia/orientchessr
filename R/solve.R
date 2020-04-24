#' Solve puzzle
#'
#' @param .data tibble or list
#'
#' @return The puzzle
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' pname <- c("JIANG", "SHI", "SHI", "MA", "JIU", "che", "che", "TZU", "pau", "pau", "shuai")
#' px <- c( 5, 4, 5, 6, 5, 2, 5, 4, 5, 5, 4)
#' py <- c(10,10, 9,10, 8, 4, 2, 3, 3, 4, 1)
#'
#' initiate_chessboard() %>%
#'   add_one(pname, px, py) %>%
#'   solve()
#'
#' initiate_chessboard() %>%
#'   add_one_pro(pname, px, py) %>%
#'   solve()
#'
#' initiate_chessboard() %>%
#'   add_one(pname, px, py) %>%
#'   show_current() %>%
#'   solve()
solve <- function(.data) {
  if(tibble::is_tibble(.data)) {
    .data <- .data
  } else if(tibble::is_tibble(.data[[1]])) {
    .data <- .data[[1]]
  } else if(tibble::is_tibble(.data[[1]][[1]])) {
    .data <- .data[[length(.data)]][[1]]
  } else {
    stop("Not puzzle data structure")
  }

  # gf is same as show_current()
  df2 <- .data %>%
    dplyr::mutate(p1 = dplyr::case_when(name %in% ccname ~ TRUE,
                                        TRUE ~ FALSE))

  gf <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = cl, ggplot2::aes(sx, sy, xend = ex, yend = ey)) +
    ggplot2::geom_point(data = df2, ggplot2::aes(x, y, color = p1, fill = p1), shape = 21, size = 10) +
    ggplot2::geom_text(data = df2, ggplot2::aes(x + 0.02, y + 0.05, label = name, color = p1), size = 5) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::scale_fill_manual(values = c("white", "white")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  ls <- list(dplyr::tibble(name = character(), before = character(), after = character()),
             .data %>%
               dplyr::transmute(name = name, at = stringr::str_c(x, y, sep = ",")) %>%
               dplyr::mutate(state = TRUE),
             gf)
  ls
}
