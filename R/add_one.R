#' Add one or more pieces to the chessboard
#'
#' @param data list
#' @param name character, name of the chess
#' @param x numeric, x-axis
#' @param y numeric, y-axis
#'
#' @return Add pieces to the data
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' puzzle() %>%
#'   add_one("MA", 5, 1)
#'
#' pname <- c("BING", "ZU", "SHI", "SHI", "XIANG", "PAO")
#' px <- c(1,1,4,6,3,2)
#' py <- c(4,7,1,1,1,3)
#'
#' puzzle() %>%
#'   add_one(pname, px, py)
add_one <- function(data, name, x, y) {

  if (!is.data.frame(data)) {
    df <- data[[length(data)]][[1]]
  }
  else {
    df <- data
  }

  df <- df %>%
    dplyr::add_row(name = name, x = x, y = y) %>%
    dplyr::filter(name != "name")
  df
}
