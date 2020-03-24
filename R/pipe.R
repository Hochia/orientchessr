#' Pipe graphics
#'
#' orientchessr uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' df <- initiate_chessboard() %>% add_one("name", 5, 1)
NULL
