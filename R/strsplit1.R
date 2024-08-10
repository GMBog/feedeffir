#' Title
#'
#' @param x
#' @param split
#'
#' @return
#' @export
#'
#' @examples
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

x <- "alfa,bravo,charlie,delta"
strsplit1(x, split = ",")
