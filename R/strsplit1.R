# It is reasonable to make one new .R file for each user-facing function
# and name the file after de function
# As you add more functions you will want to relax this, and start grouping
# related functions together.


# Here is how to write the documentation that is opened when asked for help.
# After writing this, execute the command document()

#' Split a string
#'
#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")


strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}
