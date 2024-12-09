#' Calculate IO Point Distance
#'
#' @description This function calculates the distance to the I/O point based on the provided position in the warehouse. The taxicab geometry is used to calculate the distance.
#'
#' @param i Integer index representing the position.
#' @param warehouse_height Integer representing the height of the warehouse (number of levels or rows).
#'
#' @return A numeric value representing the calculated distance to the I/O point.
#'
#' @details The function calculates the x and y coordinates of the position within the warehouse and returns the sum of x and y as the distance.
#'
#' @author
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' @references
#' Dmytrów, K. (2022). Analytical and simulation determination of order picking time in a low storage warehouse for shared storage systems. Operations Research and Decisions, 32(2), 34–51. \doi{10.37190/ord220203}
#'
#' Petrović, M., Malešević, B., Banjac, B., & Obradović, R. (2014). Geometry of some taxicab curves.
#' *4th International Scientific Conference on Geometry and Graphics*. Serbian Society for Geometry and Graphics, University of Niš, Srbija.
#'
#' @examples
#' iopoint_distance(i = 1,  warehouse_height = 10)
#' iopoint_distance(i = 15,  warehouse_height = 5)
#'
#' @export
iopoint_distance <- function(i, warehouse_height) {
  x = -2 + 3 * ((i - 1) %/% (2 * warehouse_height) + 1)
  y = warehouse_height - (i - 1) %% warehouse_height
  return(x + y)
}
