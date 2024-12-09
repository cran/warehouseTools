#' Calculate the Number of Neighbors for a Warehouse Location
#'
#' @description This function calculates the number of neighboring products for a specific product at a given location in the warehouse.
#'
#' @param product The product for which neighbors are being calculated.
#' @param location The specific location of the product in the warehouse.
#' @param goods_and_locations A data frame containing the products and their corresponding locations in the warehouse. It should have at least two columns: `product` and `location`.
#' @param warehouse_height warehouse height
#'
#' @return An integer representing the number of neighboring products at the specified location.
#'
#' @details The function calculates how many products share a similar location pattern in the warehouse by grouping locations and checking whether they fall within the same row or column as the provided location. The function returns the total number of products in the same general area, excluding the current product.
#'
#' @author
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' @references
#' Dmytrów, K. (2022). Analytical and simulation determination of order picking time in a low storage warehouse for shared storage systems. Operations Research and Decisions, 32(2), 34–51. \doi{10.37190/ord220203}
#'
#' @examples
#' scenario <- generate_sample_goods_and_locatons_scenario()
#' nr <- nr_neighbors(1,1,scenario,25)
#' @export
nr_neighbors = function(product, location, goods_and_locations,warehouse_height) {
  return(sum(vapply(unique(goods_and_locations$product), function(p) {
    return (any((dplyr::filter(goods_and_locations, .data$product == p)$location - 1) %/% (2 *
                                                                                warehouse_height) == (location - 1) %/% (2 * warehouse_height)
    ))
  }, numeric(1))) - 1)
}
