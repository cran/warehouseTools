#' Create a Rectangular Warehouse Layout from COPRAS Locations
#'
#' This function generates a rectangular warehouse layout matrix based on COPRAS-assigned locations, given the warehouse dimensions.
#'
#' @param locations A numeric vector representing the COPRAS-assigned locations of items in the warehouse.
#' @param warehouse_width An integer specifying the width of the warehouse.
#' @param warehouse_height An integer specifying the height of the warehouse.
#'
#' @details
#' The function maps the given COPRAS-assigned item locations to a rectangular warehouse layout. The x and y coordinates are calculated based on the warehouse dimensions and adjusted for the warehouse structure. Additional rows are added for boundary positions at the top and bottom of the warehouse.
#'
#' The resulting matrix contains the following columns:
#' - `x`: The x-coordinate (column) of the warehouse.
#' - `y`: The y-coordinate (row) of the warehouse.
#'
#' The matrix is sorted by x and y coordinates and ensures unique rows in the layout.
#'
#' @return A matrix representing the rectangular warehouse layout. Each row corresponds to a coordinate in the warehouse.
#'
#' @examples
#' # Example usage
#' locations <- c(1, 5, 10, 15)
#' warehouse_width <- 8
#' warehouse_height <- 4
#' rectangular_warehouse_from_copras_locations(locations, warehouse_width, warehouse_height)
#'
#' @author
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' scenario2 <- generate_sample_goods_and_locatons_scenario(warehouse_height = 10,warehouse_width = 10,nr_goods = 5)
#' copras <- copras_assignment(scenario2)
#' locations<-unique(unlist(lapply(copras, function(x)\{return(x$copras$location)\} )))
#' coordinates <-  rectangular_warehouse_from_copras_locations(locations,warehouse_height = 10,warehouse_width = 10)
#'
#' @references
#' Gudehus, T., & Kotzab, H. (2012). *Comprehensive Logistics*. Springer Berlin Heidelberg.
#' \doi{10.1007/978-3-642-24367-7}
#'
#' Zavadskas, E. K., Kaklauskas, A., & Šarka, V. (1994). The new method of multicriteria complex proportional assessment projects.
#' In E. K. Zavadskas & P. Linnert (Eds.), *Technological and economic development of economy. Volume 3. Business Management* (pp. 131–140). Vilnius: „Technika”.
#'
#'
#' @export
rectangular_warehouse_from_copras_locations <- function(locations,
                                                        warehouse_width,
                                                        warehouse_height) {
  warehouse_width_reduced <- warehouse_width %/% 2
  warehouse <- data.frame(
    x = c(
      (locations-1) %/% (2 * warehouse_height) + 1,
      1:warehouse_width_reduced,
      1:warehouse_width_reduced
    ),
    y = c(
      warehouse_height - (locations - 1) %% warehouse_height,
      rep(0, warehouse_width_reduced),
      rep(warehouse_height + 1, warehouse_width_reduced)
    )
  )
  return(as.matrix(warehouse %>% arrange(.data$x, .data$y) %>% distinct))
}



scenario2 <- generate_sample_goods_and_locatons_scenario(warehouse_height = 10,warehouse_width = 10,nr_goods = 20)
copras <- copras_assignment(scenario2)
locations<-unique(unlist(lapply(copras, function(x){return(x$copras$location)} )))
coordinates <-  rectangular_warehouse_from_copras_locations(locations,warehouse_height = 10,warehouse_width = 10)
