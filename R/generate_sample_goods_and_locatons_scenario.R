#' Generate Sample Goods and Locations Scenario for a Warehouse
#'
#' @description This function generates a sample scenario of goods and their locations in a warehouse, along with related data such as storage time, demand ratio, and full packages demand ratio.
#'
#' @param warehouse_height An integer specifying the height of the warehouse (default is 25).
#' @param warehouse_width An integer specifying the width of the warehouse (default is 40).
#' @param warehouse_size The total size of the warehouse (height * width), default is calculated as `warehouse_height * warehouse_height`.
#' @param nr_goods An integer specifying the number of different goods/products in the warehouse (default is 10).
#' @param max_nr_locations A vector specifying the maximum number of locations each product can occupy (default is `rep(10, nr_goods)`).
#' @param max_storage_time A vector specifying the maximum storage time for each product (default is `rep(30, nr_goods)`).
#' @param max_demand A vector specifying the maximum demand for each product (default is `rep(200, nr_goods)`).
#' @param actual_demand A vector specifying the actual demand for each product (default is `rep(100, nr_goods)`).
#' @param full_packaging_amount A vector specifying the full packaging amount for each product (default is `rep(20, nr_goods)`).
#'
#' @return A data frame containing the generated scenario data with columns for:
#'
#' - `product`: Product identifier.
#'
#' - `location`: Location of the product in the warehouse.
#'
#' - `storage_time`: Storage time of the product.
#'
#' - `demand_ratio`: The ratio of demand relative to the actual demand.
#'
#' - `full_packages_demand_ratio`: The ratio of full packages relative to the demand.
#'
#' - `distance`: The calculated distance to the I/O point.
#'
#' - `neighbors`: The number of neighboring products for each location.
#'
#' @details The function randomly assigns goods to locations in the warehouse and calculates various parameters such as storage time, demand ratio, and the full packages demand ratio for each product. It also calculates the number of neighboring products at each location and the distance from the location to the I/O point.
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
#' scenario2 <- generate_sample_goods_and_locatons_scenario(warehouse_height = 10,
#' warehouse_width = 10,nr_goods = 5)
#'
#'
#' @import dplyr
#' @importFrom graphics lines text
#' @importFrom stats runif
#'
#' @export
generate_sample_goods_and_locatons_scenario <- function(warehouse_height = 25,
                                                        warehouse_width = 40,
                                                        warehouse_size = warehouse_height * warehouse_height,
                                                        nr_goods = 10,
                                                        # check if size is equal to nr_goods if no, extend
                                                        max_nr_locations = rep(10, nr_goods),
                                                        max_storage_time = rep(30, nr_goods),
                                                        max_demand = rep(200, nr_goods),
                                                        actual_demand = rep(100, nr_goods),
                                                        full_packaging_amount = rep(20 , nr_goods)) {
  product = NULL
  location = NULL
  storage_time = NULL
  distance = NULL
  demand_ratio = NULL
  full_packages_demand_ratio = NULL

  for (i in 1:nr_goods) {
    locations_for_product = sample(1:warehouse_size, sample(1:max_nr_locations[i], 1))
    for (j in locations_for_product) {
      product = c(product, i)
      location = c(location, j)
      storage_time = c(storage_time, sample(1:max_storage_time[i], 1))
      demand = sample(1:max_demand[i], 1)
      if (demand > actual_demand[i]) {
        demand = actual_demand[i]
      }
      dr = demand / actual_demand[i]
      demand_ratio =  c(demand_ratio, dr)
      if (runif(1, 0, 1) >= 0.5) {
        full_packages_demand_ratio = c(
          full_packages_demand_ratio,
          demand %/% full_packaging_amount[i] * full_packaging_amount[i] / actual_demand[i]
        )
      }
      else{
        full_packages_demand_ratio = c(full_packages_demand_ratio, 0)
      }
    }
  }
  goods_and_locations = data.frame(product,
                                   location,
                                   storage_time,
                                   demand_ratio,
                                   full_packages_demand_ratio)
  neighbors = NULL
  distance_calculated = NULL
  for (i in 1:nrow(goods_and_locations)) {
    neighbors = c(
      neighbors,
      nr_neighbors(
        goods_and_locations$product[i],
        goods_and_locations$location[i],
        goods_and_locations,
        warehouse_height
      )
    )
    distance_calculated = c(
      distance_calculated,
      iopoint_distance(
        goods_and_locations$location[i],
        warehouse_height
      )
    )
  }
  goods_and_locations = cbind(goods_and_locations, distance = distance_calculated, neighbors)
}
