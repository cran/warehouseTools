#' COPRAS Method for Selection of Locations in a Warehouse
#'
#' @description This function allows for selection of locations in a warehouse to be visited by the picker during order picking using the COPRAS (COmplex PRoportional ASsessment) method. The method applies weighted criteria such as storage time, distance from the I/O (depot) point, degree of demand satisfaction, degree of demand satisfaction in full packages, and the number of other items on the pick list in the neighborhood of analyzed location to determine the selection of locations that satisfies the given take-out strategy.
#'
#' @param goods_and_locations A data frame containing information about goods and their locations. It should include at least the following columns: ‘product‘, ‘location‘, ‘storage_time‘, ‘distance‘, ‘demand_ratio‘, ‘full_packages_demand_ratio‘, and ‘neighbors‘.
#' @param weights A numeric vector of weights for the criteria used in the COPRAS method. The default is ‘c(rep(0.2, 5))‘, representing the weights for storage time, distance (distance from the I/O point), demand ratio (degree of demand satisfaction), full packages demand ratio (degree of demand satisfaction in full packages), and number of neighbors (number of other items on the pick list in the neighborhood of analyzed location), respectively.
#'
#' @return A list of results for each product, where each result includes:
#' - `normalized_and_weighted`: A data frame containing the normalized and weighted values for the criteria, along with the COPRAS scores (`q`) and rankings (`rank`) for each location.
#' - `copras`: A data frame containing the selected locations for each product based on the COPRAS method, with columns `product`, `location`, `rank`, and `demand_ratio`.
#'
#' @details The function normalizes the criteria for each product and multiplies them by the provided weights. It then computes two scores, `si_plus` (positive criteria) and `si_minus` (negative criteria), which are used to calculate the COPRAS score (`q`). The function selects the best locations based on the highest COPRAS score while ensuring that the cumulative demand ratio for the selected locations meets or exceeds 1 (i.e., satisfies demand).
#'
#' @author
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' @references
#' Gudehus, T., & Kotzab, H. (2012). *Comprehensive Logistics*. Springer Berlin Heidelberg.
#' \doi{10.1007/978-3-642-24367-7}
#'
#' Zavadskas, E. K., Kaklauskas, A., & Šarka, V. (1994). The new method of multicriteria complex proportional assessment projects.
#' In E. K. Zavadskas & P. Linnert (Eds.), *Technological and economic development of economy. Volume 3. Business Management* (pp. 131–140). Vilnius: „Technika”.
#'
#' @examples
#' # Assuming `goods_and_locations` is a data frame with appropriate columns
#' scenario2 <- generate_sample_goods_and_locatons_scenario(warehouse_height = 10,
#' warehouse_width = 10,nr_goods = 5)
#' copras <- copras_assignment(scenario2)
#'
#' @importFrom clusterSim data.Normalization
#' @export
copras_assignment <- function(goods_and_locations, weights = c(rep(0.20, 5))) {
  results = NULL

  for (k in unique(goods_and_locations$product)) {
    glp = goods_and_locations %>% filter(.data$product == k)
    z1 = suppressWarnings(data.Normalization(glp$storage_time, "n11"))
    t1 = weights[1] * z1
    z2 = suppressWarnings(data.Normalization(glp$distance, "n11"))
    t2 = weights[2] * z2
    z3 = suppressWarnings(data.Normalization(glp$demand_ratio, "n11"))
    t3 = weights[3] * z3
    if (nrow(glp) == 1 ||
        sum(glp$full_packages_demand_ratio) == 0) {
      z4 = glp$full_packages_demand_ratio
    } else{
      z4 = suppressWarnings(data.Normalization(glp$full_packages_demand_ratio, "n11"))
    }
    t4 = weights[4] * z4
    if (nrow(glp) == 1 || sum(glp$neighbors) == 0) {
      z5 = glp$neighbors
    } else{
      z5 = suppressWarnings(data.Normalization(glp$neighbors, "n11"))
    }
    t5 = weights[5] * z5
    si_plus = t1 + t3 + t4 + t5
    si_minus = t2
    q = si_plus + sum(si_minus) / (si_minus * sum(1 / si_minus))
    normalized_and_weighted = data.frame(
      product = glp$product,
      location = glp$location,
      demand_ratio = glp$demand_ratio,
      z1,
      t1,
      z2,
      t2,
      z3,
      t3,
      z4,
      t4,
      z5,
      t5,
      si_plus,
      si_minus,
      q,
      rank = rank(-q),row.names = 1:nrow(glp)
    )
    n_and_w = normalized_and_weighted %>% arrange(rank)
    for (i in 1:nrow(n_and_w)) {
      t = n_and_w[1:i, ]
      if (sum(t$demand_ratio) >= 1) {
        break
      }
    }
    results[[k]] = list(
      normalized_and_weighted = normalized_and_weighted,
      copras = t %>% dplyr::select(.data$product, .data$location, .data$rank, .data$demand_ratio)
    )
  }
  return(results)
}
