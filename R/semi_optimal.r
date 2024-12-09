#' Semi-Optimal Heuristic for the Picker’s Route Designation
#'
#' This function applies a semi-optimal heuristic for warehouse routing problems by optimizing aisle configurations and creating a feasible tour graph.
#'
#' @param coordinates A data frame or matrix with columns `x` and `y` representing the coordinates of warehouse locations.
#' @param arcs A matrix representing the initial arcs in the warehouse graph. If `NULL`, the arcs will be generated using the `create_arcs` function.
#'
#' @details
#' The heuristic optimizes aisle usage by identifying empty aisles and minimizing the loss associated with routing through the warehouse. It performs the following steps:
#' \itemize{
#'   \item Calculates the warehouse height and width based on coordinates.
#'   \item Identifies and isolates empty aisles.
#'   \item Evaluates configurations using a binary vector approach and selects the configuration with the minimum routing loss.
#'   \item Adjusts arcs to create a semi-optimal tour graph.
#' }
#'
#' The algorithm returns an optimized graph that balances routing efficiency and warehouse constraints.
#' @author
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' @references
#' Dmytrów, K. (2022). Analytical and simulation determination of order picking time in a low storage warehouse for shared storage systems. Operations Research and Decisions, 32(2), 34–51. \doi{10.37190/ord220203}
#'
#' Le-Duc, T. (2005). Design and Control of Efficient Order. Erasmus Research Institute of Management (ERIM).
#'
#' Tarczyński, G. (2012). Analysis of the Impact of Storage Parameters and the Size of Orders on the Choice of the Method for Routing Order Picking. Operations Research and Decisions, 22(4), 105–120. \doi{10.5277/ord120406}
#'
#' @return A matrix representing the optimized warehouse graph, including the routing paths and IO points.
#'
#'
#' @examples
#' coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
#'                        0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
#'                        11), ncol = 2, byrow = FALSE,
#'                        dimnames = list(NULL, c("x", "y")))
#' semi_optimal_heuristic(coordinates)
#'
#'
#' @seealso \code{\link{midpoint_heuristic}}, \code{\link{return_heuristic}}, \code{\link{sshape_heuristic}}
#'
#' @export
semi_optimal_heuristic <- function(coordinates, arcs = NULL) {
  if (is.null(arcs)) {
    arcs <- create_arcs(coordinates)
  }
  warehouse_height = max(coordinates[, "y"]) - 1

  oa <- optimize_aisles(arcs)
  # print(oa$gain)
  # arcs_changed <- oa$arcs_changed
  arcs_isolated <- oa$arcs_isolated
  # gain <- oa$gain
  loss <- oa$loss
  changed <- oa$changed
  empty_aisles <- oa$empty_aisles
  min_change <- Inf
  min_change_bits <- NULL
  warehouse_width_reduced = find_reduced_with(arcs)
  # counting empty aisle to last not empty aisle
  warehouse_width_actual = warehouse_width_reduced
  pstart <- Sys.time()
  if (!is.null(empty_aisles)) {
    changed <- changed[-(empty_aisles)]
  }
  empty_aisles =  empty_aisles[empty_aisles < warehouse_width_reduced]
  if (!is.null(empty_aisles)) {
    mapping_not_empty <- (1:warehouse_width_reduced)[-empty_aisles]
    warehouse_width_reduced = warehouse_width_reduced - length(empty_aisles)
  } else{
    mapping_not_empty <- 1:warehouse_width_reduced
  }
  for (i in 0:(2 ** (warehouse_width_reduced - 1) - 1)) {
    # if (i %% 5000 == 0) {
    #   print(paste0(i, "   ", Sys.time() - pstart))
    # }
    binary_vector_raw <- intToBits(i)
    bits <-
      as.integer(binary_vector_raw)[1:(warehouse_width_reduced - 2)]
    if (parity_full_s(count_by_sequence(c(TRUE, bits, TRUE)))) {
      if (sum(loss[!bits]) < min_change) {
        # print(count_by_sequence(bits))
        min_change_bits <- bits
        min_change <- sum(loss[!bits])
      }
    }
  }
  # cat("min_loss ", min_change, "   ")
  if (is.null(min_change_bits)) {
    min_change_bits <- rep(TRUE, warehouse_width_reduced - 2)
  }
  full_aisles = c(TRUE, min_change_bits, TRUE)
  changed_for_optimal <- c(FALSE, !min_change_bits, FALSE)
  arcs_optimal <- arcs
  for (i in 1:warehouse_width_reduced) {
    if (changed_for_optimal[i]) {
      arcs_optimal %>% as.data.frame() %>% dplyr::filter(.data$x_from != mapping_not_empty[i] |
                                                    .data$x_to != mapping_not_empty[i]) %>% as.matrix() -> arcs_optimal
      # Midpoint in place of full aisle
      new_edges <- edges_from_aisle(arcs_isolated, mapping_not_empty[i])
      # way to and return thus we need to rbind twice
      arcs_optimal %>% rbind(new_edges) %>% rbind(new_edges) %>%  as.matrix() -> arcs_optimal
    }
  }
  for (j in (1:warehouse_width_actual)[-mapping_not_empty]) {
    arcs_optimal %>% as.data.frame() %>% dplyr::filter(.data$x_from != j |
                                                  .data$x_to != j) %>% as.matrix() -> arcs_optimal
  }
  arcs_optimal = create_tour_graph(
    arcs_optimal,
    warehouse_width_reduced,
    warehouse_width_actual,
    warehouse_height,
    full_aisles,
    mapping_not_empty
  )
  return(add_io_points(
    delete_empty_final_aisles(arcs_optimal, warehouse_width_actual)
  ))
}
