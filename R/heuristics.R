#' Midpoint Heuristic for the Picker’s Route Designation
#'
#' @description This heuristic generates a return route solution for the picker’s route designation in a warehouse by removing arcs at the top row of the warehouse and duplicating the remaining arcs to form a round trip.
#'
#' @param arcs A data frame or matrix representing the arcs (edges) in the warehouse. The arcs should have columns `x_from`, `x_to`, `y_from`, and `y_to`, which represent the coordinates of the connections between aisles and shelves.
#'
#' @return A matrix of arcs after applying the midpoint heuristic, with edges that cross the warehouse's middle line excluded and I/O points added.
#'
#' @details The heuristic first determines the warehouse height and the reduced width by examining the arcs. For each aisle, the edges are split and removed if they cross beyond the midpoint of the warehouse's height. Finally, I/O points are added, and any empty final aisles are deleted.
#'
#' @seealso \code{\link{return_heuristic}}, \code{\link{sshape_heuristic}}
#'
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
#' @examples
#'
#' coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
#'                        0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
#'                        11), ncol = 2, byrow = FALSE,
#'                        dimnames = list(NULL, c("x", "y")))
#' midpoint_heuristic(create_arcs(coordinates))
#'
#' @export
midpoint_heuristic <- function(arcs) {
  warehouse_height = max(arcs[, "y_to"]) - 1
  warehouse_width_reduced = find_reduced_with(arcs)
  for (aisle in 2:(warehouse_width_reduced - 1)) {
    edges <- edges_from_aisle(arcs, aisle)
    arcs <- arcs %>%  rbind(edges)
    arcs %>% as.data.frame() %>% dplyr::filter(
      .data$x_from !=  aisle | .data$x_to != aisle |
        .data$y_from > ceiling(warehouse_height / 2)  |
        .data$y_to <= ceiling(warehouse_height / 2)
    ) -> arcs
  }
  return(add_io_points(delete_empty_final_aisles(arcs, warehouse_width_reduced)))
}



#' Return Heuristic for the Picker’s Route Designation
#'
#' @description This heuristic generates a return route solution for the Traveling Salesman Problem (TSP) in a warehouse by removing arcs at the top row of the warehouse and duplicating the remaining arcs to form a round trip.
#'
#' @param arcs A data frame or matrix representing the arcs (edges) in the warehouse.
#'
#' @return A matrix of arcs after applying the return heuristic, where edges at the top row are removed and remaining arcs are duplicated to form a return route. I/O points are added at the end.
#'
#' @details The heuristic first removes the arcs corresponding to the top row of the warehouse. Then, the remaining arcs are duplicated to simulate a return trip, and I/O points are added.
#'
#' @seealso \code{\link{midpoint_heuristic}}, \code{\link{sshape_heuristic}}
#'
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
#' @examples
#' coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
#'                        0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
#'                        11), ncol = 2, byrow = FALSE,
#'                        dimnames = list(NULL, c("x", "y")))
#' return_heuristic(create_arcs(coordinates))
#'
#'
#' @export
return_heuristic <- function(arcs) {
  warehouse_height = max(arcs[, "y_to"]) - 1
  warehouse_width_reduced = find_reduced_with(arcs)
  arcs %>%  as.data.frame() %>% dplyr::filter(.data$y_to != warehouse_height + 1) -> arcs
  arcs %>%  rbind(arcs) -> arcs
  return(add_io_points(delete_empty_final_aisles(arcs, warehouse_width_reduced)))
}


#' S-Shape Heuristic for the Picker’s Route Designation
#'
#' @description This heuristic generates an S-shape traversal solution for the picker’s route designation in a warehouse by alternating traversal directions across aisles and ensuring no crossovers between top and bottom rows.
#'
#' @param arcs A data frame or matrix representing the arcs (edges) in the warehouse.
#'
#' @return A matrix of arcs after applying the S-shape heuristic, where traversal alternates between aisles, and I/O points are added.
#'
#' @details The S-shape heuristic removes unnecessary arcs and ensures a consistent traversal direction across the aisles. If an odd number of aisles is found, the last aisle is treated separately, and I/O points are added.
#'
#' @seealso \code{\link{midpoint_heuristic}}, \code{\link{return_heuristic}}
#'
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
#' @examples
#' coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
#'                        0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
#'                        11), ncol = 2, byrow = FALSE,
#'                        dimnames = list(NULL, c("x", "y")))
#' sshape_heuristic(create_arcs(coordinates))
#'
#'
#' @export
sshape_heuristic <- function(arcs) {
  warehouse_height = max(arcs[, "y_to"]) - 1
  warehouse_width_reduced = find_reduced_with(arcs)


  arcs %>%  as.data.frame() %>% dplyr::filter(.data$y_to == warehouse_height + 1 &
                                                .data$y_from == 0) %>% nrow() -> empty_nr
  arcs %>%  as.data.frame() %>% dplyr::filter(!(.data$y_to == warehouse_height + 1 &
                                                  .data$y_from == 0)) -> arcs
  if (empty_nr %% 2 == 1) {
    arcs %>%  rbind(edges_from_aisle(arcs, warehouse_width_reduced)) %>%
      dplyr::filter(
        !(
          .data$x_to == warehouse_width_reduced &
            .data$x_from == warehouse_width_reduced &
            .data$y_to == warehouse_height + 1
        )
      ) -> arcs
  }
  dir = 0
  for (i in 1:(warehouse_width_reduced - 1)) {
    if (edges_from_aisle(arcs, i) %>% nrow() != 0) {
      dir = dir + 1
    }
    if (dir %% 2 == 0) {
      arcs %>% dplyr::filter(
        !(
          .data$x_from == i &
            .data$x_to == i + 1 &
            .data$y_from == warehouse_height + 1 & .data$y_to == warehouse_height + 1
        )
      ) -> arcs
      arcs %>% rbind(c(i, 0, i + 1, 0, 3)) -> arcs
    }
  }
  return(add_io_points(delete_empty_final_aisles(arcs, warehouse_width_reduced)))
}


