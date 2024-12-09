#' Retrieve Edges from an Aisle
#'
#' @description This internal function retrieves the edges (connections) within a specific aisle from a set of arcs.
#' The edges are filtered based on the `x_from` and `x_to` values, and ordered by `y_from`.
#'
#' @param arcs A data frame or matrix containing arc data, where each row represents an edge with `x_from`, `x_to`, `y_from`, and `y_to` values.
#' @param aisle An integer representing the aisle number to filter the edges.
#'
#' @return A data frame of edges that correspond to the specified aisle, ordered by the `y_from` values.
#'
#' @keywords internal
edges_from_aisle <- function(arcs, aisle) {
  return(as.data.frame(arcs) %>% dplyr::filter(.data$x_from == aisle & .data$x_to == aisle) %>% arrange(.data$y_from))
}


#' Add Unique IDs to Arcs
#'
#' @description This internal function adds a unique identifier to each row of an arcs data frame, where each row represents an edge.
#'
#' @param arcs A data frame or matrix containing arc data.
#'
#' @return A data frame with an additional column `id`, representing the unique identifier for each arc.
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
#' @keywords internal
add_id_to_arcs <- function(arcs) {
  tcn <- colnames(arcs)
  arcs <- cbind(arcs, 1:nrow(arcs))
  colnames(arcs) = c(tcn, "id")
  return(arcs)
}


#' Create Arcs from Coordinates
#'
#' This internal function generates a graph representation from a set of warehouse coordinates by creating arcs (edges) between valid points.
#'
#' @param coordinates A matrix or data frame with two columns representing the x and y coordinates of warehouse locations.
#'
#' @details
#' The function identifies valid arcs between coordinate pairs based on specific rules:
#' - Arcs are created only for points that are aligned either horizontally or vertically.
#' - Horizontal arcs are added only if they are on the warehouse boundaries (e.g., ground level or the top level).
#' - Vertical arcs are created if there are no intermediate points between the two coordinates in the same column.
#' - The distance of each arc is calculated based on the Manhattan distance.
#'
#' The output graph is represented as a matrix, where each row corresponds to an arc and contains:
#' - \code{x_from}: x-coordinate of the starting point.
#' - \code{y_from}: y-coordinate of the starting point.
#' - \code{x_to}: x-coordinate of the ending point.
#' - \code{y_to}: y-coordinate of the ending point.
#' - \code{distance}: Length of the arc.
#'
#' @return A matrix with columns \code{x_from}, \code{y_from}, \code{x_to}, \code{y_to}, and \code{distance}, representing the arcs of the graph.
#'
#' @export
create_arcs <- function(coordinates) {
  warehouse_height <- max(coordinates[, 2]) - 1
  arcs <- NULL
  for (i in 1:(nrow(coordinates) - 1)) {
    for (j in (i + 1):nrow(coordinates)) {
      v1 <- coordinates[i, ]
      v2 <- coordinates[j, ]
      x1 <- v1[1]
      y1 <- v1[2]
      x2 <- v2[1]
      y2 <- v2[2]
      if (y1 != y2 && x1 != x2) {
        next
      }
      if (y1 == y2) {
        if (y1 != 0 && y1 != warehouse_height + 1) {
          next
        }
        if (abs(x1 - x2) != 1) {
          next
        }
        v1[1] = min(x1, x2)
        v2[1] = max(x1, x2)
        len = 3
      }
      else{
        ymin = min(y1, y2)
        ymax = max(y1, y2)
        if (coordinates %>% as.data.frame() %>% dplyr::filter(.data$x == x1 &
                                                              .data$y > ymin & .data$y < ymax) %>% nrow() != 0) {
          next
        }
        if (ymin == 0 || ymax == warehouse_height + 1) {
          len = ymax - ymin
        }
        else{
          len = ymax - ymin
        }
        v1[2] = min(y1, y2)
        v2[2] = max(y1, y2)
      }
      arcs <- rbind(arcs, c(v1, v2, len))
    }
  }
  colnames(arcs) = c("x_from", "y_from", "x_to", "y_to", "distance")
  return(arcs)
}


#' Optimize Aisles for Warehouse Layout
#'
#' @description This internal function optimizes the aisles in a warehouse layout by finding the best cut points to reduce travel distances between arcs (edges).
#'
#' @param arcs A data frame or matrix containing arc data, where each row represents an edge with columns such as `x_from`, `x_to`, `y_from`, `y_to`, and `distance`.
#'
#' @return A list containing the following elements:
#' - `changed`: A logical vector indicating which aisles were changed.
#' - `gain`: The gain in optimization for each aisle.
#' - `loss`: The loss for isolated aisles.
#' - `arcs_changed`: The updated arcs without the `id` column.
#' - `arcs_isolated`: The isolated arcs without the `id` column.
#' - `empty_aisles`: A vector of aisles that were found to be empty.
#'
#' @keywords internal
optimize_aisles <- function(arcs) {
  arcs_isolated <- arcs <- add_id_to_arcs(arcs)
  changed <- NULL
  gain <- NULL
  loss <- NULL
  n = max(arcs[, 1])
  empty_aisles <- NULL
  for (aisle in 2:(n - 1)) {
    edges <- edges_from_aisle(arcs, aisle)
    min_dist <- sum(edges$distance)
    min_dist_isolated <- 2 * sum(edges$distance) + 1
    cut_point = -1
    cut_point_isolated = -1
    if (edges %>% nrow() == 1) {
      empty_aisles <- c(empty_aisles, aisle)
    } else {
      for (u in 1:nrow(edges)) {
        cut_u = edges[u, "y_from"]
        cut_next_u = edges[u, "y_to"]
        t = 2 * sum(edges %>% dplyr::filter(.data$y_from < cut_u) %>% dplyr::select(.data$distance)) +
          2 * sum(edges %>% dplyr::filter(.data$y_from >= cut_next_u) %>% dplyr::select(.data$distance))
        if (t < min_dist) {
          cut_point <- cut_u
          cut_next_point <- cut_next_u
          cut_id = edges[u, "id"]
          min_dist <- t
        }
        if (t < min_dist_isolated) {
          cut_point_isolated <- cut_u
          cut_next_point_isolated <- cut_next_u
          cut_id_isolated = edges[u, "id"]
          min_dist_isolated <- t
        }
      }
      if (cut_point != -1) {
        edges %>% dplyr::filter(.data$x_from <= cut_point) %>% apply(1, function(edge) {
          arcs <- rbind(arcs, as.vector(edge))
        })
        edges %>% dplyr::filter(.data$x_from > cut_next_point) %>% apply(1, function(edge) {
          arcs <- rbind(arcs, as.vector(edge))
        })
        arcs <- arcs[arcs[, 6] != cut_id, ]
        changed <- c(changed, TRUE)
        gain <- c(gain, min_dist - sum(edges$distance))
      } else {
        changed <- c(changed, FALSE)
        gain <- c(gain, 0)
      }
      if (cut_point_isolated != -1) {
        edges %>% dplyr::filter(.data$x_from <= cut_point_isolated) %>% apply(1, function(edge) {
          arcs_isolated <- rbind(arcs_isolated, as.vector(edge))
        })
        edges %>% dplyr::filter(.data$x_from > cut_next_point_isolated) %>% apply(1, function(edge) {
          arcs_isolated <- rbind(arcs_isolated, as.vector(edge))
        })
        arcs_isolated <- arcs_isolated[arcs_isolated[, 6] != cut_id_isolated, ]
        loss <- c(loss, min_dist_isolated - sum(edges$distance))
      } else {
        loss <- c(loss, 0)
      }
    }
  }
  return(
    list(
      changed = changed,
      gain = gain,
      loss = loss,
      arcs_changed = arcs[, -6],
      arcs_isolated = arcs_isolated[, -6],
      empty_aisles = empty_aisles
    )
  )
}


#' Count Sequential Occurrences in a Vector
#'
#' @description This internal function counts the number of sequential occurrences of values in a vector and returns the results as a data frame.
#'
#' @param v A vector of values to be analyzed.
#'
#' @return A data frame with two columns: `id` representing the unique values and `count` representing the number of consecutive occurrences of each value.
#'
#' @keywords internal
count_by_sequence <- function(v) {
  current_t = v[1]
  current_count = 0
  result = data.frame(id = 0, count = 0)
  for (t in v) {
    if (t == current_t) {
      current_count <- current_count + 1
    } else {
      result %>% rbind(c(current_t, current_count)) -> result
      current_count <- 1
      current_t <- t
    }
  }
  result %>% rbind(c(current_t, current_count)) -> result
  result <- result[-1, ]
  return(result)
}


#' Check Parity of Sequences
#'
#' @description This internal function checks the parity (even or odd) of sequences of consecutive occurrences in a data frame created by `count_by_sequence`.
#'
#' @param counted A data frame created by `count_by_sequence` with columns `id` and `count`.
#'
#' @return A logical value indicating whether the sequence has valid parity (`TRUE`) or not (`FALSE`).
#'
#' @keywords internal
parity_full_s <- function(counted) {
  if (nrow(counted) == 1 & counted[1, "count"] %% 2 == 0) {
    return (FALSE)
  }
  if (counted[1, "count"] %% 2 != counted[nrow(counted), "count"] %% 2) {
    return(FALSE)
  }
  if (counted[nrow(counted), "count"] %% 2 == 0) {
    return(FALSE)
  }
  return (counted %>% slice(2:(nrow(counted) - 1)) %>% dplyr::filter(.data$id == TRUE & .data$count %% 2 == 1) %>% nrow() == 0)
}

#' Create a Tour Graph for Warehouse Aisles
#'
#' @description This internal function creates a graph representing a tour through warehouse aisles based on a set of arcs and various parameters like warehouse width and height.
#'
#' @param arcs A matrix or data frame representing the arcs (edges) in the warehouse.
#' @param warehouse_width_reduced The reduced width of the warehouse.
#' @param warehouse_width_actual The actual width of the warehouse.
#' @param warehouse_height The height of the warehouse.
#' @param full_aisles A logical vector indicating which aisles are full.
#' @param mapping_not_empty A vector mapping non-empty aisles.
#'
#' @return A matrix of arcs representing the updated tour graph.
#'
#' @keywords internal
create_tour_graph <- function(arcs, warehouse_width_reduced, warehouse_width_actual, warehouse_height, full_aisles, mapping_not_empty) {
  direction = 1
  for (j in 2:(warehouse_width_reduced - 1)) {
    if

    (full_aisles[j] & full_aisles[j + 1]) {
      if (direction == 1) {
        duplicate = warehouse_height + 1
        remove = 0
      } else {
        remove = warehouse_height + 1
        duplicate = 0
      }
      if (direction == 1) {
        for (i in (mapping_not_empty[j]:(mapping_not_empty[j + 1] - 1))) {
          arcs %>% rbind(c(i, duplicate, i + 1, duplicate, 3)) -> arcs
          arcs %>% as.data.frame() %>% dplyr::filter(!(.data$x_from == i & .data$x_to == i + 1 & .data$y_from == remove & .data$y_to == remove)) %>% as.matrix() -> arcs
        }
      }
      direction = abs(direction - 1)
    } else {
      direction = 1
    }
  }
  return(arcs %>% as.matrix())
}

#' Find Reduced Warehouse Width
#'
#' @description This internal function finds the reduced width of the warehouse by analyzing the arcs and detecting the last non-empty aisle.
#'
#' @param arcs A data frame or matrix representing the arcs in the warehouse.
#'
#' @return The reduced warehouse width as an integer.
#'
#' @keywords internal
find_reduced_with <- function(arcs) {
  warehouse_width_reduced =  max(arcs[, "x_to"])
  while (warehouse_width_reduced > 0) {
    if (arcs %>% as.data.frame() %>% dplyr::filter(.data$x_from == warehouse_width_reduced & .data$x_to == warehouse_width_reduced) %>% nrow() == 1) {
      warehouse_width_reduced = warehouse_width_reduced - 1
    } else {
      break
    }
  }
  return(warehouse_width_reduced)
}

#' Delete Empty Final Aisles from Arcs
#'
#' @description This internal function removes arcs corresponding to empty final aisles from the warehouse.
#'
#' @param arcs A data frame or matrix representing the arcs in the warehouse.
#' @param warehouse_width_reduced The reduced width of the warehouse.
#'
#' @return A matrix of arcs with empty final aisles removed.
#'
#' @keywords internal

delete_empty_final_aisles <- function(arcs, warehouse_width_reduced) {
  arcs %>% as.data.frame() %>% dplyr::filter(.data$x_to <= warehouse_width_reduced) %>% as.matrix() -> arcs
  return(arcs)
}
#' Add I/O Points to the Warehouse Arcs
#'
#' @description This internal function adds I/O (input/output) points to the warehouse arcs.
#'
#' @param arcs A data frame or matrix representing the arcs in the warehouse.
#'
#' @return A matrix of arcs with I/O points added.
#'
#' @keywords internal
add_io_points <- function(arcs) {
  for (i in 1:2) {
    arcs %>% rbind(c(0, 0, 1, 0, 1)) -> arcs
  }
  return(arcs)
}
