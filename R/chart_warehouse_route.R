#' Plot Warehouse Route with Coordinates and Arcs usually returned by heuristic
#'
#' @description This function visualizes a warehouse layout and the corresponding route with arcs representing connections between different coordinates (locations). It allows customization of point markers, line colors, line widths, and text labels.
#'
#' @param coordinates A matrix or data frame representing the coordinates (x, y) of the warehouse locations.
#' @param arcs A matrix or data frame representing the arcs (connections) between locations. Each row should contain four columns: x_from, y_from, x_to, and y_to, with an optional fifth column for arc labels.
#' @param pch The plotting character to use for the points in the plot (default is 18).
#' @param col_warehouse_lines The color of the warehouse lines (default is "gray").
#' @param lwd_warehouse_lines The line width for the warehouse lines (default is 1).
#' @param col_arcs_lines The color of the arcs that represent duplicate routes (default is "darkblue").
#' @param lwd_arcs_lines The line width for the arcs representing duplicate routes (default is 2.5).
#' @param text_cex Numeric value indicating the size of the text labels for arcs (default is 0.7).
#' @param text_col The color of the text labels for arcs (default is "darkorange").
#' @param ... Additional graphical parameters passed to the underlying plot function.
#'
#' @return The function produces a plot of the warehouse layout with arcs representing connections between locations. It does not return any value.
#'
#' @details
#' The function visualizes a route through a warehouse using arcs that connect different coordinates. It highlights duplicate arcs using different line colors and line widths. The function also supports text labels for arcs, with customizable text size and color. The user can pass additional graphical parameters through the `...` argument, which are forwarded to the `plot` function.
#'
#'#'
#' @author
#' Krzysztof Dmytrów \email{krzysztof.dmytrow@usz.edu.pl} [aut] \href{https://orcid.org/0000-0001-7657-6063}{ORCID: 0000-0001-7657-6063}
#'
#' Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} [aut, cre] \href{https://orcid.org/0000-0002-4943-8703}{ORCID: 0000-0002-4943-8703}
#'
#' @references
#' Le-Duc, T. (2005). *Design and Control of Efficient Order*. Erasmus Research Institute of Management (ERIM).
#'
#' Ratliff, H. D., & Rosenthal, A. S. (1983). Order-Picking in a Rectangular Warehouse: A Solvable Case of the Traveling Salesman Problem.
#' *Operations Research, 31*(3), 507–521. \doi{10.1287/opre.31.3.507}
#'
#' @examples
#' # Example usage
#' coordinates <- matrix(c(1, 1, 2, 2, 3, 3), ncol = 2)
#' arcs <- matrix(c(1, 1, 2, 2, 1, 2, 2, 3, 3, 2), ncol = 5, byrow = TRUE)
#' chart_warehouse_route(coordinates, arcs, pch = 19, col_warehouse_lines = "red",
#' lwd_warehouse_lines = 2)
#'
#' @export
chart_warehouse_route <- function(coordinates, arcs,pch = 18,col_warehouse_lines= "gray",lwd_warehouse_lines=1,
                                  col_arcs_lines= "darkblue",lwd_arcs_lines=2.5,text_cex=0.7,text_col="darkorange", ...) {
  non_unique_rows <- arcs %>%
    as.data.frame() %>%
    count(across(everything())) %>%
    filter(n > 1)
  plot(coordinates, pch=pch,...)
  lapply(1:nrow(arcs), function(v) {
    lines(arcs[v, c(1, 3)], arcs[v, c(2, 4)], col=col_warehouse_lines,lwd=lwd_warehouse_lines )
  })
  if (nrow(non_unique_rows) != 0) {
    lapply(1:nrow(non_unique_rows), function(v) {
      lines(non_unique_rows[v, c(1, 3)],
            non_unique_rows[v, c(2, 4)],
            col = col_arcs_lines,
            lwd = lwd_arcs_lines)
      lines(
        non_unique_rows[v, c(1, 3)],
        non_unique_rows[v, c(2, 4)] - 0.2,
        col = col_arcs_lines,
        lwd = lwd_arcs_lines
      )
    })
  }
  text((arcs[, 1] + arcs[, 3]) / 2,
       (arcs[, 2] + arcs[, 4]) / 2,
       arcs[, 5],
       cex = text_cex,
       col = text_col
  )
}
