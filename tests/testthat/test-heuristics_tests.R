context("warehouseTools")

test_that("midpoint_heuristic works correctly", {
  # Mock input arcs data
  coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
                        0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
                        11), ncol = 2, byrow = FALSE,
                        dimnames = list(NULL, c("x", "y")))

  arcs<-create_arcs(coordinates)

  # Apply midpoint heuristic
  result <- midpoint_heuristic(arcs)

  # Check result is not NULL
  expect_true(!is.null(result))

  # Check that result has fewer rows than twice original (indicating some edges were removed)
  expect_true(nrow(result) <= 2* nrow(arcs))

  #Check that I/O points are added (IO points usually have y_from = 0)
  expect_true(any(result[,"y_from"] == 0))
})

test_that("return_heuristic works correctly", {
  # Mock input arcs data
  coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
                          0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
                          11), ncol = 2, byrow = FALSE,
                        dimnames = list(NULL, c("x", "y")))


  arcs<-create_arcs(coordinates)
  # Apply midpoint heuristic
  result <- return_heuristic(arcs)

  # Apply return heuristic
  result <- return_heuristic(arcs)

  # Check result is not NULL
  expect_true(!is.null(result))

  expect_true(nrow(result) <= 2*  nrow(arcs))

  # Check that I/O points are added
  expect_true(any(result[,"y_from"] == 0))
})

test_that("sshape_heuristic works correctly", {
  # Mock input arcs data
  coordinates <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
                          0, 4, 11, 0, 10, 11, 0, 1, 5, 11, 0, 4, 11, 0, 4,
                          11), ncol = 2, byrow = FALSE,
                        dimnames = list(NULL, c("x", "y")))

  arcs<-create_arcs(coordinates)
  # Apply midpoint heuristic
  result <- sshape_heuristic(arcs)

  # Check result is not NULL
  expect_true(!is.null(result))

  # Check that the result contains more or equal twice  rows than original (S-shape heuristic doesn't usually remove edges)
  expect_true(nrow(result) <=2* nrow(arcs))

  # Check that I/O points are added
  expect_true(any(result[,"y_from"] == 0))
})
