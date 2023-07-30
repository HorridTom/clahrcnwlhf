# Spline helper functions


# This one only works for integer X... use get_spline_eqns below instead for
# arbitrary X
get_basis_functions_for_linear_spline_with_one_knot <- function(spline_basis) {

  knots <- attr(spline_basis, "knots")
  boundary_knots <- attr(spline_basis, "Boundary.knots")

  spline_basis_df <- as_tibble(spline_basis) %>%
    mutate(x = row_number()) %>%
    pivot_longer(cols = -x) %>%
    mutate(value = as.numeric(value))

  basis_fn_names <- spline_basis_df %>%
    distinct(name) %>%
    pull(name)

  spline_basis_list <- list()

  for(basis_fn_name in basis_fn_names) {
    basis_fn_df <- spline_basis_df %>%
      filter(name == basis_fn_name)

    # Segment 1
    x1 <- boundary_knots[1]
    x2 <- knots[1]
    y1 <- basis_fn_df %>%
      filter(x == x1) %>%
      pull(value)
    y2 <- basis_fn_df %>%
      filter(x == x2) %>%
      pull(value)

    seg1_m <- (y2 - y1) / (x2 - x1)
    seg1_c <- (x2*y1 - x1*y2) / (x2 - x1)

    # Segment 2
    x1 <- knots[1]
    x2 <- boundary_knots[2]
    y1 <- basis_fn_df %>%
      filter(x == x1) %>%
      pull(value)
    y2 <- basis_fn_df %>%
      filter(x == x2) %>%
      pull(value)

    seg2_m <- (y2 - y1) / (x2 - x1)
    seg2_c <- (x2*y1 - x1*y2) / (x2 - x1)

    spline_basis_list <- append(spline_basis_list, list(list(m1 = seg1_m,
                                                             c1 = seg1_c,
                                                             m2 = seg2_m,
                                                             c2 = seg2_c)))
    names(spline_basis_list)[length(spline_basis_list)] <- as.character(basis_fn_name)

  }

  return(spline_basis_list)
}


get_basis_functions_linear_one_knot_as_data_frame <- function(x_min,
                                                              x_knot,
                                                              x_max) {
  stopifnot(is.integer(x_min))
  stopifnot(is.integer(x_max))

  spline_basis <- tibble::tibble(x = c(x_min:x_max))

  spline_basis <- spline_basis %>%
    dplyr::mutate(`1` = case_when(x < x_min ~ NA_real_,
                                 x >= x_min & x <= x_knot ~ (1/(x_knot - x_min))*x - (x_min/(x_knot - x_min)),
                                 x > x_knot & x <= x_max ~ -(1/(x_max - x_knot))*x + (x_max/(x_max - x_knot)),
                                 x > x_max ~ NA_real_
                                 ),
                  `2` = case_when(x < x_min ~ NA_real_,
                                 x >= x_min & x <= x_knot ~ 0,
                                 x > x_knot & x <= x_max ~ (1/(x_max - x_knot))*x - (x_knot/(x_max - x_knot))
                                 )
                  )

  return(spline_basis)
}

expand_one_knot_spline_linear_predictor <- function(beta1,
                                                    beta2,
                                                    alpha,
                                                    basis_functions,
                                                    intercept = FALSE) {
  beta_1 = beta1 * basis_functions[["1"]][["m1"]] +
    beta2 * basis_functions[["2"]][["m1"]]

  beta_2 = beta1 * basis_functions[["1"]][["m2"]] +
    beta2 * basis_functions[["2"]][["m2"]]

  regression_coefficients <- list(beta_1 = beta_1, beta_2 = beta_2)

  if(intercept) {
    alpha_1 = alpha +
      beta1 * basis_functions[["1"]][["c1"]] +
      beta2 * basis_functions[["2"]][["c1"]]

    alpha_2 = alpha +
      beta1 * basis_functions[["1"]][["c2"]] +
      beta2 * basis_functions[["2"]][["c2"]]

    regression_coefficients <- c(regression_coefficients,
                                 list(alpha_1 = alpha_1,
                                      alpha_2 = alpha_2))
  }

  return(regression_coefficients)

}

get_linear_combination_for_difference_in_log_rr <- function(spline_basis, x_vals) {

  basis_functions <- get_spline_eqns(spline_basis, x_vals = x_vals)

  k1 = basis_functions[["1"]][["m2"]] - basis_functions[["1"]][["m1"]]
  k2 = basis_functions[["2"]][["m2"]] - basis_functions[["2"]][["m1"]]

  K <- matrix(c(0, k1, k2), nrow = 1L, ncol = 3L)

  return(K)

}



################################################################################

get_mc <- function(x0, y0, x1, y1) {

  m = (y1 - y0) / (x1 - x0)
  c = y1 - m*x1

  return(list(m = m, c = c))

}

get_spline_eqns <- function(spline_basis, x_vals) {

  knot <- attr(spline_basis, "knots")
  boundary_knots <- attr(spline_basis, "Boundary.knots")


  spline_basis_df <- as_tibble(spline_basis) %>%
    distinct() %>%
    mutate(x = row_number()) %>%
    pivot_longer(cols = -x) %>%
    mutate(value = as.numeric(value)) %>%
    left_join(x_vals, by = c("x" = "x"))

  basis_fn_names <- spline_basis_df %>%
    distinct(name) %>%
    pull(name)

  spline_basis_list <- list()

  for(basis_fn_name in basis_fn_names) {
    basis_fn_df <- spline_basis_df %>%
      filter(name == basis_fn_name)

    p01 <- basis_fn_df %>%
      filter(X_t <= knot) %>%
      slice_min(X_t)

    x01 <- p01 %>% pull(X_t)
    y01 <- p01 %>% pull(value)

    p11 <- basis_fn_df %>%
      filter(X_t <= knot) %>%
      slice_max(X_t)

    x11 <- p11 %>% pull(X_t)
    y11 <- p11 %>% pull(value)

    p02 <- basis_fn_df %>%
      filter(X_t > knot) %>%
      slice_min(X_t)

    x02 <- p02 %>% pull(X_t)
    y02 <- p02 %>% pull(value)

    p12 <- basis_fn_df %>%
      filter(X_t > knot) %>%
      slice_max(X_t)

    x12 <- p12 %>% pull(X_t)
    y12 <- p12 %>% pull(value)

    mc1 <- get_mc(x0 = x01, y0 = y01, x1 = x11, y1 = y11)
    m1 <- mc1$m
    c1 <- mc1$c

    mc2 <- get_mc(x0 = x02, y0 = y02, x1 = x12, y1 = y12)
    m2 <- mc2$m
    c2 <- mc2$c

    spline_basis_list <- append(spline_basis_list, list(list(m1 = m1,
                                                             c1 = c1,
                                                             m2 = m2,
                                                             c2 = c2)))
    names(spline_basis_list)[length(spline_basis_list)] <- as.character(basis_fn_name)

  }



  return(spline_basis_list)

}



