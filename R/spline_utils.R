ExtractSplineTerm <- function (termsObject, SplineTerm) {
  ## term labels
  tl <- attr(termsObject, "term.labels")  ## tl <- labels(termsObject)
  ## is SplineTerm found in termsObject?
  if (!(SplineTerm %in% tl)) {
    ## header
    h <- "\n  Required SplineTerm not found! Available terms are:\n"
    ## ordered list
    ol <- sprintf("    * %s\n", tl)
    ## stop
    stop(h, ol, call. = FALSE)
  }
  ## is SplineTerm a bs() or ns()?
  is_bs <- grepl("bs(", SplineTerm, fixed = TRUE)
  is_ns <- grepl("ns(", SplineTerm, fixed = TRUE)
  if (!(is_bs || is_ns))
    mystop("Provided SplineTerm is neither 'bs()' nor 'ns()' from package 'splines'!")
  ## position of SplineTerm in termsObject
  pos <- match(SplineTerm, tl)
  ## extract predict call
  predvars <- attr(termsObject, "predvars")  ## try: as.list(predvars)
  SplineCall <- predvars[[2L + pos]]  ## try: as.list(SplineCall)
  ## change variable name in SplineCall to x
  SplineCall[[2]] <- quote(x)
  ## extract degree and knots from SplineCall
  if (is_bs) {
    degree <- SplineCall[[3]]
    interior_knots <- unname(SplineCall[[4]])
    boundary_knots <- SplineCall[[5]]
    SplineCall[[4]] <- interior_knots
  } else {
    degree <- 3L
    interior_knots <- unname(SplineCall[[3]])
    boundary_knots <- SplineCall[[4]]
    SplineCall[[3]] <- interior_knots
  }
  x <- c(boundary_knots[1], interior_knots, boundary_knots[2])
  ## return
  list(pos = pos, degree = degree, knots = x, call = SplineCall)
}


RegSplineAsPiecePoly <- function (RegModel, SplineTerm, shift = TRUE) {
  warning("HORRIBLE HACK!!! Will only work for one specific model!!!!")

  ## input validation
  if (!inherits(RegModel, c("lm", "glm", "lme")))
    mystop("This function only works with models that inherit 'lm', 'glm' or 'lme' class!")

  ## extract "terms" on the RHS of the model formula
  RegSpline <- ExtractSplineTerm(terms(RegModel), SplineTerm)
  pos <- RegSpline$pos

  ## regression coefficients associated with SplineTerm
  if (inherits(RegModel, c("lm", "glm"))) {
    RegSplineCoef <- with(RegModel, coefficients[c(FALSE, TRUE, TRUE)]) # HORRIBLE HACK!!!!!!
  } else {
    ind <- attr(RegModel$fixDF, "assign")[[SplineTerm]]
    RegSplineCoef <- RegModel$coefficients$fixed[ind]  ## fixed.effects(RegModel)
  }
  RegSplineCoef <- unname(RegSplineCoef)

  ## is there `NA` in coefficients?
  na <- is.na(RegSplineCoef)
  if (any(na)) {
    warning("NA coefficients found for SplineTerm; Replacing NA by 0")
    RegSplineCoef[na] <- 0
  }

  ## it is not possible to use CubicInterpSplineAsPiecePoly for reparametrization
  ## it only supports ns() term but not bs() term
  PiecePolyCoef <- PiecePolyRepara(RegSpline, RegSplineCoef, shift)

  ## return coefficient matrix, shift, knots, SplineCall and RegSplineCoef
  structure(list(PiecePoly = list(coef = PiecePolyCoef, shift = shift),
                 knots = RegSpline$knots), class = c("PiecePoly", "RegSpline"))
}


PiecePolyRepara <- function (SplineTerm, SplineCoef, shift = TRUE) {
  x <- SplineTerm$knots
  degree <- SplineTerm$degree
  SplineCall <- SplineTerm$call
  n_pieces <- length(x) - 1L
  ## initialize piecewise polynomial coefficients (matrix)
  PiecePolyCoef <- matrix(0, degree + 1L, n_pieces)
  ## loop through pieces
  i <- 1L
  while (i <= n_pieces) {
    ## an evenly-spaced grid between x[i] and x[i + 1]
    xg <- seq.int(x[i], x[i + 1L], length.out = degree + 1L)
    ## spline values on the grid
    yg <- c(eval(SplineCall, data.frame(x = xg)) %*% SplineCoef)
    ## basis matrix on the grid
    Xg <- outer(xg - shift * x[i], 0:degree, "^")
    ## estimate linear regression yg ~ Xg + 0 by solving normal equation
    A <- base::crossprod(Xg)
    b <- base::crossprod(Xg, yg)
    U <- chol.default(A)
    PiecePolyCoef[, i] <- base::backsolve(U, base::forwardsolve(t.default(U), b))
    ## next piece
    i <- i + 1L
  }
  ## return
  PiecePolyCoef
}
