betareg.test <- function (formula, data, subset, na.action, weights, offset, 
          link = c("logit", "probit", "cloglog", "cauchit", "log", 
                   "loglog"), link.phi = NULL, type = c("ML", "BC", "BR"), 
          control = betareg.control(...), model = TRUE, y = TRUE, x = FALSE, 
          ...) {
  
  
  cl <- match.call()
  if (missing(data)) 
    data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  oformula <- as.formula(formula)
  formula <- as.Formula(formula)
  if (length(formula)[2L] < 2L) {
    formula <- as.Formula(formula(formula), ~1)
    simple_formula <- TRUE
  }
  else {
    if (length(formula)[2L] > 2L) {
      formula <- Formula(formula(formula, rhs = 1:2))
      warning("formula must not have more than two RHS parts")
    }
    simple_formula <- FALSE
  }
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1L)
  mtZ <- delete.response(terms(formula, data = data, rhs = 2L))
  Y <- model.response(mf, "numeric")
  X <- model.matrix(mtX, mf)
  Z <- model.matrix(mtZ, mf)
  if (length(Y) < 1) 
    stop("empty model")
  if (!(min(Y) > 0 & max(Y) < 1)) 
    stop("invalid dependent variable, all observations must be in (0, 1)")
  n <- length(Y)
  type <- match.arg(type)
  if (is.character(link)) 
    link <- match.arg(link)
  if (is.null(link.phi)) 
    link.phi <- if (simple_formula) 
      "identity"
  else "log"
  if (is.character(link.phi)) 
    link.phi <- match.arg(link.phi, c("identity", "log", 
                                      "sqrt"))
  weights <- model.weights(mf)
  if (is.null(weights)) 
    weights <- 1
  if (length(weights) == 1) 
    weights <- rep.int(weights, n)
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)
  expand_offset <- function(offset) {
    if (is.null(offset)) 
      offset <- 0
    if (length(offset) == 1) 
      offset <- rep.int(offset, n)
    as.vector(offset)
  }
  offsetX <- expand_offset(model.offset(model.part(formula, 
                                                   data = mf, rhs = 1L, terms = TRUE)))
  offsetZ <- expand_offset(model.offset(model.part(formula, 
                                                   data = mf, rhs = 2L, terms = TRUE)))
  if (!is.null(cl$offset)) 
    offsetX <- offsetX + expand_offset(mf[, "(offset)"])
  offset <- list(mean = offsetX, precision = offsetZ)
  rval <- betareg.fit(X, Y, Z, weights, offset, link, link.phi, 
                      type, control)
  rval$call <- cl
  rval$formula <- oformula
  rval$terms <- list(mean = mtX, precision = mtZ, full = mt)
  rval$levels <- list(mean = .getXlevels(mtX, mf), precision = .getXlevels(mtZ, 
                                                                           mf), full = .getXlevels(mt, mf))
  rval$contrasts <- list(mean = attr(X, "contrasts"), precision = attr(Z, 
                                                                       "contrasts"))
  if (model) 
    rval$model <- mf
  if (y) 
    rval$y <- Y
  if (x) 
    rval$x <- list(mean = X, precision = Z)
  class(rval) <- "betareg"
  return(rval)
}