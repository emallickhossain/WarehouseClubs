# Custom mlogit functions
library(Rcpp)
library(data.table)

#' Custom mlogit.data function
#'
#' @param data data.table with column names choice, alt, chid, and id (optional)
#' @param choice
#' @param shape
#' @param alt.var
#' @param chid.var
#' @param id.var
#'
#' @return
#' @export
#'
#' @examples
mlogit.dataMal <- function(data, choice = "choice", shape = "long", alt.var = "alt",
                           chid.var = "chid", id.var = NULL){

  # Checking column names
  if (!choice %in% names(data)) stop("Change name of choice col to 'choice'")
  if (!alt.var %in% names(data)) stop("Change name of alt col to 'alt'")
  if (!chid.var %in% names(data)) stop("Change name of choice id col to 'chid'")

  if (is.null(id.var)) {
    setnames(data, c(chid.var, alt.var, choice), c("chid", "alt", "choice"))
  } else {
    if (!id.var %in% names(data)) stop("Change name of id to 'id'")
    setnames(data, c(id.var, chid.var, alt.var, choice), c("id", "chid", "alt", "choice"))
  }

  if (shape == "long") {
    # Creating new id column that is id and chid combined (will separate out later)
    if (is.null(id.var)) {
      data[, ':=' (id_chid = paste(NA, chid, sep = "_"),
                   chid    = NULL)]
    } else {
      data[, ':=' (id_chid = paste(id, chid, sep = "_"),
                   chid    = NULL,
                   id      = NULL)]
    }

    # Filling in missing rows
    setkey(data, id_chid, alt)
    data <- data[CJ(id_chid, alt, unique = TRUE)]
    data[is.na(choice), "choice" := 0L]
    data[, c("id", "chid") := tstrsplit(id_chid, "_", fixed = TRUE)]
    data[, ':=' (id_chid = NULL,
                 id      = suppressWarnings(as.integer(id)),
                 chid    = as.integer(chid),
                 alt     = as.factor(alt))]

    if (is.null(id.var)) {
      data[, "id" := NULL]
      setkey(data, chid, alt)
    } else {
      setkey(data, id, chid, alt)
    }
    setcolorder(data, c("id", "chid", "alt"))
  }
  return(data)
}

mlogitMal <- function(formula, data){
  nframe <- length(sys.calls())
  start.time <- proc.time()

  # 1 ######################################################
  # Check what kind of model is estimated
  ##########################################################
  callT$method <- 'nr'

  # 3 ######################################################
  # compute the model.frame (subset of data)
  ##########################################################
  mf <- model.frame(formula, data)

  # 4 ###########################################################
  # get the dimensions of the model
  ###############################################################
  index <- index(mf)
  alt <- data$alt
  chid <- data$chid
  alt.lev <- levels(alt)
  J <- length(alt.lev)
  n <- uniqueN(chid)

  # 5 ###########################################################
  # extract the elements of the model
  ###############################################################
  # extract the individual index if it is relevant
  id <- NULL

  # extract the X matrix for the standard deviation estimation
  X <- model.matrix(formula, mf)
  K <- ncol(X)

  # extract the response
  y <- model.response(mf)
  choice <- na.omit(alt[y])

  # compute the choice frequency table
  freq <- table(alt[y])

  # Xl and yl are lists of length J which contains n matrix / vector
  # of covariates and response (a boolean) ; yv is a vector that
  # contains the chosen alternative
  Xl <- vector(length = J, mode = "list")
  names(Xl) <- levels(alt)
  for (i in levels(alt))  Xl[[i]] <- X[altnoNA == i, , drop = FALSE]
  yl <- split(y, altnoNA)
  yl <- lapply(yl, function(x){x[is.na(x)] <- FALSE ; x})
  attr(yl, "chid") <- as.character(levels(chid))
  attr(yl, "id") <- as.character(levels(id))

  # 6 ######################################################
  # compute the starting values
  ##########################################################
  start <- mlogit.start(formula = formula, data = data, mf = mf)
  names.sup.coef <- attr(start, "names.sup.coef")

  # 7 ###################################################################
  # Estimate the model using mlogit.optim and passing the correct arguments
  #######################################################################
  # construct the call for mlogit.optim
  opt <- callT

  # if constPar is numeric, insert the relevant value in the start
  # vector and transform the constPar vector to character
  if (! is.null(opt$constPar)){
    theconstPar <- eval(opt$constPar)
    if (is.numeric(theconstPar)){
      if (is.null(names(theconstPar)))
        stop('the numeric constPar vector should be named')
      start[names(theconstPar)] <- theconstPar
      opt$constPar <- names(theconstPar)
    }
  }

  # include the automatically computed starting values
  opt$start <- start

  # select the argument of mlogit that should be passed to
  # mlogit.optim
  m <- match(c("method", "print.level", "iterlim", "start", "constPar","tol",
               "ftol", "steptol"), names(opt), 0L)
  opt <- opt[c(1L, m)]
  opt[[1]] <- as.name('mlogit.optim')
  opt$logLik <- as.name('lnl.slogit')
  opposite <- - 1
  opt[c('opposite')] <- list(as.name('opposite'))

  # model specific arguments
  opt[c('X', 'y')] <- list(as.name('Xl'), as.name('yl'))
  x <- eval(opt, sys.frame(which = nframe))

  # 8 ###########################################################
  # put the result in form
  ###############################################################
  # some general features
  n <- sum(freq)
  x$est.stat$elaps.time <- proc.time() - start.time
  logLik <- structure( - as.numeric(x$optimum),
                       df = length(x$coefficients),
                       null = sum(freq * log(freq / n)),
                       class = "logLik"
  )


  # if no hessian is returned, use the BHHH approximation
  if (is.null(attr(x$optimum, 'hessian'))) hessian <- - crossprod(attr(x$optimum, 'gradi'))
  else hessian <- - attr(x$optimum, 'hessian')
  fitted <- attr(x$optimum, "fitted")
  probabilities <- attr(x$optimum, "probabilities")
  linpred <- attr(x$optimum, "linpred")
  resid <- Reduce("cbind", yl) - fitted
  attr(x$coefficients, "fixed") <- attr(x$optimum, "fixed")
  attr(x$coefficients, "sup") <- names.sup.coef
  gradient <- - attr(x$optimum, "gradi")

  # Compute the covariance matrix of the errors
  alt.names <- colnames(probabilities)
  J <- length(alt.names)
  Omega <- matrix(0, J, J, dimnames = list(alt.names, alt.names))
  diag(Omega) <- pi ^ 2 / 6

  mfindex <- index(mf)
  mf$probabilities <- as.numeric(t(probabilities))
  if (! is.null(linpred)) mf$linpred <- as.numeric(t(linpred))
  mf <- structure(mf,
                  class = c("mlogit.data", "data.frame"),
                  index = mfindex)

  result <- structure(
    list(
      coefficients  = x$coefficients,
      logLik        = logLik,
      gradient      = gradient,
      hessian       = hessian,
      est.stat      = x$est.stat,
      fitted.values = fitted,
      probabilities = probabilities,
      linpred       = linpred,
      residuals     = resid,
      omega         = Omega,
      model         = mf,
      freq          = freq,
      formula       = formula,
      call          = callT),
    class = 'mlogit'
  )
  result
}

# mlogit.start compute the starting values, giving the arguments of mlogit
mlogit.start <- function(formula, data, mf, start = NULL){
  nframe <- length(sys.calls())
  callT <- match.call(expand.dots = TRUE)

  # extract the X and eventually Xs matrix
  if (length(formula)[2] == 4){
    sformula <- formula(as.Formula(formula), rhs = 4)
    Xs <- model.matrix(sformula, mf)[! duplicated(index(mf)$chid), - 1]
    formula <- mFormula(formula(as.Formula(formula), rhs = 1:3))
  }
  else Xs <- NULL
  X <- model.matrix(formula, mf)
  alt.lev <- levels(index(mf)$alt)
  J <- length(alt.lev)
  K <- ncol(X)
  colnamesX <- colnames(X)

  # give names to the supplementary coefficients and values if start
  # is null or of length K
  sup.coef <- numeric(0)
  names.sup.coef <- character(0)

  ## start can be :
  ##   1. NULL, in this case estimate the multinomial logit model,
  ##   2. a vector of length K ; then add starting values for the
  ##   supplementary coefficients,
  ##   3. a full set ; then just name the coefs.

  if (is.null(start)){
    callst <- callT
    callst$formula <- formula
    start <- rep(0, K)
    names(start) <- colnamesX
    callst$start <- start
    callst$print.level <- 0
    callst[c("constPar", "iterlim")] <- NULL
    callst$print.level <- 0
    if (length(callst$formula)[2] == 4)
      callst$formula <- mFormula(formula(callst$formula, rhs = 1:3))
    callst[[1]] <- as.name("mlogit")
    start <- coef(eval(callst, parent.frame()))
  }
  if (length(start) == K){
    names(start) <- colnamesX
    names(sup.coef) <- names.sup.coef
    start <- c(start, sup.coef)
  }
  else{
    names(start) <- colnamesX
  }
  structure(start, names.sup.coef = names.sup.coef)
}

suml <- function(x){
  n <- length(x)
  if (!is.null(dim(x[[1]]))){
    d <- dim(x[[1]])
    s <- matrix(0,d[1],d[2])
    for (i in 1:n){
      x[[i]][is.na(x[[i]])] <- 0
      s <- s+x[[i]]
    }
  }
  else{
    s <- rep(0,length(x[[n]]))
    for (i in 1:n){
      x[[i]][is.na(x[[i]])] <- 0
      s <- s+x[[i]]
    }
  }
  s
}

lnl.slogit <- function(param, X, y, gradient = FALSE,
                       hessian = FALSE, opposite, direction = rep(0, length(param)),
                       initial.value = NULL,stptol = 1E-01){
  step <- 2
  repeat{
    step <- step / 2
    if (step < stptol) break
    Xb <- lapply(X, function(x) crossprod(t(x), param + step * direction))
    eXb <- lapply(Xb, exp)
    seXb <- suml(eXb)
    P <- lapply(eXb, function(x){v <- x / seXb; v[is.na(v)] <- 0; as.vector(v)})
    Pch <- Reduce("+", mapply("*", P, y, SIMPLIFY = FALSE))
    names(Pch) <- attr(y, "chid")
    lnl <- sum(opposite * log(Pch))
    if (is.null(initial.value) || lnl <= initial.value) break
  }
  if (gradient | hessian) PX <- suml(mapply("*", X, P, SIMPLIFY = FALSE))
  if (gradient){
    Xch <- suml(mapply("*", X, y, SIMPLIFY = FALSE))
    gradi <-  opposite * (Xch - PX)
    attr(lnl, "gradi") <- gradi
    attr(lnl, "gradient") <- if (is.matrix(gradi)) apply(gradi, 2, sum) else sum(gradi)
  }
  if (hessian){
    XmPX <- lapply(X, function(x){g <- x - PX; g[is.na(g)] <- 0; g})
    hessian <-   - suml( mapply(function(x, y) crossprod(x * y, y),
                                P, XmPX, SIMPLIFY = FALSE))
    attr(lnl, "hessian") <- opposite * hessian
  }
  if (step < stptol) lnl <- NULL
  else{
    Xb <- Reduce("cbind", Xb)
    P <- Reduce("cbind", P)
    dimnames(P) <- dimnames(Xb) <- list(attr(y, "chid"), names(y))
    attr(lnl, "probabilities") <- P
    attr(lnl, "linpred") <- Xb
    attr(lnl, "fitted") <- Pch
    attr(lnl, "step") <- step
  }
  lnl
}
