# Custom logsum function
logsum <- function (coef, X = NULL, formula = NULL, data = NULL, type = NULL,
                    output = c("chid", "obs")) {
  if (is.numeric(coef))
    beta <- coef
  else {
    if (inherits(coef, "mlogit"))
      beta <- coef(coef)
    else stop("coef should be either a numeric or a mlogit object")
  }
  if (is.null(X) & (is.null(data))) {
    if (inherits(coef, "mlogit")) {
      idx <- index(coef)
      X <- model.matrix(coef)
    }
    else stop("only one argument is provided, it should be a mlogit object")
  }
  else {
    if (!is.null(X)) {
      if (!inherits(X, "matrix") & !inherits(X, "mlogit"))
        stop("X should be either a matrix or a mlogit object")
      if (is.matrix(X)) {
        if (!is.null(attr(X, "index")))
          idx <- attr(X, "index")
        else {
          if (inherits(coef, "mlogit")) {
            idx <- index(coef)
            if (nrow(idx) != nrow(X))
              stop("X has no index and its dimension is uncorrect")
          }
          else stop("no index in for the coef and the X argument")
        }
      }
      if (inherits(X, "mlogit")) {
        idx <- index(X)
        X <- model.matrix(X)
      }
    }
    else {
      if (is.null(data))
        stop("the X or data argument should be provided")
      else {
        if (inherits(data, "mlogit"))
          data <- model.frame(data)
        if (!is.data.frame(data))
          stop("data should be a data.frame")
        if (!inherits(data, "mlogit.data")) {
          if (is.null(attr(coef, "index")))
            stop("no index available to compute the model.matrix")
          else {
            idx <- index(coef)
            if (nrow(idx) != nrow(data))
              stop("uncompatible dimensions")
            else {
              data <- structure(data, index = idx, class = c("mlogit.data",
                                                             "data.frame"))
            }
          }
        }
        else idx <- index(data)
        if (!is.null(formula))
          X <- model.matrix(formula, data)
        else {
          if (inherits(coef, "mlogit")) {
            mf <- update(coef, data = data, estimate = FALSE)
            idx <- index(mf)
            X <- model.matrix(formula(mf), model.frame(mf))
          }
          else stop("no formula provided to compute the model.matrix")
        }
      }
    }
  }
  output <- match.arg(output)
  if (!is.null(type)) {
    if (!type %in% c("group", "global"))
      stop("type should be one of 'group' or 'local'")
  }
  idx$nb <- 1:nrow(idx)
  coefsubset <- intersect(names(beta), colnames(X))
  X <- X[, coefsubset, drop = FALSE]
  idx$linpred <- as.numeric(crossprod(t(X[, coefsubset, drop = FALSE]),
                                      beta[coefsubset]))
  if (!is.null(idx$group) & (is.null(type) || type == "group")) {
    iv <- log(with(idx, tapply(exp(linpred), list(chid,
                                                  group), sum)))
    if (output == "obs") {
      iv <- data.frame(chid = rep(rownames(iv), each = ncol(iv)),
                       group = rep(colnames(iv), nrow(iv)), iv = as.numeric(t(iv)))
      iv <- merge(idx, iv)
      iv <- iv[order(iv$nb), "iv"]
    }
  }
  else {
    iv <- log(with(idx, tapply(exp(linpred), chid, sum, na.rm = TRUE)))
    if (output == "obs") {
      iv <- data.frame(chid = rownames(iv), iv = as.numeric(iv))
      iv <- merge(idx, iv)
      iv <- iv[order(iv$nb), "iv"]
    }
  }
  iv
}
