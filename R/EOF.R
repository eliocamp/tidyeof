#' Empirical Orthogonal Function
#'
#' Computes Singular Value Decomposition (also known as Principal Components
#' Analysis or Empirical Orthogonal Functions).
#'
#' @param data a data.frame
#' @param formula a formula passed to \code{\link[data.table]{dcast}} to build
#' the matrix that will be used in the SVD decomposition (see Details)
#' @param value.var optional name of the data column (see Details)
#' @param n which singular values to return (if \code{NULL}, returns all)
#' @param B number of bootstrap samples used to estimate confidence intervals.
#' Ignored if <= 1.
#' @param probs the probabilities of the lower and upper values of estiamted
#' confidence intervals. If named, it's names will be used as column names.
#' @param rotate if `TRUE`, scores and loadings will be rotated using [varimax]
#' @param suffix character to name de principal components
#' @param fill value to infill implicit missing values or `NULL` if the
#' data is dense.
#'
#' @return
#' \describe{
#'    \item{left}{data.table with left singular vectors}
#'    \item{right}{data.table with right singular vectors}
#'    \item{sdev}{data.table with singular values, their explained variance,
#'    and, optionally, cuantiles estimated via bootstrap}
#' }
#'
#' @details
#' Singular values can be computed over matrices so \code{formula} denotes how
#' to build a matrix from the data. It is a formula of the form VAR ~ LEFT | RIGHT
#' (see [Formula::Formula]) in which VAR is the variable whose values will
#' populate the matrix, and LEFT represent the variables used to make the rows
#' and RIGHT, the columns of the matrix.
#' Think it like "VAR *as a function* of LEFT *and* RIGHT".
#'
#' Alternatively, if `value.var` is not `NULL`, it's possible to use the
#' (probably) more familiar [data.table::dcast] formula interface. In that case,
#' `data` must be provided.
#'
#' The result of VAR ~ LHS | RHS and VAR ~ RHS | LHS (ie, terms reversed)
#' is the same.
#'
#' The variable combination used in this formula *must* identify
#' an unique value in a cell. For the time being, no error will be raised, but
#' there will be a message from \code{\link[data.table]{dcast}}. If not every
#' combination is present in the data,
#'
#' In the result, the left and right vectors have dimensions of the LEFT and RIGHT
#' part of the `formula`, respectively.
#'
#' It is much faster to compute only some singular vectors, so is advisable not
#' to set n to \code{NULL}. If the irba package is installed, EOF uses
#' [irlba::irlba] instead of [base::svd] since it's much faster.
#'
#' The boostraping procedure follows Fisher et.al. (2016) and returns the
#' standard deviation of each singular value.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(data.table)
#'
#' arrests <- USArrests %>%
#'   mutate(state = rownames(USArrests)) %>%
#'   melt(id.vars = "state", variable.name = "crime", value.name = "rate") %>%
#'   group_by(crime) %>%
#'   mutate(rate_anomaly = (rate - mean(rate))/sd(rate))
#'
#' arrests_eof <- EOF(rate_anomaly ~ crime | state, data = arrests, n = 1:4)
#'
#' summary(arrests_eof)
#'
#' autoplot(arrests_eof)
#'
#' ggplot(arrests_eof$right, aes(PC, state)) +
#'   geom_raster(aes(fill = rate_anomaly))
#'
#' cut(arrests_eof, 1:2) %>%
#'   .$left %>%
#'   dcast(crime ~ PC, value.var = "rate_anomaly") %>%
#'   ggplot(aes(PC1, PC2)) +
#'   geom_text(aes(label = crime))
#'
#' # Alternative interface
#' arrests_eof <- EOF(crime ~ state, value.var = "rate_anomaly", data = arrests, n = 1:4)
#'
#' @references
#' Fisher, A., Caffo, B., Schwartz, B., & Zipunnikov, V. (2016). Fast, Exact Bootstrap Principal Component Analysis for p > 1 million. Journal of the American Statistical Association, 111(514), 846–860. http://doi.org/10.1080/01621459.2015.1062383
#' @export
#' @import data.table
#' @import Formula
#' @import formula.tools
#' @importFrom stats as.formula quantile varimax runif
#' @importFrom scales percent
EOF <- function(formula, value.var = NULL, data = NULL, n = 1, B = 0,
                probs = c(lower = 0.025, mid = 0.5, upper = 0.975),
                rotate = FALSE, suffix = "PC", fill = 0) {
    if (!is.null(fill)) {
        if (!(is.na(fill) | is.null(fill) | is.finite(fill))) {
            stop("fill must be numeric or NULL")
        }
    }
    if (!is.null(value.var)) {
        if (is.null(data)) stop("data must not be NULL if value.var is not NULL")
        data <- copy(data)
        f <- as.character(formula)
        f <- stringr::str_replace(f, "~", "\\|")
        formula <- Formula::as.Formula(paste0(value.var, " ~ ", f))
    }

    f <- as.character(formula)
    f <- stringr::str_split(f,"~", n = 2)[[1]]

    value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

    matrix.vars <- f[stringr::str_detect(f, "\\|")]
    matrix.vars <- stringr::str_split(matrix.vars,"\\|", n = 2)[[1]]

    row.vars <- stringr::str_squish(stringr::str_split(matrix.vars[1], "\\+")[[1]])
    col.vars <- stringr::str_squish(stringr::str_split(matrix.vars[2], "\\+")[[1]])

    if (is.null(data)) {
        formula <- Formula::as.Formula(formula)
        data <- as.data.table(eval(quote(model.frame(formula, data  = data))))
    } else {
        # Check if columns are indata
        all.cols <- c(value.var, row.vars, col.vars)
        missing.cols <- all.cols[!(all.cols %in% colnames(data))]
        if (length(missing.cols) != 0) {
            stop(paste0("Columns not found in data: ", paste0(missing.cols, collapse = ", ")))
        }
        data <- setDT(data)[, (all.cols), with = FALSE]
    }

    setDT(data)
    dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
    dcast.formula <- as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))
    value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

    g <- .tidy2matrix(data, dcast.formula, value.var, fill = fill)

    if (length(g$matrix) < nrow(data)) {
        stop(paste("The formula ", as.character(formula), " does not identify an unique observation for each cell."))
    }

    tall <- dim(g$matrix)[1] > dim(g$matrix)[2]
    v.g  <- norm(abs(g$matrix), type = "F")

    if (is.null(n)) n <- seq_len(min(ncol(g$matrix), nrow(g$matrix)))

    if (requireNamespace("irlba", quietly = TRUE) &
        max(n) < 0.5 *  min(ncol(g$matrix), nrow(g$matrix))) {
        set.seed(42)
        eof <- irlba::irlba(g$matrix, nv = max(n), nu = max(n), rng = runif)
    } else {
        eof <- svd(g$matrix, nu = max(n), nv = max(n))
        eof$d <- eof$d[1:max(n)]
    }
    remove(data)
    gc()
    pcomps <- paste0(suffix, n)
    if (rotate == TRUE & max(n) > 1) {
        # Rotation
        eof$D <- diag(eof$d, ncol = max(n), nrow = max(n))
        loadings <- t(with(eof, D%*%t(v)))
        scores <- eof$u
        R <- varimax(loadings, normalize = FALSE)
        eof$u <- eof$u%*%R$rotmat

        # Recover rotated V and D matrixs
        loadings <- R$loadings
        class(loadings) <- "matrix"
        eof$d <- sqrt(apply(loadings, 2, function(x) sum(x^2)))
        eof$v <- t(diag(1/eof$d)%*%t(loadings))
    }

    # setDF(data)
    right <- cbind(data.table(rep(pcomps, each = nrow(eof$v))), c(eof$v[, n]))
    colnames(right) <- c(suffix, value.var)
    right <- cbind(unique(g$coldims), right)
    right[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]

    left <- cbind(data.table(rep(pcomps, each = nrow(eof$u))), c(eof$u[, n]))
    colnames(left) <- c(suffix, value.var)
    left <- cbind(unique(g$rowdims), left)
    left[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]

    # setDT(data)
    r2 <- eof$d^2/v.g^2
    sdev <- data.table(pcomps, eof$d)
    colnames(sdev) <- c(suffix, "sd")
    sdev[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]
    sdev[, r2 := r2]

    if (B > 1) {
        set.seed(42)
        if (!tall) {
            names(eof) <- c("d", "v", "u", "iter", "mprod")
        }
        loadings <- with(eof, diag(d, ncol = max(n), nrow = max(n))%*%t(v))
        p <- nrow(eof$v)
        sdevs <- lapply(seq_len(B), function(x) {
            Prow <- sample(seq_len(p), replace = TRUE)
            m <- loadings[, Prow]
            eof <- svd(m)
            if (rotate == TRUE) {
                loadings <- t(with(eof, diag(d, ncol = max(n), nrow = max(n))%*%t(v)))
                R <- varimax(loadings, normalize = FALSE)
                loadings <- R$loadings
                class(loadings) <- "matrix"
                return(sqrt(apply(loadings, 2, function(x) sum(x^2))))
            } else {
                return(svd(m)$d)
            }
        })

        se <- lapply(data.table::transpose(sdevs), quantile, probs = probs, names = FALSE)
        se <- data.table::transpose(se)
        if (is.null(names(probs))) names(probs) <- scales::percent(probs)
        sdev[, names(probs) := se]
    }


    return(structure(list(left = left, right = right, sdev = sdev),
                     call = match.call(),
                     class = c("eof", "list"),
                     suffix = suffix,
                     value.var = value.var))
}

