## Methos for EOF

#' @export
cut.eof <- function(x, n, ...) {
    var <- attr(x, "suffix")
    value.var <- attr(x, "value.var")
    return(structure(lapply(as.list(x), function(x) {
        x[as.numeric(get(var)) %in% n]
    }),
    class = c("eof", "list"),
    suffix = var,
    value.var = value.var))
}

#' @export
#' @importFrom stats screeplot
#' @import ggplot2
screeplot.eof <- function(x, npcs = "all", type = NULL, main = NULL, ...) {
    var <- attr(x, "suffix")
    r2 <- "r2"
    if (npcs[1] == "all") npcs <- as.numeric(unique(x$sdev[[var]]))
    ggplot(x$sdev[as.numeric(get(var)) %in% npcs], aes_(as.name(var), as.name(r2))) +
        geom_point() +
        geom_line(aes_string(paste0("as.numeric(", var, ")"))) +
        scale_y_continuous(name = expression(R^2))
}

#' @export
#' @importFrom ggplot2 autoplot
#' @import ggplot2
autoplot.eof <- function(object, n = "all", ...) {
    screeplot(object, n)
}

#' @export
#' @importFrom stats predict sd
predict.eof <- function(object, n = NULL, ...) {
    `%>%` <- dplyr::`%>%`
    if (!inherits(object, "eof")) {
        stop("eof must be an EOF object")
    }

    if(!is.null(n)) object <- cut(object, n)

    value.var <- attr(object, "value.var")
    pc <- attr(object, "suffix")

    right.vars <- colnames(object$right)[!(colnames(object$right) %in% c(pc, value.var))]
    right.formula <- as.formula(paste0(pc, " ~ ", paste0(right.vars, collapse = "+")))

    right <- object$right %>%
        .[object$sdev, on = pc] %>%
        .[, (value.var) := get(value.var)*sd] %>%
        .tidy2matrix(right.formula, value.var)

    left.vars <- colnames(object$left)[!(colnames(object$left) %in% c(pc, value.var))]
    left.formula <- as.formula(paste0(pc, " ~ ", paste0(left.vars, collapse = "+")))
    left <- .tidy2matrix(object$left, left.formula, value.var)

    dt <- cbind(.extend.dt(left$coldims, each = nrow(right$coldims)),
                .extend.dt(right$coldims, n = nrow(left$coldims)),
                c(t(right$matrix)%*%left$matrix))
    colnames(dt)[length(colnames(dt))] <- value.var
    return(dt)
}

.extend.dt <- function(dt, n = NULL, each = NULL) {
    if (!is.null(n)) {
        r <- as.data.table(lapply(dt, rep, n = n))
    } else {
        r <- as.data.table(lapply(dt, rep, each = each))
    }
    r
}


#' @export
print.eof <- function(x, ...) {
    cat("left:\n")
    print(x$left)
    cat("\nright:\n")
    print(x$right)
    cat("\nsdev:\n")
    print(x$sdev)
}

# #' @export
# `[.eof` <- function(x, left, right, PC) {
#     if (!missing(PC)) {
#         x <- cut(x, PC)
#     }
#     if (!missing(left)) {
#         left <- substitute(left)
#         x$left <- x$left[eval(left), ]
#     }
#     if (!missing(right)) {
#         right <- substitute(right)
#         x$right <- x$right[eval(right), ]
#     }
#
#     x
# }

#' @method summary eof
#' @export
summary.eof <- function(object, ...) {
    cat("Importance of components:\n")
    pc <- attr(object, "suffix")
    sdev <- object$sdev[, .(PC = get(pc), sd, r2)]
    sdev[, cum.r2 := cumsum(r2)]
    cat("Component", "Explained variance", "Cumulative variance\n")
    p <- lapply(seq_len(nrow(sdev)), function(x) {
        cat(formatC(sdev[x, ]$PC, width = 7), "  ",
            formatC(scales::percent(sdev[x, ]$r2), width = 18),
            formatC(scales::percent(sdev[x, ]$cum.r2), width = 19))
        cat("\n")
    })
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("cum.r2", "row__", "col__", "r2", "."))
}
