# nocov start

.tidy2matrix <- function(data, formula, value.var, fill = NULL, ...) {
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])
    data <- as.data.table(data)
    data[, row__ := .GRP, by = c(row.vars)]
    data[, col__ := .GRP, by = c(col.vars)]
    if (is.null(fill)){
        fill <- 0
        rowdims <- data[col__ == 1, (row.vars), with = FALSE]
        coldims <- data[row__ == 1, (col.vars), with = FALSE]
    } else {
        rowdims <- unique(data[, (row.vars), with = FALSE])
        coldims <- unique(data[, (col.vars), with = FALSE])
    }

    data.m <- matrix(fill[1], nrow = max(data[["row__"]]),
                     ncol = max(data[["col__"]]))
    data.m[cbind(data[["row__"]], data[["col__"]])] <- data[[value.var]]

    return(list(matrix = data.m,
                coldims = coldims,
                rowdims = rowdims))
}

