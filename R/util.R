subset_list = function(list, names) {
    subnames = intersect(names(list), names)
    list[subnames]
}

# tryCatch statement conditional on the first argument
condTryCatch = function(cond, expr, error) {
    if(cond) {
        tryCatch(expr=expr, error=error)
    } else {
        eval(expr)
    }
}

is.wholeNumber = function(x) {
    is.numeric(x) & is.finite(x) & (x %% 1 == 0)
}