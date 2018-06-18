subset_list = function(list, names) {
    subnames = intersect(names(list), names)
    list[subnames]
}

is.formula = function(x) {
    inherits(x, 'formula')
}

# tryCatch statement conditional on the first argument
condTryCatch = function(cond, expr, error) {
    if(cond) {
        tryCatch(expr=expr, error=error)
    } else {
        eval(expr)
    }
}