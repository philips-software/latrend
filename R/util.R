subset_list = function(list, names) {
    subnames = intersect(names(list), names)
    list[subnames]
}

is.formula = function(x) {
    inherits(x, 'formula')
}