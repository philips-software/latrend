subset_list = function(list, names) {
    subnames = intersect(names(list), names)
    list[subnames]
}
