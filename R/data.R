get_id = function(data) {
    if(haskey(data)) {
        key(data)[1]
    } else {
        names(data)[1]
    }
}

get_time = function(data) {
    if(haskey(data) && length(key(data)) >= 2) {
        key(data)[2]
    } else {
        names(data)[2]
    }
}

get_value = function(data) {
    if(haskey(data) && length(key(data)) >= 2) {
        setdiff(names(data), key(data))[1]
    } else {
        names(data)[3]
    }
}

get_trend_id = function(trends) {
    names(trends)[1]
}

get_trend_time = function(trends) {
    names(trends)[2]
}

get_trend_value = function(trends) {
    names(trends)[3]
}