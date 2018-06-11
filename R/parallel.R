doPar = function(parallel=TRUE) {
    if(parallel) {
        return(`%dopar%`)
    } else {
        return(`%do%`)
    }
}