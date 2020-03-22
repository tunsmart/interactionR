preventive = function(OR10, OR01) {
    return((OR10 < 1) || (OR01 < 1))
}

invalid = function(model) {
    return(model$family$link != "logit" && class(model) != "coxph" && class(model) != 
        "clogit")
}
