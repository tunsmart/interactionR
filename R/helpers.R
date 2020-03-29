preventive = function(OR10, OR01) {
    return((OR10 < 1) || (OR01 < 1))
}

invalid = function(model) {
    cls = c("glm", "coxph", "clogit", "lm")
    return(class(model)[1] %in% cls)
}

