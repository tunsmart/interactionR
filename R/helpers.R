preventive = function(OR10, OR01) {
    return((OR10 < 1) || (OR01 < 1))
}

invalid = function(model) {
    return(class(model) != "glm" && class(model) != "coxph" && class(model) !=
        "clogit")
}

