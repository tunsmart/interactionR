preventive <- function(OR10, OR01) {
  return((OR10 < 1) || (OR01 < 1))
}

invalid <- function(model) {
  cls <- c("glm", "svyglm", "coxph", "clogit")
  return(class(model)[1] %in% cls)
}

check_arguments <- function(model, exposure_names) {
  if (!invalid(model)) {
    stop("The 'model' argument must be a regression model object fit with glm(), coxph() or clogit()")
  } else if (class(exposure_names) != "character" || length(exposure_names) != 2) {
    stop("Argument 'exposure_names' requires a character vector of the names of the two exposure variables")
  } else if (anyNA(match(exposure_names, names(coef(model))))) {
    stop("At least one of the exposure names you have identified cannot be found in your model")
  }
}

extract_pvals <- function(model) {
  if ("glm" %in% class(model)) {
   return(summary(model)$coefficients[,4])
  } else {
   return(summary(model)$coefficients[,5])
  }
}

# for bstrap
trio <- function(obj) {
  exp1 <- names(coef(obj))[2]
  exp2 <- names(coef(obj))[3]
  inta <- paste(exp1, exp2, sep = ":")

  b1 <- coef(obj)[exp1]
  b2 <- coef(obj)[exp2]
  b3 <- coef(obj)[inta]

  OR10 <- exp(b1)
  OR01 <- exp(b2)
  OR11 <- exp(b1 + b2 + b3)
  out <- c(RERI = OR11 - OR10 - OR01 + 1, AP = (OR11 - OR10 - OR01 + 1) / OR11, SI = (OR11 -
    1) / (OR10 + OR01 - 2))
  return(out)
}
