preventive <- function(OR10, OR01) {
  return((OR10 < 1) || (OR01 < 1))
}

invalid <- function(model) {
  cls <- c("glm", "coxph", "clogit", "lm")
  return(class(model)[1] %in% cls)
}

#for bstrap
trio = function(obj){

  exp1 <- names(coef(obj))[2]
  exp2 <- names(coef(obj))[3]
  inta <- paste(exp1, exp2, sep = ":")

  b1 <- coef(obj)[exp1]
  b2 <- coef(obj)[exp2]
  b3 <- coef(obj)[inta]

  OR10 <- exp(b1)
  OR01 <- exp(b2)
  OR11 <- exp(b1 + b2 + b3)
  out = c(RERI = OR11 - OR10 - OR01 + 1, AP = (OR11 - OR10 - OR01 + 1) / OR11, SI = (OR11 -
                                                                                       1) / (OR10 + OR01 - 2))
  return(out)
}
