#' Confidence intervals for interaction measures using bootstrapping as described by Assmann et al (1996).
#'
#' @param  model A fitted model object of class glm. Requires that the two binary exposure variables are listed first in the call formula.
#'
#' @param  ci.level Magnitude of the returned CI level. Default is 0.95
#'
#' @param  em   TRUE (the default), for effect modification assessment. FALSE, for interaction.
#'
#' @param recode If TRUE, recodes the exposures - if at least one of the exposures is protective - such that the stratum with the lowest risk becomes the new reference category when the two exposures are considered jointly.
#'
#' @param seed The random seed to use for generating the bootstrap samples for confidence intervals (for reproducibility). Default is 12345, but can be set to any number.
#'
#' @param  s   Number of bootstrap resampling. Default is 1000
#'
#' @return  a list object of class 'interactionR' that includes a dataframe containing all effect estimates necessary for full reporting of effect modification or interaction analysis. @seealso \code{\link{interactionR_table}} for how to generate a publication-ready table from this object.
#'
#' @examples
#' ## Model fitting using dataset from assmann et al.
#' ## The data is available in the package.
#' m <- glm(h ~ ns * smk,
#'   family = binomial(link = "logit"),
#'   data = HDiscdata
#' )
#' \donttest{
#' interactionR_boot(m,
#'   ci.level = 0.95, em = FALSE, recode = FALSE,
#'   seed = 12345, s = 1000
#' )
#' }
#' @references
#' Assmann SF, Hosmer DW, Lemeshow S, Mundt KA. Confidence intervals for measures of interaction. Epidemiology 1996:286-90.
#'
#' @export
#' @importFrom car Boot Confint
interactionR_boot <- function(model, ci.level = 0.95, em = T, recode = F, seed = 12345, s = 1000) {
  if (class(model)[1] != "glm") {
    stop("The 'model' argument must be a regression model object fit with glm()")
  }

  # Estimates the critical value from the supplied CI.level for
  # subsequent CI estimations
  alpha <- 1 - ci.level
  z <- qnorm(1 - alpha / 2)

  beta1 <- names(coef(model))[2]
  beta2 <- names(coef(model))[3]
  beta3 <- paste(beta1, beta2, sep = ":")
  varNames <- c(beta1, beta2, beta3)

  # estimating coefficients to check for any preventive exposures
  b1 <- coef(model)[beta1]
  b2 <- coef(model)[beta2]
  b3 <- coef(model)[beta3]

  #### Recode section code is adapted from Marthur and Vanderweele 2018 (doi: 10.1097/EDE.0000000000000752) ####

  # check if any exposure is preventive
  if (preventive(OR10 = exp(b1), OR01 = exp(b2))) {
    if (!recode) {
      stop("Error: At least one exposure is preventive. Set argument recode=TRUE for the exposures to be automatically recoded. see Knol et al. (2011) European Journal of Epidemiology, 26(6), 433-438")
    }
    if (recode) {
      temp <- data.frame(cat = c("OR10", "OR01", "OR11"), value = c(
        exp(b1),
        exp(b2), exp(b1 + b2 + b3)
      ))
      ref.cat <- temp$cat[which.min(temp$value)]

      E1.ref <- substr(ref.cat, 3, 3)
      E2.ref <- substr(ref.cat, 4, 4)

      # extract the raw data that was used to fit the model
      if (class(model$data) == "environment") {
        stop("Error: Pass the raw data that you used to fit the model to the 'data' argument of your glm(), clogit(), or coxph() call")
      }
      dat.ir <- model$data

      # recode based on new reference category
      dat.ir[[beta1]] <- ifelse(dat.ir[[beta1]] == E1.ref, 0, 1)
      dat.ir[[beta2]] <- ifelse(dat.ir[[beta2]] == E2.ref, 0, 1)

      # inform the user
      warning(
        "Recoding exposures; new reference category for ",
        beta1, " is ", E1.ref, " and for ", beta2, " is ", E2.ref
      )

      # refit model with user's original call
      model <- update(model, . ~ ., data = dat.ir)
      # get new coefficients and ORs
      b1 <- coef(model)[beta1]
      b2 <- coef(model)[beta2]
      b3 <- coef(model)[beta3]
    }
  }

  #### End of recode section ####

  classes <- c("clogit", "coxph")
  mod_class <- class(model)[1]

  if (mod_class %in% classes) {
    se_vec <- summary(model)$coefficients[, 3]
  } else {
    se_vec <- summary(model)$coefficients[, 2]
  } # extracts the SE vector for the coefficients from the model

  v1 <- se_vec[beta1]^2
  v2 <- se_vec[beta2]^2
  v3 <- se_vec[beta3]^2

  ### Extracts the variance-covariance matrix from the model### for use in
  ### the delta method CI estimation for RERI and AP###
  v_cov <- vcov(model)
  v_cov1 <- v_cov[varNames, varNames] # for deltamethod
  cov12 <- v_cov[beta1, beta2]
  cov13 <- v_cov[beta1, beta3]
  cov23 <- v_cov[beta2, beta3]
  v123 <- v1 + v2 + v3 + (2 * (cov12 + cov13 + cov23))
  v12 <- v1 + v2 + (2 * (cov12))
  v13 <- v1 + v3 + (2 * (cov13))
  v23 <- v2 + v3 + (2 * (cov23))


  # Estimates individual and joint effects ORs (with CI) from the model
  OR00 <- 1 # reference OR
  OR10 <- as.numeric(exp(b1))
  CI.ll_OR10 <- exp(confint.default(model)[beta1, 1])
  CI.ul_OR10 <- exp(confint.default(model)[beta1, 2]) # This is also OR of X on D (A==0)
  p.OR10 <- pvals[beta1]
  OR01 <- as.numeric(exp(b2))
  CI.ll_OR01 <- exp(confint.default(model)[beta2, 1])
  CI.ul_OR01 <- exp(confint.default(model)[beta2, 2]) # This is also OR of A on D (X==0)
  p.OR01 <- pvals[beta2]
  OR11 <- as.numeric(exp(b1 + b2 + b3))
  CI.ll_OR11 <- exp(b1 + b2 + b3 - z * sqrt(v123))
  CI.ul_OR11 <- exp(b1 + b2 + b3 + z * sqrt(v123))
  q1 <- abs(log(OR11)/sqrt(v123))
  p.OR11 <- exp(-0.717*q1 - 0.416*q1^2) #see BMJ 2011;343:d2304

  ### Estimates the effect (and CI) of A on D (X==1) ###
  OR_X1 <- as.numeric(exp(b2 + b3)) # OR of A on D (X==1)
  CI.ll_OR_X1 <- exp(b2 + b3) * exp(-z * sqrt(v23))
  CI.ul_OR_X1 <- exp(b2 + b3) * exp(z * sqrt(v23))
  q2 <- abs(log(OR_X1)/sqrt(v23))
  p.OR_X1 <- exp(-0.717*q2 - 0.416*q2^2)


  ### Estimates the effect (and CI) of X on D (A==1) ###
  OR_A1 <- as.numeric(exp(b1 + b3)) # OR of X on D (A==1)
  CI.ll_OR_A1 <- exp(b1 + b3 - z * sqrt(v13))
  CI.ul_OR_A1 <- exp(b1 + b3 + z * sqrt(v13))
  q3 <- abs(log(OR_A1)/sqrt(v13))
  p.OR_A1 <- exp(-0.717*q3 - 0.416*q3^2)

  # Effect modification on the multiplicative scale and CI
  OR_M <- as.numeric(exp(b3))
  CI.ll_OR_M <- exp(confint.default(model)[beta3, 1])
  CI.ul_OR_M <- exp(confint.default(model)[beta3, 2])
  p.OR_M <- pvals[beta3]

  set.seed(seed)
  b <- car::Boot(model, f = trio, R = s, method = c("case"))
  bmatrix <- car::Confint(b, level = ci.level, type = "perc")

  # Extracts additive interaction measures and CIs from the bootstrap
  RERI <- bmatrix[1, 1]
  CI.ll_RERI <- bmatrix[1, 2]
  CI.ul_RERI <- bmatrix[1, 3]
  p.RERI <- NA

  AP <- bmatrix[2, 1]
  CI.ll_AP <- bmatrix[2, 2]
  CI.ul_AP <- bmatrix[2, 3]
  p.AP <- NA

  SI <- bmatrix[3, 1]
  CI.ll_SI <- bmatrix[3, 2]
  CI.ul_SI <- bmatrix[3, 3]
  p.SI <- NA


  d <- data.frame(Measures = c(
    "OR00", "OR01", "OR10", "OR11", paste("OR(",
      beta2, " on outcome [", beta1, "==0]",
      sep = ""
    ), paste("OR(",
      beta2, " on outcome [", beta1, "==1]",
      sep = ""
    ), "Multiplicative scale",
    "RERI", "AP"
  ), Estimates = c(
    OR00, OR01, OR10, OR11, OR01, OR_X1,
    OR_M, RERI, AP
  ), CI.ll = c(
    NA, CI.ll_OR01, CI.ll_OR10, CI.ll_OR11,
    CI.ll_OR01, CI.ll_OR_X1, CI.ll_OR_M, CI.ll_RERI, CI.ll_AP
  ), CI.ul = c(
    NA,
    CI.ul_OR01, CI.ul_OR10, CI.ul_OR11, CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR_M,
    CI.ul_RERI, CI.ul_AP
  ), p = c(
    NA, p.OR01, p.OR10, p.OR11, p.OR01, p.OR_X1, p.OR_M, p.RERI, p.AP
  ))
  rownames(d) <- NULL



  if (!em) {
    d <- data.frame(Measures = c(
      "OR00", "OR01", "OR10", "OR11", paste("OR(",
        beta2, " on outcome [", beta1, "==0]",
        sep = ""
      ), paste("OR(",
        beta2, " on outcome [", beta1, "==1]",
        sep = ""
      ), paste("OR(",
        beta1, " on outcome [", beta2, "==0]",
        sep = ""
      ), paste("OR(",
        beta1, " on outcome [", beta2, "==1]",
        sep = ""
      ), "Multiplicative scale",
      "RERI", "AP", "SI"
    ), Estimates = c(
      OR00, OR01, OR10, OR11,
      OR01, OR_X1, OR10, OR_A1, OR_M, RERI, AP, SI
    ), CI.ll = c(
      NA,
      CI.ll_OR01, CI.ll_OR10, CI.ll_OR11, CI.ll_OR01, CI.ll_OR_X1,
      CI.ll_OR10, CI.ll_OR_A1, CI.ll_OR_M, CI.ll_RERI, CI.ll_AP,
      CI.ll_SI
    ), CI.ul = c(
      NA, CI.ul_OR01, CI.ul_OR10, CI.ul_OR11,
      CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR10, CI.ul_OR_A1, CI.ul_OR_M,
      CI.ul_RERI, CI.ul_AP, CI.ul_SI
    ), p = c(
      NA, p.OR01, p.OR10, p.OR11, p.OR01, p.OR_X1, p.OR10, p.OR_A1,
      p.OR_M, p.RERI, p.AP, p.SI
    ))
    rownames(d) <- NULL
  }

  raw_data <- model$data


  if (exists("dat.ir")) {
    raw_data <- dat.ir
  }

  ir <- list(dframe = d, exp_names = c(beta1, beta2), analysis = em, call = model$call, dat = raw_data, bootstrap = b)
  attr(ir, "class") <- "interactionR"


  invisible(ir)
}
