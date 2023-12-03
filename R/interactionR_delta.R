#' Computes confidence intervals for interaction measures using the delta method by Hosmer and Lemeshow
#'
#' @param  model A fitted model object of class glm, clogit or coxph
#'
#' @param  exposure_names A character vector of two named binary exposure variables present in the fitted model: the default is an empty vector. If effect modification is being assessed, to get the right orientation of the table output, the first variable should be the putative effect modifier, the second, the main exposure. If it's interaction, the order doesn't matter.
#'
#' @param  ci.level Magnitude of the returned CI level. Default is 0.95
#'
#' @param  em   TRUE (the default), for effect modification assessment. FALSE, for interaction.
#'
#' @param  recode If TRUE, recodes the exposures - if at least one of the exposures is protective - such that the stratum with the lowest risk becomes the new reference category when the two exposures are considered jointly.
#'
#' @return  a list object of class 'interactionR' that includes a dataframe containing all effect estimates necessary for full reporting of effect modification or interaction analysis. @seealso \code{\link{interactionR_table}} for how to generate a publication-ready table with this object.
#'
#' @examples
#' ## Using Case-control data from Rothman and Keller (1972)
#' ## evaluating the joint effect of alcohol and smoking
#' ## on oral cancer risk is included in the package
#' ## (cited in Hosmer and Lemeshow (1992) and Zou (2008))
#'
#' ## fit the interaction model
#' model.glm <- glm(oc ~ alc * smk,
#'   family = binomial(link = "logit"),
#'   data = OCdata
#' )
#'
#' ## Then pass the fitted model to the function
#' interactionR_delta(model.glm,
#'   exposure_names = c("alc", "smk"),
#'   ci.level = 0.95, em = FALSE, recode = FALSE
#' )
#' @export
#' @importFrom msm deltamethod
interactionR_delta <- function(model, exposure_names = c(), ci.level = 0.95, em = T,
                               recode = F) {
  check_arguments(model, exposure_names)

  # Estimates the critical value from the supplied CI.level for
  # subsequent CI estimations
  alpha <- 1 - ci.level
  z <- qnorm(1 - alpha / 2)

  # Extracts the names for the main exposure (beta1), the effect modifier
  # (beta2) and their interaction term
  e1 <- grep(exposure_names[1], names(coef(model)), value = TRUE, ignore.case = TRUE)
  e2 <- grep(exposure_names[2], names(coef(model)), value = TRUE, ignore.case = TRUE)
  e1_e2 <- intersect(e1,e2)
  if (length(e1_e2) != 1) {
    stop("The interaction you specified in your exposure_names argument cannot be found in the model")
  }
  beta1 <- e1[1]
  beta2 <- e2[1]
  beta3 <- e1_e2[1]

  varNames <- c(beta1, beta2, beta3)

  # estimating coefficients to check for any preventive exposures
  b1 <- coef(model)[beta1]
  b2 <- coef(model)[beta2]
  b3 <- coef(model)[beta3]

  #### Recode section code is adapted from Marthur and Vanderweele 2018 (doi: 10.1097/EDE.0000000000000752) ####

  # check if any exposure is preventive
  if (preventive(OR10 = exp(b1), OR01 = exp(b2))) {
    if (!recode) {
      warning("At least one exposure is preventive. Set argument recode=TRUE for the exposures to be automatically recoded. see Knol et al. (2011) European Journal of Epidemiology, 26(6), 433-438")
    }
    if (recode) {
      if ("coxph" %in% class(model)) {
        stop("Currently, interactionR() cannot automatically recode models fitted with coxph or clogit. Recode your exposure variables following the examples in Knol et al. (2011) European Journal of Epidemiology, 26(6), 433-438, re-fit your model, and re-run interactionR()")
      }
      temp <- data.frame(cat = c("OR10", "OR01", "OR11"), value = c(
        exp(b1),
        exp(b2), exp(b1 + b2 + b3)
      ))
      ref.cat <- temp$cat[which.min(temp$value)]

      E1.ref <- substr(ref.cat, 3, 3)
      E2.ref <- substr(ref.cat, 4, 4)

      # extract the raw data that was used to fit the model
      dat.ir <- model.frame(model)

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

  if ("coxph" %in% class(model)) {
    se_vec <- summary(model)$coefficients[, 3]
  } else {
    se_vec <- summary(model)$coefficients[, 2]
  } # extracts the SE vector for the coefficients from the model

  v1 <- se_vec[beta1]^2
  v2 <- se_vec[beta2]^2
  v3 <- se_vec[beta3]^2

  #Extracts p-values from the model
  pvals <- pvals <- extract_pvals(model)

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
  CI.ll_OR_X1 <- exp(b2 + b3 - z * sqrt(v23))
  CI.ul_OR_X1 <- exp(b2 + b3 + z * sqrt(v23))
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


  # Estimates measures of effect modification on the additive scale and
  # calculates their CI and p-value with the delta method implemented in
  # the msm package


  # RERI, CI and p-value
  RERI <- OR11 - OR01 - OR10 + 1
  se_RERI <- deltamethod(g = ~ exp(x1 + x2 + x3) - exp(x1) - exp(x2) +
    1, mean = c(b1, b2, b3), cov = v_cov1)
  CI.ll_RERI <- RERI - z * se_RERI
  CI.ul_RERI <- RERI + z * se_RERI
  p.RERI <- 1 - pnorm(RERI/se_RERI)

  # AP, CI and p-value
  AP <- RERI / OR11
  se_AP <- deltamethod(g = ~ (exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1) / exp(x1 +
    x2 + x3), mean = c(b1, b2, b3), cov = v_cov1)
  CI.ll_AP <- AP - z * se_AP
  CI.ul_AP <- AP + z * se_AP
  p.AP <- 1 - pnorm(abs(AP)/se_AP)


  # SI, CI and p-value
  lnSI <- log((exp(b1 + b2 + b3) - 1)) - log((exp(b1) + exp(b2) - 2))
  SI <- exp(lnSI)
  se_SI <- deltamethod(g = ~ log((exp(x1 + x2 + x3) - 1)) - log((exp(x1) +
    exp(x2) - 2)), mean = c(b1, b2, b3), cov = v_cov1)

  CI.ll_SI <- exp(lnSI - z * se_SI)
  CI.ul_SI <- exp(lnSI + z * se_SI)
  p.SI <- 1 - plnorm(exp(lnSI/se_SI))

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

  d$p = round(d$p, 5)

  ir <- list(dframe = d, exp_names = c(beta1, beta2), analysis = em, call = model$call, dat = raw_data)
  attr(ir, "class") <- "interactionR"


  invisible(ir)
}
