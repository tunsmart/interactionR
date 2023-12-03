#' @title
#' Full reporting of effect modification and interaction analysis
#'
#' @description
#' For two binary exposures included in a regression model as an interaction term for a binary outcome, \code{interactionR} returns all effect estimates necessary to fully report effect modification or interaction analysis as recommended by Knol and Vanderweele (2012) [\doi{10.1093/ije/dyr218}]. Estimation of confidence intervals (CI) for measures of additive interaction (RERI, AP, and SI) is based on the delta method described by Hosmer and Lemeshow (1992) [\doi{10.1097/00001648-199209000-00012}] or the variance recovery 'mover' method described by Zou (2008) [\doi{10.1093/aje/kwn104}].
#'
#' @details
#' Effect modification is assessed when the effect of an exposure on an outcome differs within the strata of another exposure, while,
#' the assessment of the joint effect of two exposures on outcome is Interaction. Both terms are often used interchangeably in the literature.
#'
#' Despite the widespread analysis of interaction in the literature, the reporting is inadequate. To remedy this, Knol and Vanderweele (2012) proposed a set of recommendations that ensures full reporting for readers to be able to assess all dimensions of interaction.
#' The function returns all the effect estimates to fulfill these recommendations.
#'
#' Also, assessment of interaction is scale dependent: multiplicative or additive. Interaction on a multiplicative scale means that the combined effect of the two exposures is greater (or less) than the product of the individual effects of the two exposures. Interaction on an additive scale means that the combined effect of two exposures is greater (or less) than the sum of the individual effects of two exposures.
#' Whereas, interaction on the additive scale is more relevant to public health, most authors merely report on the multiplicative scale. The recommendations mentioned above ensures reporting on both scales.
#'
#' This function calculates three indices to assess the presence of additive interaction, as defined by Rothman (1998): (1) the relative excess risk due to interaction (RERI), (2) the proportion of disease among those with both exposures that is attributable to their interaction (AP), and (3) the synergy index (SI).
#' A RERI or AP of zero means no interaction or perfect additivity. A RERI or AP of greater than zero means positive interaction or more than additivity. A RERI or AP of less than zero means negative interaction or less than additivity. RERI ranges from zero to infinity while AP ranges from -1 to +1.
#'
#' The synergy index is the ratio of the combined effects and the individual effects. An SI of one means no interaction or perfect additivity. An SI of greater than one means positive interaction or more than additivity. An SI of less than one means negative interaction or less than additivity. SI ranges from zero to infinity.
#'
#' The delta method as described by Hosmer and Lemeshow (1992) is the most widely used method to calculate the confidence intervals of these interaction measures and the provides this function.
#' However, the poor performance of this method for typical use-cases is well documented. Therefore, the function also provides the option for these CIs estimations with the better performing MOVER method introduced by Zou (2008).
#'
#' Finally, additive interaction as described by Rothman (1998) assumes that the two exposures under study are risk factors for the outcome (i.e. RR > 1). As shown by Knol et al. (2012), If at least one of the two exposures are preventive (i.e. RR < 1) then estimates of RERI and AP becomes invalid (the SI is unaffected), and the exposures need to be recoded to risk factors such that stratum with the lowest risk becomes the new reference category when the two exposures are considered together.
#' The function can automatically carry out this recoding.
#'
#' @param  model A fitted model object of class glm, clogit or coxph
#'
#' @param  exposure_names A character vector of two named binary exposure variables present in the fitted model: the default is an empty vector. If effect modification is being assessed, to get the right orientation of the table output, the first variable should be the putative effect modifier, the second, the main exposure. If it's interaction, the order doesn't matter.
#'
#' @param  ci.type A character string ("delta" or "mover") specifying the method to use for the estimation of CI for the measures of additive interaction. Default is "delta".
#'
#' @param  ci.level Magnitude of the returned CI level. Default is 0.95
#'
#' @param  em   TRUE (the default), for effect modification assessment. FALSE, for interaction.
#'
#' @param  recode If TRUE, recodes the exposures - if at least one of the exposures is protective - such that the stratum with the lowest risk becomes the new reference category when the two exposures are considered jointly.
#'
#' @return  a list object of class 'interactionR' that includes a dataframe containing all effect estimates necessary for full reporting of effect modification or interaction analysis. @seealso \code{\link{interactionR_table}} for how to generate a publication-ready table from this object.
#'
#'
#' @examples
#' ## Using Case-control data from Rothman and Keller (1972)
#' ## evaluating the joint effect of alcohol and smoking
#' ## on oral cancer risk is included in the package
#' ## (cited in Hosmer and Lemeshow (1992) and Zou (2008))
#' ## fit the interaction model
#' model.glm <- glm(oc ~ alc * smk,
#'   family = binomial(link = "logit"),
#'   data = OCdata
#' )
#'
#' ## Then pass the fitted model to the function
#' interactionR(model.glm,
#'   exposure_names = c("alc", "smk"),
#'   ci.type = "delta", ci.level = 0.95,
#'   em = FALSE, recode = FALSE
#' )
#'
#' ## Because the delta method was selected, the returned CI for the trio
#' ## of interaction measures is as reported in page 455 of Hosmer and Lemeshow (1992) for this dataset
#'
#' ## To get CIs using the variance recovery method, set the 'ci.type' to "mover"
#' interactionR(model.glm,
#'   exposure_names = c("alc", "smk"),
#'   ci.type = "mover", ci.level = 0.95,
#'   em = FALSE, recode = FALSE
#' )
#' ## Now the CI returned for RERI is as Figure 4 in Zou (2008)
#'
#' ## To demonstrate the recoding feature of the function
#' ## We simulate a dataset with three binary variables: one outcome and two preventive exposures
#'
#' ## Generate exposure variables
#' n <- 2000
#' set.seed(750)
#' exp1 <- rbinom(n, size = 1, p = 0.2)
#' set.seed(520)
#' exp2 <- rbinom(n, size = 1, p = 0.6)
#'
#' ## Make at least one of the exposures preventive for the outcome
#' b0 <- log(1)
#' bexp1 <- log(0.4)
#' bexp2 <- log(1.1)
#' bexp1exp2 <- log(0.75)
#'
#' ## Generate outcome
#' ppred <- b0 + bexp1 * exp1 + bexp2 * exp2 + bexp1exp2 * exp1 * exp2
#' p <- exp(ppred) / (1 + exp(ppred))
#' set.seed(30)
#' outcome <- rbinom(n, size = 1, p = p)
#'
#' ## Create dataframe
#' d <- data.frame(outcome, exp1, exp2)
#'
#' ## Fit a logistic regression model with the data
#' model.prev <- glm(outcome ~ exp1 * exp2, family = binomial(link = "logit"), data = d)
#'
#' ## With this model, calling the function with the default FALSE parameter for
#' ## the 'recode' argument returns an error
#' ## And informs the user to set 'recode' to TRUE if they want to automatically recode the variables
#' ## Set to TRUE, the function recodes the data and generate estimates
#' ## for additive interaction measures with the new data which
#' ## can be examined in the returned list object by the function
#'
#' interactionR(model.prev,
#'   exposure_names = c("exp1", "exp2"),
#'   ci.type = "delta", ci.level = 0.95,
#'   em = FALSE, recode = TRUE
#' )
#' @references
#' Knol MJ, VanderWeele TJ. Recommendations for presenting analyses of effect modification and interaction. Int J Epidemiol 2012; 41:514-20.
#'
#' Hosmer DW, Lemeshow S. Confidence interval estimation of interaction. Epidemiology 1992; 3:452-6.
#'
#' Zou GY. On the Estimation of Additive Interaction by Use of the Four-by-two Table and Beyond. American Journal of Epidemiology 2008; 168:212-24.
#'
#' Rothman K, Greenland S (1998). Modern Epidemiology. Lippincott - Raven Philadelphia, USA.
#'
#' Knol, M.J., VanderWeele, T.J., Groenwold, R.H.H. et al. Estimating measures of interaction on an additive scale for preventive exposures. Eur J Epidemiol 26, 433–438 (2011). https://doi.org/10.1007/s10654-011-9554-9


#' @export
#' @importFrom msm deltamethod
interactionR <- function(model, exposure_names = c(), ci.type = "delta", ci.level = 0.95,
                         em = T, recode = F) {
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
  pvals <- extract_pvals(model)

  ### Extracts the variance-covariance matrix from the model### for use in
  ### the delta and MOVER method CI estimation for RERI and AP###
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
  l1 <- exp(confint.default(model)[beta1, 1])
  u1 <- exp(confint.default(model)[beta1, 2]) # This is also OR of X on D (A==0)
  p.OR10 <- pvals[beta1]
  OR01 <- as.numeric(exp(b2))
  l2 <- exp(confint.default(model)[beta2, 1])
  u2 <- exp(confint.default(model)[beta2, 2]) # This is also OR of A on D (X==0)
  p.OR01 <- pvals[beta2]
  OR11 <- as.numeric(exp(b1 + b2 + b3))
  l3 <- exp(b1 + b2 + b3 - z * sqrt(v123))
  u3 <- exp(b1 + b2 + b3 + z * sqrt(v123))
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

  if (ci.type == "mover") {
    # Estimates measures of effect modification on the additive scale and
    # calculates their CI with the 'MOVER' method (Zou (2018)
    # https://doi.org/10.1093/aje/kwn104)

    # RERI, CI and p-value
    RERI <- OR11 - OR01 - OR10 + 1
    r12 <- (v1 + cov12 + cov13) / sqrt(v1 * v123)
    r13 <- (cov12 + v2 + cov23) / sqrt(v2 * v123)
    r23 <- cov12 / sqrt(v1 * v2)

    p1 <- (OR11 - l3)^2 + (u1 - OR10)^2 + (u2 - OR01)^2
    p2 <- 2 * r12 * (OR11 - l3) * (u1 - OR10)
    p3 <- 2 * r13 * (OR11 - l3) * (u2 - OR01)
    p4 <- 2 * r23 * (u1 - OR10) * (u2 - OR01)
    p5 <- p1 - p2 - p3 + p4
    p6 <- p5^0.5

    L <- 1 + OR11 - OR10 - OR01 - p6

    k1 <- (u3 - OR11)^2 + (OR10 - l1)^2 + (OR01 - l2)^2
    k2 <- 2 * r12 * (u3 - OR11) * (OR10 - l1)
    k3 <- 2 * r13 * (u3 - OR11) * (OR01 - l2)
    k4 <- 2 * r23 * (OR10 - l1) * (OR01 - l2)
    k5 <- (k1 - k2 - k3 + k4)^0.5

    U <- 1 + OR11 - OR10 - OR01 + k5
    p.RERI <- NA


    # AP, CI and p-value
    theta1 <- 1 / exp(b1 + b2 + b3)
    theta2 <- 1 / exp(b2 + b3)
    theta3 <- 1 / exp(b1 + b3)
    AP <- theta1 - theta2 - theta3 + 1
    APr12 <- (cov12 + cov13 + v2 + (2 * cov23) + v3) / sqrt(v23 * v123)
    APr13 <- (v1 + cov12 + (2 * cov13) + cov23 + v3) / sqrt(v13 * v123)
    APr23 <- (cov12 + cov23 + cov13 + v3) / sqrt(v23 * v13)

    APl1 <- theta1 * exp(-z * sqrt(v123))
    APu1 <- theta1 * exp(z * sqrt(v123))

    APl2 <- theta2 * exp(-z * sqrt(v23))
    APu2 <- theta2 * exp(z * sqrt(v23))

    APl3 <- theta3 * exp(-z * sqrt(v13))
    APu3 <- theta3 * exp(z * sqrt(v13))

    APp1 <- (theta1 - APl1)^2 + (APu2 - theta2)^2 + (APu3 - theta3)^2
    APp2 <- 2 * APr12 * (theta1 - APl1) * (APu2 - theta2)
    APp3 <- 2 * APr13 * (theta1 - APl1) * (APu3 - theta3)
    APp4 <- 2 * APr23 * (APu2 - theta2) * (APu3 - theta3)
    APp5 <- APp1 - APp2 - APp3 + APp4
    APp6 <- APp5^0.5

    APL <- 1 + theta1 - theta2 - theta3 - APp6

    APk1 <- (APu1 - theta1)^2 + (theta2 - APl2)^2 + (theta3 - APl3)^2
    APk2 <- 2 * APr12 * (APu1 - theta1) * (theta2 - APl2)
    APk3 <- 2 * APr13 * (APu1 - theta1) * (theta3 - APl3)
    APk4 <- 2 * APr23 * (theta2 - APl2) * (theta3 - APl3)
    APk5 <- (APk1 - APk2 - APk3 + APk4)^0.5

    APU <- 1 + theta1 - theta2 - theta3 + APk5
    p.AP <- NA


    # SI, CI and p-value
    SItheta1 <- log((exp(b1 + b2 + b3) - 1))
    SItheta2 <- log((exp(b1) + exp(b2) - 2))
    lnSI <- SItheta1 - SItheta2
    SI <- exp(lnSI)

    vSItheta1 <- (exp(b1 + b2 + b3) / (exp(b1 + b2 + b3) - 1))^2 * v123
    vSItheta2 <- ((exp(2 * b1) * v1) + (exp(2 * b2) * v2) + (2 * exp(b1 +
      b2) * cov12)) / (exp(b1) + exp(b2) - 2)^2
    SIl1 <- SItheta1 - z * sqrt(vSItheta1)
    SIu1 <- SItheta1 + z * sqrt(vSItheta1)
    SIl2 <- SItheta2 - z * sqrt(vSItheta2)
    SIu2 <- SItheta2 + z * sqrt(vSItheta2)

    SIr <- ((exp(b1) * (v1 + cov12 + cov13)) + (exp(b2) * (cov12 + v2 +
      cov23))) / sqrt(v123 * ((exp(2 * b1) * v1) + (exp(2 * b2) * v2) +
      (2 * exp(b1 + b2) * cov12)))

    lnSIL <- (SItheta1 + (-SItheta2)) - sqrt((SItheta1 - SIl1)^2 + ((-SItheta2) -
      (-SIl2))^2 + (2 * SIr * (SItheta1 - SIl1) * ((-SItheta2) -
      (-SIl2))))
    lnSIU <- (SItheta1 + (-SItheta2)) + sqrt((SIu1 - SItheta1)^2 + ((-SIu2) -
      (-SItheta2))^2 + (2 * SIr * (SIu1 - SItheta1) * ((-SIu2) -
      (-SItheta2))))
    SIL <- exp(lnSIL)
    SIU <- exp(lnSIU)
    p.SI <- NA

  } else if (ci.type == "delta") {
    # Estimates measures of effect modification on the additive scale and
    # calculates their CI and p-value with the delta method implemented in
    # the msm package

    # RERI, CI and p-value
    RERI <- OR11 - OR01 - OR10 + 1
    se_RERI <- deltamethod(g = ~ exp(x1 + x2 + x3) - exp(x1) - exp(x2) +
      1, mean = c(b1, b2, b3), cov = v_cov1)
    L <- RERI - z * se_RERI
    U <- RERI + z * se_RERI
    p.RERI <- 1 - pnorm(RERI/se_RERI)


    # AP, CI and p-value
    AP <- RERI / OR11
    se_AP <- deltamethod(g = ~ (exp(x1 + x2 + x3) - exp(x1) - exp(x2) +
      1) / exp(x1 + x2 + x3), mean = c(b1, b2, b3), cov = v_cov1)
    APL <- AP - z * se_AP
    APU <- AP + z * se_AP
    p.AP <- 1 - pnorm(abs(AP)/se_AP)



    # SI, CI and p-value
    lnSI <- log((exp(b1 + b2 + b3) - 1)) - log((exp(b1) + exp(b2) -
      2))
    SI <- exp(lnSI)
    se_SI <- deltamethod(g = ~ log((exp(x1 + x2 + x3) - 1)) - log((exp(x1) +
      exp(x2) - 2)), mean = c(b1, b2, b3), cov = v_cov1)

    SIL <- exp(lnSI - z * se_SI)
    SIU <- exp(lnSI + z * se_SI)
    p.SI <- 1 - plnorm(exp(lnSI/se_SI))

  } else {
    stop("Argument 'ci.type' must be 'delta' or 'mover' ")
  }

  d <- data.frame(
    Measures = c(
      "OR00", "OR01", "OR10", "OR11", paste("OR(",
        beta2, " on outcome [", beta1, "==0]",
        sep = ""
      ), paste("OR(",
        beta2, " on outcome [", beta1, "==1]",
        sep = ""
      ), "Multiplicative scale",
      "RERI"
    ), Estimates = c(
      OR00, OR01, OR10, OR11, OR01, OR_X1, OR_M,
      RERI
    ), CI.ll = c(NA, l2, l1, l3, l2, CI.ll_OR_X1, CI.ll_OR_M, L),
    CI.ul = c(NA, u2, u1, u3, u2, CI.ul_OR_X1, CI.ul_OR_M, U
   ), p = c(
     NA, p.OR01, p.OR10, p.OR11, p.OR01, p.OR_X1, p.OR_M, p.RERI
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
      l2, l1, l3, l2, CI.ll_OR_X1, l1, CI.ll_OR_A1, CI.ll_OR_M, L,
      APL, SIL
    ), CI.ul = c(
      NA, u2, u1, u3, u2, CI.ul_OR_X1, u1, CI.ul_OR_A1,
      CI.ul_OR_M, U, APU, SIU
    ),p = c(
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
