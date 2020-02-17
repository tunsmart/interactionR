#' Computes confidence intervals for interaction measures using the variance recovery method by Zou (2018)
#'
#' @param  model A fitted model object of class glm or coxph
#' @param  coef A vector of two numbers indicating the position of the two interaction terms in the model summary. The default is 2 and 3, which assumes that the two exposures are listed first before any additional covariates
#' @param  CI.level A number indicating the confidence level for the CI estimation. The default is 0.95 CI
#' @param  type A letter 'e' for effect modification or 'i' for interaction. The default is 'e'.
#' @return A dataframe containing all the neccessary estimates required to full report effect modification or interaction with the CI estimates for interaction measures computed using the variance recovery method
#' @examples
#' ei.mover(model.glm, coef = c(2,3), CI.level = 0.95, type = "i")

#' @export
ei.mover = function(model, coef = c(2, 3), CI.level = 0.95, type = "e") {

    # if (model$family$link != 'logit') { stop ('The 'model' argument must be a
    # regression model object fit with glm() and link = logit') } else if (!type =
    # 'e' || 'i') { stop ('Argument 'type' must be either 'e' for effect
    # modification or 'i' for interaction') }

    # Estimates the critical value from the supplied CI.level for subsequent CI
    # estimations
    alpha = 1 - CI.level
    z = qnorm(1 - alpha/2)

    # Extracts the names for the main exposure (beta1), the effect modifier (beta2)
    # and their joint efffect (beta1 + beta2 + beta1:beta2)
    beta1 = names(coef(model))[coef[1]]
    beta2 = names(coef(model))[coef[2]]
    beta3 = paste(beta1, beta2, sep = ":")

    varNames = c(beta1, beta2, beta3)

    se_vec = summary(model)$coefficients[, 2]  #extracts the SE vector for the coefficients
    # from the model
    v1 = se_vec[beta1]^2
    v2 = se_vec[beta2]^2
    v3 = se_vec[beta3]^2

    ### Extracts the variance-covariance matrix from the model### for use in the
    ### delta method CI estimation for RERI and AP###
    v_cov = vcov(model)
    v_cov1 = v_cov[varNames, varNames]  #for deltamethod
    cov12 = v_cov[beta1, beta2]
    cov13 = v_cov[beta1, beta3]
    cov23 = v_cov[beta2, beta3]
    v123 = v1 + v2 + v3 + (2 * (cov12 + cov13 + cov23))
    v12 = v1 + v2 + (2 * (cov12))
    v13 = v1 + v3 + (2 * (cov13))
    v23 = v2 + v3 + (2 * (cov23))


    # Estimates individual and joint effects ORs (with CI) from the model
    b1 = coef(model)[beta1]
    b2 = coef(model)[beta2]
    b3 = coef(model)[beta3]
    OR00 = 1  #reference OR
    OR10 = as.numeric(exp(b1))
    l1 = exp(confint.default(model)[beta1, 1])
    u1 = exp(confint.default(model)[beta1, 2])  # This is also OR of X on D (A==0)
    OR01 = as.numeric(exp(b2))
    l2 = exp(confint.default(model)[beta2, 1])
    u2 = exp(confint.default(model)[beta2, 2])  # This is also OR of A on D (X==0)
    OR11 = as.numeric(exp(b1 + b2 + b3))
    l3 = exp(b1 + b2 + b3 - z * sqrt(v123))
    u3 = exp(b1 + b2 + b3 + z * sqrt(v123))

    ### Estimates the effect (and CI) of A on D (X==1) ###
    OR_X1 = as.numeric(exp(b2 + b3))  # OR of A on D (X==1)
    CI.ll_OR_X1 = exp(b2 + b3 - z * sqrt(v23))
    CI.ul_OR_X1 = exp(b2 + b3 + z * sqrt(v23))


    ### Estimates the effect (and CI) of X on D (A==1) ###
    OR_A1 = as.numeric(exp(b1 + b3))  # OR of X on D (A==1)
    CI.ll_OR_A1 = exp(b1 + b3 - z * sqrt(v13))
    CI.ul_OR_A1 = exp(b1 + b3 + z * sqrt(v13))

    # Effect modification on the multiplicative scale and CI
    OR_M = as.numeric(exp(b3))
    CI.ll_OR_M = exp(confint.default(model)[beta3, 1])
    CI.ul_OR_M = exp(confint.default(model)[beta3, 2])


    # Estimates measures of effect modification on the additive scale and
    # calculates their CI with the 'MOVER' method (Zou (2018)
    # https://doi.org/10.1093/aje/kwn104)

    # RERI, CI and p-value
    RERI = OR11 - OR01 - OR10 + 1
    r12 = (v1 + cov12 + cov13)/sqrt(v1 * v123)
    r13 = (cov12 + v2 + cov23)/sqrt(v2 * v123)
    r23 = cov12/sqrt(v1 * v2)

    p1 = (OR11 - l3)^2 + (u1 - OR10)^2 + (u2 - OR01)^2
    p2 = 2 * r12 * (OR11 - l3) * (u1 - OR10)
    p3 = 2 * r13 * (OR11 - l3) * (u2 - OR01)
    p4 = 2 * r23 * (u1 - OR10) * (u2 - OR01)
    p5 = p1 - p2 - p3 + p4
    p6 = p5^0.5

    L = 1 + OR11 - OR10 - OR01 - p6

    k1 = (u3 - OR11)^2 + (OR10 - l1)^2 + (OR01 - l2)^2
    k2 = 2 * r12 * (u3 - OR11) * (OR10 - l1)
    k3 = 2 * r13 * (u3 - OR11) * (OR01 - l2)
    k4 = 2 * r23 * (OR10 - l1) * (OR01 - l2)
    k5 = (k1 - k2 - k3 + k4)^0.5

    U = 1 + OR11 - OR10 - OR01 + k5


    # AP, CI and p-value
    theta1 = 1/exp(b1 + b2 + b3)
    theta2 = 1/exp(b2 + b3)
    theta3 = 1/exp(b1 + b3)
    AP = theta1 - theta2 - theta3 + 1
    APr12 = (cov12 + cov13 + v2 + (2 * cov23) + v3)/sqrt(v23 * v123)
    APr13 = (v1 + cov12 + (2 * cov13) + cov23 + v3)/sqrt(v13 * v123)
    APr23 = (cov12 + cov23 + cov13 + v3)/sqrt(v23 * v13)

    APl1 = theta1 * exp(-z * sqrt(v123))
    APu1 = theta1 * exp(z * sqrt(v123))

    APl2 = theta2 * exp(-z * sqrt(v23))
    APu2 = theta2 * exp(z * sqrt(v23))

    APl3 = theta3 * exp(-z * sqrt(v13))
    APu3 = theta3 * exp(z * sqrt(v13))

    APp1 = (theta1 - APl1)^2 + (APu2 - theta2)^2 + (APu3 - theta3)^2
    APp2 = 2 * APr12 * (theta1 - APl1) * (APu2 - theta2)
    APp3 = 2 * APr13 * (theta1 - APl1) * (APu3 - theta3)
    APp4 = 2 * APr23 * (APu2 - theta2) * (APu3 - theta3)
    APp5 = APp1 - APp2 - APp3 + APp4
    APp6 = APp5^0.5

    APL = 1 + theta1 - theta2 - theta3 - APp6

    APk1 = (APu1 - theta1)^2 + (theta2 - APl2)^2 + (theta3 - APl3)^2
    APk2 = 2 * APr12 * (APu1 - theta1) * (theta2 - APl2)
    APk3 = 2 * APr13 * (APu1 - theta1) * (theta3 - APl3)
    APk4 = 2 * APr23 * (theta2 - APl2) * (theta3 - APl3)
    APk5 = (APk1 - APk2 - APk3 + APk4)^0.5

    APU = 1 + theta1 - theta2 - theta3 + APk5


    # SI, CI and p-value
    SItheta1 = log((exp(b1 + b2 + b3) - 1))
    SItheta2 = log((exp(b1) + exp(b2) - 2))
    lnSI = SItheta1 - SItheta2
    SI = exp(lnSI)

    vSItheta1 = (exp(b1 + b2 + b3)/(exp(b1 + b2 + b3) - 1))^2 * v123
    vSItheta2 = ((exp(2 * b1) * v1) + (exp(2 * b2) * v2) + (2 * exp(b1 + b2) *
        cov12))/(exp(b1) + exp(b2) - 2)^2
    SIl1 = SItheta1 - z * sqrt(vSItheta1)
    SIu1 = SItheta1 + z * sqrt(vSItheta1)
    SIl2 = SItheta2 - z * sqrt(vSItheta2)
    SIu2 = SItheta2 + z * sqrt(vSItheta2)

    SIr = ((exp(b1) * (v1 + cov12 + cov13)) + (exp(b2) * (cov12 + v2 + cov23)))/sqrt(v123 *
        ((exp(2 * b1) * v1) + (exp(2 * b2) * v2) + (2 * exp(b1 + b2) * cov12)))

    lnSIL = (SItheta1 + (-SItheta2)) - sqrt((SItheta1 - SIl1)^2 + ((-SItheta2) -
        (-SIl2))^2 + (2 * SIr * (SItheta1 - SIl1) * ((-SItheta2) - (-SIl2))))
    lnSIU = (SItheta1 + (-SItheta2)) + sqrt((SIu1 - SItheta1)^2 + ((-SIu2) - (-SItheta2))^2 +
        (2 * SIr * (SIu1 - SItheta1) * ((-SIu2) - (-SItheta2))))
    SIL = exp(lnSIL)
    SIU = exp(lnSIU)


    if (type == "i") {
        out = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(",
            beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", beta2,
            " on outcome [", beta1, "==0]", sep = ""), paste("OR(", beta1, " on outcome [",
            beta2, "==0]", sep = ""), paste("OR(", beta1, " on outcome [", beta2,
            "==1]", sep = ""), "Multiplicative scale", "RERI", "AP", "SI"), Estimates = c(OR00,
            OR01, OR10, OR11, OR01, OR_X1, OR10, OR_A1, OR_M, RERI, AP, SI), CI.ll = c(NA,
            l2, l1, l3, l2, CI.ll_OR_X1, l1, CI.ll_OR_A1, CI.ll_OR_M, L, APL, SIL),
            CI.ul = c(NA, u2, u1, u3, u2, CI.ul_OR_X1, u1, CI.ul_OR_A1, CI.ul_OR_M,
                U, APU, SIU))
        rownames(out) = NULL
        return(out)
    } else {

        out = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(",
            beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", beta2,
            " on outcome [", beta1, "==0]", sep = ""), "Multiplicative scale",
            "RERI"), Estimates = c(OR00, OR01, OR10, OR11, OR01, OR_X1, OR_M, RERI),
            CI.ll = c(NA, l2, l1, l3, l2, CI.ll_OR_X1, CI.ll_OR_M, L), CI.ul = c(NA,
                u2, u1, u3, u2, CI.ul_OR_X1, CI.ul_OR_M, U))
        rownames(out) = NULL
        return(out)
    }

}

