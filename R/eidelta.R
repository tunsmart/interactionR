#' Computes confidence intervals for interaction measures using the delta method by Hosmer and Lemeshow
#'
#' @param  model A fitted model object of class glm or coxph
#' @param  coef A vector of two numbers indicating the position of the two interaction terms in the model summary. The default is 2 and 3, which assumes that the two exposures are listed first before any additional covariates
#' @param  CI.level A number indicating the confidence level for the CI estimation. The default is 0.95 CI
#' @param  type A letter 'e' for effect modification or 'i' for interaction. The default is 'e'.
#' @return A dataframe containing all the neccessary estimates required to full report effect modification or interaction with the CI estimates for interaction measures computed using the delta method
#' @examples
#' ei.delta(model.glm, coef = c(2,3), CI.level = 0.95, type = 'i')
#' @export
#' @importFrom msm deltamethod
ei.delta = function(model, coef = c(2, 3), CI.level = 0.95, type = "e") {
    
    # if (model$family$link != 'logit') { stop ('The 'model' argument must be a
    # regression model object fit with glm() and link = logit') } else if
    # (!type = 'e' || 'i') { stop ('Argument 'type' must be either 'e' for
    # effect modification or 'i' for interaction')}
    
    # Estimates the critical value from the supplied CI.level for subsequent CI
    # estimations
    alpha = 1 - CI.level
    z = qnorm(1 - alpha/2)
    
    # Extracts the names for the main exposure (beta1), the effect modifier
    # (beta2) and their joint efffect (beta1 + beta2 + beta1:beta2)
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
    CI.ll_OR10 = exp(confint.default(model)[beta1, 1])
    CI.ul_OR10 = exp(confint.default(model)[beta1, 2])  # This is also OR of X on D (A==0)
    OR01 = as.numeric(exp(b2))
    CI.ll_OR01 = exp(confint.default(model)[beta2, 1])
    CI.ul_OR01 = exp(confint.default(model)[beta2, 2])  # This is also OR of A on D (X==0)
    OR11 = as.numeric(exp(b1 + b2 + b3))
    CI.ll_OR11 = exp(b1 + b2 + b3 - z * sqrt(v123))
    CI.ul_OR11 = exp(b1 + b2 + b3 + z * sqrt(v123))
    
    ### Estimates the effect (and CI) of A on D (X==1) ###
    OR_X1 = as.numeric(exp(b2 + b3))  # OR of A on D (X==1)
    CI.ll_OR_X1 = exp(b2 + b3) * exp(-z * sqrt(v23))
    CI.ul_OR_X1 = exp(b2 + b3) * exp(z * sqrt(v23))
    
    
    ### Estimates the effect (and CI) of X on D (A==1) ###
    OR_A1 = as.numeric(exp(b1 + b3))  # OR of X on D (A==1)
    CI.ll_OR_A1 = exp(b1 + b3 - z * sqrt(v13))
    CI.ul_OR_A1 = exp(b1 + b3 + z * sqrt(v13))
    
    # Effect modification on the multiplicative scale and CI
    OR_M = as.numeric(exp(b3))
    CI.ll_OR_M = exp(confint.default(model)[beta3, 1])
    CI.ul_OR_M = exp(confint.default(model)[beta3, 2])
    
    
    # Estimates measures of effect modification on the additive scale and
    # calculates their CI and p-value with the delta method implemented in the
    # msm package
    
    
    # RERI, CI and p-value
    RERI = OR11 - OR01 - OR10 + 1
    se_RERI = deltamethod(g = ~exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1, mean = c(b1, 
        b2, b3), cov = v_cov1)
    CI.ll_RERI = RERI - z * se_RERI
    CI.ul_RERI = RERI + z * se_RERI
    p_RERI = 1 - pnorm(RERI/se_RERI)
    
    # AP, CI and p-value
    AP = RERI/OR11
    se_AP = deltamethod(g = ~(exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1)/exp(x1 + 
        x2 + x3), mean = c(b1, b2, b3), cov = v_cov1)
    CI.ll_AP = AP - z * se_AP
    CI.ul_AP = AP + z * se_AP
    p_AP = 1 - pnorm(abs(AP)/se_AP)
    
    
    # SI, CI and p-value
    lnSI = log((exp(b1 + b2 + b3) - 1)) - log((exp(b1) + exp(b2) - 2))
    SI = exp(lnSI)
    se_SI = deltamethod(g = ~log((exp(x1 + x2 + x3) - 1)) - log((exp(x1) + 
        exp(x2) - 2)), mean = c(b1, b2, b3), cov = v_cov1)
    
    CI.ll_SI = exp(lnSI - z * se_SI)
    CI.ul_SI = exp(lnSI + z * se_SI)
    
    
    
    if (type == "i") {
        out = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(", 
            beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", 
            beta2, " on outcome [", beta1, "==1]", sep = ""), paste("OR(", 
            beta1, " on outcome [", beta2, "==0]", sep = ""), paste("OR(", 
            beta1, " on outcome [", beta2, "==1]", sep = ""), "Multiplicative scale", 
            "RERI", "AP", "SI"), Estimates = c(OR00, OR01, OR10, OR11, OR01, 
            OR_X1, OR10, OR_A1, OR_M, RERI, AP, SI), CI.ll = c(NA, CI.ll_OR01, 
            CI.ll_OR10, CI.ll_OR11, CI.ll_OR01, CI.ll_OR_X1, CI.ll_OR10, CI.ll_OR_A1, 
            CI.ll_OR_M, CI.ll_RERI, CI.ll_AP, CI.ll_SI), CI.ul = c(NA, CI.ul_OR01, 
            CI.ul_OR10, CI.ul_OR11, CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR10, CI.ul_OR_A1, 
            CI.ul_OR_M, CI.ul_RERI, CI.ul_AP, CI.ul_SI))
        rownames(out) = NULL
        return(out)
    } else {
        
        out = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(", 
            beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", 
            beta2, " on outcome [", beta1, "==1]", sep = ""), "Multiplicative scale", 
            "RERI", "AP"), Estimates = c(OR00, OR01, OR10, OR11, OR01, OR_X1, 
            OR_M, RERI, AP), CI.ll = c(NA, CI.ll_OR01, CI.ll_OR10, CI.ll_OR11, 
            CI.ll_OR01, CI.ll_OR_X1, CI.ll_OR_M, CI.ll_RERI, CI.ll_AP), CI.ul = c(NA, 
            CI.ul_OR01, CI.ul_OR10, CI.ul_OR11, CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR_M, 
            CI.ul_RERI, CI.ul_AP))
        rownames(out) = NULL
        return(out)
    }
    
}

