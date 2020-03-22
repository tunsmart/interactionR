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
emm.delta = function(model, exposure_names = c(), ci.level = 0.95, em = T, 
    recode = F) {
    
    if (invalid(model)) {
        stop("The 'model' argument must be a regression model object fit with glm() and link = 'logit', coxph() or clogit()")
    } else if (class(exposure_names) != "character") {
        stop("Argument 'exposure_names' requires a character vector of the names of the two exposure variables ")
    }
    
    # Estimates the critical value from the supplied CI.level for
    # subsequent CI estimations
    alpha = 1 - ci.level
    z = qnorm(1 - alpha/2)
    
    # Extracts the names for the main exposure (beta1), the effect modifier
    # (beta2) and their joint efffect (beta1 + beta2 + beta1:beta2)
    beta1 = exposure_names[1]
    beta2 = exposure_names[2]
    beta3 = paste(beta1, beta2, sep = ":")
    
    varNames = c(beta1, beta2, beta3)
    
    # estimating coefficients to check for any preventive exposures
    b1 = coef(model)[beta1]
    b2 = coef(model)[beta2]
    b3 = coef(model)[beta3]
    
    
    # check if any exposure is preventive
    if (preventive(OR10 = exp(b1), OR01 = exp(b2))) {
        if (!recode) 
            stop("Error: At least one exposure is preventive. Set argument recode=TRUE for the exposures to be automatically recoded. see Knol et al. (2011) European Journal of Epidemiology, 26(6), 433-438")
        if (recode) {
            # find stratum with lowest overall risk, conditional on covariates
            temp = data.frame(cat = c("OR10", "OR01", "OR11"), value = c(exp(b1), 
                exp(b2), exp(b1 + b2 + b3)))
            refcat = temp$cat[which.min(temp$value)]  # get category's name
            
            # extract first 'subscript' number
            E1.ref = substr(refcat, 3, 3)
            # extract second 'subscript' number
            E2.ref = substr(refcat, 4, 4)
            
            # recode each exposure based on new reference category
            dat = model$data
            dat[[beta1]] = ifelse(dat[[beta1]] == E1.ref, 0, 1)
            dat[[beta2]] = ifelse(dat[[beta2]] == E2.ref, 0, 1)
            
            # inform the user
            warning("Recoding exposures; new reference category for ", 
                beta1, " is ", E1.ref, " and for ", beta2, " is ", E2.ref)
            
            # refit model with user's original call but recoded data
            model = update(model, . ~ ., data = dat)
            
            # get new coefficients and ORs
            b1 = coef(model)[beta1]
            b2 = coef(model)[beta2]
            b3 = coef(model)[beta3]
            
        }
    }
    
    se_vec = summary(model)$coefficients[, 2]  #extracts the SE vector for the coefficients
    # from the model
    v1 = se_vec[beta1]^2
    v2 = se_vec[beta2]^2
    v3 = se_vec[beta3]^2
    
    ### Extracts the variance-covariance matrix from the model### for use in
    ### the delta method CI estimation for RERI and AP###
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
    # calculates their CI and p-value with the delta method implemented in
    # the msm package
    
    
    # RERI, CI and p-value
    RERI = OR11 - OR01 - OR10 + 1
    se_RERI = deltamethod(g = ~exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 
        1, mean = c(b1, b2, b3), cov = v_cov1)
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
    
    d = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(", 
        beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", 
        beta2, " on outcome [", beta1, "==1]", sep = ""), "Multiplicative scale", 
        "RERI", "AP"), Estimates = c(OR00, OR01, OR10, OR11, OR01, OR_X1, 
        OR_M, RERI, AP), CI.ll = c(NA, CI.ll_OR01, CI.ll_OR10, CI.ll_OR11, 
        CI.ll_OR01, CI.ll_OR_X1, CI.ll_OR_M, CI.ll_RERI, CI.ll_AP), CI.ul = c(NA, 
        CI.ul_OR01, CI.ul_OR10, CI.ul_OR11, CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR_M, 
        CI.ul_RERI, CI.ul_AP))
    rownames(d) = NULL
    
    
    
    if (!em) {
        d = data.frame(Measures = c("OR00", "OR01", "OR10", "OR11", paste("OR(", 
            beta2, " on outcome [", beta1, "==0]", sep = ""), paste("OR(", 
            beta2, " on outcome [", beta1, "==1]", sep = ""), paste("OR(", 
            beta1, " on outcome [", beta2, "==0]", sep = ""), paste("OR(", 
            beta1, " on outcome [", beta2, "==1]", sep = ""), "Multiplicative scale", 
            "RERI", "AP", "SI"), Estimates = c(OR00, OR01, OR10, OR11, 
            OR01, OR_X1, OR10, OR_A1, OR_M, RERI, AP, SI), CI.ll = c(NA, 
            CI.ll_OR01, CI.ll_OR10, CI.ll_OR11, CI.ll_OR01, CI.ll_OR_X1, 
            CI.ll_OR10, CI.ll_OR_A1, CI.ll_OR_M, CI.ll_RERI, CI.ll_AP, 
            CI.ll_SI), CI.ul = c(NA, CI.ul_OR01, CI.ul_OR10, CI.ul_OR11, 
            CI.ul_OR01, CI.ul_OR_X1, CI.ul_OR10, CI.ul_OR_A1, CI.ul_OR_M, 
            CI.ul_RERI, CI.ul_AP, CI.ul_SI))
        rownames(d) = NULL
    }
    
    tabler(d = d, beta1 = beta1, beta2 = beta2, em = em)
    
}

