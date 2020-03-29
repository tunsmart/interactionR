#' Computes confidence intervals for interaction measures using bootstrapping as described by Assmann et al (1996).
#'
#' @param  dat a data frame containing the variables required to estimate the measures of additive interactions in the following order of columns:
#' \itemize{
#'   \item \code{1)} the first column should be the outcome variable
#'   \item \code{2 & 3)} the second and third column should be the two exposures under study
#'   \item \code{others} the rest of the columns could be the covariates that requires adjustment in the model, if any.
#' }
#'
#'
#' @return a dataframe containing effect estimates of additive interaction measures with bootstrapped 95% CI limits.
#'
#' @examples
#' data(HDiscdata) # loads the dataset from assmann et al. which is available in the package
#' head(HDiscdata, 2L) # prints the first two line to confirm the data is in the right format
#' interactionR_bstrap(HDiscdata)
#'
#' @references
#' Assmann SF, Hosmer DW, Lemeshow S, Mundt KA. Confidence intervals for measures of interaction. Epidemiology 1996:286-90.
#'
#' @export
#' @importFrom boot boot
interactionR_bstrap = function(dat) {

   trio  = function(data, indices) {
        data = data[indices, ]
        m = glm(data[, 1] ~ data[, 2] * data[, 3], family = binomial(link = "logit"),
            data = data)

        beta1 = names(coef(m))[2]
        beta2 = names(coef(m))[3]
        beta3 = paste(beta1, beta2, sep = ":")

        b1 = coef(m)[beta1]
        b2 = coef(m)[beta2]
        b3 = coef(m)[beta3]

        OR10 = exp(b1)
        OR01 = exp(b2)
        OR11 = exp(b1 + b2 + b3)
        c(OR11 - OR10 - OR01 + 1, (OR11 - OR10 - OR01 + 1)/OR11, (OR11 -
            1)/(OR10 + OR01 - 2))

    }


    bstrap = boot(data = dat, trio, R = 1000)
    RERI = bstrap$t0[1]
    AP = bstrap$t0[2]
    SI = bstrap$t0[3]
    RERI.CI = quantile(bstrap$t[, 1], probs = c(0.025, 0.975), type = 9)
    AP.CI = quantile(bstrap$t[, 2], probs = c(0.025, 0.975), type = 9)
    SI.CI = quantile(bstrap$t[, 3], probs = c(0.025, 0.975), type = 9)

    out = data.frame(Measures = c("RERI", "AP", "SI"), Estimates = c(RERI,
        AP, SI), CI.LL = c(RERI.CI[1], AP.CI[1], SI.CI[1]), CI.UL = c(RERI.CI[2],
        AP.CI[2], SI.CI[2]))
    rownames(out) = NULL

    return(out)

}
