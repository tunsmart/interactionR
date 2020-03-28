#' @export
#' @importFrom boot boot
interactionR_bstrap = function(dat) {

    foo = function(data, indices) {
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
        # RERI = OR11-OR10-OR01+1
        c(OR11 - OR10 - OR01 + 1, (OR11 - OR10 - OR01 + 1)/OR11, (OR11 -
            1)/(OR10 + OR01 - 2))

    }


    bstrap = boot(data = dat, foo, R = 1000)
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
