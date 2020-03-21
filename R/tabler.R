#' @import huxtable
tabler = function(d, beta1, beta2, type = NULL) {
    d$Estimates = as.character(round(d$Estimates, 2))
    d$CI.ll = as.character(round(d$CI.ll, 2))
    d$CI.ul = as.character(round(d$CI.ul, 2))
    E1.absent = paste(beta1, "absent", sep = " ")
    E1.present = paste(beta1, "present", sep = " ")
    E2.absent = paste(beta2, "absent", sep = " ")
    E2.present = paste(beta2, "present", sep = " ")
    WithinStrataEffect1 = paste("Effect of", beta2, "within the strata of",
        beta1, sep = " ")
    WithinStrataEffect2 = paste("Effect of", beta1, "within the strata of",
        beta2, sep = " ")

    if (type == "e") {
        t = hux(c1 = c(NA, NA, E1.absent, E1.present, "Multiplicative scale",
            "RERI"), c2 = c(E2.absent, "OR [95% CI]", NA, NA, NA, NA), c3 = c(E2.present,
            "OR [95% CI]", NA, NA, NA, NA), c4 = c(WithinStrataEffect1, "OR [95% CI]",
            NA, NA, NA, NA))
        t[3, 2] = paste("1", "[Reference]", sep = " ")
        t[3, 3] = paste(d[2, 2], " [", d[2, 3], ", ", d[2, 4], "]", sep = "")
        t[3, 4] = paste(d[5, 2], " [", d[5, 3], ", ", d[5, 4], "]", sep = "")
        t[4, 2] = paste(d[3, 2], " [", d[3, 3], ", ", d[3, 4], "]", sep = "")
        t[4, 3] = paste(d[4, 2], " [", d[4, 3], ", ", d[4, 4], "]", sep = "")
        t[4, 4] = paste(d[6, 2], " [", d[6, 3], ", ", d[6, 4], "]", sep = "")
        t[5, 2] = paste(d[7, 2], " [", d[7, 3], ", ", d[7, 4], "]", sep = "")
        t[6, 2] = paste(d[8, 2], " [", d[8, 3], ", ", d[8, 4], "]", sep = "")

        caption(t) = paste("Modification of the effect of", beta2, "by", beta1,
            sep = " ")

    } else {
        t = hux(c1 = c(NA, NA, E1.absent, E1.present, WithinStrataEffect2,
            "Multiplicative scale", "RERI", "AP", "SI"), c2 = c(E2.absent,
            "OR [95% CI]", NA, NA, NA, NA, NA, NA, NA), c3 = c(E2.present,
            "OR [95% CI]", NA, NA, NA, NA, NA, NA, NA), c4 = c(WithinStrataEffect1,
            "OR [95% CI]", NA, NA, NA, NA, NA, NA, NA))
        t[3, 2] = paste("1", "[Reference]", sep = " ")
        t[3, 3] = paste(d[2, 2], " [", d[2, 3], ", ", d[2, 4], "]", sep = "")
        t[3, 4] = paste(d[5, 2], " [", d[5, 3], ", ", d[5, 4], "]", sep = "")
        t[4, 2] = paste(d[3, 2], " [", d[3, 3], ", ", d[3, 4], "]", sep = "")
        t[4, 3] = paste(d[4, 2], " [", d[4, 3], ", ", d[4, 4], "]", sep = "")
        t[4, 4] = paste(d[6, 2], " [", d[6, 3], ", ", d[6, 4], "]", sep = "")
        t[5, 2] = paste(d[7, 2], " [", d[7, 3], ", ", d[7, 4], "]", sep = "")
        t[5, 3] = paste(d[8, 2], " [", d[8, 3], ", ", d[8, 4], "]", sep = "")
        t[6, 2] = paste(d[9, 2], " [", d[9, 3], ", ", d[9, 4], "]", sep = "")
        t[7, 2] = paste(d[10, 2], " [", d[10, 3], ", ", d[10, 4], "]", sep = "")
        t[8, 2] = paste(d[11, 2], " [", d[11, 3], ", ", d[11, 4], "]", sep = "")
        t[9, 2] = paste(d[12, 2], " [", d[12, 3], ", ", d[12, 4], "]", sep = "")
        caption(t) = paste("Interaction of", beta1, "and", beta2, sep = " ")
    }
    right_padding(t) = 10
    left_padding(t) = 10


    quick_docx(t, file = "interaction_table.docx")
    return(t)
}
