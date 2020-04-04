#' Publication-ready tables for effect modification and interaction analysis
#'
#' Generates a publication-ready table for effect modification and interaction analysis based on Tables 1 and 3 in Knol and Vanderweele (2012) [\url{https://doi.org/10.1093/ije/dyr218}].
#' Users can modify the function's output like any huxtable object @seealso \code{\link[huxtable]{huxtable}}. The confidence intervals for additive interaction measures will be as selected from the \code{\link{interactionR}} function
#'
#' @param obj An object of class 'interactionR' generated from the main function \code{\link{interactionR}}
#'
#' @return  saves a publication-ready microsoft word Table corresponding to Table 1 or Table 3 respectively in Knol and Vanderweele (2012) to the working directory.
#' It also returns an object of class huxtable corresponding to the saved table.
#'
#' @examples
#' library(interactionR)
#' data(OCdata)
#'
#' ## fit the interaction model
#' model.glm <- glm(oc ~ alc * smk,
#'   family = binomial(link = "logit"),
#'   data = OCdata
#' )
#' ## Then pass the fitted model to the main function
#' value <- interactionR(model.glm,
#'   exposure_names = c("alc", "smk"),
#'   ci.type = "delta", ci.level = 0.95,
#'   em = FALSE, recode = FALSE
#' )
#'
#' ## Use the tabling function to generate a table
#' interactionR_table(value)
#' @import huxtable
#' @export
interactionR_table <- function(obj) {
  if (class(obj) != "interactionR") {
    stop("Argument 'obj' must be an object of class 'interactionR',
             use the interactionR() function to generate such object ")
  }
  beta1 <- obj$exp_names[1]
  beta2 <- obj$exp_names[2]
  em <- obj$analysis
  d <- obj$dframe
  d$Estimates <- as.character(round(d$Estimates, 2))
  d$CI.ll <- as.character(round(d$CI.ll, 2))
  d$CI.ul <- as.character(round(d$CI.ul, 2))
  E1.absent <- paste(beta1, "absent", sep = " ")
  E1.present <- paste(beta1, "present", sep = " ")
  E2.absent <- paste(beta2, "absent", sep = " ")
  E2.present <- paste(beta2, "present", sep = " ")
  WithinStrataEffect1 <- paste("Effect of", beta2, "within the strata of",
    beta1,
    sep = " "
  )
  WithinStrataEffect2 <- paste("Effect of", beta1, "within the strata of",
    beta2,
    sep = " "
  )

  if (grepl("\\blog\\b", obj$call[3]) || grepl("poisson", obj$call[3])) {
    effect_measure = "RR [95% CI]"
  } else {
    effect_measure = "OR [95% CI]"
  }

  if (em) {
    t <- hux(
      c1 = c(
        NA, NA, E1.absent, E1.present, "Multiplicative scale",
        "RERI"
      ), c2 = c(E2.absent, effect_measure, NA, NA, NA, NA),
      c3 = c(E2.present, effect_measure, NA, NA, NA, NA), c4 = c(
        WithinStrataEffect1,
        effect_measure, NA, NA, NA, NA
      )
    )
    t[3, 2] <- paste("1", "[Reference]", sep = " ")
    t[3, 3] <- paste(d[2, 2], " [", d[2, 3], ", ", d[2, 4], "]", sep = "")
    t[3, 4] <- paste(d[5, 2], " [", d[5, 3], ", ", d[5, 4], "]", sep = "")
    t[4, 2] <- paste(d[3, 2], " [", d[3, 3], ", ", d[3, 4], "]", sep = "")
    t[4, 3] <- paste(d[4, 2], " [", d[4, 3], ", ", d[4, 4], "]", sep = "")
    t[4, 4] <- paste(d[6, 2], " [", d[6, 3], ", ", d[6, 4], "]", sep = "")
    t[5, 2] <- paste(d[7, 2], " [", d[7, 3], ", ", d[7, 4], "]", sep = "")
    t[6, 2] <- paste(d[8, 2], " [", d[8, 3], ", ", d[8, 4], "]", sep = "")

    caption(t) <- paste("Modification of the effect of", beta2, "by",
      beta1,
      sep = " "
    )
    t <- theme_article(t)
  } else {
    t <- hux(c1 = c(
      NA, NA, E1.absent, E1.present, WithinStrataEffect2,
      "Multiplicative scale", "RERI", "AP", "SI"
    ), c2 = c(
      E2.absent,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ), c3 = c(
      E2.present,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ), c4 = c(
      WithinStrataEffect1,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ))
    t[3, 2] <- paste("1", "[Reference]", sep = " ")
    t[3, 3] <- paste(d[2, 2], " [", d[2, 3], ", ", d[2, 4], "]", sep = "")
    t[3, 4] <- paste(d[5, 2], " [", d[5, 3], ", ", d[5, 4], "]", sep = "")
    t[4, 2] <- paste(d[3, 2], " [", d[3, 3], ", ", d[3, 4], "]", sep = "")
    t[4, 3] <- paste(d[4, 2], " [", d[4, 3], ", ", d[4, 4], "]", sep = "")
    t[4, 4] <- paste(d[6, 2], " [", d[6, 3], ", ", d[6, 4], "]", sep = "")
    t[5, 2] <- paste(d[7, 2], " [", d[7, 3], ", ", d[7, 4], "]", sep = "")
    t[5, 3] <- paste(d[8, 2], " [", d[8, 3], ", ", d[8, 4], "]", sep = "")
    t[6, 2] <- paste(d[9, 2], " [", d[9, 3], ", ", d[9, 4], "]", sep = "")
    t[7, 2] <- paste(d[10, 2], " [", d[10, 3], ", ", d[10, 4], "]",
      sep = ""
    )
    t[8, 2] <- paste(d[11, 2], " [", d[11, 3], ", ", d[11, 4], "]",
      sep = ""
    )
    t[9, 2] <- paste(d[12, 2], " [", d[12, 3], ", ", d[12, 4], "]",
      sep = ""
    )
    caption(t) <- paste("Interaction of", beta1, "and", beta2, sep = " ")
    t <- theme_article(t)
  }
  right_padding(t) <- 10
  left_padding(t) <- 10

  uprompt <- askYesNo("Do you want to save a Microsoft Word copy of the em/interaction table to your working directory?", default = FALSE)

  if (is.na(uprompt) || !uprompt) {
    print_screen(t)
    invisible(t)
  } else if (uprompt) {
    quick_docx(t, file = "interaction_table.docx")
    print(paste("The file 'interaction_table.docx' has been saved to", getwd(), sep = " "))
    print_screen(t)
    invisible(t)
  }
}
