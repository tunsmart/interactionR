#' Publication-ready tables for effect modification and interaction analysis
#'
#' Generates a publication-ready table for effect modification and interaction analysis based on Tables 1 and 3 in Knol and Vanderweele (2012) [\doi{10.1093/ije/dyr218}].
#' Users can modify the function's output like any flextable object @seealso \code{\link[flextable]{flextable}}. The confidence intervals for additive interaction measures will be as selected from the \code{\link{interactionR}} function
#'
#' @param obj An object of class 'interactionR' generated from any of the main functions in the \code{\link{interactionR}} package
#'
#' @param file_path An optional user-specified string representing the file path to save the generated Word table instead of the current working directory
#'
#' @return  saves a publication-ready microsoft word Table corresponding to Table 1 or Table 3 respectively in Knol and Vanderweele (2012) to the working directory (with user's permission).
#' It also returns an object of class flextable corresponding to the saved table for further manipulation.
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
#' @import flextable
#' @import officer
#' @export
interactionR_table <- function(obj, file_path = NA) {
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
    effect_measure <- "RR [95% CI]"
  } else {
    effect_measure <- "OR [95% CI]"
  }



  if (em) {
    t <- data.frame(c(
      NA, NA, E1.absent, E1.present, "Multiplicative scale",
      "RERI"
    ), c(NA, effect_measure, NA, NA, NA, NA),
    c(NA, effect_measure, NA, NA, NA, NA), c(
      NA,
      effect_measure, NA, NA, NA, NA
    ),
    stringsAsFactors = FALSE
    )
    names(t) <- c("*", E2.absent, E2.present, WithinStrataEffect1)

    t[3, 2] <- paste("1", "[Reference]", sep = " ")
    t[3, 3] <- paste(d[2, 2], " [", d[2, 3], ", ", d[2, 4], "]", sep = "")
    t[3, 4] <- paste(d[5, 2], " [", d[5, 3], ", ", d[5, 4], "]", sep = "")
    t[4, 2] <- paste(d[3, 2], " [", d[3, 3], ", ", d[3, 4], "]", sep = "")
    t[4, 3] <- paste(d[4, 2], " [", d[4, 3], ", ", d[4, 4], "]", sep = "")
    t[4, 4] <- paste(d[6, 2], " [", d[6, 3], ", ", d[6, 4], "]", sep = "")
    t[5, 2] <- paste(d[7, 2], " [", d[7, 3], ", ", d[7, 4], "]", sep = "")
    t[6, 2] <- paste(d[8, 2], " [", d[8, 3], ", ", d[8, 4], "]", sep = "")


    t2 <- flextable(t)
    t2 <- set_caption(t2, paste("Modification of the effect of", beta1, "and", beta2, sep = " "))
  } else {
    t <- data.frame(c(
      NA, NA, E1.absent, E1.present, WithinStrataEffect2,
      "Multiplicative scale", "RERI", "AP", "SI"
    ), c(
      NA,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ), c(
      NA,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ), c(
      NA,
      effect_measure, NA, NA, NA, NA, NA, NA, NA
    ), stringsAsFactors = FALSE)

    names(t) <- c("*", E2.absent, E2.present, WithinStrataEffect1)

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

    t2 <- flextable(t)
    t2 <- set_caption(t2, paste("Interaction of", beta1, "and", beta2, sep = " "))
  }

  if (!is.na(file_path)) {
    # checks if user pre-specified a file path
    path <- paste(file_path, "interaction.docx", sep = "\\")
    save_as_docx(t2, path = path)
    print(paste("The file 'interaction_table.docx' has been saved to", file_path, sep = " "))
    flextable_to_rmd(t2)
    invisible(t2)
  } else {
    uprompt <- askYesNo("Do you want to save a Microsoft Word copy of the em/interaction table to your working directory?", default = FALSE)
    # Gets permission from the user to save Word file into the working directory
    # Default is NO

    if (is.na(uprompt) || !uprompt) {
      # if permission is declined or NA, working directory is untouched
      flextable_to_rmd(t2)
      invisible(t2)
    } else if (uprompt) {
      # if permission is given (as Yes), table is saved to working directory and user is informed
      save_as_docx(t2, path = "interaction_table.docx")
      print(paste("The file 'interaction_table.docx' has been saved to", getwd(), sep = " "))
      flextable_to_rmd(t2)
      invisible(t2)
    }
  }
}
