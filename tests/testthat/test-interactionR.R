library(interactionR)
## Generate exposure variables
n <- 2000
set.seed(750)
exp1 <- rbinom(n, size = 1, p = 0.2)
set.seed(520)
exp2 <- rbinom(n, size = 1, p = 0.6)

## Make at least one of the exposures preventive for the outcome
b0 <- log(1)
bexp1 <- log(0.4)
bexp2 <- log(1.1)
bexp1exp2 <- log(0.75)

## Generate outcome
ppred <- b0 + bexp1 * exp1 + bexp2 * exp2 + bexp1exp2 * exp1 * exp2
p <- exp(ppred) / (1 + exp(ppred))
set.seed(30)
outcome <- rbinom(n, size = 1, p = p)

## Create dataframe
d <- data.frame(outcome, exp1, exp2)

## Fit a logistic regression model with the data
model.prev <- glm(outcome ~ exp1 * exp2, family = binomial(link = "logit"))


test_that("generates warning when at least one exposure is preventive and recode is set to false", {
  expect_warning(
    interactionR(model.prev,
      exposure_names = c("exp1", "exp2"),
      ci.type = "delta", ci.level = 0.95,
      em = FALSE, recode = FALSE
    ),
    "At least one exposure is preventive. Set argument recode=TRUE for the exposures to be automatically recoded. see Knol et al. (2011) European Journal of Epidemiology, 26(6), 433-438",
    fixed = TRUE
  )
})

test_that("generates error when data is not specified within the model call", {
  expect_error(
    interactionR(model.prev,
      exposure_names = c("exp1", "exp2"),
      ci.type = "delta", ci.level = 0.95,
      em = FALSE, recode = TRUE
    ),
    "Error: Pass the raw data",
    fixed = TRUE
  )
})

test_that("Informs the user when carrying out the recoding", {
  model.prev2 <- glm(outcome ~ exp1 * exp2, family = binomial(link = "logit"), data = d)
  expect_warning(
    interactionR(model.prev2,
      exposure_names = c("exp1", "exp2"),
      ci.type = "delta", ci.level = 0.95,
      em = FALSE, recode = TRUE
    ),
    "Recoding exposures; new reference category",
    fixed = TRUE
  )
})

test_that("confirms that a dataframe was included as part of the returned object", {
  model.prev2 <- glm(outcome ~ exp1 * exp2, family = binomial(link = "logit"), data = d)
  value <- interactionR(model.prev2,
    exposure_names = c("exp1", "exp2"),
    ci.type = "delta", ci.level = 0.95,
    em = FALSE, recode = TRUE
  )
  expect_equal(
    class(value$dat),
    "data.frame"
  )
})

test_that("confirms that the correct dataframe was included as part of the returned object", {
  model.prev2 <- glm(outcome ~ exp1 * exp2, family = binomial(link = "logit"), data = d)
  value <- interactionR(model.prev2,
    exposure_names = c("exp1", "exp2"),
    ci.type = "delta", ci.level = 0.95,
    em = FALSE, recode = TRUE
  )
  expect_equal(
    names(value$dat)[1],
    "outcome"
  )
})
