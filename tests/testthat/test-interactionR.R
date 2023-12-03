library(interactionR)

## Generate exposure variables
n <- 2000
exp1 <- rbinom(n, size = 1, p = 0.2)
exp2 <- rbinom(n, size = 1, p = 0.6)
exp3 <- runif(n)

## Make at least one of the exposures preventive for the outcome
b0 <- log(1)
bexp1 <- log(0.4)
bexp2 <- log(1.1)
bexp1exp2 <- log(0.75)

## Generate outcome
ppred <- b0 + bexp1 * exp1 + bexp2 * exp2 + bexp1exp2 * exp1 * exp2
p <- exp(ppred) / (1 + exp(ppred))
outcome <- rbinom(n, size = 1, p = p)

## Create dataframe
d <- data.frame(outcome, exp1, exp2, exp3)

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

test_that("generates error when non-suported model is fitted", {
  model <- lm(exp3 ~ exp2, data=d)
  expect_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp2"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    ),
    "The 'model' argument must be a regression model object fit with glm(), coxph() or clogit()",
    fixed = TRUE
  )
})

test_that("generates error when exposure names is not two", {
  model <- glm(exp3 ~ exp2, data=d)
  expect_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp2", "exp3"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    ),
    "Argument 'exposure_names' requires a character vector of the names of the two exposure variables",
    fixed = TRUE
  )
})

test_that("generates error when exposure name cannot be found in model", {
  model <- glm(exp3 ~ exp2*exp1, data=d)
  expect_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp3"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    ),
    "At least one of the exposure names you have identified cannot be found in your model",
    fixed = TRUE
  )
})


test_that("works correctly with numeric exposure variables", {
  model <- glm(exp3 ~ exp1 * exp2, data=d)
  expect_no_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp2"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    )
  )
})

test_that("works correctly with character/factor exposure variables", {
  d$exp1 <- as.factor(exp1)
  d$exp2 <- as.factor(exp2)
  model <- glm(outcome ~ exp1 * exp2, data=d, family = binomial(link = "logit"))
  expect_no_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp2"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    )
  )
})

test_that("works correctly with unknown postfix in exposure variables (using factor levels)", {
  d$exp1 <- factor(exp1, levels = c(0, 1), labels = c("No", "Yes"))
  d$exp2 <- factor(exp2, levels = c(0, 1), labels = c("No", "Yes"))

  model <- glm(outcome ~ exp1 * exp2, data=d, family = binomial(link = "logit"))
  expect_no_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp2"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    )
  )
})


test_that("generates error when exposures interaction cannot be found in the model", {
  model <- glm(outcome ~ exp2*exp3 + exp1, data=d)
  expect_error(
    interactionR(model,
                 exposure_names = c("exp1", "exp3"),
                 ci.type = "delta", ci.level = 0.95,
                 em = FALSE, recode = FALSE
    ),
    "The interaction you specified in your exposure_names argument cannot be found in the model",
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
