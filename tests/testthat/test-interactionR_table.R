library (interactionR)
data (OCdata)

test_that("generates error when input parameter is not of class 'interactionR' ", {

  expect_error(interactionR_table(OCdata),
               "Argument 'obj' must be an object of class 'interactionR'")
})
