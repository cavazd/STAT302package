test_that("Input that can't be coerced to formula throws error", {
  expect_error(my_lm("string", my_penguins))
})

test_that("Output is a matrix", {
  expect_is(
    my_lm(flipper_length_mm ~ body_mass_g, data = my_penguins),
    class = "matrix"
  )
})

test_that("Any input for data that is not a data.frame throws error", {
  expect_error(
    my_lm(flipper_length_mm ~ body_mass_g, data = "my_penguins")
  )
})
