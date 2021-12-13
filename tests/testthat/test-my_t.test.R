test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Should throw an error if x isnt a numeric vector", {
  expect_error(
    my_t.test(c("1", "2"), alternative = "two.sided", mu = 5)
  )
})

test_that("Should throw error if alternative isn't one of the three
          responses.", {
  expect_error(
    my_t.test(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
              alternative = "two", mu = 5)
  )
})

test_that("Should throw error if mu isn't numeric", {
  expect_error(
    my_t.test(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
              alternative = "two.sided", mu = "5")
  )
})

test_that("Returns a list", {
  expect_is(
    my_t.test(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
              alternative = "two.sided", mu = 5),
    "list"
  )
})
