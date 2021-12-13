test_that("non data frame in train and cl throws error", {
  expect_error(
    my_knn_cv(train = "string",
              cl = my_penguins[,c("species")],
              k_nn = 1,
              k_cv = 5)
  )

  expect_error(
    my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                     "bill_depth_mm",
                                     "flipper_length_mm",
                                     "body_mass_g")],
              cl = "test",
              k_nn = 1,
              k_cv = 5)
  )
})

test_that("number of columns in cl not being 1 throws error", {
  expect_error(
    my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                     "bill_depth_mm",
                                     "flipper_length_mm",
                                     "body_mass_g")],
              cl = my_penguins[,c("species", "sex")],
              k_nn = 1,
              k_cv = 5)
  )
})

test_that("k_nn & k_cv not being numeric throws error", {
  expect_error(
    my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                   "bill_depth_mm",
                                   "flipper_length_mm",
                                   "body_mass_g")],
            cl = my_penguins[,c("species")],
            k_nn = "string",
            k_cv = 5)
  )

  expect_error(
    my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                     "bill_depth_mm",
                                     "flipper_length_mm",
                                     "body_mass_g")],
              cl = my_penguins[,c("species")],
              k_nn = 1,
              k_cv = "stirng")
  )
})

test_that("returns a list", {
  expect_is(
    my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                                "bill_depth_mm",
                                                "flipper_length_mm",
                                                "body_mass_g")],
                         cl = my_penguins[,c("species")],
                         k_nn = 1,
                         k_cv = 5),
    "list"
  )
})

test_that("the class output is the same length
          as the train input without na vals", {
  expect_equal(
    length(my_knn_cv(train = my_penguins[,c("bill_length_mm",
                                            "bill_depth_mm",
                                            "flipper_length_mm",
                                            "body_mass_g")],
              cl = my_penguins[,c("species")],
              k_nn = 1,
              k_cv = 5)$class),
    nrow(na.omit(
      my_penguins[,c("bill_length_mm",
                     "bill_depth_mm",
                     "flipper_length_mm",
                     "body_mass_g")]
    ))
  )
})
