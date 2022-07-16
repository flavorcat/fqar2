test_that("assessment_glance works", {

  test <- download_assessment(25002)
  test <- assessment_glance(test)

  expect_equal(ncol(test), 52)
  expect_equal(nrow(test), 1)
  expect_equal(typeof(test$`Total Mean C`), "double")
})

test_that("assessment_inventory works", {
  test <- download_assessment(25002)
  test <- assessment_inventory(test)

  expect_equal(ncol(test), 9)
  expect_equal(typeof(test$C), "double")
})

test_that("assessment_list_glance works", {
  test_vec <- c(25961, 25640)
  test <- download_assessment_list(63, id %in% test_vec)
  test <- assessment_list_glance(test)

  expect_equal(ncol(test), 52)
  expect_equal(typeof(test$`Total Mean C`), "double")
  expect_gt(nrow(test), 1)
})
