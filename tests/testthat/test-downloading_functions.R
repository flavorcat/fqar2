test_that("download_assessment works", {
  expect_equal(ncol(download_assessment(25002)), 9)
  expect_error(download_assessment("hi"), "assessment_id must be an integer.")
  expect_error(download_assessment(2.5), "assessment_id must be an integer.")
  expect_error(download_assessment(25003), "The requested assessment is not public")
})



test_that("download_assessment_list works", {
  two_assessments <- download_assessment_list(1, id == 8 | id == 12)
  expect_equal(class(two_assessments), "list")
  expect_equal(length(two_assessments), 2)
  expect_equal(class(two_assessments[[1]]), "data.frame")
  expect_equal(ncol(two_assessments[[1]]), 9)
  expect_error(download_assessment_list(-2), "invalid database")
  expect_warning(download_assessment_list(1, id == "hi"), "No matches found. Empty list returned.")
})



test_that("download_transect works", {
  expect_equal(ncol(download_transect(6322)), 14)
  expect_error(download_transect("hi"), "transect_id must be an integer.")
  expect_error(download_transect(2.5), "transect_id must be an integer.")
})


test_that("download_transect_list works", {
  two_transects <- download_transect_list(1, id == 6570 | id == 6322)
  expect_equal(length(two_transects), 2)
  expect_equal(class(two_transects[[1]]), "data.frame")
  expect_equal(ncol(two_transects[[1]]), 14)
  expect_error(download_transect_list(-2), "invalid database")
  expect_warning(download_transect_list(1, id == "hi"), "No matches found. Empty list returned.")
})



