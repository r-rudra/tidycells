test_that("TRY shiny works", {
  shinytest::expect_pass(shinytest::testApp("TRY_testApp", compareImages = TRUE))
})
