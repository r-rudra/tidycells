
# attach required functions
source("testlib/shiny_test.R")

test_that("shiny widgets works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("miniUI")
  skip_if_not_installed("shinytest")

  skip_on_cran()

  # it may fail in covr (as of now running)
  # skip_on_covr()

  # skip on travis
  #  as json are differing in fractional values
  #  mostly because of plot brush values in shiny
  skip_on_travis()
  # only test in windows
  # later : brush related tests may be done separately
  skip_on_os("linux")
  skip_on_os("mac")
  skip_on_os("solaris")

  inst_deps()

  if (!shinytest::dependenciesInstalled()) {
    skip("shinytest - dependencies not installed")
  }

  nowats <- getOption("AutoUnloadShiny")
  options(AutoUnloadShiny = FALSE)

  cd <- iris %>%
    head() %>%
    as_cell_df(take_col_names = TRUE) %>%
    basic_classifier()
  ca <- analyze_cells(cd)

  # need a interactive session always
  if (!interactive()) {
    expect_error(shiny_check())
  }

  # load shiny and miniUI
  shiny_check(force_load = TRUE)

  app_crop <- temp_app_create(
    shiny_app_crop(cd, test_this = TRUE),
    name = "crop"
  )

  app_va_classify <- temp_app_create(
    shiny_app_va_classify(cd, test_this = TRUE),
    name = "va_classify"
  )

  app_data_block <- temp_app_create(
    shiny_app_data_block(ca, test_this = TRUE),
    name = "data_block"
  )

  app_omod <- temp_app_create(
    shiny_app_orientation_modification(ca, test_this = TRUE),
    name = "omod"
  )

  # "ui_traceback-click_traceback": null is coming without reproducible options
  # disabled it
  check_shiny_app_traceback <- FALSE
  # optional module based on DT
  if (rlang::is_installed("DT") & check_shiny_app_traceback) {
    app_traceback <- temp_app_create(
      shiny_app_traceback(ca, test_this = TRUE),
      name = "traceback"
    )
  }


  # if testing for images enable this
  # also set when plotly not present (screenshots are with plotly)
  # like test_image <- TRUE
  # else set test_image <- FALSE
  test_image <- FALSE

  if (test_image) {
    image_test(enable_now = TRUE)
    expect_true(image_test())
  } else {
    expect_false(image_test())
  }
  # untar recoded tests
  # this is because GitHub Pull and Push is changing json sometimes
  uta <- untar_tests()

  test_temp_app(app_crop, untar_adds = uta)
  test_temp_app(app_va_classify, untar_adds = uta)
  test_temp_app(app_data_block, untar_adds = uta)
  test_temp_app(app_omod, untar_adds = uta)
  # optional module based on DT
  if (rlang::is_installed("DT") & check_shiny_app_traceback) {
    # don't check image for DT
    # as DT actions are not recorded properly
    test_temp_app(app_traceback, test_img = FALSE, untar_adds = uta)
  } else {
    if (!rlang::is_installed("DT")) {
      expect_error(visual_traceback(ca), "DT")
    }
  }

  # clean tar
  clean_untars(uta)


  # reset option
  options(AutoUnloadShiny = nowats)
})
