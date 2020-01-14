app <- ShinyDriver$new("../")
app$snapshotInit("sample")

app$snapshot()
app$setInputs(`ui_crop-data_del` = "click")
app$snapshot()
app$snapshot()
app$setInputs(`ui_crop-data_crop` = "click")
app$snapshot()
app$setInputs(`ui_crop-data_reset` = "click")
app$snapshot()
app$setInputs(now_tab_main = "Parameters")
app$setInputs(`ui_plot_tune-adaptive_txt_size` = FALSE)
app$snapshot()


source("../../testlib/shiny_after_test_routines.R")
shiny_after_test_routines(app)
