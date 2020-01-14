#shinyApp
require(tidycells)
require(miniUI)
require(shiny)
tidycells::visual_crop(tidycells::as_cell_df(head(iris), take_col_names = TRUE))

