

merge_admaps <- function(x, y) {
  this <- list()
  this$raw_map <- x$raw_map %>%
    bind_rows(y$raw_map) %>%
    distinct()
  this$map <- x$map %>%
    bind_rows(y$map) %>%
    distinct()
  this$dimension_analysis <- list()
  this$dimension_analysis$data_gid_dim <- x$dimension_analysis$data_gid_dim %>%
    bind_rows(y$dimension_analysis$data_gid_dim) %>%
    distinct()
  this$dimension_analysis$attr_data_dim <- x$dimension_analysis$attr_data_dim %>%
    bind_rows(y$dimension_analysis$attr_data_dim) %>%
    distinct()
  this
}
