# GLOBAL SETTINGS FOR APPS

statsapps_plot_base_size <- 16

statsapps_plot_theme <- function(base_size = statsapps_plot_base_size) {
  ggplot2::theme_classic(base_size = base_size)
}

statsapps_plot_theme_code <- function(base_size = statsapps_plot_base_size) {
  paste0("theme_classic(base_size = ", base_size, ")")
}
