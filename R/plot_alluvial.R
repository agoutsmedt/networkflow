plot_alluvial <- function(alluv_dt = NA,
                          window_column = "Window",
                          y_alluv_column = "y_alluv",
                          stratum_column = "intertemporal_name",
                          alluvium_column = "ID_Art",
                          color = NA,
                          label_column = "intertemporal_name_label",
                          order_column = "minimize_crossing_order")
{
  #' Plot the alluvial after having followed the workflow of networkflow.
  #'
  #' @description
  #' An easy way to plot the alluvial by using the columns created by the different function of networkflow.
  #'
  #' @param alluv_dt
  #' Data.frame of the alluvial created using the networkflow::networks_to_alluv function
  #'
  #' @param window_column
  #' The column with your time windows.
  #'
  #' @param y_alluv_column
  #' The name of the column with y values created with [networks_to_alluv()][networkflow::networks_to_alluv()]
  #'
  #' @param stratum_column
  #' The column with the identifier or label of the inter-temporal cluster. By default, "intertemporal_name", as it is
  #' the name of the column created with [intertemporal_cluster_naming()][networkflow::intertemporal_cluster_naming()].
  #'
  #' @param alluvium_column
  #' The column with the unique identifier of each node.
  #'
  #' @param color
  #' The colors you want to use in your alluvial
  #' It can be a vector of colors or a two columns data.frame with the first column as the distinct observations of the stratum_column and a second column with the vector of colors you want to use
  #'
  #' @param label_column
  #' The name of the column with labels created with [label_alluvial()][networkflow::label_alluvial()]
  #'
  #' @param order_column
  #' The name of the column with the order of the stratum in a way that minimize crossing created with [minimize_crossing_alluvial()][networkflow::minimize_crossing_alluvial()]
  #'
  #' @export


  . <- N <- NULL

  # So ggplot accepts columns defined in the functions
  window_column_plot <- rlang::ensym(window_column)
  y_alluv_column_plot <- rlang::ensym(y_alluv_column)
  stratum_column_plot <- rlang::ensym(stratum_column)
  alluvium_column_plot <- rlang::ensym(alluvium_column)
  label_column_plot <- rlang::ensym(label_column)

  # # List of clusters to color
  # variable_list <- alluv_dt[, .N, .(stratum_column), env = list(stratum_column = stratum_column)][order(-N)][[1]]
  # n_colors <- length(variable_list)
  #
  # if(inherits(color, "vector")){
  #   color_list <- color
  #   # Verify that the user have given the correct number of colors.
  #   if(length(color_list) != n_colors){warning("You have given an incomplete number of colors. You need a vector with ", print(n_colors), " color(s). The function will proceed by repeating provided colors or remove unecessary ones.")}
  #   main_colors_table <- data.table(
  #     intertemporal_name = variable_list,
  #     main_colors = rep(color_list, length.out = n_colors))
  # } else {
  #   if(inherits(color, "data.frame")){
  #     # Verify that the user have given the correct number of colors.
  #     if(length(color[[1]]) != n_colors | length(color[[2]]) != n_colors){warning("You have given an incomplete number of colors or an incomplete list of distinct column_to_color values. You need a table with ", print(n_colors), " color(s) and an equal number of distinct values for column_to_color The function will proceed with missing colors in the network.")}
  #     main_colors_table <- color
  #   } else {
  #     warning("Your {.field color} is neither a vector of color characters, nor a data.frame. The function will proceed with a RColorBrewer palettes with 19 distinct colors")
  #     color_list <- c(RColorBrewer::brewer.pal(7, name = "Dark2"), RColorBrewer::brewer.pal(12, name = "Paired"))
  #     main_colors_table <- data.table(
  #       observation = variable_list,
  #       color = rep(color_list, length.out = n_colors))
  #   }
  # }
  #
  # setnames(main_colors_table, "observation", stratum_column)
  # alluv_plot <- merge(alluv_dt, main_colors_table, by = stratum_column, all.x = TRUE)

  alluv_plot$intertemporal_name <- forcats::fct_reorder(alluv_plot$intertemporal_name, alluv_plot$minimize_crossing_order,min, .desc = TRUE)

  alluv_plot < - ggplot2::ggplot(alluv_plot,
                                 ggplot2::aes(x = !!window_column_plot, y = !!y_alluv_column_plot, stratum = !!stratum_column_plot, alluvium = !!alluvium_column_plot, fill = color, label = !!stratum_column_plot)) +
    ggalluvial::geom_stratum(alpha =1, size=1/10) +
    ggalluvial::geom_flow() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_identity() +
    ggplot2::ggtitle("") +
    ggrepel::geom_label_repel(stat = "stratum", ggplot2::aes(label = !!label_column_plot))

  return(alluv_plot)

}
