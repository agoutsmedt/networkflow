
  #' Plot the Alluvial
  #'
  #' @description
  #' An easy way to plot the alluvial by using the columns created by the different function of networkflow.
  #'
  #' @param alluv_dt
  #' A data.frame of an alluvial created with [networks_to_alluv()][networkflow::networks_to_alluv()].
  #'
  #' @param intertemporal_cluster_column
  #' The column with the identifier of inter-temporal clusters. If you have used
  #' [add_clusters()][networkflow::add_clusters()] and [merge_dynamic_clusters()][networkflow::merge_dynamic_clusters()],
  #' it is of the form `dynamic_cluster_{clustering_method}`.
  #'
  #' @param node_id
  #' The column with the unique identifier of each node.
  #'
  #' @param window_column
  #' The column of the alluvial with your time window. By default, "window", as created by
  #' [networks_to_alluv()][networkflow::networks_to_alluv()].
  #'
  #' @param color_column
  #' The column with the colors associated to the  categories of `intertemporal_cluster_column`.
  #' By default, "color", as the result of [color_alluvial()][networkflow::color_alluvial()].
  #'
  #' @param color_alluvial
  #' If no color has been set for the categories of `intertemporal_cluster_column`, you
  #' can attribute them colors by setting `color_alluvial` to `TRUE`.
  #' [color_alluvial()][networkflow::color_alluvial()] will be used.
  #'
  #' @param color
  #' If `color_alluvial`is `TRUE`, the parameter `color` will be used to color the categories of the
  #' `intertemporal_cluster_column`. It may be a vector of colors (in a character format)
  #' or a two columns data frame with the first column as
  #' the distinct observations of the `intertemporal_cluster_column` and a second column with the
  #' vector of colors you want to use. If `NULL` colors will be automatically chosen by
  #' [color_alluvial()][networkflow::color_alluvial()]?
  #'
  #'
  #' @param minimize_crossing
  #' If `TRUE`, [minimize_crossing_alluvial()][networkflow::minimize_crossing_alluvial()] is
  #' run to reorder the `intertemporal_cluster_colum` to limit overlapping in the plot.
  #'
  #' @param prepare_label
  #' If `TRUE`, [prepare_label_alluvial()][networkflow::prepare_label_alluvial()] is used
  #' to create a column `label_x` with
  #'
  #' @param cluster_label_column
  #' If `prepare_label` is `TRUE`, [prepare_label_alluvial()][networkflow::prepare_label_alluvial()] is
  #' used and the values of `cluster_label_column` are taken to be displayed as label.
  #' By default, "cluster_label", as it is
  #' the default name of the column created with [name_clusters()][networkflow::name_clusters()].
  #' But you may also use the same column as in `intertemporal_cluster_column`.
  #'
  #' @param print_plot_code
  #' Set to `TRUE` if you want the ggplot2 code to be printing. It is useful if you are not
  #' totally satisfied of the plot and want to manipulate the code yourself.
  #'
  #' @export

plot_alluvial <- function(alluv_dt,
                          intertemporal_cluster_column,
                          node_id,
                          window_column = "window",
                          color_column = "color",
                          color_alluvial = FALSE,
                          color = NULL,
                          minimize_crossing = FALSE,
                          prepare_label = FALSE,
                          cluster_label_column = "cluster_label",
                          print_plot_code = FALSE)
{
  . <- N <- y_alluv <- label_x <- NULL

  if(color_alluvial){
    cli::cli_alert_info("{.emph color_alluvial} is {.code TRUE}. {.fun color_alluvial} is used to color the alluvial.")
    alluv_dt <- color_alluvial(alluv_dt,
                               column_to_color = intertemporal_cluster_column,
                               color = color)
    color_column <- "color"
  }
  if(minimize_crossing){
    cli::cli_alert_info("{.emph minimize_crossing} is {.code TRUE}. {.fun minimize_crossing_alluvial} is used to rearrange cluster order for a better visualisation.")
    alluv_dt <- minimize_crossing_alluvial(alluv_dt = alluv_dt,
                                           intertemporal_cluster_column = intertemporal_cluster_column,
                                           node_id = node_id)
  }

  if(prepare_label){
    cli::cli_alert_info("{.emph prepare_label} is {.code TRUE}. {.fun prepare_label_alluvial} is used to create and position a label for each cluster.")
    alluv_dt <- prepare_label_alluvial(alluv_dt,
                                       cluster_label_column = cluster_label_column)
  }

  window_column <- rlang::ensym(window_column)
  stratum_column <- rlang::ensym(intertemporal_cluster_column)
  alluvium_column <- rlang::ensym(node_id)

  if("label_x" %in% colnames(alluv_dt)) {
    label_column <- rlang::ensym(cluster_label_column)
  }

  if(is.null(color_column)){
    cli::cli_alert_info("{.emph color_alluvial} is {.code FALSE} and no {.emph color_column} has been given.
                        Colors will be determined automatically by {.pkg ggplot2}.")
    color_variable <- stratum_column
  } else{
    if(color_column %in% colnames(alluv_dt)){
      color_variable <- rlang::ensym(color_column)
    } else {
      cli::cli_alert_info("{.emph color_alluvial} is {.code FALSE} and the column {.emph {color_column}} does not exist in the alluvial.
                          Colors will be determined automatically by {.pkg ggplot2}.")
      color_variable <- stratum_column
      color_column <- NULL # That was a false color column that we can remove
    }
  }

  if("minimize_crossing_order" %in% colnames(alluv_dt)){
    alluv_dt[[intertemporal_cluster_column]] <- forcats::fct_reorder(alluv_dt[[intertemporal_cluster_column]],
                                                            alluv_dt[["minimize_crossing_order"]],
                                                            min,
                                                            .desc = TRUE)

  }

  plot_alluvial <- rlang::expr(
    ggplot2::ggplot(alluv_dt, ggplot2::aes(x = !!window_column,
                                           y = y_alluv,
                                           stratum = !!stratum_column,
                                           alluvium = !!alluvium_column,
                                           fill = !!color_variable)) +
      ggalluvial::geom_stratum(alpha = 1, show.legend = FALSE) +
      ggalluvial::geom_flow(show.legend = FALSE) +
      {if(! is.null(color_column)) ggplot2::scale_fill_identity()} +
      {if("label_x" %in% colnames(alluv_dt)) ggrepel::geom_label_repel(ggplot2::aes(label = label_x),
                                                                       stat = ggalluvial::StatStratum,
                                                                       show.legend = FALSE)} +
      ggplot2::theme_minimal())


  if(print_plot_code){
    cli::cli_h1("{.pkg ggplo2} code used to produce the plot")
    print(plot_alluvial)
  }

  eval(plot_alluvial)
}
