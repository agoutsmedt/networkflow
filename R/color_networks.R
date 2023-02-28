
  #' Color Nodes and Edges of Networks
  #'
  #' @description
  #' `r lifecycle::badge("experimental")`
  #'
  #' This function takes as input a tibble graph (from [tidygraph](https://tidygraph.data-imaginist.com/))
  #' or a list of tibble graphs and creates a color column for graphs edges and nodes, for the selected
  #' categorical variable (most likely a cluster column). You may either provide the color palette,
  #' provide a data.frame associating the different values of the categorical variable with
  #' colors, or let the function provide colors (see details).
  #' You can chose a column with a categorical variable
  #'
  #' @param graphs
  #' A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/) or a list of tibble
  #' graphs.
  #'
  #' @param column_to_color
  #' The column of the categorical variable to use to color nodes and edges. For instance,
  #' the `cluster_{clustering_method}` created with [add_clusters()][networkflow::add_clusters()]
  #' or the `dynamic_cluster_{clustering_method}` created with
  #' [merge_dynamic_clusters()][networkflow::merge_dynamic_clusters()].
  #'
  #' @param color
  #' The colors to use. It may be a vector of colors (in a character format)
  #' or a two columns data.frame with the first column as
  #' the distinct observations of the `column_to_color` and a second column with the
  #' vector of colors you want to use.
  #'
  #' @param unique_color_across_list
  #' If set to `TRUE`, in a list of tibble graphs, the same categorical variable will
  #' be considered as a different variable in different graphs and thus receive a different
  #' color. In other words, if set to `TRUE` cluster "01" in two different graphs will
  #' have two different colors. If set to `FALSE` (by default), cluster "01" will have
  #' the same color in every graphs it exists.
  #'
  #' @details
  #' The best practice is to provide a list of colors equals to the number of categorical
  #' variable to color. If you provide more colors, excess colors will not be used. If you
  #' provide less colors, colors will be recycled. If you provide no colors, `palette.colors()`
  #' of base R will be used: the 7 colors of `ggplot2` palette will be used (black is excluded) and
  #' then the 7 colors of `Okabe-Ito` palette (black and gray are excluded). Above 14 colors,
  #' the colors of the two palettes will be recycled.
  #'
  #' @return The same tibble graph or list of tibble graphs as input, but with a new `color`
  #' column for both nodes and edges.
  #'
  #' @examples
  #' library(networkflow)
  #'
  #' nodes <- Nodes_stagflation |>
  #' dplyr::rename(ID_Art = ItemID_Ref) |>
  #' dplyr::filter(Type == "Stagflation")
  #'
  #' references <- Ref_stagflation |>
  #' dplyr::rename(ID_Art = Citing_ItemID_Ref)
  #'
  #' temporal_networks <- build_dynamic_networks(nodes = nodes,
  #' directed_edges = references,
  #' source_id = "ID_Art",
  #' target_id = "ItemID_Ref",
  #' time_variable = "Year",
  #' cooccurrence_method = "coupling_similarity",
  #' time_window = 20,
  #' edges_threshold = 1,
  #' overlapping_window = TRUE,
  #' filter_components = TRUE)
  #'
  #' temporal_networks <- add_clusters(temporal_networks,
  #' objective_function = "modularity",
  #' clustering_method = "leiden")
  #'
  #' temporal_networks <- color_networks(graphs = temporal_networks,
  #' column_to_color = "cluster_leiden",
  #' color = NULL)
  #'
  #' temporal_networks[[1]]
  #'
  #' @export

color_networks <- function(graphs,
                           column_to_color,
                           color = NULL,
                           unique_color_across_list = FALSE)
{
  . <- to <- from <- window <- N <- data <- NULL

  if(inherits(graphs, "list")){
    unique_graph <- FALSE
    variable_list <- lapply(graphs, function(tbl)(tbl %N>%
                                                    data.table::as.data.table() %>%
                                                    .[, .SD, .SDcols = c(column_to_color)])) %>%
      data.table::rbindlist(idcol = "window") %>%
      {if(unique_color_across_list) tidyr::unite(., {{ column_to_color }}, dplyr::everything()) else dplyr::select(., -window)}
  } else{
    if(inherits(graphs, "tbl_graph")){
      unique_graph <- TRUE
      variable_list <- graphs %N>%
        data.table::as.data.table %>%
        .[, .SD, .SDcols = c(column_to_color)]
      graphs <- list(graphs) #If it's graph alone, make it into the list so the next part of the function work

    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }

  variable_list <- variable_list[, .N, .(column_to_color),
                                 env = list(column_to_color = column_to_color)] %>%
    .[order(-N)] %>%
    dplyr::pull(column_to_color)
  n_colors <- length(variable_list)
  cli::cli_alert_info("{.emph unique_color_across_list} has been set to {.val {unique_color_across_list}}. There are {.val {n_colors}} different categories to color.")



  # Second, we gather a list of color
  if(inherits(color, "character")){
    # Verify that the user have given the correct number of colors.
    if(length(color) != n_colors){
      cli::cli_alert_info("The number of colors provided is different from the number of categories to color.
                        You need a vector with {.val {n_colors}} color(s). The function will proceed by repeating provided colors or remove unecessary ones.")
    }
    main_colors_table <- data.table::data.table(
      categories = variable_list,
      color = rep(color, length.out = n_colors))
  } else {
    if(inherits(color, "data.frame")){
      # Verify that the user have given the correct number of colors.
      if(length(color[[1]]) != n_colors){
        cli::cli_alert_info("The length of the data.frame provided is different from the number of categories to color.
                          You need a table with {.val {n_colors}} distinct values for {.emph {column_to_color}} and only one unique color per value of {.emph {column_to_color}}.
                          {.val NA} is used for {.emph color} in case of missing categories to color.")
      }
      main_colors_table <- color
    } else {
      cli::cli_alert_info("{.field color} is neither a vector of color characters, nor a data.frame. We will proceed with base R colors.")
      if(n_colors <= 7){ # 8 colors in ggplot2 but we remove the black one
        color <- grDevices::palette.colors(n_colors + 1, "ggplot2")[-1] # remove the black
        cli::cli_alert_info("We draw {.val {n_colors}} colors from the {.emph ggplot2} palette.")
      } else if(n_colors <= 14){
        color <- c(grDevices::palette.colors(8, "ggplot2")[-1],
                   as.character(grDevices::palette.colors(n_colors - 6, "Okabe-Ito")[-1])) # 7 colors in Okabe-Ito other than Black (the first one) and the same gray as ggplot2
        cli::cli_alert_info("We draw 7 colors from the {.emph ggplot2} palette and {.val {n_colors - 7}} colors from the {.emph Okabe-Ito} palette.")
      } else {
        color <- c(grDevices::palette.colors(7, "ggplot2")[-1],
                   as.character(grDevices::palette.colors(8, "Okabe-Ito")[-1])) # 7 colors in Okabe-Ito other than Black (the first one) and the same gray as ggplot2
        cli::cli_alert_info("We draw 7 colors from the {.emph ggplot2} palette and 7 from the {.emph Okabe-Ito} palette. As more than 14 colors are needed, the colors will be recycled.")
      }
      main_colors_table <- data.table::data.table(
        categories = variable_list,
        color = rep(color, length.out = n_colors))
    }
  }


  # Third, we color the nodes, depending on the type of clusters
  setnames(main_colors_table, "categories", column_to_color, skip_absent = TRUE)
  if(unique_color_across_list){
    main_colors_table <- main_colors_table %>%
      tidyr::separate({{column_to_color}}, c("window", column_to_color), sep = "_") %>%
      tidyr::nest(data = dplyr::all_of(c(column_to_color, "color"))) %>%
      dplyr::pull(data)
    for(i in 1:length(graphs)){
      graphs[[i]] <- graphs[[i]] %N>%
        dplyr::left_join(main_colors_table[[i]], by = column_to_color)
    }
  } else {
    graphs <- lapply(graphs, function(tbl) tbl %N>%
                       dplyr::left_join(main_colors_table, by = column_to_color)) # %>% mutate(color = ifelse(is.na(color), "grey", color)))
  }

  # Coloring Edges
  graphs <- lapply(graphs, function(tbl) tbl %E>%
                     dplyr::mutate(color_edges = mixcolor(.N()$color[to], .N()$color[from], amount1 = 0.5)))

  if(unique_graph==TRUE){ # return one graph if this was not a list from the start
    graphs <- graphs[[1]]}

  return(graphs)
}

# Copy from DescTools package, function MixColor (avoiding one more dependency)
mixcolor <- function (col1, col2, amount1 = 0.5)
{
  .mix <- function(col1, col2, amount1 = 0.5) {
    mix <- apply(grDevices::col2rgb(c(col1, col2), alpha = TRUE), 1,
                 function(x) amount1 * x[1] + (1 - amount1) * x[2])
    do.call("rgb", c(as.list(mix), maxColorValue = 255))
  }
  m <- suppressWarnings(cbind(col1, col2, amount1))
  apply(m, 1, function(x) .mix(col1 = x[1], col2 = x[2], amount1 = as.numeric(x[3])))
}
