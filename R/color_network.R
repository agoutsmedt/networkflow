color_network <- function(graphs = NA,
                        column_to_color = NA,
                        color = NA)
{
  #' Color the nodes and edges of the network according to a given variable.
  #'
  #' @description
  #'
  #' This function will create a color column for the edges and nodes in your network. You can chose a column with a categorical variable
  #' to color nodes based on this particular variable. Edges will be colored by mixing the color of connected nodes.
  #' By default, the function will color your network by using 19 colors from the RColorBrewer package, but you can chose your own color.
  #'
  #' @param graph
  #' A network or a list of networks.
  #'
  #' @param column_to_color
  #' This column designates the categorical variable you want to use to color your nodes.
  #'
  #' @param color
  #' The color you want to use in your networks.
  #' It can be a vector of colors or a two columns data.frame matching the first column as the distinct observations of the column_to_color and a second column with the vector of colors you want to use
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph
  #' @import dplyr
  #' @import magrittr
  #' @importFrom RColorBrewer brewer.pal
  #' @importFrom DescTools MixColor


  . <- to <- from <- color_ID_to <- color_ID_from <- past_id_com <- present_id_com <- share <- N <- same_evolution <- same_origin <- past_id_com.x <- past_id_com.y <- intertemporal_name <- new_cluster_column <- NULL

  # First we gather the distinct occurrences of our column_to_color ordered by number of occurrences
  if(inherits(graphs, "list")){
    list <- TRUE
    variable_list <- lapply(graphs, function(tbl)(tbl %N>% as.data.table %>% .[, .SD, .SDcols = c(column_to_color)]))
    variable_list <- rbindlist(variable_list)

  } else{
    if(inherits(graphs, "tbl_graph")){
      list <- FALSE
      variable_list <- graphs %N>% as.data.table %>% .[, .SD, .SDcols = c(column_to_color)]
      graphs <- list(graphs) #If it's graph alone, make it into the list so the next part of the function work

    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }

  variable_list <- variable_list[, .N, .(column_to_color), env = list(column_to_color = column_to_color)][order(-N)][[1]]
  n_colors <- length(variable_list)



  # Second, we gather a list of color
  if(inherits(color, "vector")){
    color_list <- color
    # Verify that the user have given the correct number of colors.
    if(length(color_list) != n_colors){warning("You have given an incomplete number of colors. You need a vector with ", print(n_colors), " color(s). The function will proceed by repeating provided colors or remove unecessary ones.")}
    main_colors_table <- data.table(
      intertemporal_name = variable_list,
      main_colors = rep(color_list, length.out = n_colors))
  } else {
    if(inherits(color, "data.frame")){
      # Verify that the user have given the correct number of colors.
      if(length(color[[1]]) != n_colors | length(color[[2]]) != n_colors){warning("You have given an incomplete number of colors or an incomplete list of distinct column_to_color values. You need a table with ", print(n_colors), " color(s) and an equal number of distinct values for column_to_color The function will proceed with missing colors in the network.")}
      main_colors_table <- color
    } else {
      warning("Your {.field color} is neither a vector of color characters, nor a data.frame. The function will proceed with a RColorBrewer palettes with 19 distinct colors")
      color_list <- c(RColorBrewer::brewer.pal(7, name = "Dark2"), RColorBrewer::brewer.pal(12, name = "Paired"))
      main_colors_table <- data.table(
        observation = variable_list,
        color = rep(color_list, length.out = n_colors))
    }
  }


  # Third, we color the graphs
  setnames(main_colors_table, "observation", column_to_color)
  graphs <- lapply(graphs, function(tbl) tbl %N>% left_join(main_colors_table)) # %>% mutate(color = ifelse(is.na(color), "grey", color)))
  graphs <- lapply(graphs, function(tbl) tbl %>%
                     activate(edges) %>%
                     mutate(color_ID_to = .N()$color[to], color_ID_from = .N()$color[from]) %>%
                     mutate(color_edges = DescTools::MixColor(color_ID_to, color_ID_from, amount1 = 0.5)) %>%
                     mutate(color_ID_to = NULL, color_ID_from = NULL))

  if(list==FALSE){ # return one graph if this was not a list from the start
    graphs <- graphs[[1]]}

  return(graphs)

}
