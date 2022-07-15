community_colors <- function(graph, palette, community_column = "Com_ID"){
  #' Creating Color Attribute For Network Communities
  #'
  #' @description This function takes as an input a tidygraph graph, with a column with a community identifier for
  #' each node and each edge. It attributes to each community a color, depending on a palette chosen.
  #' If the two nodes connected by an edge have a different community, the function mixes the color of the two communities.
  #'
  #' @param graph A tidygraph graph
  #'
  #' @param palette The palette to be used for attributing colors to communities. If you use a palette with less color
  #' than the total number of communities, you will have communities represented by the same color in your visualisation.
  #'
  #' @param community_column The name of the column with the community identifier/number. By defautl "Com_ID", as it is
  #' the name of the column when you use the [leiden_workflow()] function for detecting communities.
  #'
  #' @return The same tidygraph object but with a new `color` column in the nodes side, for each community, and a new
  #' `color_edges` column in the edges side.
  #'
  #' @export
  #' @import magrittr
  #' @import tidygraph
  #' @import dplyr
  #' @import DescTools

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  nodes <- Com_ID <- edges <- to <- from <- color_com_ID_to <- color_com_ID_from <- NULL


  graph <- graph %>%
    activate(nodes) %>%
    rename(Com_ID = {{ community_column }})

  # Setup Colors
  color <- data.table::data.table(
    Com_ID = 1:500,
    color = palette)

  color <- color %>%
    mutate(Com_ID = sprintf("%02d", Com_ID)) %>%
    mutate(Com_ID = as.character(Com_ID))

  # Add color to nodes
  graph <- graph %>%
    activate(nodes) %>%
    left_join(color)

  # Mix color for edges of different color
  graph <- graph %>% # mix color
    activate(edges) %>%
    mutate(color_com_ID_to = .N()$color[to], color_com_ID_from = .N()$color[from]) %>%
    mutate(color_edges = DescTools::MixColor(color_com_ID_to, color_com_ID_from, amount1 = 0.5))

}
