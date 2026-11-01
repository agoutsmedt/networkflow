# Column names used with non-standard evaluation (data.table, dplyr, ggplot2)
# that R CMD check would otherwise report as undefined global variables. Add
# any new one here.
utils::globalVariables(c(
  ".", "..weight", ".from", ".to", "cluster_label", "color", "edges", "from",
  "label_x", "label_y", "nodes", "to", "tooltip", "weight", "x", "y"
))
