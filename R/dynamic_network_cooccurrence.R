dynamic_network_cooccurrence <- function(nodes = NULL,
                                         directed_edges = NULL,
                                         source_column = NULL,
                                         target_column = NULL,
                                         time_variable = NULL,
                                         time_window = NULL,
                                         cooccurrence_method = c("coupling_angle","coupling_strength","coupling_similarity"),
                                         overlapping_window = FALSE,
                                         edges_threshold = 1,
                                         compute_size = FALSE,
                                         keep_singleton = FALSE,
                                         verbose = TRUE)
{
  #' Creating Dynamic Networks from a List of Nodes and Directed Edges
  #'
  #' @description
  #' `r lifecycle::badge("deprecated")`
  #'
  #' This function was implemented in an earlier version of the development of this package.
  #' It has been replaced by `networkflow::build_dynamic_networks()`.
  #' This function creates one or several tidygraph networks from a table of nodes and its
  #' directed edges. For instance, for bibliometric networks, you can give a list of
  #' articles and the list of the references these articles cite. You can use it to
  #' build a single network or multiple networks over different time windows.
  #'
  #' @param nodes
  #' The table with all the nodes and their metadata. For instance, if your nodes are
  #' articles, this table is likely to contain the year of publication, the name of the authors,
  #' the title of the article, etc... The table must have one row per node.
  #'
  #' @param directed_edges
  #' The table with of all the elements to which your nodes are connected. If your nodes are
  #' articles, the `directed_edges` table can contain the list of the references cited
  #' by these articles, the authors that have written these articles, or the affiliations
  #' of the authors of these articles.
  #'
  #' @param source_column
  #' The column with the unique identifier of each node. For instance, for a bibliographic
  #' coupling network, the id of your citing documents.
  #'
  #' @param target_column
  #' The column with the unique identifier of each element connected to the node (for
  #' instance, the identifier of the reference cited by your node if the node is an article).
  #'
  #' @param time_variable
  #' The column with the temporal variable you want to use to build your windows for the
  #' succession of networks. By default, `time_variable` is `NULL` and the function
  #' will only build one network without taking into account any temporal variable.
  #'
  #' @param time_window
  #' The length of your network relatively of the unity of the `time_variable` column. If you
  #' use a variable in years as `time_variable` and you set `time_window` at 5, the function
  #' will build network on five year windows. By default, `time_window` is `NULL` and the
  #' function will only build one network.
  #'
  #' @param overlapping_window
  #' Set to `FALSE` by default. If set to `TRUE`, and if `time_variable` and `time_window` not
  #' `NULL`, the function will create a succession of networks for moving time windows. The windows are
  #' moving one unit per one unit of the `time_variable`. For instance, for years, if `time_window`
  #' set to 5, it creates networks for successive time windows like 1970-1974, 1971-1975, 1972-1976, etc.
  #'
  #' @param cooccurrence_method
  #' Choose a cooccurrence method to build your indirect edges table. The function propose
  #' three methods that depends on the [biblionetwork package](https://agoutsmedt.github.io/biblionetwork/)
  #' and three methods that are implemented in it:
  #'
  #' - the coupling angle measure (see [biblionetwork::biblio_coupling()] for documentation);
  #' - the coupling strength measure ([biblionetwork::coupling_strength()]);
  #' - the coupling similarity measure ([biblionetwork:: coupling_similarity()]).
  #'
  #' @param edges_threshold
  #' Threshold value for building your edges. With a higher threshold, only the stronger links
  #' will be kept. See the [biblionetwork package](https://agoutsmedt.github.io/biblionetwork/)
  #' documentation and the `cooccurrence_method` parameter.
  #'
  #' @param compute_size
  #' Set to `FALSE` by default. If `TRUE`, the function uses the `directed_edges` data
  #' to calculate how many directed edges a node receives (as a target). If `directed_edges`
  #' is a table of direct citations, the functions calculates the number of time a node
  #' is cited by the other nodes. You need to have the `target_column` in the `nodes` table
  #' to make the link with the targetted nodes in the `directed_edges` table.
  #'
  #' @param keep_singleton
  #' Set to `FALSE` by default. If `TRUE`, the function removes the nodes that have no
  #' undirected edges, i.e. no cooccurrence with any other nodes. In graphical terms,
  #' these nodes are alone in the network, with no link with other nodes.
  #'
  #' @param verbose
  #' Set to `FALSE` if you don't want the function to display different sort of information.
  #'
  #' @return If `time_window` is `NULL`, the function computes only
  #' one network and return a tidygraph object built with [tbl_graph()][tidygraph::tbl_graph()].
  #' If `time_variable` and `time_window` are not `NULL`, the function returns a list
  #' of tidygraph networks, for each time window.
  #'
  #' @examples
  #' nodes <- Nodes_stagflation |>
  #' dplyr::rename(ID_Art = ItemID_Ref) |>
  #' dplyr::filter(Type == "Stagflation")
  #'
  #' references <- Ref_stagflation |>
  #' dplyr::rename(ID_Art = Citing_ItemID_Ref)
  #'
  #' temporal_networks <- dynamic_network_cooccurrence(nodes = nodes,
  #' directed_edges = references,
  #' source_column = "ID_Art",
  #' target_column = "ItemID_Ref",
  #' time_variable = "Year",
  #' cooccurrence_method = "coupling_similarity",
  #' time_window = NULL,
  #' edges_threshold = 1,
  #' compute_size = FALSE,
  #' keep_singleton = FALSE,
  #' overlapping_window = TRUE)
  #'
  #' @export

  size <- node_size <- N <- method <- NULL
  lifecycle::deprecate_warn("0.1.0", "dynamic_network_cooccurrence()", "build_dynamic_networks()")


  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)
  cooccurrence_methods <- c("coupling_angle","coupling_strength","coupling_similarity")

  # Checking various problems: lacking method,
  if(!cooccurrence_method %in% cooccurrence_methods)
    stop('You did not choose a proper method for coupling computation. You have to choose between:\n - coupling_angle\n - coupling_strength\n - coupling_similarity')

  if(nodes[, .N, source_column, env = list(source_column=source_column)][N > 1, .N] > 0){
    stop(paste0("Some identifiers in your column '", source_column, "' in your node table are not unique. You need only one row per node."))
  }

  if(! is.null(time_window) & is.null(time_variable)){
    stop("You cannot have a 'time_window' if you don't give any column with a temporal variable. Put a column in 'time_variable' or remove the 'time_window'.")
  }

  Nodes_coupling <- data.table::copy(nodes)
  Nodes_coupling[, source_column := as.character(source_column),
                 env = list(source_column = source_column)]

  if(is.null(time_variable)){
    time_variable <- "fake_column"
    Nodes_coupling[, time_variable := 1,
                   env = list(time_variable = time_variable)]
  }

  if(! target_column %in% colnames(Nodes_coupling) & compute_size == TRUE)
  {
    stop(paste0("You don't have the column '", target_column,"' in your nodes table. Set 'compute_size' to FALSE."))
  }

  if(compute_size == TRUE){
    Nodes_coupling[, target_column := as.character(target_column),
                   env = list(target_column = target_column)]
  }

  Edges <- data.table::copy(directed_edges)
  Edges <- Edges[, .SD, .SDcols = c(source_column, target_column)]
  Edges[, c(source_column, target_column) := lapply(.SD, as.character), .SDcols = c(source_column, target_column)]

  ######################### Dynamics networks *********************

  # Find the time_window
  Nodes_coupling <- Nodes_coupling[order(time_variable), env = list(time_variable = time_variable)]
  Nodes_coupling[, time_variable := as.integer(time_variable),
                 env = list(time_variable = time_variable)]

  first_year <- Nodes_coupling[, min(as.integer(time_variable)),
                               env = list(time_variable = time_variable)]
  last_year <- Nodes_coupling[, max(as.integer(time_variable)),
                              env = list(time_variable = time_variable)]

  if(!is.null(time_window)){
    if(last_year - first_year + 1 < time_window){
      stop("ERROR: your time window is greater than the number of distinct values for time_variable")
    }
  }

  if(is.null(time_window)){
    all_years <- first_year
    time_window <- last_year - first_year + 1
  } else {
    if(overlapping_window == TRUE){
      last_year <- last_year - time_window + 1
      all_years <- first_year:last_year
    } else {
      all_years <- seq(first_year, last_year, by = time_window)
      if(all_years[length(all_years)] + (time_window - 1) > last_year){
        warning(paste0("Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is ", last_year, " but the upper limit of your last time window is ",
                all_years[length(all_years)] + (time_window - 1), "."))
      }
    }
  }

  # Prepare our list
  tbl_coup_list <- list()

  for (Year in all_years) {
    nodes_of_the_year <- Nodes_coupling[time_variable >= Year & time_variable < (Year + time_window),
                                        env = list(time_variable = time_variable, Year = Year)]

    if(time_variable != "fake_column"){
      nodes_of_the_year[, time_window := paste0(Year, "-", Year + time_window - 1),
                        env = list(Year = Year)]
      if(verbose == TRUE) cat(paste0("Creation of the network for the ", Year, "-", Year + time_window - 1, " window.\n"))
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }

    edges_of_the_year <- Edges[source_column %in% nodes_of_the_year[, source_column],
                               env = list(source_column = source_column)]

    # size of nodes
    if(compute_size == TRUE){
      nb_cit <- edges_of_the_year[source_column %in% nodes_of_the_year[, source_column], .N, target_column,
                                  env = list(source_column = source_column, target_column = target_column)]
      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"

      if("node_size" %in% colnames(Nodes_coupling) == TRUE)
      {
        warning("You already have a column name 'node_size'. Columns will be duplicated now.")
      }
      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year,
                                 nb_cit,
                                 by = target_column,
                                 all.x = TRUE)
      nodes_of_the_year[is.na(node_size), node_size := 0]
    }

    # coupling
    biblio_functions <- data.table::data.table(biblio_function = c(rlang::expr(biblionetwork::biblio_coupling),
                                                                   rlang::expr(biblionetwork::coupling_strength),
                                                                   rlang::expr(biblionetwork::coupling_similarity)),
                                               method = c("coupling_angle",
                                                          "coupling_strength",
                                                          "coupling_similarity"))
    biblio_function <- biblio_functions[method == cooccurrence_method][["biblio_function"]][[1]]
    edges_of_the_year <- rlang::expr((!!biblio_function)(dt = edges_of_the_year,
                                                         source = rlang::inject(source_id),
                                                         ref = rlang::inject(target_id),
                                                         weight_threshold = rlang::inject(edges_threshold))) %>%
      eval()

    # remove nodes with no edges
    if(keep_singleton==FALSE){
      if(verbose == TRUE) cat("We remove the nodes that are alone with no edge. \n\n")
      nodes_of_the_year <- nodes_of_the_year[source_column %in% edges_of_the_year$from | source_column %in% edges_of_the_year$to, env=list(source_column=source_column)]
    }

    # make tbl
    if(length(all_years) == 1){
      tbl_coup_list <- tidygraph::tbl_graph(nodes = nodes_of_the_year,
                                            edges = edges_of_the_year,
                                            directed = FALSE,
                                            node_key = source_column)
    } else {
      tbl_coup_list[[paste0(Year, "-", Year + time_window - 1)]] <- tidygraph::tbl_graph(nodes = nodes_of_the_year,
                                                                                         edges = edges_of_the_year,
                                                                                         directed = FALSE,
                                                                                         node_key = source_column)
    }
  }

  return (tbl_coup_list)
}
