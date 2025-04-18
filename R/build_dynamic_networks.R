#' Creating One or Multiple Networks from a List of Nodes and Directed Edges
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `build_network()` creates a network from a table of nodes and its
#' directed edges. That is a special case of the more general `build_dynamic_networks()`.
#' This function creates one or several tibble graphs (built with
#' [tidygraph](https://tidygraph.data-imaginist.com/)) from a table of nodes and its
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
#' @param source_id
#' The quoted name of the column with the unique identifier of each node. For instance,
#' for a bibliographic coupling network, the id of your citing documents. It corresponds
#' to the `source` argument of [biblionetwork](https://agoutsmedt.github.io/biblionetwork/)
#' functions.
#'
#' @param target_id
#' The quoted name of the column with the unique identifier of each element connected to the node (for
#' instance, the identifier of the reference cited by your node if the node is an article).
#' It corresponds to the `ref` argument of
#' [biblionetwork](https://agoutsmedt.github.io/biblionetwork/) functions.
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
#' @param backbone_method Method used to extract the network backbone. Choose between:
#' - `"structured"`: uses cooccurrence measures from the [biblionetwork](https://agoutsmedt.github.io/biblionetwork/) package;
#' - `"statistical"`: uses statistical models from the [backbone](https://github.com/djmurphy533/backbone) package.
#'
#' @param statistical_method For `backbone_method = "statistical"`, select the null model: one of
#' `"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedfrow"`, `"fixedcol"`.
#'
#' @param alpha Significance threshold for statistical backbone extraction. Required if
#' `backbone_method = "statistical"`.
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
#' is cited by the other nodes. You need to have the `target_id` in the `nodes` table
#' to make the link with the targetted nodes in the `directed_edges` table.
#'
#' @param keep_singleton
#' Set to `FALSE` by default. If `TRUE`, the function removes the nodes that have no
#' undirected edges, i.e. no cooccurrence with any other nodes. In graphical terms,
#' these nodes are alone in the network, with no link with other nodes.
#'
#' @param filter_components
#' Set to  `TRUE` if you want to run `networkflow::filter_components()`
#' to filter the components of the network(s) and keep only the biggest component(s). If
#' you don't change the defaults parameters of `networkflow::filter_components()`,
#' it will keep only the main component.
#'
#' @param ...
#' Additional arguments from `networkflow::filter_components()`.
#'
#' @param verbose
#' Set to `FALSE` if you don't want the function to display different sort of information.
#'
#' @details `build_network()` has been added for convenience but it is just
#' a special case of the more general `build_dynamic_networks()`, with
#'
#'
#'
#' @return If `time_window` is `NULL`, the function computes only
#' one network and return a tidygraph object built with [tbl_graph()][tidygraph::tbl_graph()].
#' If `time_variable` and `time_window` are not `NULL`, the function returns a list
#' of tidygraph networks, for each time window.
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
#' overlapping_window = TRUE)
#'
#' temporal_networks[[1]]
#'
#' @export
build_dynamic_networks <- function(nodes,
                                   directed_edges,
                                   source_id,
                                   target_id,
                                   time_variable = NULL,
                                   time_window = NULL,
                                   cooccurrence_method = c("coupling_angle","coupling_strength","coupling_similarity"),
                                   overlapping_window = FALSE,
                                   edges_threshold = 1,
                                   compute_size = FALSE,
                                   keep_singleton = FALSE,
                                   filter_components = FALSE,
                                   ...,
                                   verbose = TRUE)
{
  size <- node_size <- N <- method <- NULL


  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)
  cooccurrence_methods <- c("coupling_angle","coupling_strength","coupling_similarity")

  # Checking various problems: lacking method,
  if(length(cooccurrence_method) > 1){
    cli::cli_abort(c(
      "You did not choose any method for cooccurrence computation. You have to choose between: ",
      "*" = "\"coupling_angle\";",
      "*" = "\"coupling_strength\";",
      "*" = "\"coupling_similarity\"."))
  }
  if(!cooccurrence_method %in% cooccurrence_methods){
    cli::cli_abort(c(
      "You did not choose an existing method for cooccurrence computation. You have to choose between: ",
      "*" = "\"coupling_angle\";",
      "*" = "\"coupling_strength\";",
      "*" = "\"coupling_similarity\"."))
  }
  if(nodes[, .N, source_id, env = list(source_id=source_id)][N > 1, .N] > 0){
    cli::cli_alert_warning("Some identifiers in your column {.field {source_id}} in your nodes table are not unique. You need only one row per node.")
  }

  if(! is.null(time_window) & is.null(time_variable)){
    cli::cli_abort("You cannot have a {.emph time_window} if you don't give any column with a temporal variable.
                          Put a column in {.emph time_variable} or remove the {.emph time_window}.")
  }

  # giving information on the method

  if(verbose == TRUE){
    cli::cli_alert_info("The method use for co-occurence is the {.emph {cooccurrence_method}} method.")
    cli::cli_alert_info("The edge threshold is: {.val {edges_threshold}}.")
    if(keep_singleton == FALSE) cli::cli_alert_info("We remove the nodes that are alone with no edge. \n\n")
  }

  # let's extract the information we need
  Nodes_coupling <- data.table::copy(nodes)
  Nodes_coupling[, source_id := as.character(source_id),
                 env = list(source_id = source_id)]

  if(is.null(time_variable)){
    time_variable <- "fake_column"
    Nodes_coupling[, time_variable := 1,
                   env = list(time_variable = time_variable)]
  }

  if(! target_id %in% colnames(Nodes_coupling) & compute_size == TRUE)
  {
    cli::cli_abort("You don't have the column {.field {target_id}} in your nodes table. Set {.emph compute_size} to {.val FALSE}.")
  }

  if(compute_size == TRUE){
    Nodes_coupling[, target_id := as.character(target_id),
                   env = list(target_id = target_id)]
  }

  Edges <- data.table::copy(directed_edges)
  Edges <- Edges[, .SD, .SDcols = c(source_id, target_id)]
  Edges[, c(source_id, target_id) := lapply(.SD, as.character), .SDcols = c(source_id, target_id)]

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
      cli::cli_alert_warning("Your time window is larger than the number of distinct values of {.field {time_variable}}")
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
        cli::cli_warn("Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is {.val {last_year}}, but the upper limit of your last time window is
                {.val {all_years[length(all_years)] + (time_window - 1)}}.")
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
      if(verbose == TRUE) cli::cli_h1("Creation of the network for the {.val {Year}}-{.val {Year + time_window - 1}} window.")
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }

    edges_of_the_year <- Edges[source_id %in% nodes_of_the_year[, source_id],
                               env = list(source_id = source_id)]

    # size of nodes
    if(compute_size == TRUE){
      nb_cit <- edges_of_the_year[source_id %in% nodes_of_the_year[, source_id], .N, target_id,
                                  env = list(source_id = source_id, target_id = target_id)]
      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"

      if("node_size" %in% colnames(Nodes_coupling) == TRUE)
      {
        cli::cli_warn("You already have a column name {.field node_size}. The content of the column will be replaced.")
      }
      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year,
                                                        nb_cit,
                                                        by = target_id,
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
      nodes_of_the_year <- nodes_of_the_year[source_id %in% edges_of_the_year$from | source_id %in% edges_of_the_year$to, env=list(source_id=source_id)]
    }

    # make tbl
    if(length(all_years) == 1){
      tbl_coup_list <- tidygraph::tbl_graph(nodes = nodes_of_the_year,
                                            edges = edges_of_the_year,
                                            directed = FALSE,
                                            node_key = source_id)
    } else {
      tbl_coup_list[[paste0(Year, "-", Year + time_window - 1)]] <- tidygraph::tbl_graph(nodes = nodes_of_the_year,
                                                                                         edges = edges_of_the_year,
                                                                                         directed = FALSE,
                                                                                         node_key = source_id)
    }
  }
  if(filter_components == TRUE){
    tbl_coup_list <- filter_components(tbl_coup_list, ...)
  }
  return (tbl_coup_list)
}

#' @rdname build_dynamic_networks
#' @export

build_dynamic_networks2 <- function(nodes,
                                    directed_edges,
                                    source_id,
                                    target_id,
                                    time_variable = NULL,
                                    time_window = NULL,
                                    backbone_method = c("statistical", "structured"),
                                    statistical_method = c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol"),
                                    alpha = NULL,
                                    coupling_measure = c("coupling_angle", "coupling_strength", "coupling_similarity"),
                                    edges_threshold = 1,
                                    overlapping_window = FALSE,
                                    compute_size = FALSE,
                                    keep_singleton = FALSE,
                                    filter_components = FALSE,
                                    ...,
                                    verbose = TRUE) {

  size <- node_size <- N <- method <- NULL

  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)

  # Checking the methods
  backbone_methods = c("statistical", "structured")

  coupling_measures <- c("coupling_angle",
                         "coupling_strength",
                         "coupling_similarity")

  statistical_methods <- c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol")


  if (length(backbone_method) > 1) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\"."
      )
    )
  }

  if (!backbone_method %in% backbone_methods) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\";"
      )
    )
  }

  # check various setting for the structured methods

  if (backbone_method == "structured") {

    # Checking various problems: lacking method,
    if (length(coupling_measure) > 1) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }

    if (!coupling_measure %in% coupling_measures) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }

  }

  # check various setting for the statistical methods
  if (backbone_method == "statistical") {
    # check if a model is given
    if (length(statistical_method) > 1) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }

    if (!statistical_method %in% statistical_methods) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }

    # check if alpha is given
    if (is.null(alpha)) {
      cli::cli_abort(
        "For statistical backbone extraction, you have to choose a significance level alpha."
      )
    }

  }

  # warning if the source_id is not unique
  if (nodes[, .N, source_id, env = list(source_id = source_id)][N > 1, .N] > 0) {
    cli::cli_alert_warning(
      "Some identifiers in your column {.field {source_id}} in your nodes table are not unique. You need only one row per node."
    )
  }

  # check settings for intertemporal networks
  if (!is.null(time_window) & is.null(time_variable)) {
    cli::cli_abort(
      "You cannot have a {.emph time_window} if you don't give any column with a temporal variable. Put a column in {.emph time_variable} or remove the {.emph time_window}."
    )
  }

  # VERBOSE

  if (verbose == TRUE) {
    if (length(statistical_method > 0))
      cli::cli_alert_info(paste(
        "We extract the network backbone using the",
        backbone_method,
        "method."
      ))

    if (keep_singleton == FALSE)
      cli::cli_alert_info("Keep_singleton == FALSE: removing the nodes that are alone with no edge. \n\n")
  }


  # CHECKING THE DATA

  # NODES
  nodes_coupling <- data.table::copy(nodes)
  nodes_coupling[, source_id := as.character(source_id), env = list(source_id = source_id)]

  if (is.null(time_variable)) {
    time_variable <- "fake_column"
    nodes_coupling[, time_variable := 1, env = list(time_variable = time_variable)]
  }


  if (!target_id %in% colnames(nodes_coupling) &
      compute_size == TRUE) {
    cli::cli_abort(
      "You don't have the column {.field {target_id}} in your nodes table. Set {.emph compute_size} to {.val FALSE}."
    )
  }

  if (compute_size == TRUE) {
    nodes_coupling[, target_id := as.character(target_id), env = list(target_id = target_id)]
  }

  # EDGES

  edges <- data.table::copy(directed_edges)
  edges <- edges[, .SD, .SDcols = c(source_id, target_id)] # we keep only the columns we need
  edges <- unique(edges) # in case there are some duplicates
  edges[, c(source_id, target_id) := lapply(.SD, as.character), .SDcols = c(source_id, target_id)] # we need to have character columns

  ######################### Dynamics networks *********************

  # define the time window
  nodes_coupling <- nodes_coupling[order(time_variable), env = list(time_variable = time_variable)]
  nodes_coupling[, time_variable := as.integer(time_variable), env = list(time_variable = time_variable)]

  first_year <- nodes_coupling[, min(as.integer(time_variable)), env = list(time_variable = time_variable)]
  last_year <- nodes_coupling[, max(as.integer(time_variable)), env = list(time_variable = time_variable)]

  if (!is.null(time_window)) {
    if (last_year - first_year + 1 < time_window) {
      cli::cli_alert_warning(
        "Your time window is larger than the number of distinct values of {.field {time_variable}}"
      )
    }
  }

  if (is.null(time_window)) {
    all_years <- first_year
    time_window <- last_year - first_year + 1
  } else {
    if (overlapping_window == TRUE) {
      last_year <- last_year - time_window + 1
      all_years <- first_year:last_year
    } else {
      all_years <- seq(first_year, last_year, by = time_window)
      if (all_years[length(all_years)] + (time_window - 1) > last_year) {
        cli::cli_warn(
          "Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is {.val {last_year}}, but the upper limit of your last time window is
                {.val {all_years[length(all_years)] + (time_window - 1)}}."
        )
      }
    }
  }

  # Prepare our list
  tbl_coup_list <- list()

  for (year in all_years) {
    nodes_of_the_year <- nodes_coupling[time_variable >= year &
                                          time_variable < (year + time_window), env = list(time_variable = time_variable, year = year)]

    if (time_variable != "fake_column") {
      nodes_of_the_year[, time_window := paste0(year, "-", year + time_window - 1), env = list(year = year)]

      if (verbose == TRUE)
        cli::cli_h1(
          "Generation of the network for the {.val {year}}-{.val {year + time_window - 1}} time window."
        )
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }

    edges_of_the_year <- edges[source_id %in% nodes_of_the_year[, source_id], env = list(source_id = source_id)]

    # size of nodes
    if (compute_size == TRUE) {
      nb_cit <- edges_of_the_year[source_id %in% nodes_of_the_year[, source_id], .N, target_id, env = list(source_id = source_id, target_id = target_id)]

      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"

      if ("node_size" %in% colnames(nodes_coupling) == TRUE)
      {
        cli::cli_warn(
          "You already have a column name {.field node_size}. The content of the column will be replaced."
        )
      }

      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year,
                                                        nb_cit,
                                                        by = target_id,
                                                        all.x = TRUE)

      nodes_of_the_year[is.na(node_size), node_size := 0]
    }



    # backbone

    if (backbone_method == "statistical") {
      # prepare backbone function
      backbone_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(backbone::sdsm),
            rlang::expr(backbone::fdsm),
            rlang::expr(backbone::fixedfrow),
            rlang::expr(backbone::fixedcol),
            rlang::expr(backbone::fixedfill)
          ),
          method = c("sdsm", "fdsm", "fixedfrow", "fixedcol", "fixedfill")
        )

      backbone_functions <- backbone_functions[method == statistical_method][["biblio_function"]][[1]]

      # Evaluate the expression and catch internal errors to backbone package

      tryCatch({
        # using backbone with edgelist is simpler but lead to error in backbone function
        edges_of_the_year <-
          rlang::expr((!!backbone_functions)(
            B = as.data.frame(edges_of_the_year),
            alpha = rlang::inject(alpha)
          )) %>%
          eval() %>%
          data.table::as.data.table()

      }, error = function(e) {
        stop(
          "The backbone function failed with an error. Read the backbone documentation for more information. Error message: ",
          e$message
        )
      })
    }


    # coupling
    if (backbone_method == "structured") {
      biblio_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(biblionetwork::biblio_coupling),
            rlang::expr(biblionetwork::coupling_strength),
            rlang::expr(biblionetwork::coupling_similarity)
          ),
          method = c(
            "coupling_angle",
            "coupling_strength",
            "coupling_similarity"
          )
        )

      biblio_function <- biblio_functions[method == coupling_measure][["biblio_function"]][[1]]

      # evaluate the expression and catch internal errors to biblionetwork package

      tryCatch({
        edges_of_the_year <-
          rlang::expr((!!biblio_function)(
            dt = edges_of_the_year,
            source = rlang::inject(source_id),
            ref = rlang::inject(target_id),
            weight_threshold = rlang::inject(edges_threshold)
          )
          ) %>%
          eval()

      }, error = function(e) {
        stop(
          "The coupling function failed with an error. Read the biblionetwork documentation for more information. Error message: ",
          e$message
        )
      })

    }

    edges_of_the_year[, source_id := from]
    edges_of_the_year[, target_id := to]

    # remove nodes with no edges
    if (keep_singleton == FALSE) {
      nodes_of_the_year <- nodes_of_the_year[source_id %in% edges_of_the_year$from |
                                               source_id %in% edges_of_the_year$to, env = list(source_id = source_id)]
    }

    # make tbl
    if (length(all_years) == 1)
    {
      tbl_coup_list <- tidygraph::tbl_graph(
        nodes = nodes_of_the_year,
        edges = edges_of_the_year,
        directed = FALSE,
        node_key = source_id
      )
    } else {
      tbl_coup_list[[paste0(year, "-", year + time_window - 1)]] <-
        tidygraph::tbl_graph(
          nodes = nodes_of_the_year,
          edges = edges_of_the_year,
          directed = FALSE,
          node_key = source_id
        )
    }
  }

  if (filter_components == TRUE) {
    tbl_coup_list <- filter_components(tbl_coup_list, ...)
  }
  return (tbl_coup_list)
}

#' @rdname build_dynamic_networks
#' @export

build_network <- function(nodes,
                          directed_edges,
                          source_id,
                          target_id,
                          cooccurrence_method = c("coupling_angle","coupling_strength","coupling_similarity"),
                          edges_threshold = 1,
                          compute_size = FALSE,
                          keep_singleton = FALSE,
                          filter_components = FALSE,
                          ...){
graph <- build_dynamic_networks(nodes = nodes,
                         directed_edges = directed_edges,
                         source_id = source_id,
                         target_id = target_id,
                         cooccurrence_method = cooccurrence_method,
                         edges_threshold = edges_threshold,
                         compute_size = compute_size,
                         keep_singleton = keep_singleton,
                         filter_components = FALSE,
                         ...,
                         verbose = FALSE)
if(filter_components == TRUE) graph <- filter_components(graph, ...)
return(graph)
}
