#' Follow clusters across time windows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' When clusters are detected in each time window separately, their
#' identifiers mean nothing from one window to the next: cluster "01" of the
#' 1975-1984 network and cluster "01" of the 1976-1985 network may be two
#' different literatures. `merge_dynamic_clusters()` links the clusters of
#' successive windows that share most of their nodes, and gives them a
#' common identifier, so that a cluster becomes an entity that can be born,
#' persist and disappear. This is the step that turns a series of networks
#' into a history of research communities, in the spirit of the alluvial
#' maps of \insertCite{rosvall2010;textual}{networkflow}.
#'
#' Two rules are available, chosen with `method`. Let \eqn{V_a} be the nodes
#' of cluster \eqn{a} in window \eqn{t-1} and \eqn{V_b} those of cluster
#' \eqn{b} in window \eqn{t}.
#'
#' * `method = "majority"`, a **double majority**. Cluster \eqn{b} inherits
#'   the identifier of \eqn{a} when most nodes of \eqn{a} go to \eqn{b} *and*
#'   most nodes of \eqn{b} come from \eqn{a}:
#'   \deqn{\frac{|V_a \cap V_b|}{D_a} > \tau \quad \text{and} \quad
#'   \frac{|V_a \cap V_b|}{D_b} > \tau,}
#'   with \eqn{\tau} the `threshold_similarity`, strictly above 0.5. Since
#'   \eqn{\tau > 0.5}, a cluster can inherit at most one identifier, and an
#'   identifier passes to at most one cluster.
#' * `method = "jaccard"`, an **overlap matching with a Jaccard threshold**,
#'   as in the tracking of dynamic communities of
#'   \insertCite{greene2010;textual}{networkflow}. The Jaccard index of two
#'   clusters is the share of their union that they have in common,
#'   \deqn{J(a, b) = \frac{|V_a \cap V_b|}{|V_a \cup V_b|}.}
#'   The pairs with \eqn{J(a, b) \geq \theta}, \eqn{\theta} being the
#'   `threshold_similarity`, are candidate matches. They are taken one by one,
#'   by decreasing \eqn{J} (then by decreasing number of shared nodes),
#'   skipping the pairs where \eqn{a} or \eqn{b} has already been matched:
#'   each past identifier goes to the present cluster that resembles it most,
#'   and each cluster inherits at most one identifier. \eqn{\theta} can be
#'   below 0.5: a cluster that loses a large part of its members to
#'   another cluster, or that doubles with newcomers, can keep its identity
#'   if it stays the best match of its predecessor.
#'
#' In both rules, a cluster that meets no match gets a new identifier. The
#' sets \eqn{V} depend on `similarity_type`: with `"complete"`, they are all
#' the nodes of the clusters (\eqn{D_a = |V_a|}, \eqn{D_b = |V_b|}); with
#' `"partial"`, only the nodes present in both windows count
#' (\eqn{D_a = |V_a \cap V_t|}, \eqn{D_b = |V_b \cap V_{t-1}|}, and the union
#' of the Jaccard index is restricted to them), \eqn{V_t} being all the nodes
#' of window \eqn{t}.
#'
#' There is no default `method`, `threshold_similarity` or `similarity_type`:
#' they change the dynamic clusters, so the choice must be explicit, and
#' `verbose = TRUE` reports the rule that is applied.
#'
#' @param list_graph A list of at least two `tbl_graph`s in chronological
#'   order (oldest first), as produced by [build_dynamic_networks()] and
#'   [add_clusters()].
#' @param cluster_id Character scalar, the node column with the clusters of
#'   each window (e.g. `"cluster_leiden"`).
#' @param node_id Character scalar, the node column with an identifier that
#'   is stable across windows (e.g. `"source_id"`).
#' @param method `"majority"` (double majority) or `"jaccard"` (overlap
#'   matching with a Jaccard threshold), see the description. Required.
#' @param threshold_similarity The threshold of the rule. For
#'   `method = "majority"`, \eqn{\tau} in \eqn{(0.5, 1]}: a cluster is linked
#'   when the share is *above* it. For `method = "jaccard"`, \eqn{\theta} in
#'   \eqn{(0, 1]}: two clusters can be linked when their Jaccard index is *at
#'   least* it. Higher values require more similar clusters and create more
#'   distinct dynamic clusters. Required.
#' @param similarity_type `"complete"` or `"partial"`, see the description.
#'   Required.
#' @param verbose Logical. If `TRUE` (default), says which rule is applied.
#'
#' @details
#' **Which windows?** The rules need nodes present in successive windows.
#' In a network of documents (e.g. bibliographic coupling), each document has
#' a single date, so windows that do not overlap (`window_step` equal to
#' `time_window`, the default of `build_dynamic_networks()`) share no node
#' and no cluster can be matched: the windows must overlap
#' (`window_step` smaller than `time_window`). Non-overlapping windows work
#' when nodes persist over time, as cited references (co-citation), authors or
#' journals do; two windows may still share no node when the literature
#' changed completely.
#'
#' **Complete or partial?** With overlapping windows, the network of window
#' \eqn{t} loses the documents of the first years of window \eqn{t-1} and
#' gains those of new years; in a growing literature, newcomers can be
#' numerous. With `"complete"`, a cluster that keeps all its past members but
#' doubles with newcomers cannot inherit the identifier under the double
#' majority, because its members coming from \eqn{a} are less than half of it:
#' the cluster is considered new because its composition changed. With
#' `"partial"`, the newcomers are ignored and the same cluster keeps its
#' identifier. Which one is right depends on what a cluster is taken to be: a
#' group of documents (complete), or a structure that recruits new documents
#' (partial).
#'
#' **Majority or Jaccard?** The double majority is demanding and
#' transparent: two clusters are the same if each is mostly made of the
#' other, and the identifier is never ambiguous. The Jaccard matching is more
#' flexible: with a low \eqn{\theta}, it follows a cluster that splits (the
#' largest piece, the best match, keeps the identifier) or that merges with
#' another. Both use the nodes of consecutive windows only; compare them
#' with [merge_diagnostics()].
#'
#' **What the rules do not do.** Clusters are compared only with the
#' previous window: a cluster that vanishes for one window and comes back
#' gets a new identifier. Splits and merges are not recorded as such: one
#' part keeps the identifier, the others are new. With a threshold close to
#' its minimum, small fluctuations of the partition (a stochastic algorithm,
#' a resolution change) may break or create continuities; it is good practice
#' to compare the results for a few thresholds and seeds.
#'
#' @return The list of graphs, with the same names, where nodes have a new
#'   column `dynamic_{cluster_id}` (e.g. `dynamic_cluster_leiden`, with
#'   identifiers "cl_1", "cl_2"...) and edges the columns
#'   `dynamic_{cluster_id}_from`, `dynamic_{cluster_id}_to` and
#'   `dynamic_{cluster_id}` (missing for edges between clusters).
#'
#' @seealso [name_clusters()] with `name_dynamic_clusters = TRUE` to label the
#'   dynamic clusters; [networks_to_alluv()] and [plot_alluvial()] to draw
#'   their evolution.
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   method = "similarity",
#'   edges_threshold = 1,
#'   verbose = FALSE
#' )
#' networks <- add_clusters(
#'   networks,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' networks <- merge_dynamic_clusters(
#'   networks,
#'   cluster_id = "cluster_leiden",
#'   node_id = "source_id",
#'   method = "majority",
#'   threshold_similarity = 0.51,
#'   similarity_type = "partial"
#' )
#' networks[[2]]
#'
#' # The same windows, with the Jaccard matching
#' merge_dynamic_clusters(
#'   networks,
#'   cluster_id = "cluster_leiden",
#'   node_id = "source_id",
#'   method = "jaccard",
#'   threshold_similarity = 0.3,
#'   similarity_type = "partial"
#' )
#'
#' @export
merge_dynamic_clusters <- function(list_graph,
                                   cluster_id,
                                   node_id,
                                   method = c("majority", "jaccard"),
                                   threshold_similarity,
                                   similarity_type = c("complete", "partial"),
                                   verbose = TRUE){

  # No default for the method, the threshold and the type of similarity: they
  # change the dynamic clusters, so the choice must be explicit.
  method <- check_explicit_choice(
    method, c("majority", "jaccard"), "method",
    context = "To link the clusters across windows"
  )
  similarity_type <- check_explicit_choice(
    similarity_type, c("complete", "partial"), "similarity_type",
    context = "To compare clusters across windows"
  )
  if (is_single_graph(list_graph, arg = "list_graph") || length(list_graph) < 2) {
    cli::cli_abort("{.arg list_graph} must be a list of at least two {.cls tbl_graph}s.")
  }
  check_merge_threshold(if (missing(threshold_similarity)) NULL else threshold_similarity, method)
  if (verbose) inform_merge_method(method, threshold_similarity, similarity_type)
  for (graph in list_graph) check_columns(node_table(graph), c(cluster_id, node_id), "list_graph")

  # Running the function again replaces the dynamic clusters of a previous run.
  new_name_col <- paste0("dynamic_", cluster_id)
  previous <- paste0(new_name_col, c("", "_from", "_to"))
  if (new_name_col %in% names(node_table(list_graph[[1]]))) {
    cli::cli_inform(c("i" = "The existing {.field {new_name_col}} columns are replaced."))
    list_graph <- lapply(list_graph, function(graph) {
      graph %E>%
        dplyr::select(-dplyr::any_of(previous)) %N>%
        dplyr::select(-dplyr::any_of(previous))
    })
  }

  # Matching needs nodes present in consecutive windows.
  ids <- lapply(list_graph, function(graph) node_table(graph)[[node_id]])
  shared <- vapply(seq_along(ids)[-1], function(i) length(intersect(ids[[i - 1]], ids[[i]])), integer(1))
  if (any(shared == 0)) {
    cli::cli_warn(c(
      "{sum(shared == 0)} pair{?s} of consecutive windows share no node: their clusters cannot be matched.",
      "i" = "Clusters are matched through the nodes they share. In a network of documents (coupling), each node has one date: the windows must overlap ({.arg window_step} smaller than {.arg time_window} in {.fn build_dynamic_networks}). If windows already overlap, the nodes may have changed completely, or the threshold may leave a window without edges."
    ))
  }

  . <- network_num <- nodes <- n_nodes <- past_id_com <- present_id_com <- share <- N <- same_evolution <- same_origin <- past_id_com.x <- past_id_com.y <- intertemporal_name <- new_cluster_column <- NULL

  #########################  Prepare everything **********************

    list_graph <- lapply(list_graph,
                       function(tbl) tbl %N>%
                         dplyr::mutate(dplyr::across(dplyr::all_of(cluster_id), ~ as.character(.))))

  # get all years to study
  old_list_name <- names(list_graph) # save old list name
  all_years <- seq_along(list_graph) # numeric list name (works for unnamed lists)
  names(list_graph) <- all_years

  # get the number of unique communities
  all_nodes <- lapply(list_graph, function(tbl) tbl %N>%
                        data.table::as.data.table()) %>%
    data.table::rbindlist(idcol = "network_num")
  n_com_unique <- all_nodes[, .N, .(cluster_id, network_num),
                            env = list(cluster_id = cluster_id)][,.N]

  unique_ids <- paste0("cl_",1:n_com_unique) # generate a number of unique ids equal to the maximum number of communities

  intertemporal_naming <- list()
  for (Year in all_years) {

    ######################### For the first year, let's just give clusters unique ids **********************

    if(is.null(list_graph[[paste0(Year-1)]])){
      dt_year <- all_nodes[network_num == as.character(Year), env = list(Year = Year)] %>%
        .[,.SD,.SDcols = c(node_id, cluster_id)] %>%  # extract the nodes of the period
        .[order(cluster_id), env = list(cluster_id = cluster_id)] #just to have a more informative numerotation of cluster then
      n_com <- dt_year[, .N, cluster_id,
                       env = list(cluster_id = cluster_id)][,.N] # number of communities
      id_com_corr <- data.table(intertemporal_name = unique_ids[1:n_com],
                                dt_year[, .SD,.SDcols = cluster_id] %>% unique) #give a unique ids to com
      unique_ids <- unique_ids[-c(1:n_com)] #remove ids taken from list
      dt_year <- dt_year[id_com_corr, on = cluster_id] %>%
        .[, .SD, .SDcols = c(cluster_id, "intertemporal_name")] %>%  #merge with unique id
        unique

      intertemporal_naming[[paste0(Year)]] <- list_graph[[paste0(Year)]] %N>%
        dplyr::left_join(dt_year, by = cluster_id)
    }

    ######################### For other years, we need to take the previous years and give new names to community of the new year  **********************

    else if(!is.null(list_graph[[paste0(Year-1)]])){

      ######################### Communities from previous year  **********************

      dt_year <- intertemporal_naming[[paste0(Year-1)]] %N>%
        dplyr::rename("past_id_com" = intertemporal_name) %>%
        dplyr::select(dplyr::all_of(c(node_id, "past_id_com"))) %>% # this is the nodes from the past
        data.table::as.data.table()

      ######################### Communities from present  **********************

      dt_year2 <- list_graph[[paste0(Year)]] %N>%
        dplyr::select(dplyr::all_of(c(node_id, cluster_id))) %>%
        data.table::as.data.table()
      n_com <- dt_year2[, .N, cluster_id, env = list(cluster_id = cluster_id)][,.N] # number of communities
      id_com_corr <- data.table(new_cluster_column = unique_ids[1:n_com],
                                dt_year2[, .SD,.SDcols = cluster_id] %>% unique) #give a unique ids to com that not 1,2,3...
      unique_ids <- unique_ids[-c(1:n_com)] #remove ids taken from list
      data.table::setnames(dt_year2, cluster_id, "present_id_com")   # this is the nodes from the present

      if (method == "majority") {
        ######################### Find the evolution of past communities  **********************

        if(similarity_type == "partial"){
          dt_list <- data.table::merge.data.table(dt_year, dt_year2, by = node_id) # if "partial" we only merge on nodes that exists in the present and in the past to get the % of nodes moving from one community to the other
        } else {
          dt_list <- data.table::merge.data.table(dt_year, dt_year2, by = node_id, all.x=TRUE) # if "complete" we keep all past nodes, including the ones that do not exist in the present network
        }

        dt_list[, n_nodes := .N, past_id_com]
        dt_list <- dt_list[, .N, .(present_id_com, past_id_com, n_nodes)]
        dt_list[, share := N/n_nodes] # compute share

        evolution_from_past <- dt_list[share > threshold_similarity, same_evolution := TRUE, env = list(threshold_similarity = threshold_similarity)]  # if above the treshold, then a communities mostly evolve into the present community
        evolution_from_past <- evolution_from_past[, .SD, .SDcols = c("present_id_com", "past_id_com", "same_evolution")]

        ######################### Find the origin of present communities  **********************

        if(similarity_type == "partial"){
          dt_list2 <- data.table::merge.data.table(dt_year, dt_year2, by = node_id) # we do the same thing as previously but for the origin of present nodes
        } else {
          dt_list2 <- data.table::merge.data.table(dt_year, dt_year2, by = node_id, all.y=TRUE)
        }

        dt_list2[, n_nodes := .N, present_id_com]
        dt_list2 <- dt_list2[,.N, .(present_id_com, past_id_com, n_nodes)]
        dt_list2[, share := N/n_nodes]

        origin_of_present <- dt_list2[share > threshold_similarity, same_origin := TRUE,
                                      env = list(threshold_similarity = threshold_similarity)] # if above the treshold, then a communities mostly originate from past community
        origin_of_present <- origin_of_present[,.SD,.SDcols = c("present_id_com", "past_id_com", "same_origin")]

        ######################### Compare communities across time according to tresholds **********************

        compare <- data.table::merge.data.table(unique(dt_year2[, .(present_id_com)]),
                                                evolution_from_past[same_evolution == TRUE],
                                                by = "present_id_com",
                                                all.x = TRUE)  # we merge past communities that respect the treshold in evolution to get correspondence
        compare <- data.table::merge.data.table(compare,
                                                origin_of_present[same_origin == TRUE],
                                                by = "present_id_com",
                                                all.x = TRUE)  # we merge past communities that respect the treshold in origin to get correspondence
        compare <- compare[same_evolution == TRUE & same_origin == TRUE]   # we keep communities that respect both tresholds
        compare[past_id_com.x == past_id_com.y, intertemporal_name := paste0(past_id_com.x)] # we past the name of the old communities as present communities
        compare <- compare[past_id_com.x == past_id_com.y, .SD,.SDcols = c("present_id_com", "intertemporal_name")] # we only keep name for rows where both communities are the same (i.e., one community meeting both tresholds)
        data.table::setnames(compare, "present_id_com", cluster_id)

      } else {
        # Jaccard matching: the clusters of the two windows are compared by
        # the share of their union that they have in common, and each past
        # identifier goes to the present cluster that resembles it most
        # (see the details of merge_dynamic_clusters()).
        compare <- match_clusters_jaccard(dt_year, dt_year2, node_id, threshold_similarity, similarity_type)
        data.table::setnames(compare, "present_id_com", cluster_id)
      }

      ######################### Inject new names **********************

      final_names <- data.table::merge.data.table(id_com_corr, compare, by = cluster_id, all.x = TRUE) # we get the new names
      final_names[is.na(intertemporal_name), intertemporal_name := new_cluster_column] # if there is no correspondence, a present community is a new community, and is given a new name
      final_names <- final_names[, .SD, .SDcols = c(cluster_id, "intertemporal_name")]

      intertemporal_naming[[paste0(Year)]] <- list_graph[[paste0(Year)]] %N>%
        dplyr::left_join(final_names, by = cluster_id)
    }
  }
  names(intertemporal_naming) <- old_list_name
  intertemporal_naming <- lapply(intertemporal_naming,
                                 function(tbl) tbl %N>% dplyr::rename({{ new_name_col }} := intertemporal_name)) # We want a name that corresponds to the initial column
  intertemporal_naming <- lapply(intertemporal_naming,
                                 add_dynamic_cluster_to_edges,
                                 cluster_id = cluster_id,
                                 new_name_col = new_name_col) # add dynamic cluster id to edges
  return (intertemporal_naming)
}

# The dynamic cluster of the two ends of each edge, and their common dynamic
# cluster (missing for edges between clusters), computed from the nodes.
add_dynamic_cluster_to_edges <- function(graph, cluster_id, new_name_col) {
  dynamic <- node_table(graph)[[new_name_col]]
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  dynamic_from <- dynamic[ends[, 1]]
  dynamic_to <- dynamic[ends[, 2]]
  graph %E>%
    dplyr::mutate(
      !!paste0(new_name_col, "_from") := dynamic_from,
      !!paste0(new_name_col, "_to") := dynamic_to,
      !!new_name_col := ifelse(dynamic_from == dynamic_to, dynamic_from, NA_character_)
    ) %>%
    tidygraph::activate("nodes")
}

# The threshold of each method: a number in (0.5, 1] for the double majority
# (above 0.5, a cluster inherits at most one identifier), in (0, 1] for the
# Jaccard matching.
check_merge_threshold <- function(threshold, method, call = rlang::caller_env()) {
  lower <- if (method == "majority") 0.5 else 0
  ok <- is.numeric(threshold) && length(threshold) == 1 && !is.na(threshold) &&
    threshold > lower && threshold <= 1
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg threshold_similarity} must be a number in ({lower}, 1] with {.code method = \"{method}\"}.",
        "i" = if (method == "majority") "Above 0.5, a cluster can inherit at most one identifier.",
        "i" = if (method == "jaccard") "The Jaccard index of two clusters is between 0 and 1; clusters are linked when it is at least the threshold."
      ),
      call = call
    )
  }
  invisible(threshold)
}

inform_merge_method <- function(method, threshold, similarity_type) {
  rule <- if (method == "majority") {
    "a cluster inherits the identifier of a past cluster when more than {.val {threshold}} of the nodes of each go to / come from the other (double majority)"
  } else {
    "a cluster inherits the identifier of the past cluster with which its Jaccard index (shared nodes over the union of the two clusters) is the highest, if it is at least {.val {threshold}}; each identifier passes to one cluster at most"
  }
  scope <- if (similarity_type == "complete") "all the nodes of the clusters" else "the nodes present in both windows only"
  cli::cli_inform(c("i" = paste0("Linking clusters across windows with the {.val {method}} rule: ", rule, ". The comparison counts ", scope, " ({.arg similarity_type} = {.val {similarity_type}}).")))
  invisible()
}

# Jaccard matching of the clusters of two successive windows. `past` and
# `present` have the nodes of the previous window with their dynamic cluster
# (`past_id_com`) and the nodes of the window with their cluster
# (`present_id_com`). Returns the correspondence present cluster -> past
# identifier, one to one: the pairs are taken by decreasing Jaccard index (then
# by decreasing number of shared nodes), a cluster or an identifier being used
# once.
match_clusters_jaccard <- function(past, present, node_id, threshold, similarity_type) {
  N <- jaccard <- size_past <- size_present <- past_id_com <- present_id_com <- NULL
  in_both <- data.table::merge.data.table(past, present, by = node_id)
  overlap <- in_both[, .N, by = list(past_id_com, present_id_com)]
  if (similarity_type == "partial") {
    # only the nodes that exist in both windows are compared
    size_past <- in_both[, list(size_past = .N), by = past_id_com]
    size_present <- in_both[, list(size_present = .N), by = present_id_com]
  } else {
    size_past <- past[, list(size_past = .N), by = past_id_com]
    size_present <- present[, list(size_present = .N), by = present_id_com]
  }
  overlap <- data.table::merge.data.table(overlap, size_past, by = "past_id_com")
  overlap <- data.table::merge.data.table(overlap, size_present, by = "present_id_com")
  overlap[, jaccard := N / (size_past + size_present - N)]
  candidates <- overlap[jaccard >= threshold][order(-jaccard, -N, past_id_com, present_id_com)]
  used_past <- character(0)
  used_present <- character(0)
  keep <- logical(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    if (!candidates$past_id_com[i] %in% used_past && !candidates$present_id_com[i] %in% used_present) {
      keep[i] <- TRUE
      used_past <- c(used_past, candidates$past_id_com[i])
      used_present <- c(used_present, candidates$present_id_com[i])
    }
  }
  out <- candidates[keep, list(present_id_com, intertemporal_name = past_id_com)]
  out
}
