intertemporal_cluster_naming <- function(list_graph = NA,
                                         cluster_column = NA,
                                         node_key = NA,
                                         threshold_similarity = 0.5001,
                                         similarity_type = c("complete, partial")){

  #' Find Similar Clusters across Multiple Temporal Networks
  #'
  #' @description
  #' `r lifecycle::badge("deprecated")`
  #'
  #' This function creates a new column "intertemporal_name" for each network from a list
  #' of temporal networks to
  #' identify similar clusters across time. The function gives the same name to two
  #' clusters from two succesive temporal networks if they match the conditions defined by
  #' the user: `threshold_similarity`, `cluster_colum` and `similarity_type`.
  #'
  #' @param list_graph
  #' A list of networks. The list is expected to be ordered in a sequential order
  #' from the oldest to the most recent network.
  #'
  #' @param cluster_column
  #' The column with the identifier of the cluster.
  #'
  #' @param node_key
  #' The column with the unique identifier of each node.
  #'
  #' @param threshold_similarity
  #' The threshold_similarity variable defines how sensitive the function is to giving the same name to two clusters.
  #' A higher threshold will lead to more communities.
  #'
  #' For example, if you have two temporal networks with two communities each. Communities A and B for the older network,
  #' and communities A' and B' for the more recent network. A threshold of 0.51 with a "complete" similarity_type means that community A' will be given the name
  #' A if 51% of the nodes from A' in the more recent network originate from A in the older network, and 51% of the node from A in the older
  #' network becomes in A' in the more recent network.
  #'
  #' @param similarity_type
  #' Choose a similarity type to compare the threshold to:
  #'
  #' - "complete" similarity compute the share of nodes going from a older community to a more recent community on all the nodes in both networks
  #' - "partial" similarity compute the share of nodes going from a older community to a more recent community only on nodes that exists in both networks
  #'
  #' Complete similarity is particularly suited if the number of nodes in your networks is relatively stable over time as the threshold capture the share of all nodes
  #' moving between clusters. Partial similarity can be particularly useful when the number of nodes in your networks increases rapidly. The interpretation of the threshold
  #' is that it captures the share of nodes existing in both networks moving between clusters.
  #'
  #' For example, with a complete similarity threshold of 0.51, if (1) all nodes from community A in network t-1 go into community A' in network t+1, and (2) all nodes in community A'
  #' present in network t-1 originate from community A, but (3) the number of nodes in A' is more than twice of A because of new nodes that did not exists in t-1,
  #' A' will never meet the threshold requirement to be named A despite a strong similarity between the two clusters.
  #' Conceptually, this might be a desired behavior of the function because one might considered that A' is too different from A to be considered the same cluster as its composition is changed from new nodes.
  #' In that case complete similarity is the right choice. However, if one consider that A and A' are very similar because all the nodes that exists in both networks are identified as part of the same community,
  #' then partial threshold similarity is more desirable.
  #'
  #' @return
  #' The function returns the same list of networks used as input in `list_graph` but with a new column `intertemporal_name`. The column
  #' is the result of the inter-temporal grouping of the original clusters of the `cluster_column`.
  #'
  #' @examples
  #' library(biblionetwork)
  #' library(magrittr)
  #' library(tidygraph)
  #'
  #' nodes <- Nodes_stagflation %>%
  #' dplyr::rename(ID_Art = ItemID_Ref) %>%
  #' dplyr::filter(Type == "Stagflation")
  #'
  #' references <- Ref_stagflation %>%
  #' dplyr::rename(ID_Art = Citing_ItemID_Ref)
  #'
  #' temporal_networks <- dynamic_network_cooccurrence(nodes = nodes,
  #' directed_edges = references,
  #' source_column = "ID_Art",
  #' target_column = "ItemID_Ref",
  #' time_variable = "Year",
  #' cooccurrence_method = "coupling_similarity",
  #' time_window = 15,
  #' edges_threshold = 1,
  #' compute_size = FALSE,
  #' keep_singleton = FALSE,
  #' overlapping_window = TRUE)
  #'
  #' temporal_networks <- lapply(temporal_networks,
  #'                                     function(tbl) tbl %N>%
  #'                                                   mutate(clusters = tidygraph::group_louvain()))
  #'
  #' intertemporal_cluster_naming(temporal_networks,
  #' cluster_column = "clusters",
  #' node_key = "ID_Art",
  #' threshold_similarity = 0.51,
  #' similarity_type = "partial")
  #'
  #' @export

  lifecycle::deprecate_warn("0.1.0", "intertemporal_cluster_naming()", "merge_dynamic_clusters()")

  if(!similarity_type %in% c("complete","partial")){
    stop('You must chose a similarity type between "complete" and "partial":\n
    - "complete" similarity compare the threshold to all the nodes in both networks\n
    - "partial" similarity compare the threshold to nodes that only exists in both networks')}
  if(!inherits(list_graph, "list") | (inherits(list_graph, "list") & length(as.list(list_graph)) == 1)){
    stop("Your data is not a list of tidygraph networks or you only have one network in your list.")
  }
  if(threshold_similarity <= 0.5 | threshold_similarity > 1){
    stop("Your threshold for similarity must be superior to 0.5 and inferior or equal to 1")}

  . <- nodes <- network_num <- nodes <- n_nodes <- past_id_com <- present_id_com <- share <- N <- same_evolution <- same_origin <- past_id_com.x <- past_id_com.y <- intertemporal_name <- new_cluster_column <- NULL

  #########################  Prepare everything **********************

  list_graph <- data.table::copy(list_graph)
  list_graph <- lapply(list_graph,
                       function(tbl) tbl %N>%
                         dplyr::mutate(dplyr::across(dplyr::all_of(cluster_column), ~ as.character(.))))

  # get all years to study
  old_list_name <- names(list_graph) # save old list name
  all_years <- c(1:length(names(list_graph))) %>% as.integer # numeric list name
  names(list_graph) <- all_years

  # get the number of unique communities
  all_nodes <- lapply(list_graph, function(tbl) tbl %N>% data.table::as.data.table()) %>%
    data.table::rbindlist(idcol = "network_num")
  n_com_unique <- all_nodes[, .N, .(cluster_column, network_num), env = list(cluster_column = cluster_column)][,.N]

  unique_ids <- paste0("cl_",1:n_com_unique) # generate a number of unique ids equal to the maximum number of communities

  intertemporal_naming <- list()
  for (Year in all_years) {

    ######################### For the first year, let's just give clusters unique ids **********************

    if(is.null(list_graph[[paste0(Year-1)]])){
      dt_year <- all_nodes[network_num == as.character(Year), env = list(Year = Year)] %>%
        .[,.SD,.SDcols = c(node_key, cluster_column)] %>%  # extract the nodes of the period
        .[order(cluster_column), env = list(cluster_column = cluster_column)] #just to have a more informative numerotation of cluster then
      n_com <- dt_year[, .N, cluster_column, env = list(cluster_column = cluster_column)][,.N] # number of communities
      id_com_corr <- data.table(intertemporal_name = unique_ids[1:n_com],
                                dt_year[, .SD,.SDcols = cluster_column] %>% unique) #give a unique ids to com
      unique_ids <- unique_ids[-c(1:n_com)] #remove ids taken from list
      dt_year <- dt_year[id_com_corr, on = cluster_column] %>%
        .[, .SD, .SDcols = c(cluster_column, "intertemporal_name")] %>%  #merge with unique id
        unique

      intertemporal_naming[[paste0(Year)]] <- list_graph[[paste0(Year)]] %N>%
        dplyr::left_join(dt_year)
    }

    ######################### For other years, we need to take the previous years and give new names to community of the new year  **********************

    else if(!is.null(list_graph[[paste0(Year-1)]])){

      ######################### Communities from previous year  **********************

      dt_year <- intertemporal_naming[[paste0(Year-1)]] %N>%
        dplyr::rename("past_id_com" = intertemporal_name) %>%
        dplyr::select(dplyr::all_of(c(node_key, "past_id_com"))) %>% # this is the nodes from the past
        data.table::as.data.table()

      ######################### Communities from present  **********************

      dt_year2 <- list_graph[[paste0(Year)]] %N>%
        dplyr::select(dplyr::all_of(c(node_key, cluster_column))) %>%
        data.table::as.data.table()
      n_com <- dt_year2[, .N, cluster_column, env = list(cluster_column = cluster_column)][,.N] # number of communities
      id_com_corr <- data.table::data.table(new_cluster_column = unique_ids[1:n_com],
                                dt_year2[, .SD,.SDcols = cluster_column] %>% unique) #give a unique ids to com that not 1,2,3...
      unique_ids <- unique_ids[-c(1:n_com)] #remove ids taken from list
      data.table::setnames(dt_year2, cluster_column, "present_id_com")   # this is the nodes from the present

      ######################### Find the evolution of past communities  **********************

      if(similarity_type == "partial"){
        dt_list <- data.table::merge.data.table(dt_year, dt_year2, by = node_key) # if "partial" we only merge on nodes that exists in the present and in the past to get the % of nodes moving from one community to the other
      } else {
        dt_list <- data.table::merge.data.table(dt_year, dt_year2, by = node_key, all.x=TRUE) # if "complete" we keep all past nodes, including the ones that do not exist in the present network
      }

      dt_list[, n_nodes := .N, past_id_com]
      dt_list <- dt_list[, .N, .(present_id_com, past_id_com, n_nodes)]
      dt_list[, share := N/n_nodes] # compute share

      evolution_from_past <- dt_list[share > threshold_similarity, same_evolution := TRUE, env = list(threshold_similarity = threshold_similarity)]  # if above the treshold, then a communities mostly evolve into the present community
      evolution_from_past <- evolution_from_past[, .SD, .SDcols = c("present_id_com", "past_id_com", "same_evolution")]

      ######################### Find the origin of present communities  **********************

      if(similarity_type == "partial"){
        dt_list2 <- data.table::merge.data.table(dt_year, dt_year2, by = node_key) # we do the same thing as previously but for the origin of present nodes
      } else {
        dt_list2 <- data.table::merge.data.table(dt_year, dt_year2, by = node_key, all.y=TRUE)
      }

      dt_list2[, n_nodes := .N, present_id_com]
      dt_list2 <- dt_list2[,.N, .(present_id_com, past_id_com, n_nodes)]
      dt_list2[, share := N/n_nodes]

      origin_of_present <- dt_list2[share > threshold_similarity, same_origin := TRUE, env = list(threshold_similarity = threshold_similarity)] # if above the treshold, then a communities mostly originate from past community
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
      setnames(compare, "present_id_com", cluster_column)

      ######################### Inject new names **********************

      final_names <- data.table::merge.data.table(id_com_corr, compare, by = cluster_column, all.x = TRUE) # we get the new names
      final_names[is.na(intertemporal_name), intertemporal_name := new_cluster_column] # if there is no correspondence, a present community is a new community, and is given a new name
      final_names <- final_names[, .SD, .SDcols = c(cluster_column, "intertemporal_name")]

      intertemporal_naming[[paste0(Year)]] <- list_graph[[paste0(Year)]] %N>%
        dplyr::left_join(final_names)
    }
  }
  names(intertemporal_naming) <- old_list_name
  return (intertemporal_naming)
}
