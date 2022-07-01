intertemporal_cluster_naming <- function(list_graph = NA,
                                         cluster_column = NA,
                                         ID_key = NA,
                                         threshold_similarity = 0.5001,
                                         similarity_type = c("complete, partial")){

  #' Find similar clusters across multiple temporal networks
  #'
  #' This function creates a new column "intertemporal_name" for each network in a list of temporal networks to
  #' identify similar clusters across time. The function give the same to name two
  #' clusters between two sequential temporal network if they match the conditions from
  #' the user defined threshold_similarity, cluster_column and similarity_type.
  #'
  #' @param list_graph
  #' A list of graph. The list of networks is expected to be ordered in a sequential order
  #' from the oldest to the most recent network.
  #'
  #' @param cluster_column
  #' The column with the name of the cluster.
  #'
  #' @param ID_key
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
  #' then partial treshold similarity is more desirable.
  #'
  #'  #' @examples
  #' library(biblionetwork)
  #' intertemporal_cluster_naming(list_of_temporal_networks,
  #' cluster_column = "Louvain_clusters",
  #' ID_key = "ids_nodes",
  #' threshold_similarity = 0.51,
  #' similarity_type = "partial")
  #'
  #' @export
  #' @import data.table
  #' @importFrom stringi stri_rand_strings
  #' @import tidygraph
  #' @import dplyr

  if(!similarity_type %in% c("complete","partial")){
    stop('You must chose a similarity type between "complete" and "partial":\n
    - "complete" similarity compare the treshold to all the nodes in both networks\n
    - "partial" similarity compare the treshold to nodes that only exists in both networks')}
  if(threshold_similarity <= 0.5 | threshold_similarity >1){
    stop("Your treshold for similarity must be superior to 0.5 and inferior or equal to 1")}

  . <- nodes <- network_num <- nodes <- n_nodes <- past_id_com <- present_id_com <- share <- N <- same_evolution <- same_origin <- past_id_com.x <- past_id_com.y <- intertemporal_name <- new_cluster_column <- NULL

  #########################  Prepare everything **********************

  list_graph <- copy(list_graph)
  list_graph <- lapply(list_graph, function(tbl) tbl %>% activate(nodes) %>% mutate(across(all_of(cluster_column), ~ as.character(.))))

  # get all years to study
  old_list_name <- names(list_graph) # save old list name
  names(list_graph) <- c(1:length(names(list_graph))) # numeric list name
  all_years <- as.numeric(names(list_graph))

  # get the number of unique communities
  all_nodes <- lapply(list_graph, function(tbl) tbl %>% activate(nodes) %>% as.data.table())
  all_nodes <- rbindlist(all_nodes, idcol = "network_num")
  n_com_unique <- all_nodes[, .N, .(cluster_column, network_num), env=list(cluster_column=cluster_column)][,.N]

  unique_ids <-  stringi::stri_rand_strings(n_com_unique, 4) # generate a number of unique ids equal to the maximum number of communities

  intertemporal_naming <- list()
  for (Year in all_years) {

    ######################### For the first year, let's just give clusters unique ids **********************

    if(is.null(list_graph[[paste0(Year-1)]])){

      dt_year <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
      dt_year <- dt_year[,.SD,.SDcols = c(ID_key, cluster_column)]
      #n_com
      n_com <- dt_year[,.N,cluster_column, env=list(cluster_column=cluster_column)][,.N]
      #give a unique ids to com
      id_com_corr <- data.table(intertemporal_name = unique_ids[1:n_com],
                                temp_cluster_column = dt_year[,unique(dt_year, by = cluster_column)][,.SD,.SDcols = c(cluster_column)][[1]])
      setnames(id_com_corr, "temp_cluster_column", cluster_column)
      #remove ids taken from list
      unique_ids <- unique_ids[-c(1:n_com)]
      #merge with unique id
      dt_year <- merge(dt_year, id_com_corr, by = cluster_column)
      dt_year <- dt_year[,.SD,.SDcols = c(ID_key, "intertemporal_name")]

      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(dt_year)
      intertemporal_naming[[paste0(Year)]] <- tbl

    }

    ######################### For other years, we need to take the previous years and give new names to community of the new year  **********************

    else if(!is.null(list_graph[[paste0(Year-1)]])){

      ######################### Communities from previous year  **********************

      dt_year <- intertemporal_naming[[paste0(Year-1)]] %>% activate(nodes) %>% as.data.table()
      setnames(dt_year, "intertemporal_name", "past_id_com")
      dt_year <- dt_year[,.SD,.SDcols = c(ID_key, "past_id_com")] # this is the nodes from the past

      ######################### Communities from present  **********************

      dt_year2 <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% as.data.table()
      dt_year2 <- dt_year2[,.SD,.SDcols = c(ID_key, cluster_column)]
      n_com <- dt_year2[,.N,cluster_column, env=list(cluster_column=cluster_column)][,.N] # number of communities
      id_com_corr <- data.table(new_cluster_column = unique_ids[1:n_com],
                                temp_cluster_column = dt_year2[,unique(dt_year2, by = cluster_column)][,.SD,.SDcols = c(cluster_column)][[1]]) #give a unique ids to com that not 1,2,3...
      setnames(id_com_corr, "temp_cluster_column", cluster_column)
      unique_ids <- unique_ids[-c(1:n_com)] #remove ids taken from list
      setnames(dt_year2, cluster_column, "present_id_com")   # this is the nodes from the present

      ######################### Find the evolution of past communities  **********************

      if(similarity_type == "partial"){
        dt_list <- merge(dt_year, dt_year2, by = ID_key) # if "partial" we only merge on nodes that exists in the present and in the past to get the % of nodes moving from one community to the other
      }
      if(similarity_type == "complete"){
        dt_list <- merge(dt_year, dt_year2, by = ID_key, all.x=TRUE) # if "complete" we keep all past nodes, including the ones that do not exist in the present network
      }

      dt_list[,n_nodes:=.N, past_id_com]
      dt_list <- dt_list[,.N, .(present_id_com, past_id_com, n_nodes)]
      dt_list[,share:=N/n_nodes] # compute share

      evolution_from_past <- dt_list[share > threshold_similarity, same_evolution:=TRUE, env=list(threshold_similarity=threshold_similarity)]  # if above the treshold, then a communities mostly evolve into the present community
      evolution_from_past <- evolution_from_past[,.SD,.SDcols = c("present_id_com", "past_id_com", "same_evolution")]

      ######################### Find the origin of present communities  **********************

      if(similarity_type == "partial"){
        dt_list2 <- merge(dt_year, dt_year2, by = ID_key) # we do the same thing as previously but for the origin of present nodes
      }
      if(similarity_type == "complete"){
        dt_list2 <- merge(dt_year, dt_year2, by = ID_key, all.y=TRUE)
      }

      dt_list2[,n_nodes:=.N, present_id_com]
      dt_list2 <- dt_list2[,.N, .(present_id_com, past_id_com, n_nodes)]
      dt_list2[,share:=N/n_nodes]

      origin_of_present <- dt_list2[share > threshold_similarity, same_origin:=TRUE, env=list(threshold_similarity=threshold_similarity)] # if above the treshold, then a communities mostly originate from past community
      origin_of_present <- origin_of_present[,.SD,.SDcols = c("present_id_com", "past_id_com", "same_origin")]

      ######################### Compare communities across time according to tresholds **********************

      compare <- dt_year2[,unique(dt_year2, by = "present_id_com")][,.SD,.SDcols = c("present_id_com")] # we get the list of all our present communities
      compare <- merge(compare, evolution_from_past[same_evolution==TRUE], by="present_id_com", all.x=TRUE)  # we merge past communities that respect the treshold in evolution to get correspondence
      compare <- merge(compare, origin_of_present[same_origin==TRUE], by="present_id_com", all.x=TRUE)  # we merge past communities that respect the treshold in origin to get correspondence
      compare <- compare[same_evolution==TRUE & same_origin==TRUE]   # we keep communities that respect both tresholds
      compare[past_id_com.x==past_id_com.y, intertemporal_name:=paste0(past_id_com.x)] # we past the name of the old communities as present communities
      compare <- compare[past_id_com.x==past_id_com.y, .SD,.SDcols = c("present_id_com", "intertemporal_name")] # we only keep name for rows where both communities are the same (i.e., one community meeting both tresholds)
      setnames(compare, "present_id_com", cluster_column)

      ######################### Inject new names **********************

      final_names <- merge(id_com_corr, compare, by = cluster_column, all.x = TRUE) # we get the new names
      final_names[is.na(intertemporal_name), intertemporal_name:=new_cluster_column] # if there is no correspodndance, a present community is a new community, and is given a new name
      final_names <- final_names[, .SD,.SDcols = c(cluster_column, "intertemporal_name")]

      tbl <- list_graph[[paste0(Year)]] %>% activate(nodes) %>% left_join(final_names)
      intertemporal_naming[[paste0(Year)]] <- tbl
    }
  }
  names(intertemporal_naming) <- c(old_list_name)
  return (intertemporal_naming)
}
