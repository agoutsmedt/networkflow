# Local test on real data: the references of the Journal of Monetary
# Economics in the authors' Scopus database. Skipped on CRAN, in CI and
# wherever the database is absent: Scopus data cannot be distributed with the
# package. It checks the invariants of the whole pipeline on a corpus much
# larger than the examples.

test_that("the dynamic pipeline runs on a real Scopus corpus", {
  skip_on_cran()
  skip_on_ci()
  db <- path.expand("~/data/scopus/scopus.duckdb")
  skip_if_not(file.exists(db), "Scopus database not available")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  nodes <- DBI::dbGetQuery(con, "
    SELECT scopus_id AS source_id, dc_title AS title,
           CAST(substr(prism_cover_date, 1, 4) AS INTEGER) AS year
    FROM article
    WHERE prism_publication_name = 'Journal of Monetary Economics'
      AND substr(prism_cover_date, 1, 4) BETWEEN '1990' AND '1999'")
  refs <- DBI::dbGetQuery(con, sprintf("
    SELECT citing_id AS source_id, cited_id AS target_id
    FROM reference WHERE citing_id IN (%s)",
    paste0("'", nodes$source_id, "'", collapse = ",")))
  skip_if(nrow(refs) == 0, "no references")

  networks <- build_dynamic_networks(nodes, refs, "source_id", "target_id",
                                     time_variable = "year", time_window = 5,
                                     window_step = 1,
                                     method = "strength",
                                     edges_threshold = 2, filter_components = TRUE,
                                     verbose = FALSE)
  expect_length(networks, 6)
  networks <- add_clusters(networks, clustering_method = "leiden",
                           objective_function = "modularity", seed = 1, verbose = FALSE)
  networks <- merge_dynamic_clusters(networks, "cluster_leiden", "source_id",
                                     similarity_type = "partial", method = "majority", threshold_similarity = 0.5001)
  networks <- name_clusters(networks, method = "tf-idf", name_dynamic_clusters = TRUE,
                            cluster_id = "dynamic_cluster_leiden", text_columns = "title",
                            clean_word_method = "none")
  all_nodes <- dplyr::bind_rows(lapply(networks, node_table))
  expect_false(anyNA(all_nodes$dynamic_cluster_leiden))
  expect_false(anyNA(all_nodes$cluster_label))
  alluv <- networks_to_alluv(networks, "dynamic_cluster_leiden", "source_id")
  expect_equal(alluv[, sum(y_alluv), by = window]$V1, rep(1, 6))
})
