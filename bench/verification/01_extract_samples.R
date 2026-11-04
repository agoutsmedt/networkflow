# Extraction of real-data samples for the verification of networkflow (cached outside the repo)
library(DBI)
cache <- tools::R_user_dir("networkflow", "cache"); dir.create(cache, recursive = TRUE, showWarnings = FALSE)
ids_sql <- function(x) paste0("'", x, "'", collapse = ",")

# ---- Scopus
if (!file.exists(file.path(cache, 'verif_scopus_30k.rds'))) {
con <- dbConnect(duckdb::duckdb(shared_home = FALSE), path.expand("~/data/scopus/scopus.duckdb"), read_only = TRUE)
citing <- dbGetQuery(con, "
  SELECT a.scopus_id AS source_id, a.dc_title AS title, a.prism_cover_date AS date,
         CAST(substr(a.prism_cover_date, 1, 4) AS INTEGER) AS year,
         a.prism_publication_name AS journal, a.subtype_description AS type
  FROM article a WHERE a.scopus_id IN (SELECT DISTINCT citing_id FROM reference)")
get_scopus <- function(nodes) {
  dbWriteTable(con, "tmp_ids", data.frame(id = nodes$source_id), temporary = TRUE, overwrite = TRUE)
  refs <- dbGetQuery(con, "SELECT r.citing_id AS source_id, r.cited_id AS target_id, r.first_author,
                                  r.year AS cited_year, r.title AS cited_title, r.source_title AS cited_journal
                           FROM reference r JOIN tmp_ids t ON t.id = r.citing_id")
  authors <- dbGetQuery(con, "SELECT a.scopus_id AS source_id, a.auid, a.indexed_name
                              FROM author a JOIN tmp_ids t ON t.id = a.scopus_id")
  list(nodes = nodes, refs = refs, authors = authors)
}
jme <- get_scopus(citing[citing$journal == "Journal of Monetary Economics", ])
set.seed(2026)
all30 <- get_scopus(citing[sample(nrow(citing), 30000), ])
saveRDS(jme, file.path(cache, "verif_scopus_jme.rds"))
saveRDS(all30, file.path(cache, "verif_scopus_30k.rds"))
cat("JME:", nrow(jme$nodes), nrow(jme$refs), nrow(jme$authors), "\n30k:", nrow(all30$nodes), nrow(all30$refs), nrow(all30$authors), "\n")
dbDisconnect(con, shutdown = TRUE)
}

# ---- WoS: economics articles 1990-2004
con <- dbConnect(duckdb::duckdb(shared_home = FALSE), path.expand("~/data/wos/wos.duckdb"), read_only = TRUE)
econ <- dbGetQuery(con, "
  SELECT a.ID_Art AS source_id, a.Titre AS title, a.Annee_Bibliographique AS year, j.Revue AS journal
  FROM articles a JOIN (SELECT DISTINCT IssueID, Revue FROM journals WHERE CAST(Code_Discipline AS VARCHAR) = '119') j ON a.IssueID = j.IssueID
  WHERE TRY_CAST(a.Annee_Bibliographique AS INTEGER) BETWEEN 1990 AND 2004 AND CAST(a.Code_Document AS VARCHAR) = '1' AND TRY_CAST(a.Nb_Reference AS INTEGER) > 0")
set.seed(2026)
econ <- econ[sample(nrow(econ), min(30000, nrow(econ))), ]
dbWriteTable(con, "tmp_ids", data.frame(id = econ$source_id), temporary = TRUE, overwrite = TRUE)
refs <- dbGetQuery(con, "SELECT r.ID_Art AS source_id, r.ItemID_Ref AS item_id, r.New_id2 AS new_id,
                                r.Nom AS first_author, r.Annee AS cited_year, r.Revue_Abbrege AS cited_journal
                         FROM \"references\" r JOIN tmp_ids t ON t.id = r.ID_Art")
authors <- dbGetQuery(con, "SELECT a.ID_Art AS source_id, a.Nom AS author FROM authors a JOIN tmp_ids t ON t.id = a.ID_Art")
saveRDS(list(nodes = econ, refs = refs, authors = authors), file.path(cache, "verif_wos_econ.rds"))
cat("WoS:", nrow(econ), nrow(refs), nrow(authors), "\n")
dbDisconnect(con, shutdown = TRUE)
