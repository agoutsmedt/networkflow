library(biblionetwork)
library(data.table)

Nodes_coupling <- as.data.table(Nodes_stagflation)
Nodes_coupling <- Nodes_coupling[source_type == "Stagflation" & source_id %in% Ref_stagflation$source_id]
Nodes_coupling$source_id <- as.character(Nodes_coupling$source_id)
Nodes_coupling <- Nodes_coupling[,-"source_type"]

Edges_coupling <- biblio_coupling(Ref_stagflation, "source_id", "target_id")
Edges_coupling <- Edges_coupling[from %in% Nodes_coupling$source_id]
Edges_coupling <- Edges_coupling[to %in% Nodes_coupling$source_id]

use_data(Nodes_coupling, overwrite = TRUE)
use_data(Edges_coupling, overwrite = TRUE)
