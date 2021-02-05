library(biblionetwork)
library(data.table)

Nodes_coupling <- as.data.table(Nodes_stagflation)
Nodes_coupling <- Nodes_coupling[Type == "Stagflation" & ItemID_Ref %in% Ref_stagflation$Citing_ItemID_Ref]
Nodes_coupling$ItemID_Ref <- as.character(Nodes_coupling$ItemID_Ref)
Nodes_coupling <- Nodes_coupling[,-"Type"]

Edges_coupling <- biblio_coupling(Ref_stagflation, "Citing_ItemID_Ref", "ItemID_Ref")
Edges_coupling <- Edges_coupling[from %in% Nodes_coupling$ItemID_Ref]
Edges_coupling <- Edges_coupling[to %in% Nodes_coupling$ItemID_Ref]

use_data(Nodes_coupling, overwrite = TRUE)
use_data(Edges_coupling, overwrite = TRUE)
