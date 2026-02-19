# Build One or Multiple Networks from Bipartite Links

**\[experimental\]**

`build_dynamic_networks()` builds one or several `tbl_graph` networks
from a node table (`source_id`) and a bipartite link table (`source_id`
-\> `target_id`).
[`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
is a wrapper for a single network.

It supports two backbone extraction methods:

- structured filtering using coupling/cooccurrence measures from
  [biblionetwork](https://agoutsmedt.github.io/biblionetwork/);

- statistical filtering using null models from
  [backbone](https://github.com/zpneal/backbone) (Neal 2022) .

The function can build a single network or multiple networks across time
windows.

## Usage

``` r
build_dynamic_networks(
  nodes,
  directed_edges,
  source_id,
  target_id,
  time_variable = NULL,
  time_window = NULL,
  projection_method = c("structured", "statistical"),
  model = c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
  alpha = NULL,
  cooccurrence_method = c("coupling_angle", "coupling_strength", "coupling_similarity"),
  edges_threshold = 1,
  overlapping_window = FALSE,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...,
  backbone_args = list(),
  verbose = TRUE
)
```

## Arguments

- nodes:

  Table of nodes and their metadata. One row per node. For example, a
  table of articles with identifiers, authors, publication year, etc.

- directed_edges:

  Table of bipartite links between `source_id` nodes and `target_id`
  entities (e.g., article -\> reference, author -\> paper).

- source_id:

  Quoted name of the source-side node identifier.

- target_id:

  Quoted name of the target-side identifier linked to each source node.

- time_variable:

  Optional name of the column with a temporal variable (e.g.,
  publication year).

- time_window:

  Optional size of the time window (in units of `time_variable`) to
  construct temporal networks.

- projection_method:

  method used to extract the network backbone. Choose between:

  - `"structured"`: uses cooccurrence measures from the
    [biblionetwork](https://agoutsmedt.github.io/biblionetwork/)
    package;

  - `"statistical"`: uses statistical models from the
    [backbone](https://github.com/zpneal/backbone) package. Defaults to
    `"structured"`. The `"statistical"` method can be computationally
    slow on large networks.

- model:

  Statistical null model from
  [backbone](https://github.com/zpneal/backbone): one of `"sdsm"`,
  `"fdsm"`, `"fixedfill"`, `"fixedrow"`, `"fixedcol"`. Required if
  `projection_method = "statistical"`.

- alpha:

  Significance threshold for statistical backbone filtering. Required if
  `projection_method = "statistical"`. Lower values keep fewer edges.

- cooccurrence_method:

  For `projection_method = "structured"`, choose the coupling method:

  - `"coupling_angle"`

  - `"coupling_strength"`;

  - `"coupling_similarity"`.

- edges_threshold:

  Threshold used to filter weak edges in structured mode.

- overlapping_window:

  Logical. If `TRUE`, builds networks using rolling time windows.

- compute_size:

  Logical. If `TRUE`, computes the number of incoming edges per node
  (e.g., citation count).

- keep_singleton:

  Logical. If `FALSE`, removes nodes with no edges in the final network.

- filter_components:

  Logical. If `TRUE`, keeps only the main component(s) using
  [`networkflow::filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

- ...:

  Additional arguments passed to
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

- backbone_args:

  Optional list of additional arguments passed to the backbone
  extraction call. If `backbone_args` includes `alpha` or `model`, those
  values override function arguments.

- verbose:

  Logical. If `TRUE`, displays progress messages.

## Value

- A single tidygraph object if `time_window` is `NULL`;

- A list of tidygraph objects (one per time window) otherwise.

## Details

The function uses bipartite links (`source_id` -\> `target_id`) to
produce source-side networks.

If `time_variable` and `time_window` are provided, it builds one network
per time window (rolling or non-overlapping). Otherwise it builds a
single network.

`projection_method = "structured"` applies coupling/cooccurrence
filtering. `projection_method = "statistical"` applies a statistical
backbone model.

## References

Neal ZP (2022). “backbone: An R Package to Extract Network Backbones.”
*PLOS ONE*, **17**, e0269137.
[doi:10.1371/journal.pone.0269137](https://doi.org/10.1371/journal.pone.0269137)
.

## See also

[`biblionetwork::biblio_coupling()`](https://agoutsmedt.github.io/biblionetwork//reference/biblio_coupling.html),
[`backbone::backbone()`](https://rdrr.io/pkg/backbone/man/backbone.html)

## Examples

``` r
library(networkflow)

nodes <- networkflow::Nodes_stagflation |>
  dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

# Structured backbone (cooccurrence)
net_structured <- build_dynamic_networks(
nodes = nodes,
directed_edges = references,
source_id = "source_id",
target_id = "target_id",
time_variable = "source_year",
time_window = 20,
projection_method = "structured",
cooccurrence_method = "coupling_similarity",
edges_threshold = 1
)
#> ℹ Backbone method selected: structured
#> ℹ Keep_singleton == FALSE: removing the nodes that are alone with no edge. 
#> Warning: Your last network is shorter than the other(s) because the cutting by time
#> window does not give a round count. The last time unity in your data is 2013,
#> but the upper limit of your last time window is 2014.
#> 
#> ── Generation of the network for the 1975-1994 time window. ────────────────────
#> 
#> ── Generation of the network for the 1995-2014 time window. ────────────────────

# Statistical backbone (backbone package)
net_statistical <- build_dynamic_networks(
nodes = nodes,
directed_edges = references,
source_id = "source_id",
target_id = "target_id",
time_variable = "source_year",
time_window = 20,
projection_method = "statistical",
model = "sdsm",
alpha = 0.05,
backbone_args = list(mtc = "holm")
)
#> ℹ Backbone method selected: statistical
#> ℹ Keep_singleton == FALSE: removing the nodes that are alone with no edge. 
#> Warning: Your last network is shorter than the other(s) because the cutting by time
#> window does not give a round count. The last time unity in your data is 2013,
#> but the upper limit of your last time window is 2014.
#> 
#> ── Generation of the network for the 1975-1994 time window. ────────────────────
#> 
#> ── Generation of the network for the 1995-2014 time window. ────────────────────
```
