# Package index

## Step 1: Building networks

Project a table of links between two kinds of entities (articles and
references, authors and papers…) onto a network, for the whole corpus or
one network per time window, and keep its main components.

- [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  **\[experimental\]** : Build a single network from bipartite links
- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  **\[experimental\]** : Build one network, or one network per time
  window, from bipartite links
- [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
  **\[experimental\]** : Keep the main component(s) of networks

## Step 2: Finding and reading clusters

Detect communities, follow them across time windows, name them and
characterise the role of their members.

- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  **\[experimental\]** : Detect communities in networks
- [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  **\[experimental\]** : Follow clusters across time windows
- [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
  **\[experimental\]** : Give readable names to clusters
- [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
  **\[experimental\]** : Find the terms that distinguish groups of nodes
  (tf-idf)
- [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
  **\[experimental\]** : Characterise the role of nodes within and
  between clusters

## Step 3: Drawing networks

Compute coordinates, colours and label positions, and plot the networks
or explore them in an interactive application.

- [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
  **\[experimental\]** : Compute the coordinates of nodes
- [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  [`color_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  **\[experimental\]** : Colour the nodes and edges of networks, or the
  clusters of an alluvial
- [`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  [`prepare_label_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  : Position the labels of clusters in network and alluvial plots
- [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
  : Plot networks
- [`launch_network_app()`](https://agoutsmedt.github.io/networkflow/reference/launch_network_app.md)
  **\[experimental\]** : Launch an Interactive Shiny App to Explore a
  Network Graph

## Step 4: Drawing the history of clusters

Turn a list of networks with dynamic clusters into an alluvial plot.

- [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)
  **\[experimental\]** : Turn a list of networks into the table of an
  alluvial plot
- [`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
  **\[experimental\]** : Order the clusters of an alluvial to reduce
  crossing flows
- [`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
  : Plot the evolution of dynamic clusters as an alluvial

## Example data

The explanations of the 1970s US stagflation and the references they
cite, from Goutsmedt (2021), “From the Stagflation to the Great
Inflation”, Revue d’Économie Politique 131(3).

- [`Authors_stagflation`](https://agoutsmedt.github.io/networkflow/reference/Authors_stagflation.md)
  : Authors of the documents explaining the 1970s US stagflation
- [`Nodes_stagflation`](https://agoutsmedt.github.io/networkflow/reference/Nodes_stagflation.md)
  : Documents explaining the 1970s US stagflation, and the documents
  they cite
- [`Ref_stagflation`](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md)
  : References cited by the documents explaining the 1970s US
  stagflation
- [`Edges_coupling`](https://agoutsmedt.github.io/networkflow/reference/Edges_coupling.md)
  : Edges of a ready-made bibliographic coupling network on the
  stagflation
- [`Nodes_coupling`](https://agoutsmedt.github.io/networkflow/reference/Nodes_coupling.md)
  : Nodes of a ready-made bibliographic coupling network on the
  stagflation

## Package

- [`networkflow`](https://agoutsmedt.github.io/networkflow/reference/networkflow-package.md)
  [`networkflow-package`](https://agoutsmedt.github.io/networkflow/reference/networkflow-package.md)
  : networkflow: a research pipeline for projected and dynamic networks
