% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_alluvial.R
\name{plot_alluvial}
\alias{plot_alluvial}
\title{Plot the Alluvial}
\usage{
plot_alluvial(
  alluv_dt,
  intertemporal_cluster_column,
  node_id,
  window_column = "window",
  color_column = "color",
  color_alluvial = FALSE,
  color = NULL,
  minimize_crossing = FALSE,
  prepare_label = FALSE,
  cluster_label_column = "cluster_label",
  print_plot_code = FALSE
)
}
\arguments{
\item{alluv_dt}{A data.frame of an alluvial created with \link[=networks_to_alluv]{networks_to_alluv()}.}

\item{intertemporal_cluster_column}{The column with the identifier of inter-temporal clusters. If you have used
\link[=add_clusters]{add_clusters()} and \link[=merge_dynamic_clusters]{merge_dynamic_clusters()},
it is of the form \verb{dynamic_cluster_\{clustering_method\}}.}

\item{node_id}{The column with the unique identifier of each node.}

\item{window_column}{The column of the alluvial with your time window. By default, "window", as created by
\link[=networks_to_alluv]{networks_to_alluv()}.}

\item{color_column}{The column with the colors associated to the  categories of \code{intertemporal_cluster_column}.
By default, "color", as the result of \link[=color_alluvial]{color_alluvial()}.}

\item{color_alluvial}{If no color has been set for the categories of \code{intertemporal_cluster_column}, you
can attribute them colors by setting \code{color_alluvial} to \code{TRUE}.
\link[=color_alluvial]{color_alluvial()} will be used.}

\item{color}{If \code{color_alluvial}is \code{TRUE}, the parameter \code{color} will be used to color the categories of the
\code{intertemporal_cluster_column}. It may be a vector of colors (in a character format)
or a two columns data frame with the first column as
the distinct observations of the \code{intertemporal_cluster_column} and a second column with the
vector of colors you want to use. If \code{NULL} colors will be automatically chosen by
\link[=color_alluvial]{color_alluvial()}?}

\item{minimize_crossing}{If \code{TRUE}, \link[=minimize_crossing_alluvial]{minimize_crossing_alluvial()} is
run to reorder the \code{intertemporal_cluster_colum} to limit overlapping in the plot.}

\item{prepare_label}{If \code{TRUE}, \link[=prepare_label_alluvial]{prepare_label_alluvial()} is used
to create a column \code{label_x} with}

\item{cluster_label_column}{If \code{prepare_label} is \code{TRUE}, \link[=prepare_label_alluvial]{prepare_label_alluvial()} is
used and the values of \code{cluster_label_column} are taken to be displayed as label.
By default, "cluster_label", as it is
the default name of the column created with \link[=name_clusters]{name_clusters()}.
But you may also use the same column as in \code{intertemporal_cluster_column}.}

\item{print_plot_code}{Set to \code{TRUE} if you want the ggplot2 code to be printing. It is useful if you are not
totally satisfied of the plot and want to manipulate the code yourself.}
}
\description{
An easy way to plot the alluvial by using the columns created by the different function of networkflow.
}
