#' Find distance between two clusters
#'
#' @param u Cluster label for the first cluster
#'
#' @param v Cluster label for the second cluster
#'
#' @param rep_pts_df A data frame containing the representative points for all clusters
#'
#' @return The distance between the clusters
#'
#' @import nabor
#'
#' @examples
#'rep_pts_df <- data.frame(x = iris$Sepal.Length, y = iris$Sepal.Width, clus.lab = 1:length(iris$Sepal.Width));
#'dist(1, 2,rep_pts_df)
#'

dist_clus <- function(u, v, rep_pts_df){
  u_rep <- rep_pts_df[rep_pts_df$clus.lab == u, 1:2]
  v_rep <- rep_pts_df[rep_pts_df$clus.lab == v, 1:2]

  nab <- nabor::knn(u_rep, v_rep, k = 1)

  clus_dist <- min(nab$nn.dists)

  return(clus_dist)
}
