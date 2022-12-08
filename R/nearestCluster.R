#' Find closest cluster
#'
#' @param u Cluster label
#'
#' @param rep_pts_df A data frame containing the representative points for all clusters
#'
#' @return A list containing the cluster ID of the closest cluster and the closest distance
#'
#' @import nabor
#'
#' @examples
#' nearestCluster(1, rep_pts_df)
#'

nearestCluster <- function(u, rep_pts_df){
  u_rep <- rep_pts_df[rep_pts_df$clus.lab == u, 1:2]
  other_rep <- rep_pts_df[rep_pts_df$clus.lab != u, 1:2]

  nab <- nabor::knn(u_rep, other_rep, k = 1)

  clus_idx <- which.min(nab$nn.dists)
  clus_dist <- nab$nn.dists[clus_idx]
  clust_id <- clus_id <- rep_pts_df$clus.lab[rep_pts_df$clus.lab != u][clus_idx]

  return(list(min.dist = clus_dist, id = clus_id))
}
