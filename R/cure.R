#' Cluster two-dimensional data
#'
#' @param x A vector of x-coordinates
#'
#' @param y a vector of y-coordinates
#'
#' @param k the number of desired clusters
#'
#' @param c the number of representative points to use in each cluster
#'
#' @param alpha the shrinkage factor to use
#'
#' @param verbose Print remaining number of clusters? If TRUE, when number of clusters
#' is divisible by 10, the number of clusters is printed. Default is FALSE.
#'
#' @return A list containing two outputs:
#' \item{points}{Data frame points containing columns for x, y,
#' the assigned cluster label, the closest cluster label, the distance to the
#' closest cluster, and the centroid of the cluster.}
#' \item{rep_pts}{Data frame containing the representative points of each cluster
#' with their cluster label}
#'
#' @import stats
#' @import nabor
#'
#' @examples
#' cure(threeClus$x, threeClus$y, 3, 2, 0.25)
#'

cure <- function(x, y, k, c, alpha, verbose = FALSE){
  #First initialize the data frame containing the points and the data frame containing the representative points

  df <- data.frame(x = x, y = y, clus.lab = 1:length(x))

  #Remove duplicated rows
  df <- df[!duplicated(df[,1:2]),]

  #Initialize data frame to hold representative points
  rep_pts_df <- df

  #Find closest cluster to each cluster
  key_mat <- as.matrix(stats::dist(df[,1:2]), diag = TRUE, upper = TRUE)
  #Add large value to diagonal so we can take min to find min distance between clusters
  diag(key_mat) <- 1e10

  clus_closest_dist <- apply(key_mat, 2, min)
  clus_closest <- apply(key_mat, 2, which.min)

  df$closest.cluster <- clus_closest
  df$closest.cluster.dist <- clus_closest_dist

  #Add column for mean
  df$cluster.mean.x <- df$x
  df$cluster.mean.y <- df$y

  q <- length(unique(df$clus.lab))

  while(q > k){
    if(verbose){
      if(q %% 10 == 0){
        print(q)
      }
    }


    #extract min from data frame - use u and v to keep consistent with the notation from the paper
    idx_min <- which.min(df$closest.cluster.dist)
    u <- df$clus.lab[idx_min]
    v <- df$closest.cluster[idx_min]

    ####################################################################
    #This is the merge(u,v) function from the paper
    #Get the points contained in u and v from data frame
    u_pts <- df[df$clus.lab == u, 1:2] #columns 1 and 2 contain the x and y coordinates of the representative points
    v_pts <- df[df$clus.lab == v, 1:2]

    w_pts <- rbind(u_pts,v_pts)

    #Find a new label for the merged cluster
    new_lab <- max(df$clus.lab) + 1 #guaranteed not to be used already

    #Insert Annie's function which returns the representative points for the new cluster
    rep_pts_new <- rep_pts(x = w_pts, npt = c, alpha = alpha)
    rep_pts_new <- as.data.frame(cbind(rep_pts_new, new_lab))
    names(rep_pts_new) <- c("x", "y", "clus.lab")

    #Update mean
    df$cluster.mean.x[df$clus.lab %in% c(u,v)] <- mean(df$x[df$clus.lab %in% c(u,v)])
    df$cluster.mean.y[df$clus.lab %in% c(u,v)] <- mean(df$y[df$clus.lab %in% c(u,v)])

    #Change labels
    df$clus.lab[df$clus.lab %in% c(u, v)] <- new_lab

    #Delete the representative points of u and v from the data frame
    rep_pts_df <- rep_pts_df[!(rep_pts_df$clus.lab %in% c(u,v)),]

    #Now add the new representative points
    rep_pts_df <- rbind(rep_pts_df, rep_pts_new)

    #Initialize closest cluster to w as minimum cluster label not equal to w
    w_closest <- min(df$clus.lab[df$clus.lab != new_lab])
    w_closest_dist <- dist_clus(w_closest, new_lab, rep_pts_df)

    #Loop through clusters and update all of the distances
    cluster_to_loop <- unique(df$clus.lab)
    cluster_to_loop <- cluster_to_loop[cluster_to_loop != new_lab]
    for(lab in cluster_to_loop){
      dist_curr <- dist_clus(lab, new_lab, rep_pts_df)
      if(dist_curr < w_closest_dist){
        w_closest <- lab
        w_closest_dist <- dist_curr
      }

      x_idx <- which(df$clus.lab == lab)[1]
      x.closest <- df$closest.cluster[x_idx]
      if(x.closest %in% c(u, v)){
        if(df$closest.cluster.dist[x_idx] < dist_curr){
          #Need to find closest cluster to x
          nc <- nearestCluster(lab, rep_pts_df)
          x.closest <- nc$id
          x.closest.dist <- nc$min.dist
        }else{ #closest cluster must be merged cluster
          x.closest <- new_lab
          x.closest.dist <- dist_curr
        }

        df$closest.cluster[df$clus.lab == lab] <- x.closest
        df$closest.cluster.dist[df$clus.lab == lab] <- x.closest.dist

      }else if(df$closest.cluster.dist[x_idx] > dist_curr){ #closest cluster must be merged cluster
        x.closest <- new_lab
        x.closest.dist <- dist_curr
        df$closest.cluster[df$clus.lab == lab] <- x.closest
        df$closest.cluster.dist[df$clus.lab == lab] <- x.closest.dist
      }
    }

    df$closest.cluster[df$clus.lab == new_lab] <- w_closest
    df$closest.cluster.dist[df$clus.lab == new_lab] <- w_closest_dist

    q <- length(unique(df$clus.lab))

  }

  return(list(points = df, rep_pts = rep_pts_df))

}
