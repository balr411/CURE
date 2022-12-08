#' Find the representative points of a cluster
#'
#' @param x A data frame containing the points in the cluster
#'
#' @param npt Number of representative points to use
#'
#' @param alpha Shrinkage parameter
#'
#' @return A data frame containing the x and y coordinates of the representative points
#'
#' @examples
#' clus_pts <- data.frame(x = iris$Sepal.Length, y = iris$Sepal.Width, clus.lab = 1:length(iris$Sepal.Width))
#' rep_pts(clus_pts, 3, 0.75)

rep_pts <- function(x, npt = 3, alpha = 0.5){
  max_dist <- function(x, pts){ #could do an iteration of this with stats::dist which would probably be faster
    idx_max <- which.max(rowSums(apply(pts, 1, function(pt_row) (x[,1]-pt_row[1])^2+(x[,2]-pt_row[2])^2),na.rm = T)) #should we be taking the square roots here?
    return(list(pt = x[idx_max,], idx = idx_max)) #return both the index and the farthest point
  }

  #Note that if there are less points in the cluster than representative points, all points are the well-scattered points
  n_iter <- min(npt, nrow(x))
  centroid <- colMeans(x)

  if(n_iter > nrow(x)){
    pts <- matrix(NA, nrow = n_iter, ncol = 2)
    #Get first scattered point
    #Note that we only use the centroid to find the first scattered point and then don't use it anymore
    mat <- matrix(centroid, nrow = nrow(x), ncol = 2, byrow = T)
    dists_centroid <- rowSums((x - mat)^2)
    idx_max <- which.max(dists_centroid)
    pts[1,] <- x[idx_max,]
    x <- x[-idx_max,] #Careful of when we have few points left in x maybe?

    #Now loop to find the other well scattered points
    for(i in 2:n_iter){
      dist_curr <- max_dist(x, pts)
      pts[i,] <- dist_curr$pt
      x <- x[-dist_curr$idx,]
    }
  }else{
    pts <- x
  }

  centroid_mat <- matrix(centroid, nrow = nrow(pts), ncol = 2, byrow = T)
  repts = pts + alpha*(centroid_mat - pts)
  return(repts)
}
