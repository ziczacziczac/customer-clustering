clustering_result_analysis <- function(savedClusteringResultFilePath){
  for (path in savedClusteringResultFilePath) {
    print("#########################################################");
    print(path);
    
    clustering_result <- load_clustering_result(path);
    for (i in 1:length(clustering_result)) {
      
    
      print(paste("Number clusters:", i))
      #print(paste("Mean distance of clusters:", mean(clustering_result[[i]]@clusinfo[, 2])))
      #print(paste("Difference between number of elements:", sd(clustering_result[[i]]@clusinfo[, 1])))
      print(paste("Elased time:", clustering_result[[i]]@proctime[3]))
      #print(paste("Mean silhouete value:", silhouette_mean(clustering_result[[i]]@cluster, clustering_result[[i]]@distmat)))
      #print(cvi(clustering_result[[i]], b = NULL, type = "internal"))
    }
    remove(clustering_result)
  }
}

clustering_result_analysis_for_one <- function(path, distmat){
 
  print("#########################################################");
  print(path);
  print(dim(distmat))
  clustering_result <- load_clustering_result(path);
  for (i in 1:length(clustering_result)) {
    clustering_result[[i]]@distmat <- distmat
    print(paste("Number clusters:", i))
    #print(paste("Mean distance of clusters:", mean(clustering_result[[i]]@clusinfo[, 2])))
    #print(paste("Difference between number of elements:", sd(clustering_result[[i]]@clusinfo[, 1])))
    print(paste("Elased time:", clustering_result[[i]]@proctime[3]))
    #print(paste("Mean silhouete value:", silhouette_mean(clustering_result[[i]]@cluster, distmat)))
    #print(cvi(clustering_result[[i]], b = NULL, type = "internal"))
  }
  
}

plot_clustering_result <- function(savedClusteringResultFilePath, data, dist_types, number_sample = 10) {
  windowsFonts(
    A=windowsFont("Times New Roman"),
    B=windowsFont("Bookman Old Style"),
    C=windowsFont("Comic Sans MS"),
    D=windowsFont("Symbol")
  )
  for (m in 1:length(savedClusteringResultFilePath)) {
    print("#########################################################");
    path <- savedClusteringResultFilePath[m]
    dist_type <- dist_types[m] 
    print(path);
    
    clustering_result <- load_clustering_result(path);
    
    for (i in 2:2) {
      print(paste("Number clusters:", i + 2))
      
      par(mfrow = c(2, 2))
      
      result <- clustering_result[[i]];
      centroids <- result@centroids;
      cluster_lables <- result@cluster
      
      for(j in 1:length(centroids)) {
        member_of_clusters <- which(cluster_lables %in% j);
        si <- sorted_index(data, centroids[[j]], member_of_clusters, dist_type)
        plot(centroids[[j]], col = "red", lwd = 4, ylim = c(0, 1), xlim = c(0, 48), type = "l", main = paste("Cluster", j), xlab = "", ylab = "", axes = F, font.lab = 2, family = "A")
        axis(1, family = 'A', font = 2)
        axis(2, family = 'A', font = 2)
        if(length(si) < number_sample){
          number_sample <- length(si);
        }
        for(k in 1:number_sample) {
          lines(data[, si[k]], col = "gray66");
        }
        lines(centroids[[j]], col = "red", lwd = 4)
        
      }
      
    }
    remove(clustering_result)
  }
}

sorted_index <- function(data, centroid_series, cluster_member_index, dist_type) {
  dis <- c();
  for(i in 1:length(cluster_member_index)) {
    if(dist_type == "dtw") {
      dis[i] <- dtw(data[, cluster_member_index[i]], centroid_series)$distance;
    } else {
      dis[i] <- sdtw(data[, cluster_member_index[i]], centroid_series);
    }
  }
  
  names(dis) <- cluster_member_index;
  sorted_dis <- sort(dis)
  return(as.integer(names(sorted_dis)));
}