balanace_smooth <- function (balance_matrix, custom_df = 25) {
  balance_smooth_matrix <- c();
  for(i in 1:length(balance_matrix[1, ])){
    sm <- smooth.spline(balance_matrix[, i], df = custom_df);
    balance_smooth_matrix <- cbind(balance_smooth_matrix, sm[[2]]);
  }
  
  write.table(balance_smooth_matrix, "../data/balance_smooth.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  return(balance_smooth_matrix);
}

normalize <- function(balance_matrix, isSmooth = TRUE){
  balance_nor <- c();
  for(i in 1:length(balance_matrix[1, ])){
    x <- (balance_matrix[, i] - min(balance_matrix[, i])) / (max(balance_matrix[, i]) - min(balance_matrix[, i]))
    x[is.na(x)] <- 0;
    balance_nor <- cbind(balance_nor, x);
  }
  if(isSmooth) {
    write.table(balance_nor, "../data/balance_smooth_nor.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  } else {
    write.table(balance_nor, "../data/balance_nor.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  }
  
  return(balance_nor)
}

#Anomally detection based distance
get_outlier_score <- function(balance_matrix, k) {
  matrix <- t(balance_matrix);
  colnames(matrix) <- 1:ncol(matrix);
  rownames(matrix) <- 1:nrow(matrix);
  outlier_score <- KNN_SUM(matrix, k);
  
  names(outlier_score) <- 1:nrow(matrix);
  outlier_score <- sort(outlier_score, decreasing = TRUE);
  
  return(outlier_score);
}

#Filter anomally
filter_annomaly <- function(sorted_outlier_score, outlier_score_threshold, balance_open, balance_smooth_nor){
  outlier_index <- as.integer(names(sorted_outlier_score)[sorted_outlier_score > 1300]);
  
  print("Write balance open outlier to balance_open_annomally.csv");
  write.table(balance_open[, outlier_index], "../data/balance_open_anomally.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  
  print("Write balance open after filter to balance_open_clean.csv");
  write.table(balance_open[, -outlier_index], "../data/balance_open_clean.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  
  print("Write balance smooth normalized and filtered  outlier to balance_smooth_nor_clean.csv");
  write.table(balance_smooth_nor[, -outlier_index], "../data/balance_smooth_nor_clean.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  
  print("Write balance smooth normalized outlier to balance_smooth_nor_anomally.csv");
  write.table(balance_smooth_nor[, outlier_index], "../data/balance_smooth_nor_anomally.csv", row.names = FALSE, col.names = FALSE, sep = ",");
  
  return(outlier_index);
}