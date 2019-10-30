read_customer_data <- function(filePath){
  balance_open <- read.csv(filePath, sep = ",", header = FALSE);
  return(balance_open);
}

save_clustering_result <- function(clustering_result, filePath){
  save(clustering_result, file = filePath);
  rm(clustering_result);
}

load_clustering_result <- function(filePath) {
  env <- new.env()
  nm <- load(filePath, env)[1]
  env[[nm]]
  return(env[[nm]]);
}