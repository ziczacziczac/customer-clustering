quantileFrame <- function(behavior){
  quant <- c(0.05, 0.1, 0.25, 0.30, 0.40, 0.50, 0.75, 0.90, 0.95)
  f <- 1:length(quant)
  if(class(behavior) == "data.frame" ||class(behavior) == "matrix"){
    for(i in 1:length(behavior[1, ])){
      q <- quantile(behavior[, i], quant)
      c <- 1:length(quant)
      for(j in 1:length(quant)){
        c[j] = q[[j]]
      }
      #print(c)
      f <- rbind(f, c)
      
    }
  } else {
    for(i in 1:length(behavior)){
      q <- quantile(behavior[[i]], quant)
      c <- 1:length(quant)
      for(j in 1:length(quant)){
        c[j] = q[[j]]
      }

      f <- rbind(f, c)
      
    }
  }
  
  f <- t(f[-1, ])
  row.names(f) <- paste(quant * 100, '%', sep = "")
  
  return(f)
}

summaryData <- function(data, cluster_lables){
  
  number_cluster = max(cluster_lables);
  
  f <- c(0, 0, 0, 0, 0, 0, 0)
  
  for(i in 1:number_cluster){
    cluster_i <- which(cluster_lables %in% i)
    col_means <- colMeans(data[, cluster_i]);
    row_means <- rowMeans(data[, cluster_i]);
    
    na <- length(cluster_i)
    max <- max(col_means)
    min <- min(col_means)
    median <- median(col_means)
    mean <- mean(col_means)
    sd <- sd(col_means)
    sd_month <- sd(row_means)
    mean_month <- mean(row_means)
    
    f <- cbind(f, c(na, max, min, median, mean, sd, sd_month, mean_month))
    row.names(f) <- c("SO LUONG TAI KHOAN", "GIA TRI LON NHAT", "GIA TRI NHO NHAT", "TRUNG VI", "TRUNG BINH MOI TAI KHOAN MOI THANG", "DO LECH THEO TAI KHOAN", "DO LECH CHUAN THANG", "TRUNG BINH MOI TAI KHOAN")
  }

  
  
  return(f[, -1])
}



summaryClusters <- function(data){
  
  f <- c(0, 0, 0, 0, 0, 0)
  for(i in 1:length(data)){
    
    number <- length(data[[i]])
    max <- max(data[[i]])
    min <- min(data[[i]])
    median <- median(data[[i]])
    mean <- mean(data[[i]])
    sd <- sd(data[[i]])
    
    f <- cbind(f, c(number, max, min, median, mean, sd))
  }
  row.names(f) <- c("SO TAI KHOAN", "GTLN", "GTNN", "TRUNG VI", "TRUNG BINH", "DO LECH CHUAN")
  return(f[, -1])
}


arima_model_cluster <- function(data, cluster_labels){
  windowsFonts(
    A=windowsFont("Times New Roman"),
    B=windowsFont("Bookman Old Style"),
    C=windowsFont("Comic Sans MS"),
    D=windowsFont("Symbol")
  )
  number_cluster <- max(cluster_labels)
  f <- c(0, 0, 0)
  par(mfrow = c(2, 2))
  coll = c("red", "black", "orange", "blue")
  for(i in 1:number_cluster){
    list_cluster_id_i <- which(cluster_labels %in% i)
    write.table(list_cluster_id_i, paste("cluster_", i, ".csv", sep = ""), row.names = FALSE, col.names = FALSE)
    cluster_data <- data[, list_cluster_id_i]
    h <- hts(cluster_data)
    s <- aggts(h)[, 1]
    s_model <- auto.arima(s)
    summed_series <- aggts(h)[, 1]
    plot(summed_series / 10 ^ 3, type = "l", col = coll[i], main = paste("CLUSTER", i), ylab = "", xlab = "", lwd = 2, axes = F, family = "A", xlim = c(0, 50), cex.lab = 2)
    axis(1, family = 'A', font = 2)
    axis(2, family = 'A', font = 2)
    p <- length(s_model$model$phi)
    q <- length(s_model$model$theta)
    d <- length(s_model$model$Delta)
    #print(paste(p, d, q))
    f <- rbind(f, c(p, d, q))
  }
  return(f[-1, ])
}
