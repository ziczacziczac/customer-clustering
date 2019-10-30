silhouette_manual <- function(labels, distMatrix){
    number_cluster = max(labels);
    sildist = c(1:(number_cluster));

    for(i in 1:length(labels)){
        own_cluster = labels[i];

        member = which(labels %in% own_cluster);


        c <- c();
        if(length(member) == 1){
            c[own_cluster] = 0;
        } else {
            call = cbind(i, member);
            distance_to_own = sum(distMatrix[call]) / (length(member) - 1);
            c[own_cluster] = distance_to_own;
        }
        #print(call);
        for(j in 1:number_cluster){
            if(j != own_cluster){
                m = which(labels %in% j);

                call = cbind(i, m);
                c[j] = sum(distMatrix[call]) / length(m);

            }
        }

        sildist = rbind(sildist, c);
    }
    return(sildist[-1, ]);
}

silhouette_table <- function(labels, sildist){
    table = c(1, 1, 1, 1);
    for(i in 1:length(sildist[, 1])){
        own_cluster = labels[i];
        mindistance_to_neighbor = max(sildist[i, ]);
        neighbor = own_cluster;
        for(j in 1:length(sildist[i,])){
            if(j != own_cluster){
                if(mindistance_to_neighbor >= sildist[i, j]){
                    mindistance_to_neighbor = sildist[i, j];
                    neighbor = j;
                }
            }
        }

        ai = sildist[i, own_cluster];
        bi = sildist[i, neighbor];
        if(sildist[i, own_cluster] == 0){
            silhouette = 0;
        } else {
            silhouette = (bi - ai) / max(ai, bi);
        }

        table = rbind(table, c(i, own_cluster, neighbor, silhouette));

    }
    return(table[-1, ]);
}
silhouette_mean <- function(silhouette_table){
  return(mean(silhouette_table[, 4]))
}

silhouette_mean <- function(labels, distMatrix){
  sil_manual <- silhouette_manual(labels, distMatrix)
  sil_table <- silhouette_table(labels, sil_manual)
  return(round(mean(sil_table[, 4]), 2))
}
