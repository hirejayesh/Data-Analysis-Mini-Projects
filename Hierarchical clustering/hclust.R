#calculating euclidean distance between row x and row y
dis <-function(x,y){
	sqr <- 0	
	for(i in 1:length(x)){
		sqr <- sqr + (x[i] - y[i])^2
      }
	dist <- sqrt(sqr)
	return(dist)
}

linkage_dist <- function(x, y, linkage){
	min_dist <- 0
	max_dist <- 0
	avg_dist <- 0
	centroid_dist <- 0
	if(is.matrix(x)){
		row <- nrow(x)
	}
	else{
		row <- 1
	}
	if(is.matrix(y)){
		col <- nrow(x)
	}
	else{
		col <- 1
	}
	# mat[i,j] stores distance between ith row of x and jth row of y
	mat <- matrix(nrow = row, ncol= col, byrow= TRUE)
	for(i in 1:row){
		for(i in 1:col){
			if(is.matrix(x) && is.matrix(y)){
				mat[i,j] <- dis(x[i,], y[j,])
			}else if(is.matrix(x) && is.vector(y)){
				mat[i,j] <- dis(x[i,], y)
			}else if(is.vector(x) && is.matrix(y)){
				mat[i,j] <- dis(x, y[j,])
			}else{
				mat[i,j] <- dis(x, y[j,])
			}
		}
	}
	
	centroid_x <- c()
	centroid_y <- c()
	if(is.matrix(x)){
		for(i in 1:ncol(x)){
			centroid_x[i] <- mean(x[,i])
	}else{
		centroid_x <- x
	}
	if(is.matrix(y)){
		for(i in 1:ncol(y)){
			centroid_y[i] <- mean(y[,i])
	}else{
		centroid_y <- y
	}
	
	min_dist <- min(mat)
	max_dist <- max(mat)
	avg_dist <- sum(mat)/(row*col)
	centroid_dist <- dis(centroid_x,centroid_y)
	
	if(linkage=="single")
		return(min_dist)
	if(linkage=="complete")
		return(max_dist)
	if(linkage=="average")
		return(avg_dist)
	if(linkage=="centroid")
		return(centroid_dist)
}


hcluster <- function(data, link){
	if(!is.matrix(data)) data<- as.matrix(data)
	
	#considering row index as marking of the rows for clustering
	#initailizing  matrix storing the clusters
	#initially column 1 stores indivisual row nos
	#gradually, we will store clusters in successive colmuns

	 
	mat<- matrix(0, nrow=nrow(data), ncol=nrow(data)
	for (i in 1:nrow(data)){
		mat[i,1]<-i
	}
	
	linkage<- link
	#top level cluster will contain all rows
	# and it will be stored in first row of mat
	#so compute till first row is not filled up
	while(mat[1,nrow(data)){
		for (i in 1:nrow(data)) {
			for(j in 1:nrow(data)){
				dist <- c()
				if(i != j){
					dis <- linkage_dist(data[i,], data[j,], linkage)
					dist<- c(dist, dis)
				}
			}
			min_index <- which(dist==min(dist))
			m <- 1
			while(mat[i,m] !=0){
				m <- m+1
			}
			m<- m+1
			k<-1
			while(mat[min_index,k] !=0){
				if(i<j){
					mat[i,m] <- mat[j,k]
				}else if{
					mat[j,m] <- mat[i,k]	
				}
				m <- m+1
				k <- k+1
			}
			print(mat)
		}
	}
	return(mat)
}	
				
				
data <- read.table("nci.data.txt", header=T, sep=",")
cluster1<- hcluster(data,"single")
cluster1<- hcluster(data,"complete")
cluster1<- hcluster(data,"average")
cluster1<- hcluster(data,"centroid")

km<- kmeans(data,2)
print(km$cluster)
km<- kmeans(data,3)
print(km$cluster)
km<- kmeans(data,5)
print(km$cluster)
km<- kmeans(data,10)
print(km$cluster)










