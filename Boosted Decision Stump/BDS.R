#calculate mse for gievn threshold of rm attribute
calc_mse_rm <- function(s){
	less_sum <- 0
	less_count <- 0
	greater_sum <- 0
	greater_count <- 0
	for(i in 1:253){
		if(train.X$rm[i] < s){
			less_sum = less_sum + train.Y[i]
			less_count = less_count + 1
		}
		else{
			greater_sum = greater_sum + train.Y[i]
			greater_count = greater_count + 1
		}
	}
	less_mean <- less_sum/less_count
	greater_mean <- greater_sum/greater_count
	
	less_square <- 0
	greater_square <- 0
	
	for(i in 1:253){
		if(train.X$rm[i] < s){
			less_square <- less_square + (train.Y[i]-less_mean)^2
		}
		else{
			greater_square <- greater_square + (train.Y[i]-greater_mean)^2
		}
	}
	train_rss <- less_square + greater_square
	train_mse <- train_rss/nrow(train.X)
	return(train_mse)
}

#calculate mse for gievn threshold of rm attribute
calc_mse_lstat <- function(s){
	less_sum <- 0
	less_count <- 0
	greater_sum <- 0
	greater_count <- 0
	for(i in 1:nrow(train.X)){
		if(train.X$lstat[i] < s){
			less_sum = less_sum + train.Y[i]
			less_count = less_count + 1
		}
		else{
			greater_sum = greater_sum + train.Y[i]
			greater_count = greater_count + 1
		}
	}
	less_mean <- less_sum/less_count
	greater_mean <- greater_sum/greater_count
	
	less_square <- 0
	greater_square <- 0
	
	for(i in 1:nrow(train.X)){
		if(train.X$lstat[i] < s){
			less_square <- less_square + (train.Y[i]-less_mean)^2
		}
		else{
			greater_square <- greater_square + (train.Y[i]-greater_mean)^2
		}
	}
	train_rss <- less_square + greater_square
	train_mse <- train_rss/nrow(train.X)
	return(train_mse)
}

#decision stump algorithm
#returns the attribute, its threshold, mean of less than region and mean of greater than region 
DS <- function(train.X, train.Y){	
	mse_array_rm <- matrix(nrow = nrow(train.X), ncol=1, byrow= TRUE)
	mse_array_lstat <- matrix(nrow = nrow(train.X), ncol=1, byrow= TRUE)

	#populate mse array for each threshold
	for(i in 1:253){
		
		s_rm <- train.X$rm[i]
		
		mse_array_rm[i] <- calc_mse_rm(s_rm)
		
		s_lstat <- train.X$lstat[i]
		mse_array_lstat[i] <- calc_mse_lstat(s_lstat)
	}	
	#getting least mse corresponding to attribute and its threshold
	if(min(mse_array_rm) < min(mse_array_lstat)){
		index <- which.min(mse_array_rm)
		s <- train.X$rm[index]	
	
		less_sum <- 0
		less_count <- 0
		greater_sum <- 0
		greater_count <- 0
		a<- c()
		
		for(i in 1:nrow(train.X)){
			if(train.X$rm[i] < s){
				less_sum = less_sum + train.Y[i]
				less_count = less_count + 1
			}
			else{
				greater_sum = greater_sum + train.Y[i]
				greater_count = greater_count + 1
			}
		}
		less_mean <- less_sum/less_count
		greater_mean <- greater_sum/greater_count
		
		a <- c("rm",s, less_mean, greater_mean)
		return(a)
	}
	else{
		index <- which.min(mse_array_lstat)
		s <- train.X$lstat[index]	
	
		less_sum <- 0
		less_count <- 0
		greater_sum <- 0
		greater_count <- 0
		a<- c()
		
		for(i in 1:nrow(train.X)){
			if(train.X$lstat[i] < s){
				less_sum = less_sum + train.Y[i]
				less_count = less_count + 1
			}
			else{
				greater_sum = greater_sum + train.Y[i]
				greater_count = greater_count + 1
			}
		}
		less_mean <- less_sum/less_count
		greater_mean <- greater_sum/greater_count
		
		a <- c("lstat",s, less_mean, greater_mean)
		return(a)
	}
}
DS_mse <- function(train.X, train.Y, test.X, test.Y){
	# a is in form e.g.["lstat",s, less_mean, greater_mean]
	a <- DS(train.X, train.Y)
	s <- a[2]
	less_mean <- as.numeric(a[3])
	greater_mean <- as.numeric(a[4])
	diff_square <- 0
	for(i in 1:nrow(test.X)){
		if(a[1]=="rm"){
			if(test.X$rm[i]<s){
				#a[3] is less than threshold mean
				diff_square = diff_square+ (test.Y[i]- less_mean)^2
			}else{
				#a[3] is greater than threshold mean
				diff_square = diff_square+ (test.Y[i]- greater_mean)^2
			}
		}
		else if(a[1]=="lstat"){
			if(test.X$lstat[i]<s){
				diff_square = diff_square+ (test.Y[i]- less_mean)^2
			}else{
				diff_square = diff_square+ (test.Y[i]- greater_mean)^2
			}

		}	
	}
	ds_test_mse <- diff_square/nrow(test.X)
	return(ds_test_mse)
}

#Boosted decision stump algorithm
BDS <- function(train.X, train.Y, n, B){
	r <- train.Y
	ds <- matrix(nrow=B, ncol=4,byrow=TRUE)
	f_hat <- matrix(nrow=B, ncol=nrow(train.X),byrow=TRUE)
	for(i in 1:B){
		# ds[] stores result of return array of DS algo at each iteration
		ds[i,] <- DS(train.X, r)
		for(j in 1:nrow(train.X)){
			if(ds[i,1]=="rm"){
				s_rm <- as.numeric(ds[i,2])
				less_mean_rm <- as.numeric(ds[i,3])
				greater_mean_rm <- as.numeric(ds[i,4])
				if(train.X$rm[j] < s_rm){
					f_hat[i,j] <- less_mean_rm
				}else{
					f_hat[i,j] <- greater_mean_rm
				}
			}
			else if(ds[i,1]=="lstat"){
	 			s_lstat <- as.numeric(ds[i,2])
				less_mean_lstat <- as.numeric(ds[i,3])
				greater_mean_lstat <- as.numeric(ds[i,4])
				if(train.X$lstat[j] < s_lstat){
					f_hat[i,j] <- less_mean_lstat
				}else{
					f_hat[i,j] <- greater_mean_lstat
				}
			}
			r[j] <- r[j]-n*f_hat[i,j]
		}
	}
	return(f_hat)	
}

BDS_mse <- function(train.X, train.Y, test.X, test.Y, n, B){
	f_hat <- BDS(train.X, train.Y, n, B)
	diff_square <- 0
	for(i in 1:nrow(test.X)){
		#fhat[,i] is entry for each observation
		prediction_rule <- sum(n*f_hat[,i])
		diff_square <- diff_square + (test.Y[i]- prediction_rule)^2
	}
	bds_test_mse <- diff_square/nrow(test.X)
	return(bds_test_mse)
}

# getting boston data and calculating both test mse and plotting graph 
library(MASS)
data("Boston")
set.seed(0831)
train_index <- sample(1:nrow(Boston),nrow(Boston)/2)
train.X <- Boston[train_index,]
test.X <- Boston[-train_index,]
train.Y <- train.X$medv
test.Y <- test.X$medv
ds_test_mse<- DS_mse(train.X, train.Y, test.X, test.Y)
bds_test_mse <- BDS_mse(train.X, train.Y, test.X, test.Y, 0.01, 1000)
print(ds_test_mse)
print(bds_test_mse)				
trees<- floor(runif(50, 1000, 10000))
mse <- c()
#mse as a function of no. of trees
for(i in 1:50){
	f <- trees[i]
	bds_test_mse <- BDS_mse(train.X, train.Y, test.X, test.Y, 0.01, f)
	mse <- c(mse,bds_test_mse)
}
plot(trees, mse)
	
			
			


