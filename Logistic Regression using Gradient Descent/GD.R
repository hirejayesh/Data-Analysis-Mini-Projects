
auto<-read.table("auto.txt", header=T)
attach(auto)
auto <- auto[c("horsepower", "weight", "year")]
auto<-data.matrix(auto)
auto <- scale(auto)

ori1<-ifelse(origin==1,1,0)
ori2<-ifelse(origin==2,1,0)
ori3<-ifelse(origin==3,1,0)

high <- ifelse(mpg>=23,1,0)

auto<-data.frame(auto,ori1, ori2,ori3, high)

set.seed(0893)
index <- sample(1:nrow(auto), 198)
autoTrain<-auto[train_index,]
autoTrain.X<-autoTrain[,1:6]
autoTrain.Y<-autoTrain[,7]
autoTest<-auto[-train_index,]
autoTest.X<-autoTest[,1:6]
autoTest.Y<-autoTest[,7]

learning_rate <- 0.0001
num_iterations <- 1000

logistic <- function(data.X, beta){
	px <- c()
	for(i in c(1:nrow(data.X))){
		prob <- sigmoid(data.X[i,]%*%beta)
		px <- c(px, prob)
	}
	return(px)
}

sigmoid <- function(m){
	z  <- 1/(1+exp(-m))
	return(g)
}

grad_iterate <- function(){
	for(i in c(1:num_iterations)){
		beta <- beta - learning_rate * grad(autoTrain.X,autoTrain.Y,beta,learning_rate,num_iterations)
	}
	return(beta)
}

grad <- function(autoTrain.X,autoTrain.Y,beta,learning_rate,num_iterations){
	gradient <- (1 / nrow(autoTrain.Y)) * (t(autoTrain.X) %*% (1/(1 + exp(-autoTrain.X %*% t(beta))) - autoTrain.Y))
	return(t(gradient))
}

compute_mse <- function(actual, pred){
	error = 0
	for(i in c(1:length(actual))){
		error <- error + (actual[i]-pred[i])^2
	}
	mse <- error/length(actual)
	return(mse)
}


for(i in c(1:100)){
	x<-runif(1,-0.7,0.7)
	#inital weight
	beta <- matrix(rep(x, ncol(auto)-1), nrow=1)
	#after gradient descent
	beta <- grad_iterate()
	mse <- c()
	pred <-logistic(autoTest.X, beta)
	mse1 <- compute_mse(autoTest.Y,pred)
	mse <- c(mse,mse1)
}

boxplot(mse)



	


	
	

