classify.sak <- function(test,train,train.raw,w){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(distances.sak(test[i,],train,w=w))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
  
}



distance.sak <- function(vector,data,w){
  distances <-c()
  
  for (i in 1:nrow(data)){
    
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w, open.end=FALSE,open.start = FALSE)
    
    distances<- c(distances,dtw)
    
  }
  
  distances
}


fff <- function (x,y,z){
  fff <- x+y+z
  
  fff
}




distances.sak.oe <- function(vector, data, w, TF){
  distances <-c()
  
  for (i in 1:nrow(data)){
    
    if (TF ==1){
    
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                         open.end=FALSE,open.start = FALSE)
    }
    
    if (TF ==2){
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=TRUE,open.start = FALSE)
    
    }
    
    if (TF == 3){
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=FALSE,open.start = TRUE)
      
    }
    
    if (TF ==4){
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=TRUE,open.start = TRUE)
      
      
    }
    
    
    distances<- c(distances,dtw)
  }
  
  distances
  
}


classify.sak.oe <- function(test,train,train.raw,w, TF){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(distances.sak.oe(test[i,],train,w=w, TF=TF))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
  class
  
}

classify.sak.oe(wine.test.clean,wine.train.clean, wine.train, 1, 1)

wine.test[,ncol(wine.test)]



# ALL CODE



distances.sak.oe <- function(vector, data, w, TF){
  distances <-c()
  
  for (i in 1:nrow(data)){
    
    if (TF ==1){
      
      dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=FALSE,open.start = FALSE)
    }
    
    if (TF ==2){
      dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=TRUE,open.start = FALSE)
      
    }
    
    if (TF == 3){
      dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=FALSE,open.start = TRUE)
      
    }
    
    if (TF ==4){
      dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w,
                           open.end=TRUE,open.start = TRUE)
      
      
    }
    
    
    distances<- c(distances,dtw)
  }
  
  distances
  
}



classify.sak.oe <- function(test,train,train.raw,w, TF){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(distances.sak.oe(test[i,],train,w=w, TF=TF))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
  class
  
}



beetle.test <- read.csv("beetle_test.csv",header=T)

beetle.train <- read.csv("beetle_train.csv",header=T)


beetle.test.clean <- beetle.test[,c(-1,-514)]
beetle.train.clean <- beetle.train[,c(-1,-514)]



match.count(classify_dtwvm(beetle.test.clean,beetle.train.clean,beetle.train), beetle.test[,ncol(beetle.test)])


match.count(classify.sak.oe(beetle.test.clean,beetle.train.clean, beetle.train, 1, 1),beetle.test[,ncol(beetle.test)])


beetle_percents <- percents(beetle.test.clean,beetle.train.clean,beetle.train,beetle.test)



plot(x=c(1,3,5,10,15),y=wine_percent, pch=19,xlab="K", ylab="Percent Correct", main="Wine")
lines(x=c(1,3,5,10,15), y=wine_percent,col="Red")




plot(x=c(1,3,5,10,15),y=coffee_percent, pch=19,xlab="K", ylab="Percent Correct",main="Coffee")
lines(x=c(1,3,5,10,15), y=coffee_percent, col="Brown")

plot(x=c(1,3,5,10,15),y=olive_percent, pch=19, xlab="K", ylab="Percent Correct",,main="Olive Oil")
lines(x=c(1,3,5,10,15), y=olive_percent,col="green")



## Not using entire dataset of sweed leafs (samples = 70)

plot(x=c(1,3,5,10,15),y=beetle_percents, pch=19,xlab="K", ylab="Percent Correct", main="Beetle Fly")
lines(x=c(1,3,5,10,15), y=beetle_percents, col="Steel blue")



proc.time(classify.sak(wine.test.clean, wine.train.clean,wine.train,1))


ptm <- proc.time()
h <- g+1
proc.time() - ptm


system.time(classify.sak(wine.test.clean, wine.train.clean,wine.train,1))
system.time(classify.sak(wine.test.clean, wine.train.clean,wine.train,10))

cool <- system.time (g <- 1+1)

cool[1]


system.time(calcDistSakoe(dtw_vv(t(as.matrix(olive.test.clean[2,])),t(as.matrix(olive.train.clean[4,])))$costMatrix, w=5, open.start=F, open.end=FALSE))
system.time(calcDistSakoe(dtw_vv(t(as.matrix(olive.test.clean[2,])),t(as.matrix(olive.train.clean[4,])))$costMatrix, w=1, open.start=F, open.end=FALSE))



## get run times using large data set, sweed.leaf = 100

a<- system.time(classify.sak(olive.test.clean, olive.train.clean,olive.train,1))


b<- system.time(classify.sak(olive.test.clean, olive.train.clean,olive.train,2))

c<-system.time(classify.sak(olive.test.clean, olive.train.clean,olive.train,3))

d<-system.time(classify.sak(olive.test.clean, olive.train.clean,olive.train,4))

e<- system.time(classify.sak(olive.test.clean, olive.train.clean,olive.train,10))



## Vector of run times

run_times <- c(as.numeric(a[3]),as.numeric(b[3]),as.numeric(c[3]),as.numeric(d[3]),as.numeric(e[3]))




## Run time difference with Sakoe Chiba

plot(y=run_times,x=c(1,2,3,4,10),pch=19, xlab="Sakoe Chiba Band (w=)",ylab="Run Time in Seconds",
     main = "Run Times with Sakoe chiba Band")
lines(x=c(1,2,3,4,10), y=run_times)




Tf <- function(x){
  if (x==1){
    TF <- c(FALSE,FALSE)
  }
  
  if(x==2){
    TF <-c(TRUE, FALSE)
  }
  
  if (x==3){
    TF<- c(FALSE, TRUE)
  }
  
  if (x==4){
    TF <- c(TRUE, TRUE)
  }
  
  TF
}



## Optimal W and Start/end



best_w <- function(test,train,train.raw,test.raw){
  
  param <- c()
  
  warp <- c()
  
  for (j in 1:4){
    
    for (i in c(1,2,3,5))
    {
      class <- classify.sak.oe(test,train,train.raw,i,j)
      
      match <- percent_match(class, test.raw)
      
      
      ## Fill with percentages
      
      param <- c(param, match)
      
    }    
    
    warp <- c(warp, param)
  }
  warp
}


classify.sak(wine.test.clean, wine.train.clean,wine.train,1)



wine_best <- best_w(wine.test.clean,wine.train.clean,wine.train,wine.test)

coffee_best <- best_w(coffee.test.clean,coffee.train.clean,coffee.train,coffee.test)

olive_best <- best_w(olive.test.clean,olive.train.clean,olive.train,olive.test)

beetle_best <- best_w(beetle.test.clean,beetle.train.clean,beetle.train,beetle.test)


f<- c(sample(1:625, 80))




wine_best_m <- matrix(wine_best, nrow=4, ncol=4, byrow=T)


coffee_best_m <- matrix(coffee_best, nrow=4,ncol=4,byrow=T)



olive_best_m <- matrix(olive_best, nrow=4,ncol=4,byrow=T)


beetle_best_m <- matrix(beetle_best,nrow=4,ncol=4,byrow=T)


classify_dtwvm(sweed.leaf.train.clean, sweed.leaf.test.clean, sweed.leaf.train)

beetle_best_m






test <-read.csv("wine_test.csv")
train <- read.csv("wine_train.csv")


# this places all the training vectors into a matrix for us
nsamples <- length(train$id)
nlength <- length(train[1,2:235])
mat <- matrix(0, ncol=nsamples, nrow = nlength)
for (i in 1:nsamples){
  col <- train[i,2:235]
  mat[,i] <- as.matrix(col)
}



dtw_vm(as.matrix(t(wine.test.clean[1,])),as.matrix(t(as.matrix(wine.train.clean))))






beetle_best_m


wine_best_m




classify_dtwvm <- function(test,train,train.raw, windowType, window){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(dtw_vm(t(as.matrix(test[i,])),t(as.matrix(train))))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
  class
}





classify_dtwvm_w <- function(test,train,train.raw, windowType, window){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(dtw_vm(t(as.matrix(test[i,])),t(as.matrix(train)),windowType = windowType, w=w))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
  class
}




dtw_vm(t(as.matrix(wine.test.clean[2,])),t(as.matrix(wine.train.clean)))
dtw_vm(t(as.matrix(wine.test.clean[2,])),t(as.matrix(wine.train.clean)),w=2,windowType="sakoe echiba")

classify_dtwvm_w(wine.test.clean, wine.train.clean, wine.train, windowType = "sakoe echiba", w=2)





class(dtw_vm(as.matrix(t(wine.test.clean[1,])),as.matrix(t(as.matrix(wine.train.clean)))))




## Difference using dtw-vm



percent_match(classify_dtwvm(wine.test.clean,wine.train.clean,wine.train), wine.test)


percent_match(classify_general(wine.test.clean,wine.train.clean,wine.train), wine.test)





## Get NN classification

generalNN <- function(distances, train.raw, n){
  ind <- order(distances)[1:n]
  
  classify <- c(train.raw[ind,ncol(train.raw)])
  
  generalNN <- Mode(classify)
  
  return(generalNN)
}


generalNN(classify_dtwvm(wine.test.clean,wine.train.clean,wine.train),
          wine.train, 5)







classify_generalNN_dtwvm <- function(test,train, train.raw, n){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    dist <- dtw_vm(t(as.matrix(test[i,])),t(as.matrix(train)))
    
    type <- generalNN(dist, train.raw, n)
    
    class <- c(class,type)
    
  }
  
  class
}


percent_match(classify_generalNN_dtwvm(wine.test.clean,wine.train.clean, wine.train, 1),wine.test)




percent_match(classify_generalNN_dtwvm(olive.test.clean,olive.train.clean, olive.train, 15),olive.test)

percent_match(classify_generalNN_dtwvm(wine.test.clean,wine.train.clean, wine.train, 1),wine.test)

## numbers for k=1,3,5,10,15 nearest neighbours

percents <- function(test,train,train.raw,test.raw){
  per <- c()
  
  for (i in c(1,3,5,10,15)){
    
    calc <-percent_match(classify_generalNN_dtwvm(test,train, train.raw, i),test.raw)
    per <- c(per,calc)
  }
  
  per
}






wine_percent <- percents(wine.test.clean,wine.train.clean,wine.train,wine.test)


percent_match(classify_generalNN_dtwvm(wine.test.clean,wine.train.clean, wine.train, 10), wine.test)



coffee_percent <- percents(coffee.test.clean,coffee.train.clean,coffee.train,coffee.test)


olive_percent <- percents(olive.test.clean,olive.train.clean,olive.train,olive.test)


sweed_leaf_percent <- percents(sweed.leaf.test.clean[1:70,],sweed.leaf.train.clean[1:70,],sweed.leaf.train,sweed.leaf.test[1:70,])



## Performance of k-NN with our data sets, NO warping constraints

plot(x=c(1,3,5,10,15),y=wine_percent, pch=19,xlab="K", ylab="Percent Correct", main="Wine")
lines(x=c(1,3,5,10,15), y=wine_percent,col="Red")




plot(x=c(1,3,5,10,15),y=coffee_percent, pch=19,xlab="K", ylab="Percent Correct",main="Coffee")
lines(x=c(1,3,5,10,15), y=coffee_percent, col="Brown")

plot(x=c(1,3,5,10,15),y=olive_percent, pch=19, xlab="K", ylab="Percent Correct",,main="Olive Oil")
lines(x=c(1,3,5,10,15), y=olive_percent,col="green")



## Not using entire dataset of sweed leafs (samples = 70)

plot(x=c(1,3,5,10,15),y=beetle_percents, pch=19,xlab="K", ylab="Percent Correct", main="Beetle Fly")
lines(x=c(1,3,5,10,15), y=beetle_percents, col="Steel blue")





dtw_vm(as.matrix(t(wine.test.clean[1,])),as.matrix(t(as.matrix(wine.train.clean))),windowType = "sakoe echiba",)







## Testing with the Sakoe Chiba Band



## test wapring windows w=1,..


distances.sak(wine.test.clean[1,], wine.train.clean, 3)




classify.sak(wine.test.clean, wine.train.clean,wine.train,1)


wine.test[,ncol(wine.test)]


## Check classes with different w's (4), gives us a matrix of classes in each ROW

best_w <- function(test, train, train.raw, test.raw){
  
  class <- c()
  
  match <- c()
  
  for (i in c(1,2,3,5)){
    type <- classify.sak(wine.test.clean, wine.train.clean,wine.train,1)
    
    class <- c(class, type)
  }
  
  class <- matrix(class, byrow=T, nrow=4)
  
  for (j in 1:4){
    row_match <- match.count(class[j,], test.raw)
    
    match <- c(match,row_match)
    
  }
  
  ind <- which.max(match)
  w <- c(1,2,3,5)[ind]
  
  w
}



gg <- best_w(wine.test.clean,wine.train.clean, wine.train, wine.test)


gg <- matrix(gg, byrow=T, nrow=6)


match.count(gg[6,], wine.test[,ncol(wine.test)])




## Testing using the parameters open.end and open.start



best_start <- function(test,train,train.raw,test.raw){
  
  
  
  
  ## Open.end and open.start (FF, FT, TF, TT)
  
  for (i in 1:4){}
  
  for (j in c(1,2,3,5,10))
    
}




options(digits=5)


## Percentage of correct classification based on test data

percent_match <- function(class,test.raw){
  percent_match <- match.count(class,test.raw[,ncol(test.raw)])/nrow(test.raw)
  
  percent_match
}

percent_match(classify_general(wine.test.clean,wine.train.clean,wine.train),wine.test)

percent_match(classify_general(coffee.test.clean,coffee.train.clean,coffee.train),coffee.test)

percent_match(classify_general(sweed.leaf.test.clean,sweed.leaf.train.clean,sweed.leaf.train),sweed.leaf.test)

percent_match(classify_general(olive.test.clean,olive.train.clean,olive.train),olive.test)




dtw_vm(t(as.matrix(wine.test), t(as.matrix(wine.train))))




dtw_vm

distances.sak <- function(vector,data,w){
  distances <-c()
  
  for (i in 1:nrow(data)){
    
    dtw <- calcDistSakoe(dtw_vv(t(as.matrix(vector)),t(as.matrix(data[i,])))$costMatrix, w=w, open.end=FALSE,open.start = FALSE)
    
    distances<- c(distances,dtw)
    
  }
  
  distances
}


## Classify using the Sakoe Chiba band instead

classify.sak <- function(test,train,train.raw,w){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(distances.sak(test[i,],train,w=w))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
} 

dtw_vm(t(as.matrix(wine.test.clean)),t(as.matrix(wine.train.clean)))  





distances.row <- function(vector,data){
  distances <-c()
  
  for (i in 1:nrow(data)){
    
    dtw <- dtw_vv(as.matrix(vector),as.matrix(data[i,]))$distance
    distances<- c(distances,dtw)
    
  }
  
  distances
}


dtw_vm(t(as.matrix(c(1,2,3))),t(matrix(c(1,2,3,4,5,6)),nrow=2,ncol=3,byrow=T)) <- gives matrix of dist


classify.sak <- function(test,train,train.raw,w){
  
  class <- c()
  
  for (i in 1:nrow(test)){
    
    #return index with lowest distance
    min <-which.min(distances.sak(test[i,],train,w=w))
    
    ## Return class of sample closest to it
    
    type <- train.raw[min,ncol(train.raw)]
    
    class <- c(class,type)
    
    
    
  }
  
}  
  
  
  match.count(classify_general(olive.test.clean,olive.train.clean,olive.train),olive.test[,572])
  
  
  ## Different way to calculate the distance
  
  calcDistSakoe(dtw_vv(as.matrix(c(11,12,14,11,23)),as.matrix(c(13,15,13,20,21)))$costMatrix, w=3,
                open.start = TRUE, open.end = TRUE)
  
  
  ## Example of where using Sakoe Chiba band gives us a better minimum distance
  
  
  dtw_vv(as.matrix(c(11,1,14,11,23)),t(as.matrix(c(15,15,15,20,21))))$costMatrix
  dtw_vv(t(as.matrix(c(11,1,14,11,23))),t(as.matrix(c(15,15,13,20,21))))$distance
  
  calcDistSakoe(dtw_vv(as.matrix(c(11,1,14,11,23)),as.matrix(c(15,15,13,20,21)))$costMatrix, w=1,open.start = FALSE, open.end=FALSE)
  
  
  
  
  calcDistSakoe(dtw_vv(t(as.matrix(olive.test.clean[2,])),t(as.matrix(olive.train.clean[4,])))$costMatrix, w=5, open.start=F, open.end=FALSE)
  dtw_vv(t(as.matrix(olive.test.clean[2,])),t(as.matrix(olive.train.clean[4,])))$distance
  dtw_vv(t(as.matrix(olive.test.clean[2,])),t(as.matrix(olive.train.clean[4,])))$costMatrix
  
  
  dtw_vv(as.matrix(olive.test.clean[2,]),as.matrix(olive.train.clean[4,]))$distance
  
  
  
  
  
  
  distances.sak <- function(vector,data){
    distances <-c()
    
    for (i in 1:nrow(data)){
      
      dtw <- calcDistSakoe(dtw_vv(as.matrix(vector),as.matrix(data[i,]))$costMatrix, w=w, open.start=FALSE,open.end=FALSE)
      distances<- c(distances,dtw)
      
    }
    
    distances
  }
  
  
  
  
  
  classify.sak <- function(test,train,train.raw){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest to it
      
      type <- train.raw[min,ncol(train.raw)]
      
      class <- c(class,type)
      
      
      
    }
    
    class <- match.count(class, train.raw[,ncol(train.raw)])
    class
  }
  
  
  
  
  classify.sak(olive.test.clean,olive.train.clean,olive.train)
  
  
  
  
  
  
  
  best_sakoe <- function(test,train,test.raw,train.raw){
    
  }
  
  
  
  distances.row_t <- function(vector,data){
    distances <-c()
    
    for (i in 1:nrow(data)){
      
      dtw <- dtw_vv(as.matrix(vector),as.matrix(data[i,]))$distance
      distances<- c(distances,dtw)
      
    }
    
    distances
  }
  
  
  
  classify_general_t <- function(test,train,train.raw){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest to it
      
      type <- train.raw[min,ncol(train.raw)]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Load Coffee data
  
  coffee.train <- read.csv("coffee_train.csv",header=T)
  
  coffee.test <- read.csv("coffee_test.csv",header=T)
  
  
  ## Remove ID and target variables, only matrix of observations
  
  coffee.test.clean <- coffee.test[,c(-1,-288)]
  
  coffee.train.clean <- coffee.train[,c(-1,-288)]
  
  
  ## Select vector from test data and use calculate distance using DTW on training set
  
  ## ex:
  
  dtw_vv(as.matrix(coffee.test.clean[1,]),as.matrix(coffee.train.clean[1,]))
  
  
  ## Easier function to gather distances from row vector to every row in a Matrix of equal length
  
  distances.row <- function(vector,data){
    distances <-c()
    
    for (i in 1:nrow(data)){
      
      dtw <- dtw_vv(as.matrix(vector),as.matrix(data[i,]))$distance
      distances<- c(distances,dtw)
      
    }
    
    distances
  }
  
  
  distances.row(c(22.3,24,23), matrix(c(3,5,6,20,23,25), byrow=TRUE, nrow=2, ncol=3))
  
  ## For coffee sample 1 compared to our training set we ge
  
  distances.row(coffee.test.clean[1,],coffee.train.clean)
  
  
  
  ## Find the minimum and its index
  
  which.min(distances.row(coffee.test.clean[1,],coffee.train.clean))
  
  ## Check the target/class the closest sample is (training set)
  
  coffee.train[6,288]
  
  
  ## Check what our actual class was
  
  coffee.test[1,288]
  
  
  
  ## Lets create a function to do this for the entire test data and create a vector of comparisons
  
  
  classify_coffee <- function(test,train){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest
      
      type <- coffee.train[min,288]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  
  ## DTW gets it right 27/28 times
  
  classify_coffee(coffee.test.clean,coffee.train.clean)
  
  ## Actual Classes
  
  coffee.test[,288]
  
  
  
  
  
  match.count(classify_coffee(coffee.test.clean,coffee.train.clean),coffee.test[,288])
  
  
  ## Performed very well, matching 27/28 correctly
  
  
  
  
  
  
  
  
  
  
  
  
  ## Load Wine data sets (2 classes)/ Unequal lengths
  
  
  wine.test <- read.csv("wine_test.csv",header=T)
  
  wine.train <- read.csv("wine_train.csv",header=T)
  
  
  
  
  ## Wine data with only observations
  
  wine.test.clean <- wine.test[,c(-1,-236)]
  
  wine.train.clean <- wine.train[,c(-1,-236)]
  
  
  
  ## Lets try an example again
  
  wine.test.clean[1,]
  
  
  nrow(wine.test.clean)
  
  
  
  
  
  
  
  
  
  ## Classify wines based on DTW distance to training set
  
  
  classify_wine <- function(test,train){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest
      
      type <- wine.train[min,236]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  
  which.min(distances.row(wine.test.clean[1,],wine.train.clean))
  
  # Check classes
  
  classify_wine(wine.test.clean,wine.train.clean)
  
  ## Compare to real
  
  wine.test[,236]
  
  
  match.count(classify_wine(wine.test.clean,wine.train.clean),wine.test[,236])
  
  ## Performed somewhat poor, matching 35/54
  
  
  
  
  ## Dataset with multiple classes, olive data
  
  olive.test <- read.csv("olive_test.csv",header=T)
  
  olive.train <- read.csv("olive_train.csv",header=T)
  
  
  ## Clean up the data
  
  olive.test.clean <- olive.test[,c(-1,-572)]
  
  olive.train.clean <- olive.train[,c(-1,-572)]
  
  
  classify_olive <- function(test,train){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest
      
      type <- olive.train[min,572]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  
  
  classify_olive(olive.test.clean,olive.train.clean)
  
  
  olive.test[,572]
  
  
  
  
  match.count(classify_olive(olive.test.clean,olive.train.clean),olive.test[,572])/30
  
  
  ## For 30 observations, we matched 25 correctly
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Lets try with many classes >10
  
  sweed.leaf.test <- read.csv("sweedish_leaf_test.csv",header=T)
  sweed.leaf.train <- read.csv("sweedish_leaf_train.csv",header=T)
  
  
  
  
  
  ## Clean data
  
  sweed.leaf.test.clean <- sweed.leaf.test[,c(-1,-130)]
  sweed.leaf.train.clean <- sweed.leaf.train[,c(-1,-130)]
  
  
  classify_leaf <- function(test,train){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest
      
      type <- sweed.leaf.train[min,130]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  dtw_vv(as.matrix(sweed.leaf.test.clean[1,]),as.matrix(sweed.leaf.train.clean[2,]))
  
  classify_leaf(sweed.leaf.test.clean[1:10,],sweed.leaf.train.clean)
  
  
  
  sweed.leaf.test[1:30,130]
  
  ## Check how many matches (correct classification) we have
  
  match.count <- function (x,y){
    count <- 0
    for (i in 1:length(x)){
      if (x[i] == y[i]){
        count <- count + 1
      }
    }
    count
  }
  
  
  classify_leaf(sweed.leaf.test.clean[1:100,],sweed.leaf.train.clean)
  
  ## Performs very well on the first 10, matching 9/10, doing all 625 takes much too long.
  
  
  #' @param test: The data we want to classify cleaned with only observations
  #' @param train: The training data set with known classes and cleaned to only observations
  #' @param train.raw: The rawdata with id and class
  
  
  classify_general <- function(test,train,train.raw){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      #return index with lowest distance
      min <-which.min(distances.row(test[i,],train))
      
      ## Return class of sample closest to it
      
      type <- train.raw[min,ncol(train.raw)]
      
      class <- c(class,type)
      
      
      
    }
    
    class
  }
  
  
  
  classify_leaf(sweed.leaf.test.clean[1:10,],sweed.leaf.train.clean)
  
  classify_leaf(sweed.leaf.test.clean[1:10,],sweed.leaf.train.clean)
  classify_general(sweed.leaf.test.clean[1:10,],sweed.leaf.train.clean, sweed.leaf.train)
  
  
  dtw_vv(as.matrix(sweed.leaf.test.clean[1:100,]),as.matrix(sweed.leaf.train.clean[1:100,]))$costMatrix
  
  
  calcDist(dtw_vv(as.matrix(sweed.leaf.test.clean[1:5,]),as.matrix(sweed.leaf.train.clean[1:5,]))$costMatrix,
           open.start = FALSE, open.end = FALSE)
  
  calcDistSakoe(dtw_vv(as.matrix(sweed.leaf.test.clean[1:5,]),as.matrix(sweed.leaf.train.clean[1:5,]))$costMatrix,
                w=3,open.start = FALSE, open.end=FALSE)
  
  
  dtw_vv(as.matrix(sweed.leaf.test.clean[1:5,]),as.matrix(sweed.leaf.train.clean[1:5,]))$costMatrix
  
  
  
  calcDistSakoe(dtw_vv(as.matrix(c(1,2,3,4,5)),as.matrix(c(5,2,8,10,6)))$costMatrix,
                w=2,open.start = FALSE, open.end=FALSE)
  
  dtw_vv(as.matrix(c(1,2,3,4,5)),as.matrix(c(5,2,8,10,6)))$costMatrix
  
  
  
  
  
  
  ## Find optimal distance using Sakoe
  

  
  
  
  
  
  dtw_vv(as.matrix(sweed.leaf.test.clean[1,]),as.matrix(sweed.leaf.train.clean[1:10,]))$costMatrix
  
  
  
  
  dtw_vv(as.matrix(sweed.leaf.test.clean[1,]),as.matrix(sweed.leaf.test.clean[1,]))
  
  dtw_vm(as.matrix(sweed.leaf.test.clean[1,]),as.matrix(sweed.leaf.train.clean))
  
  
  
  
  
  
  
  
  
  
  match.count(classify_general(olive.test.clean,olive.train.clean,olive.train), olive.test[,ncol(olive.test)])
  
  
  
  
  
  
  ## Lets build a 5NN using modes and test it on the wine data
  
  ## Need dataset of length greater than 10 preferabbly
  
  
  order(classify_wine(wine.test.clean,wine.train.clean))
  
  
  
  distances.row(wine.test.clean[1,],wine.train.clean)
  
  ind <- c(order(distances.row(wine.test.clean[1,],wine.train.clean),decreasing = TRUE)[1:5])
  
  ## Assign the class based on trainig set
  
  kek <- c(wine.train[ind,236])
  
  
  Mode(kek)
  
  wine.train[50,236]
  
  ncol(wine.train)
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  ## give vector of distances
  
  fiveNN<- function(distances, train.raw){
    ind <- order(distances)[1:5]
    
    classify <- c(train.raw[ind,ncol(train.raw)])
    
    fiveNN <- Mode(classify)
    
    return(fiveNN)
  }
  
  order(distances.row(wine.test.clean[1,],wine.train.clean))[1:5]
  
  c(wine.train[order(distances.row(wine.test.clean[1,],wine.train.clean))[1:5], ncol(wine.train)])
  
  Mode(c(wine.train[order(distances.row(wine.test.clean[1,],wine.train.clean))[1:5], ncol(wine.train)]))
  
  distances.row(wine.test.clean[1,],wine.train.clean)
  
  
  fiveNN(distances.row(wine.test.clean[30,], wine.train.clean),wine.train)
  
  
  
  dtw_vv(as.matrix(wine.test.clean[1,]),as.matrix(wine.test.clean[2,]),windowType = "sakoe echiba")
  
  
  
  
  
  ## Lets try classifying all data using the 5NN approach and compare to 1NN
  
  
  classify_fiveNN <- function(test,train, train.raw){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      dist <- distances.row(test[i,],train)
      
      type <- fiveNN(dist, train.raw)
      
      class <- c(class,type)
      
    }
    
    class
  }
  
  
  
  classify_fiveNN(wine.test.clean, wine.train.clean, wine.train)
  classify_wine(wine.test.clean,wine.train.clean)
  
  
  
  
  
  ## Seems to actually perform much worse
  
  
  
  
  
  
  
  
  generalNN <- function(distances, train.raw, n){
    ind <- order(distances)[1:n]
    
    classify <- c(train.raw[ind,ncol(train.raw)])
    
    generalNN <- Mode(classify)
    
    return(generalNN)
  }
  
  
  
  classify_generalNN <- function(test,train, train.raw, n){
    
    class <- c()
    
    for (i in 1:nrow(test)){
      
      dist <- distances.row(test[i,],train)
      
      type <- generalNN(dist, train.raw, n)
      
      class <- c(class,type)
      
    }
    
    class
  }
  
  
  
  
  classify_fiveNN(wine.test.clean,wine.train.clean, wine.train)
  
  classify_generalNN(wine.test.clean, wine.train.clean, wine.train, 3)
  
  wine.test[,ncol(wine.test)]
  
  
  match.count(classify_generalNN(wine.test.clean, wine.train.clean, wine.train, 35),wine.test[,ncol(wine.test)])
  
  
  match.count(classify_generalNN(olive.test.clean,olive.train.clean,olive.train,20), olive.test[,ncol(olive.test)])
  
  
  match.count(classify_generalNN(coffee.test.clean, coffee.train.clean, coffee.train, 1), coffee.test[,ncol(coffee.test)])
  
  
  
  match.count(classify_generalNN(wine.test.clean, wine.train.clean, wine.train, 1),
              wine.test[,ncol(wine.test)])
  
  
  
  
  
  ## Normalize each sequence
  
  normalize <- function(x){
    
  }
  
  
  
  
  
  
  
  dtw_vv(as.matrix(c(1,2)),as.matrix(c(1,2)), )
  
  
  
  
  
  
  calcDistSakoe(dtw_vv(as.matrix(c(11,12,14,11,23)),as.matrix(c(13,15,13,20,21)))$costMatrix, w=3,
                open.start = TRUE, open.end = TRUE)
  
  
  
  
  
  
  ## calculate distances matrix using Sakoe Chiba with constraint
  
  distances.sak <- function(vector,data,w, open.start, open.end){
    distances <-c()
    
    for (i in 1:nrow(data)){
      
      dtw <- calcDistSakoe(dtw_vv(as.matrix(vector),as.matrix(data[i,]))$costMatrix, w=w,
                           open.start = open.start, open.end = open.end)
      distances<- c(distances,dtw)
      
    }
    
    distances
  }
  
  
  
  
  distances.row(wine.test.clean[3,], wine.test.clean)
  
  distances.sak(wine.test.clean[3,], wine.test.clean, 1, open.start = F, open.end = F)
  
  
  
  dtw_vm(as.matrix(c(1,2,3)),matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3,byrow=T))
  
  
  blue <- dtw_vv(as.matrix(c(1,1,1,1,1)),as.matrix(c()))$costMatrix
  
  
  
  
  
  
  classify_sakoe <- function(){
    
  }
  
  
  
  
  
  q <- as.matrix(c(1,2,3,4,5))
  mat <- matrix(c(2,4,3,1,5,7,6,3,5,6,1,2,3,4,5,6,4,5,6,7,1,2,3,4,5), nrow=5,ncol=5)
  
  dist <- dtw_vm(q,mat)
  
  dist
  
  
  
  dtw_vm(as.matrix(sweed.leaf.test.clean[1,]),matrix(sweed.leaf.train.clean[1:10,],
                                                     nrow = 10, ncol= 128))
  
  
  length(as.matrix(sweed.leaf.test.clean[1,]))
  length(matrix(sweed.leaf.train.clean[1:10,],
                nrow = 10, ncol= 128))
  
  nrow(matrix(sweed.leaf.train.clean[1:10,],
              nrow = 10, ncol= 128))
  
  
  
  
  ## Create the NA MATRIx
  
  getNA <- function(costMat, w){
    len <- length(costMat[1,])
    curr <- w + 1
    i <- 1
    while (i <= len-w){
      costMat[curr:len,i] <- NA
      costMat[i, curr:len] <- NA
      curr <- curr + 1
      i <- i + 1
    }
    
    costMat
    
  }
  
  as.matrix((getNA(dtw_vv(as.matrix(c(1,2,3,4,5)),as.matrix(c(5,2,8,10,6)))$costMatrix, 2)))
  
  class(getNA(dtw_vv(as.matrix(c(1,2,3,4,5,1,2,3,4)),as.matrix(c(5,2,8,10,6,5,4,3,2)))$costMatrix,4))
  
  getNA(dtw_vv(as.matrix(c(1,2,3,4,5,1,2,3,4)),as.matrix(c(5,2,8,10,6,5,4,3,2)))$costMatrix,9)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  









