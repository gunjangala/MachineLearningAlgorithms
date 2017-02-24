# Machine learning Assignment3 PART 1

# pass arguments from the command line 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>1) {
  input.file=args[1] 
  seed.file=args[2]
  output.file=args[3]
}
##################################################
#packages
require(dplyr)
##################################################
#setwd("~/Google Drive/Machine Learning/Assignments/A3")
#kmeans <- t(read.table("~/course_docs/Assignments/A3/input_kmeans.txt"))
#seed <- read.table("~/course_docs/Assignments/A3/initial_seeding_kmeans.txt")

kmeans <- t(read.table(input.file))
seed <- read.table(seed.file)

##################################################

# manipulating the data frame structure for further use
tmp.df <- cbind(as.data.frame(kmeans),as.data.frame(seed))
colnames(tmp.df)[dim(tmp.df)[2]] <- c("c.id")

##################################################

## FUNCTION : ESTIMATING CENTROIDS
# This function will return maximum seven centroids when input is data with 
# seed info as last column

seven.centroids <- function(expData){
  centroid <- data.frame()
  for (i in 1:7){
    x <- as.data.frame(expData)
    # selecting a cluster
    cc <- subset(x,c.id==i)
    # dropping seed column for estimating centroids
    cc.dis <- subset(cc,select=-c.id)
    # adding centroid one by one to data frame
    centroid <- as.data.frame(rbind(centroid,colMeans(cc.dis)))
    colnames(centroid) <- colnames(cc.dis)
  }
  return (centroid)  
}

#--------------------

## FUNCTION : ESTIMATING THE NEAREST CLUSTER
# This function will estimate the nearest of all 7 centroids to a 
# datapoint and will return the index of that centroid for the given data point

best.centroid <- function(a.point,all.centroids){
  tmp <- list()
  for (i in 1:dim(all.centroids)[1]){
    # estimating distance
    tmp[[i]] <- dist(rbind(a.point,all.centroids[i,]),method="euclidean")
  }
  # selecting the nearest centroid
  new <- (which.min(unlist(tmp)))
  return (new)
}

#--------------------

## FUNCTION : CALCULATE OBJECTIVE FUNCTION
# For a given data with best cluster assignments
# and final chosen centroids, this function calculate and returns the 
# objective function 

objective_function <- function(new.data,centroid){
  all.cluster.dist <- list()
  # remove rows ie centroids that phase-out
  centroid <- centroid[complete.cases(centroid),]
  
  # calculating the objective function by definition
  for (i in 1:nrow(centroid)){
    cluster <- subset(new.data,c.id==i)
    cc <- centroid[i,]
    all.cluster.dist[[i]] <- sum(apply(cluster,1,function(x){dist(rbind(x,centroid[i,]),method="euclidean")}))
  }
  obj.func <- (sum(unlist(all.cluster.dist)))/dim(new.data)[1]
  return (obj.func)
}

######################### MAIN ############################


old.cluster.id <- tmp.df$c.id
new.cluster.id <- data.frame()

# loop to run till cluster assignments do not change
while (identical(new.cluster.id,old.cluster.id)==FALSE){
  
  # change new to old and estimate new
  old.cluster.id <- new.cluster.id
  new.cluster.id <- data.frame()
  
  # calculate centroids with cluster assignments from previous loop
  centroid <- seven.centroids(tmp.df)
  
  # estimate nearest centroid for each datapoint in the data
  for (i in 1:nrow(tmp.df)){
    all.points <- subset(tmp.df,select=-c.id)
    tmp.cluster.id <- NULL
    tmp.cluster.id  <- best.centroid(all.points[i,],centroid)
    new.cluster.id <- rbind(new.cluster.id,tmp.cluster.id)
  }  
  
  # assign new cluster id to all data points & change the column name for consistency 
  tmp.df <- as.data.frame(cbind(all.points,as.data.frame(new.cluster.id)))
  colnames(tmp.df)[ncol(tmp.df)] <- "c.id"
}

# calculate objective function for the new cluster assignments
cluster <- objective_function(tmp.df,centroid)

## CONSOLIDATING EVERYTHING 
final <- rbind(new.cluster.id,as.character(cluster))
final <- as.numeric(final[,1])

# write to output file
write.table(final,file=output.file,row.names = FALSE,col.names = FALSE)


######################  END ######################


