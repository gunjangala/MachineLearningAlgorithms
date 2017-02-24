# Rscript ggg256_A4_part2.R input_forPCA.txt random_vector.txt output_A4_part2.txt
# The outputs will also be the same as in part 1, 
# i.e. a single number containing the variance of the dataset after it has been 
# projected onto the first PC. This is the total variance explained by the first principal component.
# setwd("~/Google Drive/Machine Learning/Assignments/A4")
# input <- read.table("~/Google Drive/Machine Learning/Assignments/A4/input_forPCA.txt")
# vector <- read.table("~/Google Drive/Machine Learning/Assignments/A4/random_vector.txt")
# ________________________________________________

# pass arguments from the command line 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>1) {
  input.file=args[1] 
  input.vector=args[2]
  output.file=args[3]
}

input <- read.table(input.file)
input <- as.data.frame(input)
vector <- read.table(input.vector)

# Function to calculate orthogonal projection
orth_proj <- function (data.point, trial.vector){
  d <- (as.vector(t(data.point)))
  v <-  (as.vector(t(trial.vector)))
  orth.proj <- ((d %*% v)/(v %*% v))*(v)
  return (orth.proj)
}

# function to calculate variance explained by a vector
objective_function <- function (par,data){
  some.vector <- par
  test <- data.frame()
  for (i in 1:nrow(data)){
    proj.vector <- orth_proj(data[i,],some.vector)  
    test <- as.data.frame(rbind(test,proj.vector))
  }
  total.variance <- sum(apply(test,2,var))
  return (-total.variance)
}

# function to obtain new projection from the best vector as input
new.projection <- function (par,data){
  some.vector <- par
  test <- data.frame()
  for (i in 1:nrow(data)){
    proj.vector <- orth_proj(data[i,],some.vector)  
    test <- as.data.frame(rbind(test,proj.vector))
  }
  return (test)
}

# Run the following to get variance for PC1,PC2,PC3
PCparameters <- list()
data4PCA <- data.frame()
PCvar <- list()
previous.input <- input
for (i in 1:3){
  # using optim to obtain the best vector explaining maximum variance
  PC <- optim(par=c(vector$V1),fn=objective_function,data=previous.input)
  # get new projection for the best vector from optim()
  new.proj <- new.projection(PC$par,input)
  # new.input for next Principal component
  new.input <- previous.input-new.proj
  previous.input <- new.input
  # in case you want to look at parameters of Principal components
  #PCparameters[[i]] <- PC$par
  PCvar[[i]] <- abs(PC$value)
}

final <- (unlist(PCvar))

write.table(final,file=output.file,row.names = FALSE,col.names = FALSE)



