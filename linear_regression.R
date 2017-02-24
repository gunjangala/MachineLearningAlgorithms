# Machine learning Assignment1 

# pass arguments from the command line 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>1) {
  # default output file
  yfile=args[1] 
  xfile=args[2]
  alpha=args[3]  
  epsilon=args[4] 
  outfile=args[5] 
}

# reading in the files
x<-read.table(xfile,header=TRUE)
y<-read.table(yfile,header=FALSE)
alpha <- as.numeric(alpha)
epsilon <- as.numeric(epsilon)

# length of input values
m <- dim(y)[1]
# add a column of 1's for the intercept coefficient
X <- cbind(1, as.matrix(x))
# initialize coefficients
theta <- as.matrix(rep(0,(1+dim(x)[2])))

y<-as.matrix(y)

# x is an mxn matrix containing one row for each training record and one column for each feature
# theta is a 1xn matrix containing the weights for the hypothesis
# y is an mx1 matrix containing the response values

# define cost function
cost_function <- function(X,y,theta,m) {
  sum( (X %*% theta - y)^2 ) / (2*m)
}

# to get started with iteration in while loop,
# set two starting values whose difference is greater than epsilon
hold_cost = 0
cost = 1

# while loop to run till RSS converges 
# ie difference between two consecutive cost functions is less than epsilon
while (abs(cost - hold_cost) > epsilon){
  # while itering we assign previous cost value to "hold_cost"
  hold_cost <- cost
  # gradient descent in next three steps
  error <- (X %*% theta-y)
  pd <- t(X) %*% error / m  # partial derivative
  theta <- theta - (alpha * pd) # update theta
  # new cost value calculated with updated theta
  cost <- cost_function(X, y, theta,m)
}
  
# Calculate fitted y values from last updated theta(s)
fitted_values <- function(x,theta){
  df <- as.data.frame(x)
  for (i in 1:ncol(x)){
    # each X column in df is multiplied with corresponding coefficient & replaced in the same df
    df[,i] <- df[,i]*theta[(i+1)]
    # adding column of thetaZERO(intercept) to df
    new.df <- as.data.frame(cbind(theta[1],df))
  }
  # Adding values 
  fitted_y<-as.data.frame(rowSums(new.df))
  return (fitted_y)
}
 
# calling function 
fitted.y <- fitted_values(x,theta)

# writing to file
write.table(fitted.y,outfile,row.names = FALSE,col.names = FALSE)

### END


