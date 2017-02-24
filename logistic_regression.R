
# Machine learning Assignment2

# pass arguments from the command line 
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)>1) {
#  # default output file
  input.file=args[1] 
  output.file=args[2]
}
#########################################################################################

#setwd("~/Google Drive/Machine Learning/Assignments/A2")

# reading in the files
exp <-read.delim(input.file,header=TRUE)
exp <- exp[complete.cases(exp),]
#exp <- read.delim("~/Google Drive/Machine Learning/Assignments/A2/input_A2_revised.txt",header = TRUE,row.names = 1)
#exp <- exp[1:100,1:69] #testing with different number of columns and rows
exp <- as.data.frame(t(as.matrix(exp)))

# change gene names to x1, x2, etc and store for reference in genes.df
genes.df <- as.data.frame(colnames(exp))
for (i in 1:(length(colnames(exp)))){
  colnames(exp)[i]<-paste0("x",i)
}

# stores gene names for corresponding array of x say x1 x2 x3 etc
genes.df <- as.data.frame(cbind(genes.df,colnames(exp)))
colnames(genes.df)<-c("genes","x")

# adding categorical response column to the data
exp$response <- row.names(exp)
SPLIT=strsplit(exp$response,"_")
exp$response <- sapply(SPLIT, "[", 1)
exp$response <- sapply(exp$response, function(x) {if (x[1]=="Memory"){return (1)} else {return (0)}})
exp$response <- as.factor(exp$response)

# just saving this original data to a variable
# before starting to manipulate "exp"
exp.data <- exp

# refresh :
exp<-exp.data

# dataframe with just one response variable 
test.df<-as.data.frame(exp.data$response)
colnames(test.df) <- c("response")

# no. of total iterations
for (j in 1:50){
  #create empty lists
  models <- list()
  RSS <- list()
  q <- list()
  
  # an inner loop to iter over all predictors in data
  # length minus one to adjust for presence of "response"
  for (i in 1:((length(colnames(exp)))-1)){
    
    # iterating over different predictors
    # different combination of predictor variables in dataframe to fit the models on
    tmp.df <- as.data.frame(cbind(test.df,as.data.frame(exp[,(i)])))
    
    # assigning correct column names
    colnames(tmp.df)[ncol(tmp.df)] <- (colnames(exp[i]))
    
    #variabe for formula
    form <- (formula(paste("response", "~", ".")))
    
    # storing all models in the list of lists
    models[[i]]<- glm(form,family=binomial, data= tmp.df)
    
    # storing RSS values for all models in list of lists
    RSS[[i]] <-  sum((as.numeric(models[[i]]$y)-models[[i]]$fitted.values)^2)
    tmp.df <- test.df
  }
  
  # finding out the predictor with least RSS in the above models
  q[[j]]<-((colnames(exp)[which.min(unlist(RSS))]))
  
  # adding that predictor to dataframe used in last iteration
  test.df <- cbind(test.df,as.data.frame(exp[colnames(exp)==q[[j]]]))
  
  # dropping the selected predictor variable from the "exp" data frame to be used in our next iteration 
  pred <- q[[j]]
  exp<-exp[colnames(exp)!=pred]
  #print (colnames(test.df))
}

##########################################################################################

cross.validation <- function (dd,k) {
  result <- data.frame()
  pred.tmp <- data.frame()
  test.tmp <- data.frame()
  
  # number of observations
  n <- dim(dd)[1]
  
  # splitting the data for cross validation in k parts
  folds <- split(sample(seq(n)), rep(1:k, length = n))
  
  for (i in 1:k) {
    #print(paste("iteration: ", i))  
    # assigning a part of the splitted data frame to be omitted from training data set  while performing CV
    omit <- folds[[i]]
    
    # omitting the test set
    data_train <- dd[-omit,]	
    
    #fitting model
    fit <- glm(response~.,family=binomial,data=data_train)	
    
    # omitted observations are our test data
    data_test = dd[omit,]		
    
    # predict probabilities for test data set by re-fitting the glm model
    pred.prob <- predict(fit, subset(data_test,select=-response),type="response")
    
    # storing probabilities and observed observations of test dataset
    pred.tmp <- as.data.frame(rbind(pred.tmp, as.data.frame(pred.prob)))
    test.tmp <- as.data.frame(rbind(test.tmp,as.data.frame(data_test$response)))
  }	
  
  # finding CV error
  result <- as.data.frame(cbind(pred.tmp,test.tmp[, 1]))
  names(result) <- c("Expected", "Observed")
  result$Observed <- ifelse(as.numeric(result$Observed)==1,0,1) 
  #result$Observed <- as.numeric(result$Observed)
  result$Difference <- (result$Expected - as.numeric(result$Observed))
  #print (str(result))
  return ((mean(result$Difference^2)))
}

##########################################################################################

df <- NULL
fs.df <- as.data.frame(test.df[,2:51])
df <- as.data.frame(test.df$response)
colnames(df) <- c("response")
glm <- list()
logLik <- list()
cv.error <- list()

# Calculating CV error and log likelihhod of different models selected through forward selection
for (i in 1:50){
  
  # updating data frame for one new variable each time through the iteration
  update.df <- (as.data.frame(fs.df[,i]))
  colnames(update.df) <- (colnames(fs.df)[i])
  df <- as.data.frame(cbind(as.data.frame(df),as.data.frame(update.df)))
  
  # fitting model
  glm[[i]] <- glm (response~. , family=binomial, data= df) 
  
  # cv.error
  cv.error[[i]] <- cross.validation(df,5)
  
  # Calculating Loglikelihood
  data.LL <- as.data.frame(cbind(as.data.frame(glm[[i]]$y),as.data.frame(glm[[i]]$fitted.values)))
  data.LL.0 <- data.LL[which(data.LL$`glm[[i]]$y` == 0),]
  data.LL.1 <- data.LL[which(data.LL$`glm[[i]]$y` == 1),]
  prod.0 <- prod(1-data.LL.0$`glm[[i]]$fitted.values`)
  prod.1 <- prod(data.LL.1$`glm[[i]]$fitted.values`)
  logLik[[i]] <- log(prod.0*prod.1)
}


################### CONSOLIDATING EVERYTHING #####################################
tmp.gene <- list()
predictor <- colnames(subset(test.df,select=-c(response)))

# converting X1,2,3,4 etc to gene names
for (i in 1:length(predictor)) {tmp.gene[[i]] <- genes.df$genes[genes.df$x==predictor[i]]}

predictor <- as.vector(unlist(tmp.gene))
logLik.list <- unlist(logLik)
CVerror <- unlist(cv.error)
n <- seq(predictor)

final <- data.frame()
final <- as.data.frame(cbind(n,predictor,CVerror,logLik))
colnames(final) <- c("Iteration","Predictor","CV","Log Likelihood")

# converting all the columns to as.character()
final$Iteration <- as.character(final$Iteration)
final$Predictor <- as.character(final$Predictor)
final$CV <- as.character(final$CV)
final$`Log Likelihood` <- as.character(final$`Log Likelihood`)


## writing final to output file
#write.table(final,file="solution.txt",row.names = FALSE,col.names=TRUE)
write.table(final,file=output.file,row.names = FALSE,col.names=TRUE)

