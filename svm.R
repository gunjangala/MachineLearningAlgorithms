library(pls)
library(e1071)
library(caret)
require(kernlab)

P1 <- read.delim("~/Google Drive/Machine Learning/Assignments/P1/Problem1_expressionMatrix.txt", header=TRUE)
P1 <- as.data.frame(t(P1))
P1$class <- (row.names(P1))
P1$class <- substr(P1$class,0,3)
P1$class <- as.factor(P1$class)

P1$class <- sapply(P1$class, function(x) {
  if (x=="T_A"){return ("T")}
  else if (x=="TEL"){return ("TEL")}
  else if (x=="Hyp"){return ("Hyperdip")}
  else if (x=="E2A"){return ("E2A")}
  else if (x=="MLL"){return ("MLL")}
  else if (x=="BCR"){return ("BCR")}})

P1$class <- as.factor(P1$class)
x <- subset(P1,select=-class)
y <- P1$class
P1 <- as.data.frame(P1)
rownames(P1) <- NULL

####### FEATURE SELECTION
#seeds <- vector(mode = "list", length = 5+1)
#for(i in 1:6) seeds[[i]] <- sample.int(165, 2)
#seeds[[6]] <- sample.int(165, 1)
set.seed(100)
rfe.control<-rfeControl(method="cv",number=5,functions = caretFuncs,saveDetails=TRUE,
                        rerank=TRUE,verbose = TRUE,returnResamp="all",
                        p=0.8)

result.linear <- rfe(x,y, sizes=20, rfeControl=rfe.control , method="svmLinear",metric="Accuracy",maximize=TRUE)
result.radial <- rfe(x,y, sizes=20, rfeControl=rfe.control , method="svmRadial",metric="Accuracy",maximize=TRUE)
result.polynomial <- rfe(x,y, sizes=20, rfeControl=rfe.control , method="svmPoly",metric="Accuracy",maximize=TRUE)

svm.function <- function (kernel,x,y,data){
  set.seed(100)
  nsamples<-dim(data)[1]
  sample <- data[sample(nrow(data),nsamples),]
  ratio <- 0.7
  train.samples <- ratio*nsamples
  train.rows <- c(sample(nrow(sample), trunc(train.samples)))
  train.set  <- sample[train.rows, ]
  test.set   <- sample[-train.rows, ]
  target <- P1$class
  train.result <- train.set$class
  test.result  <- test.set$class
  formula <- as.formula(class ~ . )
  set.seed(100)
  svm.tune.control <- tune.control(sampling = "cross", cross=5, best.model = TRUE,performances = TRUE)
  svmTune <- tune.svm(x=x,y=y,data=data,formula=(class~.),
                      kernel=kernel, cost=10^(-10:3), 
                      gamma=10^(-10:3),
                      tunecontrol=svm.tune.control)
  svm.model <- svm(formula, data = train.set, 
                   kernel = kernel, 
                   gamma = svmTune$best.parameters$gamma, 
                   cost  = svmTune$best.parameters$cost)
  train.pred <- predict(svm.model, train.set)
  test.pred  <- predict(svm.model, test.set)
  svm.table <- table(pred = test.pred, true = test.result)
  final <- c(svmTune$best.performance,noquote(paste(svmTune$best.parameters[1])),
             noquote(paste(svmTune$best.parameters[2])))
  return (final)
}

linear.all <- svm.function("linear",x,y,P1)
radial.all <- svm.function("radial",x,y,P1)
poly.all <- svm.function("polynomial",x,y,P1)

linear.20 <- P1[,result.linear$optVariables[1:20]]
x1 <- linear.20
linear.20$class <- P1$class
svm.linear.20 <- svm.function("linear",x1,y,linear.20)

radial.20 <- P1[,result.radial$optVariables[1:20]]
x1 <- radial.20
radial.20$class <- P1$class
svm.radial.20 <- svm.function("radial",x1,y,radial.20)

polynomial.20 <- P1[,result.polynomial$optVariables[1:20]]
x1 <- polynomial.20
polynomial.20$class <- P1$class
svm.polynomial.20 <- svm.function("polynomial",x1,y,polynomial.20)


summary <- as.data.frame(rbind(linear.all,radial.all,poly.all,svm.linear.20,svm.radial.20,svm.polynomial.20))
colnames(summary) <- c("CVerror","cost","gamma")
summary$CVerror <- round((as.numeric(as.character(summary$CVerror))),4)
summary$cost <- (as.numeric(as.character(summary$cost)))
summary$gamma <- as.numeric(as.character(summary$gamma))
summary


























