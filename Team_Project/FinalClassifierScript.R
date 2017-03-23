# Team 1: Andrew Butler, Farah Abdul-Rahman, Gunjan Gala
# Project 1
# Date: 11/21/16

######################################################################################################
# Read in arguments
# arg[1] --- the expression data that was provided for the assignment
# arg[2] --- the new expression data to classify
# arg[3] --- file location for the ouput (list of classes corresponding to each column
#            in the unseen expression data from arg[2])

args <- commandArgs(trailingOnly = TRUE)

######################################################################################################
# Training Data Processing
tumor.subtype <- read.csv(args[1], sep = '\t')
# exclude ambiguous label
tumor.subtype <- tumor.subtype[, !(colnames(tumor.subtype) %in% c("BCR_ABL_Hyperdip_.10"))]
# remove hyphen from gene names
rownames(tumor.subtype)<-gsub("-","_",rownames(tumor.subtype))
prefix <- strsplit(colnames(tumor.subtype), "_", fixed = TRUE)
prefix <- sapply(prefix, FUN = function(x) {x[1]})
subtype.dict <- c("T_ALL", "TEL_AML1", "Hyperdip", "MLL", "E2A_PBX1", "BCR_ABL")
names(subtype.dict) <- c("T", "TEL", "Hyperdip", "MLL", "E2A", "BCR")
subtype <- subtype.dict[prefix]
tumor.subtype <- t(as.matrix(tumor.subtype))

######################################################################################################
# Test (New/Unseen) Data Processing
new.data <-  read.csv(args[2], sep = '\t')
rownames(new.data)<-gsub("-","_",rownames(new.data))
new.data <- t(as.matrix(new.data))


######################################################################################################
# Build Classifier (Multinomial Logistic Regression)
library(glmnet)
FitMLR <- function(data.use, classes, nfold, dfmax = NULL, plot.cve = FALSE){
  set.seed(42)
  # determine best value of alpha 
  folds <- sample(rep(1:nfold, ceiling(nrow(data.use) / nfold))[1:nrow(data.use)])
  model.list <- vector("list", 11)
  for (i in seq(0, 1, 0.1)) {
    if (i == 0 && !is.null(dfmax)) i <- 0.01
    model.cv.fit <- cv.glmnet(data.use, classes, family = "multinomial", alpha = i, nfolds = nrow(data.use), 
                              foldid = folds, parallel = T, dfmax = dfmax)
    if(i == 0.01) i <- 0
    model.list[[(i + 0.1) * 10]] <- model.cv.fit
  }
  min.cve <- numeric()
  for (i in 1:length(model.list)) {
    min.cve <- c(min.cve, min(model.list[[i]]$cvm))
  }
  best.model <- model.list[[which.min(min.cve)]]
  return(best.model)
}

final.classifier <- FitMLR(data.use = tumor.subtype, classes = subtype, nfold = 10, dfmax = 28)


######################################################################################################
# Run Classifier and Write Output

predictions <- predict(final.classifier, newx = new.data, s = "lambda.min", type = "class")
write.table(predictions, file = args[3], row.names = F, col.names = F)
