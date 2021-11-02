setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")
library(stringr)
library(caret)
all <- read.csv("data/all_20211025.csv")

##############
# 1 - Get random articles
##############

all.random <- all[sample(1:nrow(all),200),]
write.csv(all.random,"data/all_20211025_random200.csv",row.names=FALSE)

##############
# 2 - Manually inspect whether they match criteria
##############

##############
# 3 - Load and modify response data
##############
all.random <- read.csv("data/all_20211025_random200_test.csv")
all.random <- all.random[,c("Title","DOI","Keywords","Response")]
rownames(all.random) <- all.random$DOI

#Get all keywords
allkeywords <- paste(shQuote(all.random$Keywords), collapse="; ")
allkeywords <- gsub("\"","",allkeywords)
allkeywords <- unique(sort(unlist(strsplit(allkeywords, split="; "))))
allkeywords <- allkeywords[allkeywords != ""]

#Create keyword matrix
all.matrix <- matrix(ncol=length(allkeywords),nrow=nrow(all.random))
colnames(all.matrix) <- allkeywords
rownames(all.matrix) <- all.random$DOI

#Populate matrix
for (d in all.random$DOI){
  for (k in allkeywords){
    if(k %in% trimws(unlist(str_split(all.random[d,"Keywords"], ";")))){
      all.matrix[d,k] = 1
    }else{
      all.matrix[d,k] = 0
    }
}
}

#Add response colum
all.matrix <- as.data.frame(all.matrix)
all.matrix <- all.matrix[rowSums(all.matrix) != 0,]
all.matrix$Response_variable <- all.random[rownames(all.matrix),"Response"]


##############
# 4 - Create keyword model
##############

split <- sample(1:nrow(all.matrix),nrow(all.matrix)*0.6)
train <- all.matrix[split,]
train$Response_variable <- as.factor(as.character(train$Response_variable))
test <- all.matrix[-split,]
test$Response_variable <- as.factor(as.character(test$Response_variable))

folds <- 7
cvIndex <- createMultiFolds(factor(train$Response_variable), folds, times=100)
cv <- trainControl(method="repeatedcv",
                     number=folds,
                     index = cvIndex,
                     returnResamp="final",
                     classProbs=TRUE,
                     summaryFunction=twoClassSummary,
                     indexFinal=NULL,
                     savePredictions = TRUE)


#TuneGrids
grid_rf <-  expand.grid(mtry = c(80,500,1000,1500))
grid_lr <- expand.grid(cost = c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 1, 10),
  loss = "L2_primal",
  epsilon = 0.01)
grid_xgb <-  expand.grid(nrounds=500,
  gamma=0,
  eta=c(0.001, 0.01, 0.1, 1),
  max_depth=8,
  colsample_bytree= 0.8,
  min_child_weight=1,
  subsample=c(0.4, 0.5, 0.6, 0.7))

tuneList <- list(
  rf=caretModelSpec(method="rf", tuneGrid=grid_rf),
  lr=caretModelSpec(method="regLogistic", tuneGrid=grid_lr),
  xgb=caretModelSpec(method="xgbTree", tuneGrid=grid_xgb))

model_list <- caretList(
  Response_variable~., data=train,
  trControl=cv,
  methodList=c("rf", "regLogistic","xgbTree"),
  tuneList=tuneList)

results <- resamples(model_list[c(1:3)])
summary(results)
