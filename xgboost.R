Train <- read.csv("Train.csv")
Test <- read.csv("Test.csv")
eBayiPadTrain <- read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayiPadTest <- read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)
# import the xgboost library
library(xgboost)
temp  = 0
ans = 0
for(i in 1:10){
    # construct the dtrain maxtirx
    dtrain <- xgb.DMatrix(data = as.matrix(Train), label = eBayiPadTrain$sold)
    nround <- 3000
    # setup the params
    param <- list(max.depth=5, eta=0.005, nthread = 12, objective='binary:logistic')
    # cross validation for choosing params
    cvresult <- xgb.cv(param, dtrain, nround, nfold=10, min_child_weight = 1, metrics={'auc'},showsd = F,verbose = F)
    # store the best performence round num
    t_round = which.max(cvresult$test.auc.mean)
    # use the t_round
    bst <- xgboost(data = dtrain,max.depth = 5, eta = 0.005, nround = t_round,min_child_weight = 1, nthread = 12, eval_metric = "auc", objective = "binary:logistic",verbose = F)
    # making predictions for test data
    Pred <- predict(bst, newdata = as.matrix(Test))
    # sum the results
    ans = ans+ Pred

    #----output for tuning params
    #print (head(Pred))
    #print (which.max(cvresult$test.auc.mean))
    #print (max(cvresult$test.auc.mean))
    temp = temp + max(cvresult$test.auc.mean)
}
temp/10
ans/10
mean(ans/10)

MySubmission = data.frame(UniqueID = eBayiPadTest$UniqueID, Probability1 = ans/10)
write.csv(MySubmission, "submission.csv", row.names=FALSE)