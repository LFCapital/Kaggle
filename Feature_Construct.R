setwd("C:\\Users\\Lee\\Downloads\\Kaggle")

eBayTrain = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest = read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

# Since the "ipad5" and "iPad mini Retina" are not showed up in the test data,
# we can change them into unknown category
eBayTrain[eBayTrain$productline == "iPad 5",]$productline = "Unknown"
eBayTrain[eBayTrain$productline == "iPad mini Retina",]$productline = "Unknown"

# collect all condition data and price data for feature engineering
all_condition_data = c(eBayTrain$condition,eBayTest$condition)
all_price_data = c(eBayTrain$startprice,eBayTest$startprice)

# Transfer these variable to factor
eBayTrain$condition = as.factor(eBayTrain$condition)
eBayTrain$cellular = as.factor(eBayTrain$cellular)
eBayTrain$carrier = as.factor(eBayTrain$carrier)
eBayTrain$color = as.factor(eBayTrain$color)
eBayTrain$storage = as.factor(eBayTrain$storage)
eBayTrain$productline = as.factor(eBayTrain$productline)

# Do the same operation for Test data
eBayTest$condition = as.factor(eBayTest$condition)
eBayTest$cellular = as.factor(eBayTest$cellular)
eBayTest$carrier = as.factor(eBayTest$carrier)
eBayTest$color = as.factor(eBayTest$color)
eBayTest$storage = as.factor(eBayTest$storage)
eBayTest$productline = as.factor(eBayTest$productline)

#install.packages("tm")
library(tm)
#install.packages("SnowballC")

library(SnowballC)
# Building the corpus, and do the standart text analytics procedures
CorpusDescription = Corpus(VectorSource(c(eBayTrain$description, eBayTest$description)))
CorpusDescription = tm_map(CorpusDescription, content_transformer(tolower), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, PlainTextDocument, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removePunctuation, lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, removeWords, c('ipad',stopwords("english")), lazy=TRUE)
CorpusDescription = tm_map(CorpusDescription, stemDocument, lazy=TRUE)

# Transfer the corpus to word matrix
dtm = DocumentTermMatrix(CorpusDescription)
# removespare words
sparse = removeSparseTerms(dtm, 0.99)

# matrix to dataframe
orgin_matrix = as.data.frame(as.matrix(dtm))
DescriptionWords = as.data.frame(as.matrix(sparse))

#---------Feature Engineering
# count the words of goods description
DescriptionWords$WordCount1 = rowSums(DescriptionWords)
DescriptionWords$WordCount2 = rowSums(orgin_matrix)

# Get the names of condition
con_names = names(tapply(eBayTrain$sold,eBayTrain$condition,mean))
# Get the avg sold percentage for each condition
sold_per = as.data.frame(tapply(eBayTrain$sold,eBayTrain$condition,mean))
# Get the avg price for each condition
c_price = as.data.frame(tapply(eBayTrain$startprice,eBayTrain$condition,mean))

# Rename
names(sold_per) = "per"
names(c_price) = "price"

# Initialize 
DescriptionWords$c_per = 0
DescriptionWords$c_price = -1

for(i in 1:length(con_names)){
  # set the c_per to the avg condition price for all data
  DescriptionWords[all_condition_data == con_names[i],]$c_per = sold_per$per[i]
  # set the bool flag for c_price, wheck whether the item's price is above or below the avg price of the same condition
  DescriptionWords[all_condition_data == con_names[i],]$c_price = ifelse(all_price_data[all_condition_data == con_names[i]]>=c_price$price[i],1,0)
}

#colnames(DescriptionWords) = make.names(colnames(DescriptionWords))
DescriptionWordsTrain = head(DescriptionWords, nrow(eBayTrain))
DescriptionWordsTest = tail(DescriptionWords, nrow(eBayTest))

# get the startprice and biddble
DescriptionWordsTrain$startprice = eBayTrain$startprice
DescriptionWordsTest$startprice = eBayTest$startprice
DescriptionWordsTrain$biddable = eBayTrain$biddable
DescriptionWordsTest$biddable = eBayTest$biddable

# dummyencoding function
dummyEncoding <- function(df, colnum){ 
  dummyDf <- as.data.frame(model.matrix(~df[,colnum]-1))  
  names(dummyDf) <- paste("dummy_",as.character(levels(df[,colnum])),sep="") 
  df[,colnum] <- NULL 
  df <- cbind(df,dummyDf) 
  df 
}

# dummy encoding for factor variable
eBayTrain_d = dummyEncoding(eBayTrain,4)
eBayTrain_d = dummyEncoding(eBayTrain_d,4)
eBayTrain_d = dummyEncoding(eBayTrain_d,4)
eBayTrain_d = dummyEncoding(eBayTrain_d,4)
eBayTrain_d = dummyEncoding(eBayTrain_d,4)
eBayTrain_d = dummyEncoding(eBayTrain_d,4)

eBayTest_d = dummyEncoding(eBayTest,4)
eBayTest_d = dummyEncoding(eBayTest_d,4)
eBayTest_d = dummyEncoding(eBayTest_d,4)
eBayTest_d = dummyEncoding(eBayTest_d,4)
eBayTest_d = dummyEncoding(eBayTest_d,4)
eBayTest_d = dummyEncoding(eBayTest_d,4)

# construct the final feature for train and test
Train = cbind(DescriptionWordsTrain,eBayTrain_d[6:41])
Test = cbind(DescriptionWordsTest,eBayTest_d[5:40])

write.csv(Train,"Train.csv",row.names = F)
write.csv(Test,"Test.csv",row.names = F)