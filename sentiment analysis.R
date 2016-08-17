
#"If you don't understand their emotions, you don't understand your customers"

sent<-read.csv("sentiment.csv", stringsAsFactors = FALSE)
str(sent)
wc<-read.csv("sentiment.csv")
write.csv(wc,"beforepreprocess.txt")
summary(sent)
#We are interested in detecting negative sentiments.
#Pre- Processing
sent$negative <- as.factor(sent$Sentiment==0)
table(sent$negative)
table(sent$Sentiment)
library(tm)
library(SnowballC)
corpus<-Corpus(VectorSource(sent$Text))
corpus
corpus[[1]]
corpus <- tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,c(stopwords("english")))
corpus<-tm_map(corpus,stemDocument)
corpus<-tm_map(corpus,PlainTextDocument)
typeof(corpus)
writeLines(as.character(corpus), con="mycorpus.txt")
#Bag of Words
freq<-DocumentTermMatrix(corpus)
typeof(freq)
#total terms is 2156
#popular terms
#more terms means more independent variables.
freq20<-findFreqTerms(freq,lowfreq = 20) 
freq20
#total terms is 2156,words that appear atleast 20 times 233 => there are words that appear very often. remove them to make model simple.
#This is called sparsity=> various terms that appear in very less documents.
sparse<-removeSparseTerms(freq,0.995)
#keep only terms that appear in atleast 0.5% of the given terms.
#0.5% of 2156 = 10 docs.  trade off choose balance.
str(sparse)
#terms = 224.
#224/2156 = 10.3. we could retrieve only 10% of the documents.
#converting in to a dataframe. with all the terms as variables.
sentsparse<-as.data.frame(as.matrix(sparse))
str(sentsparse)
colnames(sentsparse)= make.names(colnames(sentsparse))
sentsparse$negative<-sent$negative
str(sentsparse)
typeof(sentsparse)
sentsparse$loved<-NULL
sentsparse$sucks<-NULL
sentsparse$sucked<-NULL
sentsparse$movie<-NULL
sentsparse$awesom<-NULL

#Modelling
set.seed(1234)
spl<-sample.split(sentsparse$negative,SplitRatio = 0.7)
trainsparse<-subset(sentsparse,spl==TRUE)
testsparse<-subset(sentsparse,spl==FALSE)
table(trainsparse$negative)
table(sentsparse$negative)
table(testsparse$negative)
accoftraining<-2925/nrow(sentsparse)
cat("abc :",accoftraining)
2082/nrow(trainsparse)
893/nrow(testsparse)

#Predicting
library(rpart)
library(rpart.plot)
sentCART<-rpart(negative ~.,data=trainsparse, method="class")
prp(sentCART)
predictCART<- predict(sentCART,newdata = testsparse, type="class")
table(testsparse$negative,predictCART)
(1148+860)/nrow(testsparse) #96.7
table(testsparse$negative)
1183/nrow(testsparse) #57

library(randomForest)
set.seed(1234)
sentRF<-randomForest(negative ~., data = trainsparse)
predictRF<-predict(sentRF, newdata=testsparse)
table(testsparse$negative,predictRF)
(1173+876)/nrow(testsparse) # -->98.4


