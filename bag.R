library(e1071)
data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/SN_IM.csv")
nrow(data1)
head(data1)
data2 <- datafr
nrow(data2)
library(qdap)
library(tm)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)
str(data2)
nrow(data2)
?cat()
vect <- VectorSource(data2)
nrow(vect)
corp <- VCorpus(vect)
corp
corp[15][1]$content
nrow(corp)
frequentterms <- frequency(corp,30)
frequentterms
plot(frequentterms)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}
clean_corp<-clean_corpus(corp)
clean_corp
corp_dtm<-DocumentTermMatrix(clean_corp)
corp_dtm
corp_m<-as.matrix(corp_dtm)
dim(corp_m)
head(corp_m)

library(SnowballC)
library(wordcloud)

review_dtm<-removeSparseTerms(corp_dtm,0.99)
review_dtm
head(corp)
inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm,1000)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

review_dtm_tfidf <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

inspect(review_dtm_tfidf[1,1:10])
freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

reviews=cbind(data1$ASSIGNMENT_GROUP,as.matrix(review_dtm_tfidf))
reviews =as.factor(reviews)
id_train<-sample(nrow(reviews),(reviews)*0.80)
id_train
reviews.train<-reviews[id_train,]
reviews.test<-reviews[-id_train,]

head(reviews.train)

reviews.svm = rpart(reviews~ .,method="class",data = reviews.train)

library('e1071')
library('SparseM')
library('tm')
library('SnowballC')

# LOAD DATA FROM CSV
sampledata <- read.csv("C:/Users/a550859/Documents/R/Workfolder/SN_IM.csv")
nrow(sampledata)

#rawdata <- readLines(sampledata, ok = TRUE, warn = FALSE, encoding = "utf-8")
conv <- iconv(sampledata[c(2,3)], to = "utf-8",sub="")
nrow(conv)
conv_data <- (conv[!is.na(conv)])

# CREATE DATA FRAME OF 750 TRAINING JOURNALS ARTICLES AND 250
# TEST ARTICLES INCLUDING 'Abstract'(Column 1) AND 'Journal_group' (Column 2)

nrow(traindata)
traindata<-as.data.frame(sampledata[1:100000,c(1,2)])
testdata<-sampledata[100000:147664,c(1,2)]


# SEPARATE TEXT VECTOR TO CREATE Source(),
# Corpus() CONSTRUCTOR FOR DOCUMENT TERM
# MATRIX TAKES Source()
trainvector <- as.vector(traindata$ASSIGNMENT_GROUP)
testvector <- as.vector(testdata$ASSIGNMENT_GROUP)

# CREATE SOURCE FOR VECTORS
trainsource <- VectorSource(trainvector)
testsource <- VectorSource(testvector)

# CREATE CORPUS FOR DATA
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)
nrow(traincorpus)
# PERFORMING THE VARIOUS TRANSFORMATION on "traincorpus" and "testcorpus" DATASETS #SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
traincorpus <- tm_map(traincorpus,stripWhitespace)
traincorpus <- tm_map(traincorpus,tolower)
traincorpus <- tm_map(traincorpus, removeWords,stopwords("english"))
traincorpus<- tm_map(traincorpus,removePunctuation)
traincorpus <- tm_map(traincorpus, PlainTextDocument)
traincorpus = tm_map(traincorpus, str_replace_all,"[^[:alnum:]]", " ")
testcorpus <- tm_map(testcorpus,stripWhitespace)
testcorpus <- tm_map(testcorpus,tolower)
testcorpus <- tm_map(testcorpus, removeWords,stopwords("english"))
testcorpus<- tm_map(testcorpus,removePunctuation)
testcorpus <- tm_map(testcorpus, PlainTextDocument)
testcorpus = tm_map(traincorpus, str_replace_all,"[^[:alnum:]]", " ")

nrow(traincorpus)

tdm = TermDocumentMatrix(wim_corpus, 
                         control = list(removePunctuation = TRUE, 
                                        stopwords =  TRUE, 
                                        removeNumbers = TRUE, tolower = TRUE))

# CREATE TERM DOCUMENT MATRIX
inspect(traincorpus[1,5])
trainmatrix <-t(TermDocumentMatrix(traincorpus))
testmatrix <- t(TermDocumentMatrix(testcorpus))


# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
model <- naiveBayes(as.matrix(trainmatrix),as.factor(traindata$Journal_group));

# PREDICTION
results <- predict(model,as.matrix(testmatrix));
