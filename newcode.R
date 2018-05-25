library(e1071)
library(qdap)
library(tm)
library(rpart)
library(rpart.plot)
library(e1071)
library(nnet)

data1 <-read.csv("C:/Users/a550859/Documents/R/Workfolder/SN_IM.csv")
tickets<-data.frame(data1)
nrow(data1)
createTags <- function(tickets) {
      # get the relevant columns
      df <- tickets[, c('CLOSE_NOTES', 
                        'ASSIGNMENT_GROUP', 'SHORT_DESCRIPTION', 
                        'DETAILED_DESCRIPTION')]
      
      # get the tags
      cat("Getting correct tags from text description...")
      l1dd <- which(startsWith(df$CLOSE_NOTES, '[L1][DD'))
      l1dd <- c(l1dd, which(startsWith(df$CLOSE_NOTES, '[L1] [DD')))
      l1dd <- c(l1dd, which(startsWith(df$CLOSE_NOTES, '[L1 DD')))
      l1dd <- c(l1dd, which(startsWith(df$CLOSE_NOTES, '[L1[DD')))
      l2dd <- which(startsWith(df$CLOSE_NOTES, '[L2][DD'))
      l2dd <- c(l2dd, which(startsWith(df$CLOSE_NOTES, '[L2] [DD')))
      l2dd <- c(l2dd, which(startsWith(df$CLOSE_NOTES, '[L2 DD')))
      l2dd <- c(l2dd, which(startsWith(df$CLOSE_NOTES, '[L2[DD')))
      l1 <- which(grepl('\\[L1\\]', df$CLOSE_NOTES))
      l1 <- c(l1, which(startsWith(df$CLOSE_NOTES, '[L!')))
      l1 <- setdiff(l1, l1dd)
      l2 <- which(grepl('\\[L2\\]', df$CLOSE_NOTES))
      l2 <- setdiff(l2,l2dd)
      ut <- setdiff(1:nrow(df), union_all(l1,l2, l1dd, l2dd))
      support <- which(df$ASSIGNMENT_GROUP %in% 
                         c('cfit-sys-support', 'ctg-sys-support'))
      
      # create tags from description and domain knowledge
      df$TAG <- 'NA'
      df[l1, 'TAG'] <- 'L1'
      df[l2, 'TAG'] <- 'L2'
      df[l1dd, 'TAG'] <- 'L1-DD'
      df[l2dd, 'TAG'] <- 'L2-DD'
      df[intersect(ut, support), 'TAG'] <- 'L2'
      cat("done.\n")
      
      # clean up the text data
      cat("Creating term-document matrix from description data...")
      df$DESCRIPTION <- with(df, paste(SHORT_DESCRIPTION, DETAILED_DESCRIPTION))
      toSpace <- content_transformer(function(x, pattern) 
      {return (gsub(pattern, " ", x))})
      corp <- Corpus(DataframeSource(as.data.frame(df$DESCRIPTION)))
      corp <- tm_map(corp, toSpace, "-")
      corp <- tm_map(corp, toSpace, ":")
      corp <- tm_map(corp, removePunctuation)
      corp <- tm_map(corp, toSpace, "\\.")
      corp <- tm_map(corp, removeWords, stopwords("english"))
      corp <- tm_map(corp, stripWhitespace)
      corp <- tm_map(corp, removeNumbers)
      corp <- tm_map(corp, content_transformer(tolower))
      corp <- tm_map(corp, stemDocument)
      
      # build classification model
      df$TAG <- as.factor(df$TAG)
      tagged <- which(df$TAG != 'NA')
      untagged <- which(df$TAG == 'NA')
      dtm <- DocumentTermMatrix(corp, control=list(wordLengths=c(3,20), 
                                                   bounds=list(global=c(5,10000)), 
                                                   weighting=function(x) 
                                                     weightTfIdf(x, normalize=FALSE)))
      cat("done.\n")
      
      tagDecoded <- decodeClassLabels(df$TAG)
      cat("Using MLP to build classification model...")
      model <- mlp(dtm[tagged,], tagDecoded[tagged,1:4], size=5, 
                   learnFuncParams=c(0.1), maxit=50, inputsTest=dtm[untagged,], 
                   targetsTest=tagDecoded[untagged,1:4])
      cat("done.\n")
      
      predictions <- encodeClassLabels(predict(model, dtm[untagged,]))
      for (i in 1:nlevels(df$TAG)) {
        rows <- which(predictions == i)
        if (length(rows) > 0) {
          tag <- levels(df$TAG)[i]
          predictions[rows] <- tag
          cat("Predicting", length(rows), "entries with tag", tag, "\n")
        }
      }
      
      df[untagged, 'TAG'] <- predictions
      
      cat("Returning all tags...\n")
      return(df$TAG)
    }
    