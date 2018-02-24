library(readr)
tweets <- read_csv("~/Tulika Personal/Study/R/twitter on R/tweets.csv")
View(tweets)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content
library(wordcloud)
wordcloud(corpus, scale=c(5,0.5), max.words = 100,random.order = FALSE, rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies,lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative~., data=trainSparse,method = "class")
prp(tweetCART)
predictCART = predict(tweetCART,newdata = testSparse,type = "class")
table(testSparse$Negative,predictCART)
write.csv(tweetsSparse, file="twitterone.csv")
findAssocs(frequencies,"word",corlimit=0.2)




