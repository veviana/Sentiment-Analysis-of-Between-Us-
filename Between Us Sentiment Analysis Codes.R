#Loading libraries
library(twitteR)
library(tm)
library(qdap)
library(wordcloud)
library(ggplot2)
library(syuzhet)
library(Rgraphviz)
library(tokenizers)
library(wordcloud2)
library(RColorBrewer)
library(SnowballC)
library(plyr)
library(dplyr)



#Reading the csv file 
tweets.text <- read.csv("AmongUs.csv")

#Extracting the text portion from the crawled tweet
tweets.text <- sapply(tweets.text, gettext)
tweets.text <- iconv(tweets.text,to="ASCII",sub="")

#Functions to remove tags, special characters etc
removeTags <- function(input){gsub("[@#]\\S+ *","",input)}
removeURL <- function(input){gsub("http\\S+ *","",input)}
removeEnter <- function(input){gsub("\\n","",input)}
removeSpecialChar <- function(input) {gsub("[^[:alpha:][:space:]]*","",input)}

#Call the function
tweets.text <- removeTags(tweets.text)
tweets.text <- removeURL(tweets.text)
tweets.text <- removeEnter(tweets.text)
tweets.text <- removeSpecialChar(tweets.text)


#Creating corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#Pre-processing 
tweets.text.corpus <- tm_map(tweets.text.corpus, content_transformer(tolower))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, stopwords('english'))
tweets.text.corpus <- tm_map(tweets.text.corpus, stripWhitespace)
tweets.text.corpus <- tm_map(tweets.text.corpus, removePunctuation)
tweets.text.corpus <- tm_map(tweets.text.corpus, removeNumbers)
tweets.text.corpus <- tm_map(tweets.text.corpus,stemDocument)
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, c('among', 'among'))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, c('sweep', 'sweep'))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, c('sweepuff', 'sweepuff'))
  


#Building TDM
tdm <- TermDocumentMatrix(tweets.text.corpus)
findFreqTerms(tdm,lowfreq = 10)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Construct word cloud (With minimum frequency set to 10)
wordcloud(words = d$word, 
          freq = d$freq, 
          min.freq = 10, 
          scale = c(2,.6),
          max.words=150, 
          random.order=FALSE, 
          rot.per=0.3, 
          colors=brewer.pal(4, "Dark2"))
findAssocs(tdm,"amongus",corlimit=0.15)

#Remove sparse terms
tdm <- removeSparseTerms(tdm,sparse=0.99)
inspect(tdm)

# compute the distance between terms
tdm.scale <- scale(tdm)

# perform hierarchical clustering on the terms see which terms # appear with which other terms in tweets.
d <- dist(tdm.scale,method='euclidean')
fit <- hclust(d,method='ward.D')
plot(fit)

# We cut the tree into 4 main clusters

groups <- cutree(fit, k=4)
rect.hclust(fit,k=4,border='red')


##############################################################
#
# Sentiment Analysis 
#
##############################################################

score <- get_sentiment(tweets.text, method="syuzhet")

# Use the get_nrc_sentiment function to do emotion tagging
emotions <- get_nrc_sentiment(tweets.text)
labels <- factor(emotions, levels=names(sort(table(emotions),decreasing=T)))

# combine the polarity and emotions to the tweets
combine <- data.frame(tweets.text,emotions[1:8],score)

count <- colSums(emotions[,1:8])
labels <- colnames(emotions[1:8])

senti_df <- data.frame(labels,count)


#Tweets with joy tags
joy_items <- which(emotions$joy > 0)
tweets.text[joy_items]

# Plot the emotions / polarity
ggplot(senti_df, aes(x=labels,y=count,fill=labels))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Dark2")+
  labs(x='categories',y='number of words',title='Sentiment Score of Between Us')+
  theme(plot.title = element_text(hjust = 0.5))

plot(combine$score)
boxplot(combine$score)


sentiment.positive = subset(combine$score, score >0)
sentiment.negative = subset(combine$score, score <0)
sentiment.neutral = subset(combine$score, score ==0)

#To calculate sentiment score 
Total = sum(sumpositive, sumnegative, sumneutral)

sumpositive = sum(Npositive$freq)
percpostive = (sumpositive / Total) * 100


sumnegative = sum(NNegative$freq)
percnegative = (sumnegative / Total) * 100


sumneutral = sum(NNeutral$freq)
percneutral = (sumneutral / Total) * 100


#Storing results in dataframe
dftemp=data.frame(topic=c("Positive", "Negative","Neutral"), 
                  number=c(sumpositive,sumnegative,sumneutral),
                  percentage=c(percpostive,percnegative,percneutral))

#Sentiment pie chart
ggplot(dftemp, aes(x="", y=number, fill=topic)) +
  geom_bar(stat="identity", width=1,color="white") +
  ggtitle("Sentiment Pie Chart") +
  coord_polar("y", start=0, direction = 1) +                     
  theme_void() +                                  
  scale_fill_manual(values=c("#D55E00", "#E69F00", "#009E73")) +
  theme(plot.title = element_text(hjust = 0.5,vjust=-6)) +
  geom_text(aes(label = paste0(topic,"\n",round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5), size=4)

#Scatter plot - sentiment polarity 
ggplot(data = combine, aes(x = as.numeric(rownames(combine)), y = score)) +
  geom_point(color = ifelse(score>0,"chartreuse3",ifelse(score<0,"red","yellow3"))) +
  geom_text(aes(label=ifelse(score>2.85,as.character(score),'')),hjust=0,vjust=0) +
  geom_text(aes(label=ifelse(score < -2.2,as.character(score),'')),hjust=0,vjust=0)+
  labs(x='Number of words',y='Score',title='Sentiment Polarity of Between Us')+
  theme(plot.title = element_text(hjust = 0.5)) 



