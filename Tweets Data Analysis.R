#Package installation
install.packages("quanteda")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("stringr")

#Importing data file
setwd("C:\\Users\\user\\Downloads")
tweets <- read.csv("tweets_data.csv")

#Loading data into dataframes
df_tweets <- data.frame(doc_id=row.names(tweets),
                        text=tweets$text,
                        date=tweets$date,
                        source=tweets$source)

#Converting to corpus
library(quanteda)
docs <- Corpus(DataframeSource(df_tweets))

#Data Preprocessing
#Creating the toSpace content transformer & removing URL
library(tm)
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
removeURL <- function(x) {gsub("http.*","", x)}
docs <- tm_map(docs, content_transformer(removeURL))
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, ':')	

docs <- tm_map(docs, removePunctuation) #Removing punctuations
docs <- tm_map(docs,content_transformer(tolower)) #Converting the text to lower case
docs <- tm_map(docs, removeNumbers) #Removing numbers

docs <- tm_map(docs, removeWords, stopwords("english")) #Removing english common stopwords
docs <- tm_map(docs, removeWords, c(stopwords("english"),stopwords("SMART"),"amp","â???o" ,"â???T", "â???", "T", "¦")) 

docs <- tm_map(docs, stripWhitespace) #Eliminating extra white spaces

dtm <- TermDocumentMatrix(docs) # Building a term-document matrix
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# a) Frequency plot of top 15 words
a <- barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
             col ="lightblue", main ="Frequency Plot of top 15 words",
             xlab = "Word",
             ylab = "Frequencies",
             ylim = c(0,1300))
b <- as.matrix(d[1:15,]$freq,)
text(a,b,labels=as.character(d[1:15,]$freq), pos = 3, cex = 0.8, col = "red")

# b) Generating word-cloud of 50 most common words
library(wordcloud)
library(RColorBrewer)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# c) Usage of social media hourly
library(lubridate)
datetime<-strftime(df_tweets$date, "%Y-%m-%d %H:%M:$S")
time<-strftime(datetime, format="%H:%M:%S")
time <- as.POSIXct(time,format="%H:%M:%S")
hour <- hour(time)
library(ggplot2)
library(scales)
ggplot(data=df_tweets, aes(x = hour))+ 
  geom_histogram(colour="black", fill="red", alpha=0.5, binwidth = 1)+
  labs(title="Hourly usage of social media")+
  scale_x_continuous(breaks = c(0:23))

# d) Usage of twitter per month
library(lubridate)
library(dplyr)
df_tweets <- data.frame(doc_id=row.names(tweets),
                        text=tweets$text,
                        date=tweets$date,
                        source=tweets$source)

texts_each_month <- df_tweets %>%
  mutate(datetime = ymd_hms(date)) %>%
  mutate(month=floor_date(datetime, "month"))%>%
  group_by(month)%>%
  count(text, month) %>%
  summarise(count_of_texts = sum(n))

df <- as.data.frame(texts_each_month)
df
ggplot(df, aes(x=month,y=count_of_texts)) +
  labs(title="Usage of twitter each month", x="Month", y="Usage")+
  geom_line(colour="red")+ geom_point()+
  scale_x_datetime(labels = date_format("%b(%y)"), breaks = "month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# e) Top 10 words for each source
library(tidytext)
library(stringr)
library(tidyr)
library(dplyr)
library(lubridate)

# Data Preprocessing
remove_reg <- "â???o|â???T|â???|T|¦"
df_tweets <- df_tweets %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  mutate(datetime = ymd_hms(date)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
df_tweets[] <- lapply(df_tweets, sub, pattern = "http.*", replacement = "") #Removing URL
df_tweets[] <- lapply(df_tweets, sub, pattern = "#.*", replacement = "") #Removing hashtags
df_tweets <- df_tweets[!(df_tweets$word == ""), ] #Removing empty rows
df_tweets <- df_tweets[!(df_tweets$word == "amp"), ] #Removing unnecessary words
df_tweets <- df_tweets[!(df_tweets$word == "rt"), ]

#For source = "Twitter for iPhone"
frequency_iPhone <- df_tweets %>% 
  group_by(source) %>% 
  filter(source=="Twitter for iPhone") %>%
  count(word, sort = TRUE) %>% 
  left_join(df_tweets %>% 
              group_by(source) %>% 
              filter(source=="Twitter for iPhone") %>%
              summarise(total = n()))
head(frequency_iPhone[2],10)

#For source = "Media Studio"
frequency_MediaStudio <- df_tweets %>% 
  group_by(source) %>% 
  filter(source=="Media Studio") %>%
  count(word, sort = TRUE) %>% 
  left_join(df_tweets %>% 
              group_by(source) %>% 
              filter(source=="Media Studio") %>%
              summarise(total = n()))
head(frequency_MediaStudio[2],10)

# f) Five words that were not used in the last six months of the data but were frequently used in first six months
#Finding the words that were used in first 6 months
first_6_months <- subset(df_tweets,
                         datetime >= as.POSIXct('2017-01-01 00:00') &
                           datetime <= as.POSIXct('2017-06-01 23:59'))

first_6_months <- first_6_months %>%
  group_by(word) %>%
  count(word, sort = TRUE) %>%
  left_join(first_6_months %>% 
              group_by(word) %>%
              summarise(total = n()))
head(first_6_months,20)

#Finding the words that were used in last 6 months
last_6_months <- subset(df_tweets,
                        datetime >= as.POSIXct('2018-03-01 00:00') &
                          datetime <= as.POSIXct('2018-09-01 23:59'))
head(last_6_months[5],10)
#Converting into table
table(first_6_months$word)
unique <- first_6_months[!first_6_months$word %in% last_6_months$word,]
head(unique,5)

