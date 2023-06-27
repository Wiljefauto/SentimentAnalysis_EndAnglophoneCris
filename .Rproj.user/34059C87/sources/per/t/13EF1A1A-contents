#For our project, we decided to do a Twitter Mining case on the hashtag #EndAnglophoneCrisis.


#Phase 01: Obtaining Twitter Data	

#To obtain data, our application for a twitter developer account was approved on December 10, 2020.
#We then generated all access and token keys.
#Instead of loading the “twitteR” package, we used the “rtweet” package instead.


#Loading required packages

library(rtweet)
library(httpuv)
library(tidytext)
library(tidyverse)
library(ggplot2)

#Generate R Objects with API and token access (Keys generated from our Twitter Developer Account @ucacmsi)

api_key <- "84hTf7ic2FzsB8sewk7BMBFqT"
api_secret_key <- "xZ1KEouC8afOPjsGwKG2SeJTRwnzDmD2Erm5w2JbVhd3YTnvVs"

#Authentication process via web browser (This is enable by the package "httpuv")

token <- create_token(
  app = "MSI_UCAC_App",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

#Search for 3000 tweets without rts in English language ONLY using the hashtag #EndAnglophoneCrisis

nosocrisis_tweets <- search_tweets(q = "#endanglophonecrisis", n = 3000, lang ="en", include_rts = FALSE)

class(nosocrisis_tweets)

#Exporting tweets pulled in a .csv file
write_as_csv(nosocrisis_tweets, "nosocrisis_tweets.csv")

#Phase 02: Cleaning and Analysing Twitter Data

head(nosocrisis_tweets$text)

nosocrisis_tweets$text <-  gsub("https\\S*", "", nosocrisis_tweets$text)
nosocrisis_tweets$text <-  gsub("@\\S*", "", nosocrisis_tweets$text) 
nosocrisis_tweets$text  <-  gsub("amp", "", nosocrisis_tweets$text) 
nosocrisis_tweets$text  <-  gsub("[\r\n]", "", nosocrisis_tweets$text)
nosocrisis_tweets$text  <-  gsub("[[:punct:]]", "", nosocrisis_tweets$text)

#Stopwords removal

tweets <- nosocrisis_tweets %>%
  select(text) %>%
  unnest_tokens(word, text)

tweets <- tweets %>%
  anti_join(stop_words)

# Drawing a bar chart of the most frequent words found in the tweets

tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets containing #EndAnglophoneCrisis",
       subtitle = "Stop words removed from the list")

# Wordcloud of frequent used hashtags

head(nosocrisis_tweets$hashtags)

nosocrisis_tweets$hashtags <- as.character(nosocrisis_tweets$hashtags)
nosocrisis_tweets$hashtags <- gsub("c\\(", "", nosocrisis_tweets$hashtags)
set.seed(1234)
wordcloud(nosocrisis_tweets$hashtags, min.freq=10, scale=c(1.2, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#From which platforms people are tweeting the most (at least 15 tweets)

nosocrisis_app <- nosocrisis_tweets %>%
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
nosocrisis_app <- subset(nosocrisis_app, count > 15)

#Plotting a donut chart to illustrate the result

install.packages("socviz")
library(socviz)


data <- data.frame(
  category=nosocrisis_app$source,
  count=nosocrisis_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

#Frequency of Tweets with a #EndAnglophoneCrisis hashtag

ts_plot(nosocrisis_tweets, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #EndAnglophoneCrisis hashtag",
       subtitle = paste0(format(min(nosocrisis_tweets$created_at), "%d %B %Y"), " to ", format(max(nosocrisis_tweets$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

#Top tweeting locations

nosocrisis_tweets %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)

# Top tweeters about #EndAnglophonecrisis

nosocrisis_tweets %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))


#Phase 03: Sentiment Analysis

library(syuzhet)

# Converting tweets to ASCII to track strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

#End
