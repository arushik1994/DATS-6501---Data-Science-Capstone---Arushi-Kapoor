#Loading the libraries
library(ROAuth)
library(twitteR)
library(ggplot2)
library(lubridate)
library(tm)
library(syuzhet)
library(rtweet)
library(lubridate)
library(SnowballC)
library(viridis)
library(tidytext)
library(reactable)
library(reactablefmtr)


#Authentication keys
consumer_key <-"OXcjWAW0EUjHoP84krSrvUToK"
consumer_secret <- "t153qv2EvrwpS6kM1ZlllJN55ckzE5sdlRBsMEGQGkWQ4Rd5RO"
access_token<-"1308779802044837891-IVSzlQfnns01U01XjVLul4yi3Wa9xm"
access_secret <- "840mX1LWDUOawmkB5FyobMmg8ezMiskbaRBVndwogJ4QR"

setup_twitter_oauth(consumer_key ,consumer_secret, access_token, access_secret)

#Extracting tweets
gp_gap <- searchTwitter('Gender+Pay+Gap',lang="en", n=300)
gp_gap_data <- twListToDF(gp_gap)

#Data Preprocessing
gp_gap_data$new_ct <- format(gp_gap_data$created, tz="America/New_York", usetz=TRUE)
gp_gap_data$date <- day(gp_gap_data$new_ct)
gp_gap_data$hour <- hour(gp_gap_data$new_ct)

#Graphing
#Density plot
t.dat <- gp_gap_data %>%
  mutate(likehigh = ifelse(favoriteCount > 3,"high","low"))

t.plot <- ggplot(t.dat, aes(x = hour)) + 
  geom_density(aes(fill = likehigh), alpha = 0.4) +
  xlab("24 Hour format") + ylab("Density") + ggtitle(label = "When do people tweet the most about the Gender Pay Gap?",
                                                     subtitle = "Based on a collection of 300 tweets & the number of favorites") +
  labs(fill = "Number of Favorites") +
  theme_minimal()

t.plot <- t.plot + theme(legend.position = "bottom",
                         plot.title = element_text(face = "bold", size = 18, color = "Black"),
                         plot.subtitle = element_text(size = 16, color = "Black"),
                         axis.text.x = element_text(size = 12),
                         axis.title = element_text(size = 14),
                         axis.text.y = element_text(size = 12)) +
  scale_fill_manual(breaks = c("high", "low"),
                    labels = c("High: > 3", "Low: < 3"),
                    values = c("grey","powderblue")) 
#ggplotly(t.plot)

#Text data cleaning
data.text <- gp_gap_data$text
data.text <- tolower(data.text)

#Replace blank space ("rt")/
data.text <- gsub("rt", "", data.text)

#Replace @UserName
data.text <- gsub("@\\w+", "", data.text)

#Remove punctuation
data.text <- gsub("[[:punct:]]", "", data.text)

#Remove links
data.text <- gsub("http\\w+", "", data.text)

#Remove tabs
data.text <- gsub("[ |\t]{2,}", "", data.text)

#Remove blank spaces at the beginning
data.text <- gsub("^ ", "", data.text)

#Remove blank spaces at the end
data.text <- gsub(" $", "", data.text)


#Sentiment Analysis
mysent <- get_nrc_sentiment((data.text))

#Compute total score for each sentiment
mysentscores <- data.frame(colSums(mysent[,]))
names(mysentscores) <- "Score"
mysentscores <- cbind("sentiment" = rownames(mysentscores),mysentscores)
rownames(mysentscores) <- NULL

newscore <- mysentscores %>%
  arrange(Score)

#Plot total sentiment scores across data 
sent.plot1 <- ggplot(newscore, aes(x=sentiment, y=Score)) +
  geom_bar(stat = "identity", fill = "lightblue", color="black") +
  ggtitle(label = "Sentiment Scores for Gender Pay Gap",
          subtitle = "Based on a collection of 300 tweets") + 
  xlab("Sentiment Type") +
  theme_minimal()

sent.plot1 <- sent.plot1 + theme(legend.position = "none",
                                 plot.title = element_text(face = "bold", size = 18, color = "Black"),
                                 plot.subtitle = element_text(size = 16, color = "Black"),
                                 axis.text.x = element_text(size = 12),
                                 axis.title = element_text(size = 14),
                                 axis.text.y = element_text(size = 12)) +
  scale_fill_viridis(discrete=TRUE)


#Top 10 words: positive and negative words (sentiment using bing lexicon)
#Unnest tweets and remove stop words
dat.all <- gp_gap_data %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) 

#Join with sentiment lexicon and count words
bing_word_counts <- dat.all %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Focus on top per sentiment and plot
sent.lab <- c("negative" = "Negative Words", "positive" = "Positive Words")

sent.plot2 <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = TRUE, color = "black") +
  geom_label(aes(label = n), hjust = -0.3, fill = "lightgrey") +
  ggtitle(label = "Top words contributing to sentiments",
          subtitle = "Examining positive and negative words") +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, labeller = labeller(sentiment = sent.lab)) +
  labs(x = "Contribution to sentiment",
       y = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("tomato1","olivedrab3")) + theme(legend.position = "none",
                                plot.title = element_text(face = "bold", size = 18, color = "Black"),
                                plot.subtitle = element_text(size = 16, color = "Black"),
                                axis.text.x = element_text(size = 12),
                                axis.title = element_text(size = 14),
                                axis.text.y = element_text(size = 12))


