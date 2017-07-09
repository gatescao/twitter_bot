library(twitteR)
library(httr)
library(magrittr)
library(tidytext)
library(dplyr)
library(tm)
library(tidyr)

consumer_key = "riyT4t1JIlQXSjHa7IQ4cliQI"
consumer_secret = "JZ6rnj4WRRe7J73mdk7w74TDYiv00ffEX9d3ZJwRRNqYVJuXAh"
access_token = "810192513940254720-gop8dS0tF6k6yYpr7hOveisKyPq2WCf"
access_secret = "duOun3m8nPG52UooDeZKpBZUph6bP7A8y6TOeyozgXvRs"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


nytimes = getUser("nytimes")
last_tweet <- nytimes$lastStatus
tweet.txt = last_tweet$getText()
tweet.txt = gsub('(f|ht)tp\\S+\\s*',"", tweet.txt)
tweet.txt = gsub("[^[:print:]]", "", tweet.txt)
tweet.txt = gsub("[^[:space:]]*…$", "", tweet.txt)

tweet.txt = tweet.txt %>% removePunctuation %>% removeNumbers %>% tolower

tweet_sentiments = function(twt) {
  tweet.df = data.frame(do.call(rbind, strsplit(twt, " ", fixed=TRUE)), stringsAsFactors = FALSE)
  tweet.tidy = tweet.df %>% gather() 
  tweet.tidy = tweet.tidy %>% mutate(index = 1:nrow(tweet.tidy)) %>% select(-key)
  
  sentiment = inner_join(get_sentiments("nrc"), tweet.tidy, by = c("word" = "value"))
}


tweet_sentiments = tweet_sentiments(tweet.txt)


sentiment = function(tweet_sentiments){
  # count sentiment with most occcurrrences
  counts <- count(tweet_sentiments, vars = sentiment)
  sentiment_indices <- which(counts$n == max(counts$n))
  
  # return max or first element in case of multiple max
  if(length(sentiment_indices == 1)){
    sentiment <- (counts[sentiment_indices[1], ]$vars)
    return (sentiment)
  }
  
}






