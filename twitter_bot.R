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

tweet_sentiments = function(tweet.txt) {
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

anger_response = function(tweet.txt) {
  responses = c("I'm angry!", "This is ridiculous!", "I'm FURIOUS!", "FUCK THIS!!", "BULLSHIT!!", "You cunt!", "Go fuck yourself!")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

anticipation_response = function(tweet.txt) {
  responses = c("Looking forward to it!", "I've been waiting forever.", "Finally happening", "Fiesta", "Fuck yeah!")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

disgust_response = function(tweet.txt) {
  responses = c("This makes me wanna vomit", "Disgusting", "Repulsive", "Why do such things exist", "eww")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

fear_response = function(tweet.txt) {
  responses = c("Scary", "Holy shit", "SOS", "Peeing in my pants", "Mama", "Horrifying")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

joy_response = function(tweet.txt) {
  responses = c("YAY", "Hoorah", "This calls for a celebration!", "Happy to see that", "Lekker!")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

negative_response = function(tweet.txt) {
  responses = c("Sucks", "I hate this.", "Horrible", "Someone needs to do something about it!", "Disappointed")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

positive_response = function(tweet.txt) {
  responses = c("Feeling hopeful", "The world is a nice place", "Live, Love, Laugh", "Be grateful", "This is great news")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

sadness_response = function(tweet.txt) {
  responses = c("I wish this never happened", "That's horrible", "I can't take this anymore", "This world is a sad place.", "Praying")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

surprise_response = function(tweet.txt) {
  responses = c("WOW", "Unbelievable!", "Holy cow!", "WTF?!", "OMG!")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}

trust_response = function(tweet.txt) {
  responses = c("We've got this", "Feeling good about this", "I believe in this world", "We're better tgt", "All for one and one for all")
  response = paste("@", twt$screenName, " ", sample(responses, size = 1), sep = "")
  updateStatus(response, inReplyTo = twt$id)
}



