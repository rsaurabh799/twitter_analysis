install.packages("TwitteR")
install.packages("stringr")
install.packages("xlsx")
install.packages("plyr")


library(stringr)
library(twitteR)
library(xlsx)
library(plyr)

api_key <- "gT78tc6XFBRSVZz816xJtQh5K"
api_secret <- "3hbKNw09npGAcXbUjrxKWxPsAi0HrUkgebHpwXJqcDafKia0GE"
access_token <- "https://api.twitter.com/oauth/access_token"
access_token <- "2968945153-svLaNc1myKMZy1Xvrs5WuVk6vNWgEhtpfmlq4bH"
access_token_secret <- "fu8vXfgywn0BDn4TsEmObwg7TunmV6E0LB8Wuy0RR2Vy2"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

setwd("E:/R Projects/twitter-analysis")

neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")

neg = c(neg, 'wtf')

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

tweets = searchTwitter('Obama',n=2500)
Tweets.text = laply(tweets,function(t)t$getText()) # gets text from Tweets

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function

hist(analysis$score)


