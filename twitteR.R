library(twitteR)
source("script.R")

## Build the API model to get the tweets
api_key<-"V7K1ZkputrE22Kb2Hlmleb5ze"
api_secret<-"KuEtwZjcIZ8VuKENgoQ3w6l5ngFMhnniiQlqwubCldOG6Hf6d2"
access_token<-"774465035552886785-7uBmIK2xCfnPk4zF00UM5JxrtNMCBAv"
access_token_secret<-"s51j97gnlBDeWypMLiyyoAUfHjwuwuCPieSnG33DZtC1j"
## Setup twitteR oauth
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

## Search the tweets for topic "demonetisation"
Text<-searchTwitter("JackMa",n=200,since="2018-01-01")
Text_Tweets<-sapply(Text,function(x)x$getText())%>%as.data.frame()
colnames(Text_Tweets)=c("Text")

##Clean the tweet for sentiment analysis
#  remove html links, which are not required for sentiment analysis
#  remove html links, which are not required for sentiment analysis
tweet1=stri_enc_toutf8(gsub('http\\S+\\s*',"", text$comments))
tweet2=gsub("[][!#$%()*,.:;<=>@^_`|~.{}?\"&]","",tweet1)
tweet3=gsub("[[:digit:]]","",tweet2)
tweet3=stri_trans_general(tweet3,"latin-ascii")
tweet3<-tweet3%>%data.frame()
colnames(tweet3)<-c("comments")


get_city<-function(x){
  city<-owm_cities[owm_cities$nm==x,]
  city<-city[,c(2,3,4)]
  colnames(city)<-c("city","lat","lon")
  return(city)
}

longitude<-get_city("Kolkata")[3]
latitude<-get_city("Kolkata")[2]

tweets <- twListToDF(searchTwitter("#DonateBloodSaveLife", n = 100, 
                                   geocode = paste0(latitude, ",", longitude, ",10km"))) 
