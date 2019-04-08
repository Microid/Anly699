

#library(sandwich)
#require(msm)
#library(magrittr)
#library(tokenizers)
#
#library(stringr)

library(boot)
library(pscl)


##Needed
library(MASS)
library(dplyr)
library(qdap)
library(ggplot2)
library(tidytext)
library("lmtest")

#Load data
data  = read.csv("data_withsentiment.csv", sep = "," , header = TRUE, stringsAsFactors=FALSE, 
                 colClasses = c("character", "character", "Date", "character", "integer", "character", "character", 
                                "integer", "integer", "character", "character", "character", "character", "character",
                                "integer", "integer"))

# Check which_misspelled
n_misspelled <- sapply(data$Tweet, function(x){
  length(which_misspelled(x, suggest = FALSE))
})

#log count of misspelled
data_enriched= data.frame(data$Tweet, n_misspelled, row.names = NULL)
data$errors = data_enriched$n_misspelled

#Create additional predictors
data$num_hashtags=lengths(strsplit(data$Hashtags, "\\W+"))
data$num_mentions=lengths(strsplit(data$UserMentionID,"\\W+" ))

#write to intermediate file for later use
write.csv(data, file = "fulldataset.csv")

df = read.csv("fulldataset.csv")



#clean tweets longer than 140
df=df[df$len<=140,]

#Convert data types
df$Source=as.character(df$Source)
df$Source[is.na(df$Source)] <- " "

df$Hashtags=as.character(df$Hashtags)
df$Hashtags[is.na(df$Hashtags)] <- " "


#take only world cup hashtags
hashtags = c("WorldCup2018","WorldCupRussia2018","Russia2018","worldcup")
df <- filter(df, grepl(paste(hashtags,collapse="|"),df$Hashtags,ignore.case=TRUE))

#take only primary sources
sources=c("Twitter for Android", "Twitter for iPhone", "Twitter Web Client", "Twitter Lite", "Twitter for iPad")
df=df[df$Source %in% sources,]


#Keep only the columns we need
keeps <- c( "Source", "len", "Likes", "RTs", "num_hashtags", "num_mentions", "Followers", "Friends", "sentiment", "errors")
data_cleaned = df[keeps]

#Read into a new var to help with debugging
d=data_cleaned

#mean,median
median(d$Friends)
median(d$Followers)

#Create friends categories
d$friends_cat<-c( "<100","200","300","400","500","600","700","800","900","1000-1999", "2000-2999", "3000-3999", "4000-4999",">5000")[
  findInterval(d$Friends , c(-Inf,100,200,300,400,500,600,700,800,900,1000, 2000, 3000, 4000, Inf) ) ]
d$friends_cat=as.factor(d$friends_cat)
barplot(summary(d$friends_cat))

#Create friends categories V2
d$friends_cat<-c( "Below Median","Above Median")[
  findInterval(d$Friends , c(-Inf,408, Inf) ) ]
d$friends_cat=as.factor(d$friends_cat)
barplot(summary(d$friends_cat))



#Create followers categories
d$followers_cat<-c( "<100","200","300","400","500","600","700","800","900","1000-1999", "2000-2999", "3000-3999", "4000-4999",">5000")[
  findInterval(d$Followers , c(-Inf,100,200,300,400,500,600,700,800,900,1000, 2000, 3000, 4000, Inf) ) ]
d$followers_cat=as.factor(d$followers_cat)
barplot(summary(d$followers_cat))

#Create followers categories V2
d$followers_cat<-c( "Below Median","Above Median")[
  findInterval(d$Followers , c(-Inf,347, Inf) ) ]
d$followers_cat=as.factor(d$followers_cat)
barplot(summary(d$followers_cat))


plot(density(d$Likes))
plot(density(d$RTs))

d <- within(d, {
  Source <- factor(Source, levels=1:5, labels=c("Twitter for Android", "Twitter for iPhone", 
                                            "Twitter for iPad","Twitter Lite","Twitter Web Client"))
})



plot(density(d$Likes), main="Distribution of dependent variable Likes")
plot(density(d$RTs), main="Distribution of dependent variable Retweets")



with(d, tapply(RTs, Source, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

with(d, tapply(Likes, Source, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


View(d)
mt <- glm(RTs ~ friends_cat+followers_cat+len+num_hashtags+num_mentions+sentiment+errors, data = d)

summary(mt)

#Run the Negative Binomial regression models
m1.1 <- glm.nb(RTs ~ friends_cat+followers_cat+Source+len+num_hashtags+num_mentions+sentiment+errors, data = d)
m2.2 <- glm.nb(Likes ~friends_cat+Source+followers_cat+len+num_hashtags+sentiment, data = d)

summary(m1.1)

summary(m2.2)


m3 <- glm(RTs ~ friends_cat+followers_cat+Source+len+num_hashtags+num_mentions+sentiment, family = "poisson", data = d)


lrtest(m3, m1)
#z1.2 <- zeroinfl(RTs ~ followers_cat+ friends_cat+Source+len+num_hashtags+num_mentions+sentiment+errors,data=d, dist = "negbin", EM = TRUE)
#z1 <- zeroinfl(RTs ~ Source+len+num_hashtags+num_mentions+sentiment+errors,data=d, dist = "negbin", EM = TRUE)

install.packages("RsqGLM")


modEvA::RsqGLM(model=m1.1)
modEvA::RsqGLM(model=m2.2)

View(d)

stepAIC(m3)
