library("papaja")
set.seed(1234)
library(magrittr)
library(dplyr)
library(tokenizers)
library(tidytext)
library(ggplot2)
library(stringr)



data  = read.csv("FIFA.csv", sep = "," , header = TRUE, stringsAsFactors=FALSE, 
                 colClasses = c("character", "character", "Date", "character", "integer", "character", "character", 
                                "integer", "integer", "character", "character", "character", "character", "character",
                                "integer", "integer"))
data=as.data.frame(data)

hashtags = c("WorldCup2018","WorldCupRussia2018","Russia2018","worldcup")
df <- filter(data, grepl(paste(hashtags,collapse="|"),data$Hashtags,ignore.case=TRUE))

sources=c("Twitter for Android", "Twitter for iPhone", "Twitter Web Client", "Twitter Lite", "Twitter for iPad")
df=df[df$Source %in% sources,]

source_table=table(df$Source)
sl = as.data.frame(source_table)
slices <- sl$Freq
lbls <- names(source_table)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Tweet Sources")


df=df[df$len<=140,]
hist(df$len, breaks =5, main="Frequency of tweet lengths", xlab= "Tweet Length", ,cex=0.6,col="gray",  las=3)


len_table=as.data.frame(table(df$len))
pctdf <- as.data.frame(len_table)
s=sum(pctdf$Freq)
pctdf$Perc <- (pctdf$Freq / s) * 100
pctdf <- pctdf[order(pctdf$Perc,decreasing =TRUE),]
names(pctdf)=c("Length", "Frequency", "Percentage")
toprint= head(pctdf, n=10)
apa_table(toprint, caption = "Tweet length breakdown")


tl=table(df$Likes)
tr=table(df$RTs)

sltl = as.data.frame(tl)
slicestl <- sltl$Freq
lblstl <- names(tl)
pcttl <- (slicestl/sum(slicestl)*100)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
dftl=as.data.frame(cbind(names(tl),pcttl))
dftl$pcttl=as.numeric.factor(dftl$pcttl)
names(dftl) = c("Likes","PCT")
apa_table(head(dftl,n=10), caption = "Likes breakdown")


sltr = as.data.frame(tr)
slicestr <- sltr$Freq
lblstr <- names(tr)
pcttr <- (slicestr/sum(slicestr)*100)
dftr=as.data.frame(cbind(names(tr),pcttr))
dftr$pcttr=as.numeric.factor(dftr$pcttr)
names(dftr) = c("RTs","PCT")
#head(dftr,n=10)
apa_table(head(dftr,n=10), caption="Retweets breakdown")


head(df)


