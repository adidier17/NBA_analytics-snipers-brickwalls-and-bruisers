###HW 4
library(psych)
###1
music<-read.csv("music.csv")

fit1<-factanal(music[,-28],factors=2,score="reg",rotation="varimax")
fit1 #31, 47 maybe, 49 maybe, 53, 54

#musicpca<-prcomp(music[,-28],scale=T)
#musicpca$rotation #31, 49

alpha(music[,-28],check.keys=TRUE) #31, 34, 43, 45, 49

##all three methods suggest removing 31 and 49, also removing 54 because of promiscuity

fit2<-factanal(music[,-c(4,22,27,28)],factors=2,score="reg",rotation="varimax")
fit2loadings<-fit2$loadings
fit2loadings[fit2loadings<.3]<-0
fit2loadings
#Factor 1 is social aspects of music. Includes questions 28, 29, 30, 32, 33, 34, 43, 46, 47, 50, 52
#Factor 2 is personal aspects. Includes questions 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 48, 51

music1<-music[,c(1,2,3,5,6,7,16,19,20,23,25)]
music2<-music[,c(8,9,10,11,12,13,14,15,17,18,21,24)]

alpha(music1)
alpha(music2)

###2
book<-read.csv("book.csv")
book<-log(book+1)
bookpca<-prcomp(book,scale=T)
plot(bookpca) #let's say 3 factors
fit3<-factanal(book,factors=3,score="reg",rotation="varimax")
fit3loadings<-fit3$loadings
fit3loadings[fit3loadings<.3]<-0
fit3loadings #fiction, legends, philosophy, religion*, psychology*, music*, facsimilie, politics, maps, gamesriddles, sports
#videos, nonbooks

exclude<-c("fiction1", "legends6", "philosophy7", "facsimile17", "politics22", "maps30", "GamesRiddles38", "sports39",
           "videos50", "nonbooks99","cartoons5", "railroads27")

alpha(book,check.keys = TRUE) #remove sports
bookpca$rotation #sports

fit4<-factanal(book[,!names(book)%in%exclude],factors=3,score="reg",rotation="varimax")
fit4$loadings
#factor 1 is sciences
#factor 2 is arts/humanities
#factor 3 is hobbies








