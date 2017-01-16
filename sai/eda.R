library(plyr)
library(dplyr)

box<-read.csv("box2015.csv")

box1<-box[which(box$GAME_PLAYED==1),]
box1<-box[which(box$MINUTES_PLAYED!=0),]

#get average statistics for players
playerstats<-aggregate(.~PLAYER,box1,FUN=mean)

#average statistics for teams
teamstats<-aggregate(.~TEAM,box1,FUN=mean)

#import list of rookies
rookies<-read.csv("rookies.csv")
rookies$ROOKIE<-rep(1,73)
rookiestats<-merge(rookies,playerstats,by.x="PLAYER",by.y="PLAYER")


#pca
#player pca
pcplayer<-prcomp(avgstats[,-c(1,2,3,4,5)])
summary(pcplayer)
pcplayer$x
#plot(pcplayer)

#ordering players by PC1 as a power score
playerstrength<-data.frame(playerstats$PLAYER,pcplayer$x[,1])
names(playerstrength)<-c("Player","powerScore")
#make powerScore a percentile score called "rating"
q<-quantile(playerstrength$powerScore,probs=seq(0,1,.01))
q[101]<-q[101]+.001 #fiddling with the top score to make sure top players get assigned a rating
playerstrength$rating<-cut(playerstrength$powerScore,q)
levels(playerstrength$rating)<-seq(1,100,1)
playerstrength<-playerstrength[order(playerstrength$powerScore,decreasing=TRUE),]
playerstrength$Rank<-c(1:476)
View(playerstrength)


#team pca
pcteam<-prcomp(teamstats[,-c(1:5)])
summary(pcteam)
pcteam$x
#plot(pcteam)

#ordering teams by pc1 as a power score
pcteam$x[,1]
teamstrength<-data.frame(teamstats$TEAM,pcteam$x[,1])
names(teamstrength)<-c("Team","powerScore")
#make powerScore a percentile score called "rating"
q2<-quantile(teamstrength$powerScore,probs=seq(0,1,.01))
q2[1]<-q2[1]-.01 #fiddling with the top score to make sure the Spurs get assigned a rating
teamstrength$rating<-cut(teamstrength$powerScore,q2)
levels(teamstrength$rating)<-seq(100,1,-1)

teamstrength<-teamstrength[order(teamstrength$rating),]
teamstrength$Rank<-c(1:30)

#import teams sorted by win percentage and sort
teamwinpct<-read.csv("teams.csv")
teamwinpct$rankByWins<-c(1:30)
teamstrength<-merge(teamstrength,teamwinpct,by.x="Team",by.y="TEAM")
teamstrength<-teamstrength[order(teamstrength$rankByWins),]
teamstrength<-teamstrength[,c(1,3,2,5,4)]
#diff between PCA score and actual win pct
teamstrength<-mutate(teamstrength,diff=rankByWins-Rank)
View(teamstrength)

#rookie pca, see if KAT is still ROY
pcrookie<-prcomp(rookiestats[,-c(1,2,3,4,5)])
summary(pcrookie)
pcrookie$x

#do rankings for rookies
rookiestrength<-data.frame(rookiestats$PLAYER,pcrookie$x[,1])
names(rookiestrength)<-c("Player","powerScore")
rookiestrength<-rookiestrength[order(rookiestrength$powerScore,decreasing=TRUE),]
View(rookiestrength)
