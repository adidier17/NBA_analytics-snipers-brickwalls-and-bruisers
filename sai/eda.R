box<-read.csv("box2015.csv")

box1<-box[which(box$GAME_PLAYED==1),]

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
plot(pcplayer)

#ordering players by PC1 as a power score
playerstrength<-data.frame(playerstats$PLAYER,pcplayer$x[,1])
names(playerstrength)<-c("Player","powerScore")
playerstrength<-playerstrength[order(playerstrength$powerScore,decreasing=TRUE),]
playerstrength$Rank<-c(1:476)


#team pca
pcteam<-prcomp(teamstats[,-c(1:5)])
summary(pcteam)
pcteam$x
plot(pcteam)

#ordering teams by pc1 as a power score
pcteam$x[,1]
teamstrength<-data.frame(teamstats$TEAM,pcteam$x[,1])
names(teamstrength)<-c("Team","powerScore")
teamstrength<-teamstrength[order(teamstrength$powerScore),]
teamstrength$Rank<-c(1:30)

#import teams sorted by win percentage and sort
teamwinpct<-read.csv("teams.csv")
teamwinpct$rankByWins<-c(1:30)
teamstrength<-merge(teamstrength,teamwinpct,by.x="Team",by.y="TEAM")
teamstrength<-teamstrength[order(teamstrength$rankByWins),]
teamstrength<-teamstrength[,c(1,2,4,3,5)]
#diff between PCA score and actual win pct
teamstrength<-mutate(teamstrength,diff=rankByWins-Rank)

#rookie pca, see if KAT is still ROY
pcrookie<-prcomp(rookiestats[,-c(1,2,3,4,5)])
summary(pcrookie)
pcrookie$x

#do rankings for rookies
rookiestrength<-data.frame(rookiestats$PLAYER,pcrookie$x[,1])
names(rookiestrength)<-c("Player","powerScore")
rookiestrength<-rookiestrength[order(rookiestrength$powerScore,decreasing=TRUE),]
