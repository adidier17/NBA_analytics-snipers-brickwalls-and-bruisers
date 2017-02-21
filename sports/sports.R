library(ggplot2)
library(data.table)
setwd('/Users/ethen/Desktop/northwestern/winter/MSIA 421 Data Mining/sports')

preprocess <- function(box_data) {
	# only consider the data where a player has actually played a game
	box_data <- box_data[ MINUTES_PLAYED > 0, ]

	# extract possibly useful columns that are normalized by minutes
	box_data[, ('Points_Per_Minute') := TOTAL_POINTS / MINUTES_PLAYED]
	box_data[, ('Assists_Per_Minute') := ASSISTS / MINUTES_PLAYED]
	box_data[, ('Rebounds_Per_Minute') := REBOUNDS / MINUTES_PLAYED]
	box_data[, ('Steals_Per_Minute') := STEALS / MINUTES_PLAYED]
	box_data[, ('Turnovers_Per_Minute') := TURNOVERS / MINUTES_PLAYED]
	box_data[, ('Personal_Fouls_Per_Minute') := PERSONAL_FOULS / MINUTES_PLAYED]
	box_data[, ('Blocks_Per_Minute') := BLOCKED_SHOTS / MINUTES_PLAYED]
	box_data[, ('Field_Goals_Percentage') := FIELD_GOALS_MADE / FIELD_GOALS_ATT]
	box_data[, ('Free_Throws_Percentage') := FREE_THROWS_MADE / FREE_THROWS_ATT]
	box_data[, ('Three_Point_Percentage') := THREE_POINT_MADE / THREE_POINT_ATT]
	retain_cols <- c('PLAYER',
					 'Points_Per_Minute', 'Assists_Per_Minute', 
					 'Rebounds_Per_Minute', 'Steals_Per_Minute', 
					 'Turnovers_Per_Minute', 'Personal_Fouls_Per_Minute',
					 'Blocks_Per_Minute', 'Field_Goals_Percentage', 
					 'Free_Throws_Percentage', 'Three_Point_Percentage')
	box_data <- box_data[, retain_cols, with = FALSE]

	# replace NAs with 0
	for ( j in seq_len(ncol(box_data)) ) {
		set(box_data, which( is.na(box_data[[j]]) ), j, 0)
	}
	return(box_data)
}

box_data <- fread('box2015.csv')
box_data <- preprocess(box_data)

# exclude the player information when doing the clustering analysis
player <- box_data[['PLAYER']]
box_data[ , PLAYER := NULL ]

# set parameters for kmeans
n_start <- 10
iter_max <- 100
n_clusters <- 3:8
sse <- numeric(length(n_clusters))

# standardize the data, and use the elbow method to find the optimal number for k
box_data_scaled <- scale(box_data)
for ( k in seq_along(n_clusters) ) {
	set.seed(12345)
	fit <- kmeans(box_data_scaled, centers = k, iter.max = iter_max, nstart = n_start)
	sse[k] <- sum(fit$withinss)
}
dt_sse <- data.table(k = n_clusters, sse = sse)
ggplot( dt_sse, aes(k, sse) ) +
geom_line() + theme_bw()

# we decided to use 6 clusters, not because there is a clear elbow, but
# because the interpretation made more sense
set.seed(12345)
fit <- kmeans(box_data_scaled, centers = 6, iter.max = iter_max, nstart = n_start)


summary.kmeans = function(fit) {
	p = ncol(fit$centers)
	k = nrow(fit$centers)
	n = sum(fit$size)
	sse = sum(fit$withinss)
	xbar = t(fit$centers)%*%fit$size/n
	ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2) 
	print(data.frame(
		n=c(fit$size, n),
		Pct=(round(c(fit$size, n)/n,2)),
		round(rbind(fit$centers, t(xbar)), 2),
		RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
	))
	cat("SSE = ", sse, "; SSB = ", ssb, "\n")
	cat("R-Squared = ", ssb/(ssb+sse), "\n")
	cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
	invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}


plot.kmeans = function(fit,boxplot=F) { 
	require(lattice) 
	p = ncol(fit$centers) 
	k = nrow(fit$centers) 
	plotdat = data.frame( 
		mu=as.vector(fit$centers), 
		clus=factor(rep(1:k, p)), 
		var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers)) 
	) 
	print(dotplot(var~mu|clus, data=plotdat, 
		panel=function(...){ 
			panel.dotplot(...) 
			panel.abline(v=0, lwd=.1) 
		}, 
		layout=c(k,1), 
		xlab="Cluster Mean" 
	))
	invisible(plotdat) 
}

summary(fit)
plot(fit)

#cluster 1 is outside shooters
#cluster 2 is offensive big men/power forwards
#cluster 3 is shiftiness/agility
#cluster 4 is defensive big men
#cluster 5 is hitting your free throws
#cluster 6 is having a bad game

# 1. put the player names and cluster assignment to each row 
# 2. aggregate players to each cluster
# e.g. how many times did each player appear in each cluster
# 3. compute entropy of each player and sort them in decreasing order
box_data[, player := player]
box_data[, cluster := fit$cluster]
aggregation <- box_data[ , .(counts = .N), by = .(cluster, player) ]
entropy <- aggregation[ , {
	p <- counts / sum(counts)
	entropy <- -sum( p * log10(p) )
	list(entropy = entropy)
}, by = player ][order(-entropy), ]

#remove bad game cluster, lump outside shooters and ft cluster together
box_data2<-box_data[, player := player]
box_data2<-box_data[, cluster := fit$cluster]
box_data2$cluster[which(box_data2$cluster==5)]<-1
box_data2<-box_data2[-which(box_data2$cluster==6),]
aggregation2 <- box_data2[ , .(counts = .N), by = .(cluster, player) ]
entropy2 <- aggregation2[ , {
  p <- counts / sum(counts)
  entropy <- -sum( p * log10(p) )
  list(entropy = entropy)
}, by = player ][order(-entropy), ]
#model now shows a more interpretable figure of entropy
#since our clusters now are purely playstyle focused, we won't get inflated entropy if a player hits
#free throws or has a bad game
#the "bad game" cluster is still useful info but for this analysis I don't think we should take into account
#player consistency
