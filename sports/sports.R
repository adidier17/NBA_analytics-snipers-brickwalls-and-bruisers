library(ggplot2)
library(data.table)
setwd('/Users/ethen/Desktop/northwestern/winter/MSIA 421 Data Mining/sports')
box_data <- fread('box2015.csv')

# --------------------------------------------------------------------------------------
# preprocessing

# get the top 7 players (sorted by minutes played) from each team
starters <- box_data[, .(minutes = sum(MINUTES_PLAYED)), by = .(TEAM, PLAYER) 
				   ][order(TEAM, -minutes),]
top_players <- starters[, head(.SD, 7), by = TEAM]

preprocess <- function(box_data, minutes_threshold) {
	# only consider the data where a player has played
	# above a certain number of minutes
	box_data <- box_data[MINUTES_PLAYED > minutes_threshold,]

	# extract possibly useful columns that are normalized by minutes.
	# turnovers and personal fouls were not indicative in clusters result,
	# thus we did not include them;
	# for the three pointers, two pointers and free throws include the 
	# raw attempt count and basket made instead of turning them into
	# percentages (when turned into percentages, the cluster result was sub-optimal)
	box_data[, ('Points_Per_Minute') := TOTAL_POINTS / MINUTES_PLAYED]
	box_data[, ('Assists_Per_Minute') := ASSISTS / MINUTES_PLAYED]
	box_data[, ('Rebounds_Per_Minute') := REBOUNDS / MINUTES_PLAYED]
	box_data[, ('Steals_Per_Minute') := STEALS / MINUTES_PLAYED]
	box_data[, ('Blocks_Per_Minute') := BLOCKED_SHOTS / MINUTES_PLAYED]
	box_data[, ('Two_Points_Att') := FIELD_GOALS_ATT - THREE_POINT_ATT]
	box_data[, ('Two_Points_Made') := FIELD_GOALS_MADE - THREE_POINT_MADE]
	retain_cols <- c('PLAYER', 'Blocks_Per_Minute',
					 'Points_Per_Minute', 'Assists_Per_Minute',
					 'Rebounds_Per_Minute', 'Steals_Per_Minute',
					 'FREE_THROWS_ATT', 'THREE_POINT_ATT',
					 'FREE_THROWS_MADE', 'THREE_POINT_MADE',
					 'Two_Points_Made', 'Two_Points_Att')
	box_data <- box_data[, retain_cols, with = FALSE]

	# replace NAs with 0
	for ( j in seq_len(ncol(box_data)) ) {
		set(box_data, which( is.na(box_data[[j]]) ), j, 0)
	}
	return(box_data)
}
minutes_threshold <- 10
box_data <- preprocess(box_data, minutes_threshold)


# --------------------------------------------------------------------------------------
# clustering (kmeans)

# exclude the player information when doing the clustering analysis
player <- box_data[['PLAYER']]
box_data[, PLAYER := NULL]

# set parameters for kmeans
seed <- 12345
n_start <- 10
iter_max <- 100
n_clusters <- 3:8
sse <- numeric(length(n_clusters))

# standardize the data, and use the elbow method to find the optimal number for k
box_data_scaled <- scale(box_data)
for ( k in seq_along(n_clusters) ) {
	set.seed(seed)
	centers <- n_clusters[k]
	fit <- kmeans(box_data_scaled, centers = centers, iter.max = iter_max, nstart = n_start)
	sse[k] <- sum(fit$withinss)
}
dt_sse <- data.table(k = n_clusters, sse = sse)
ggplot( dt_sse, aes(k, sse) ) +
geom_line() + theme_bw()

# we decided to use 6 clusters, not because there is a clear elbow,
# but because the interpretation made more sense
set.seed(seed)
fit <- kmeans(box_data_scaled, centers = 6, iter.max = iter_max, nstart = n_start)

# some helper functions to interpret the cluster results
summary.kmeans = function(fit) {
	p = ncol(fit$centers)
	k = nrow(fit$centers)
	n = sum(fit$size)
	sse = sum(fit$withinss)
	xbar = t(fit$centers)%*%fit$size/n
	ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2) 
	print(data.frame(
		n = c(fit$size, n),
		Pct = (round(c(fit$size, n)/n,2)),
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
		mu = as.vector(fit$centers), 
		clus = factor(rep(1:k, p)), 
		var = factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers)) 
	) 
	print(dotplot(var~mu|clus, data=plotdat, 
		panel = function(...){ 
			panel.dotplot(...) 
			panel.abline(v=0, lwd=.1) 
		}, 
		layout = c(k,1), 
		xlab = "Cluster Mean" 
	))
	invisible(plotdat) 
}

summary(fit)
plot(fit)
# cluster interpretation
# 1. facilitator, distributors
# 2. bad games
# 3. defensive players
# 4. three point shooters
# 5. power forward/centers
# 6. all stars that's carrying their team !!!!!!


# --------------------------------------------------------------------------------------
# analysis (entropy)

# 1. put the player names and cluster assignment to each row 
# 2. aggregate players to each cluster
# e.g. how many times did each player appear in each cluster
# 3. compute entropy of each player and sort them in decreasing order
box_data[, player := player]
box_data[, cluster := fit$cluster]
aggregation <- box_data[, .(counts = .N), by = .(cluster, player)]

# manually checking each cluster to see if they make intuitive sense
aggregation[cluster == 2, ][order(-counts), ]

entropy <- aggregation[, {
	p <- counts / sum(counts)
	entropy <- -sum( p * log10(p) )
	list(entropy = entropy)
}, by = player][order(-entropy), ]


# how many times a certain team has a certain cluster (proportion) over season;
# correlation between this and the team's number of wins

# only use the top players to evaluate a team's cluster proportion
aggregation_subset <- aggregation[player %in% top_players[['PLAYER']],]
top_players_aggregation <- merge(aggregation_subset, top_players, 
								 by.x = 'player', by.y = 'PLAYER')
team_cluster <- top_players_aggregation[, .( counts = sum(counts) ), by = .(cluster, TEAM)]

# ?? normalize the cluster count 
team_cluster_wide <- dcast(team_cluster, TEAM ~ cluster, value.var = 'counts')
normalizer <- rowSums(team_cluster_wide[, -'TEAM', with = FALSE])
normalized <- team_cluster_wide[, c( .(TEAM = TEAM), lapply(.SD, function(x) { 
	x / normalizer 
}) ), .SDcols = 2:ncol(team_cluster_wide)]

normalized



# nearest players for each players, sai like it a lot !!!!!!!!!!!!!!!!!!


# -----------------------------------------------------------------------------

# team starting lineups' representation in each cluster across seasons 2015
team_wins <- fread('teamwins.csv')
exclude <- c('Losses', 'W/L%')
team_wins[ , (exclude) := NULL ]
merged <- merge(team_wins, normalized, by.x = 'Team', by.y = 'TEAM')

library(caret)
exclude <- c('Team', 'Wins')
teams <- merged[['Team']]
wins <- merged[['Wins']]
merged <- merged[, -exclude, with = FALSE]
standardize <- preProcess(merged, method = c('center', 'scale'))
merged_scaled <- predict(standardize, merged)
# merged_scaled[, 6 := NULL]
# 
# merged_scaled[order(-wins), ]
merged_scaled


for ( k in seq_along(n_clusters) ) {
	set.seed(seed)
	centers <- n_clusters[k]
	fit <- kmeans(merged_scaled, centers = centers, iter.max = iter_max, nstart = n_start)
	sse[k] <- sum(fit$withinss)
}
dt_sse <- data.table(k = n_clusters, sse = sse)
ggplot( dt_sse, aes(k, sse) ) +
geom_line() + theme_bw()

# we decided to use 6 clusters, not because there is a clear elbow,
# but because the interpretation made more sense
set.seed(seed)
fit <- kmeans(merged_scaled, centers = 3, iter.max = iter_max, nstart = n_start)
summary(fit)
plot(fit)

merged_scaled[, centers := fit$cluster]
merged_scaled[, teams := teams]

merged_scaled[centers == 1, ]
merged_scaled[centers == 2, ]
merged_scaled[centers == 3, ]

# legendary + shitty; top-heavy
merged_scaled[centers == 4, ]

# shitty ....................
merged_scaled[centers == 3, ]

merged_scaled[centers == 2, ]

merged_scaled[centers == 1, ]

# ideas worth trying:
# max, mean, standard deviation, min per cluster
merged_scaled[, wins := wins]


quantile(merged_scaled[centers == 1, ]$wins)
quantile(merged_scaled[centers == 2, ]$wins)
quantile(merged_scaled[centers == 3, ]$wins)
# when doing team aggregation, instead of doing the player's play-style
# look at the mode




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
