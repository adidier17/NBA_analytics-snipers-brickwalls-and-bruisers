#install.packages("lsa")
library(lsa)
library(caret)
library(ggplot2)
library(data.table)
setwd('/Users/ethen/sports/presentation')
box_data <- fread('box2015.csv')

# --------------------------------------------------------------------------------------
# preprocessing
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

	# change names to be snake case instead of the all caps
	old_names <- c('PLAYER', 'FREE_THROWS_ATT', 'THREE_POINT_ATT', 
				   'FREE_THROWS_MADE', 'THREE_POINT_MADE')
	new_names <- c('Player', 'Free_Throw_Att', 'Three_Point_Att', 
				   'Free_Throw_Made', 'Three_Point_Made')
	setnames(box_data, old_names, new_names)
	
	# all the features that will be in the dataset
	retain_cols <- c('Blocks_Per_Minute',
					 'Points_Per_Minute', 'Assists_Per_Minute',
					 'Rebounds_Per_Minute', 'Steals_Per_Minute',
					 'Two_Points_Made', 'Two_Points_Att', new_names)
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

# exclude the player information and standardize the data
# prior to performing cluster
player <- box_data[['Player']]
box_data[, Player := NULL]
standardize <- preProcess(box_data, method = c('center', 'scale'))
box_data_scaled <- predict(standardize, box_data)

# set parameters for kmeans
seed <- 12345
n_start <- 10
iter_max <- 100

# use the elbow method to find the optimal number for k
# n_clusters <- 3:8
# sse <- numeric(length(n_clusters))
# for ( k in seq_along(n_clusters) ) {
# 	set.seed(seed)
# 	centers <- n_clusters[k]
# 	fit <- kmeans(box_data_scaled, centers = centers, 
# 				  iter.max = iter_max, nstart = n_start)
# 	sse[k] <- sum(fit$withinss)
# }
# dt_sse <- data.table(k = n_clusters, sse = sse)
# ggplot( dt_sse, aes(k, sse) ) +
# geom_line() + theme_bw()

# we decided to use 6 clusters, not because there is a clear elbow,
# but because the interpretation made more sense
set.seed(seed)
fit <- kmeans(box_data_scaled, centers = 6, iter.max = iter_max, nstart = n_start)

# some helper functions to interpret the cluster results
# summary.kmeans = function(fit) {
# 	p = ncol(fit$centers)
# 	k = nrow(fit$centers)
# 	n = sum(fit$size)
# 	sse = sum(fit$withinss)
# 	xbar = t(fit$centers)%*%fit$size/n
# 	ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2) 
# 	print(data.frame(
# 		n = c(fit$size, n),
# 		Pct = (round(c(fit$size, n)/n,2)),
# 		round(rbind(fit$centers, t(xbar)), 2),
# 		RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
# 	))
# 	cat("SSE = ", sse, "; SSB = ", ssb, "\n")
# 	cat("R-Squared = ", ssb/(ssb+sse), "\n")
# 	cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
# 	invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
# }
# summary(fit)


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

# put the player names and cluster assignment to each row 
# aggregate players to each cluster
# i.e. how many times did each player appear in each cluster
box_data_scaled[, player := player]
box_data_scaled[, cluster := fit$cluster]
aggregation <- box_data_scaled[, .(counts = .N), by = .(cluster, player)]

# convert to wide format
player_cluster <- dcast(aggregation, player ~ cluster, 
						value.var = 'counts', fill = 0)
player <- player_cluster[['player']]
player_cluster[, player := NULL]

# normalized & standardize
normalized <- as.matrix(player_cluster) / rowSums(player_cluster)
standardize <- preProcess(normalized, method = c('center', 'scale'))
normalized_scaled <- predict(standardize, normalized)

# compute pairwise cosine distance
distance <- lsa::cosine(t(normalized_scaled))

compute_score <- function(distance, query) {
	# convert the cosine distance to a single score

	# convert from cosine distance to angle 
	# (acos gives the radian .........................)
	cosine_distance <- acos(distance[query, ]) * 180 / pi

	# normalize the score from the range of 0 ~ 180 to 100
	score <- ( (180 - cosine_distance) / 180 ) * 100
	dt_score <- data.table(score = score, player = player)
	dt_score <- dt_score[order(-score),]
	return(dt_score)
}

# make a comparison to similar players
query <- which(player == 'Stephen Curry')
compute_score(distance, query)

query <- which(player == 'DeAndre Jordan')
compute_score(distance, query)

query <- which(player == 'LeBron James')
compute_score(distance, query)

query <- which(player == 'Kevin Durant')
compute_score(distance, query)

query <- which(player == 'Dirk Nowitzki')
compute_score(distance, query)


# --------------------------------------------------------------------------------------
# get the top 7 players (sorted by minutes played) from each team
box_data <- fread('box2015.csv')
starters <- box_data[, .(minutes = sum(MINUTES_PLAYED)), by = .(TEAM, PLAYER) 
				   ][order(TEAM, -minutes),]
top_players <- starters[, head(.SD, 7), by = TEAM]


# manually checking each cluster to see if they make intuitive sense
# aggregation[cluster == 2, ][order(-counts), ]

# compute entropy of each player and sort them in decreasing order
# to look at the play-style variety
# entropy <- aggregation[, {
# 	p <- counts / sum(counts)
# 	entropy <- -sum( p * log10(p) )
# 	list(entropy = entropy)
# }, by = player][order(-entropy), ]


# how many times a certain team has a certain cluster (proportion) over season;
# correlation between this and the team's number of wins

# only use the top players to evaluate a team's cluster proportion
# put the player names and cluster assignment to each row 
# aggregate players to each cluster
# i.e. how many times did each player appear in each cluster
box_data_scaled[, player := player]
box_data_scaled[, cluster := fit$cluster]
aggregation <- box_data_scaled[, .(counts = .N), by = .(cluster, player)]
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

#-----------------------Arindam and Annie's function!!!!!!!!
team_div = function(distance, players7){
  #similarity = lsa::cosine(t(normalized_scaled))
  #angles = acos(similarity)*180/pi
  #angles
  indices = which(player %in% players7)
  angle = acos(distance[indices, indices]) * 180 / pi
  below90 = 0
  belowcount = 0
  above90 = 0
  abovecount = 0
  for(i in 2:nrow(angle)){
    for(j in 1:(i-1)){
      if(angle[i,j] < 70){
        below90 = below90 + angle[i,j]
        belowcount = belowcount + 1  
      }
      if(angle[i,j] >= 70){
        above90 = above90 + angle[i,j]
        abovecount = abovecount + 1
      }
    }
  }
  print(angle)
  below90 = below90/belowcount
  above90 = above90/abovecount
  
  stats = data.frame(b_angle = below90, b_prop = belowcount/21, a_angle = above90, a_prop = abovecount/21)
  return(stats)
}

ATL = top_players[1:7,][['PLAYER']]
ATL_div = team_div(distance, ATL)
ATL_div

teams = unique(top_players[["TEAM"]])
player_sim = data.frame()
for(team in teams){
  pNames = top_players[TEAM == team][['PLAYER']]
  temp = team_div(distance, pNames)
  player_sim = rbind(player_sim, temp[,1:2])
}

row.names(player_sim) = teams

player_sim$Team <- rownames(player_sim)


library(ggrepel)
player_sim <- data.table(player_sim)
team_wins <- fread('teamwins.csv')
exclude <- c('Losses', 'W/L%')
team_wins[, (exclude) := NULL]
merged <- merge(team_wins, player_sim)[order(-Wins)]
labels <- c( rep('top', 10), rep('middle', 10), rep('bottom', 10) )
merged[, labels := as.factor(labels)]

values <- c("#FF3333", "yellow", "#00FF00")
plot <- ggplot(merged, aes(b_angle, b_prop, fill = labels, label = Team)) +
		geom_point() + theme_bw() + 
		scale_fill_manual(values = values) +
		geom_label_repel() +
		labs(x = 'Degree of Dissimilarity', 
			 y = 'Proportion of Players', 
			 title = 'Team Diversity')
ggsave('plot.png', plot, width = 30, height = 20, units = 'cm')

# --------------------------------------------------------------------------------------
# players recommendation
# nearest players for each players, sai like it a lot !!!!!!!!!!!!!!!!!!

# aggregate player performance, note that aggregating by
# mean or median gives different result
box_data_scaled[, player := player]
aggregated_performance <- box_data_scaled[, lapply(.SD, median), by = player]

player <- aggregated_performance[['player']]
aggregated_performance[, player := NULL]
X <- as.matrix(aggregated_performance)
X_normed <- X / rowSums(X)

library(FNN)
knn_result <- FNN::get.knn(X_normed, k = 10)

query <- which(player == 'Stephen Curry')
neighbors_index <- knn_result$nn.index[query,]
player[neighbors_index]

query <- which(player == 'Klay Thompson')
neighbors_index <- knn_result$nn.index[query,]
player[neighbors_index]


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
