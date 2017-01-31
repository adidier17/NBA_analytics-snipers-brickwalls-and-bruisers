# problem 3
box_data <- fread('box2015.csv')
box_data <- box_data[ MINUTES_PLAYED > 0, ]
box_data[ , ('Points_Per_Minute') := TOTAL_POINTS / MINUTES_PLAYED ]
box_data[ , ('Assists_Per_Minute') := ASSISTS / MINUTES_PLAYED ]
box_data[ , ('Rebounds_Per_Minute') := REBOUNDS / MINUTES_PLAYED ]
box_data[ , ('Steals_Per_Minute') := STEALS / MINUTES_PLAYED ]
box_data[ , ('Turnovers_Per_Minute') := TURNOVERS / MINUTES_PLAYED ]
box_data[ , ('Personal_Fouls_Per_Minute') := PERSONAL_FOULS / MINUTES_PLAYED ]
box_data[ , ('Disqualifications_Per_Minute') := DISQUALIFICATIONS / MINUTES_PLAYED ]
box_data[ , ('Blocks_Per_Minute') := BLOCKED_SHOTS / MINUTES_PLAYED ]
box_data[ , ('Field_Goals_Percentage') := FIELD_GOALS_MADE / FIELD_GOALS_ATT ]
box_data[ , ('Free_Throws_Percentage') := FREE_THROWS_MADE / FREE_THROWS_ATT ]
box_data[ , ('Three_Point_Percentage') := THREE_POINT_MADE / THREE_POINT_ATT ]
retain_cols <- c('Points_Per_Minute', 'Assists_Per_Minute', 'Rebounds_Per_Minute',
				 'Steals_Per_Minute', 'Turnovers_Per_Minute', 'Personal_Fouls_Per_Minute',
				 'Blocks_Per_Minute', 'Disqualifications_Per_Minute',
				 'Field_Goals_Percentage', 'Free_Throws_Percentage', 'Three_Point_Percentage')
box_data <- box_data[ , retain_cols, with = FALSE ]

# replace NAs with 0
for( j in seq_len(ncol(box_data)) ) {
	set(box_data, which( is.na(box_data[[j]]) ), j, 0)
}

pca_box_data <- prcomp(box_data)
pca_box_data$sdev
screeplot(pca_box_data)

fit_box_data <- psych::principal(box_data, nfactor = 6, rotate = 'none')
fit_box_data$values

fit_box_data <- psych::principal(box_data, nfactor = 5, rotate = 'varimax')
fit_box_data$loadings
# the first rotation component is measuring a player's offensive ability/prowess,
# the second measures a player's defensive ability, there the three point
# percentage has a strong negative score in three point percentage
# the third one measures a player's hot-headedness
# the fourth one measures a player's point-guardness, the most interesting
# part is the fact that assists and steals falls in the same category
# the fifth one measure's a player's turnovers


teams <- read.csv('teams.csv')
teams$rank <- 1:30

box<-read.csv("box2015.csv")
box1<-box[which(box$GAME_PLAYED==1),]
box1<-box[which(box$MINUTES_PLAYED!=0),]
teamstats <- aggregate(. ~ TEAM, box1, FUN = mean)

remove_cols <- c('GAME_DATE', 'GAME_CODE', 'PLAYER')


