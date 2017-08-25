library(DT)
library(lsa)
library(caret)
library(shiny)
library(stringr)
library(data.table)
library(shinythemes)
library(shinydashboard)
setwd('/Users/ethen/sports/presentation')

# define global parameters
FILE_PATH <- 'box2015.csv'
POSITION_PATH <- 'positions.csv'

# include only player who has above 
# a certain number of minutes per game
MINUTES_THRESHOLD <- 10

# parameters for kmeans clustering
SEED <- 12345
N_START <- 10
ITER_MAX <- 100

position <- fread(POSITION_PATH)
position <- unique(position)
setnames(position, c('player', 'position'))

# --------------------------------------------------------------------------------------
# preprocessing
preprocess <- function(box_data, minutes_threshold) {
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
box_data <- fread(FILE_PATH)
box_data <- preprocess(box_data, MINUTES_THRESHOLD)


# clustering kmeans
# exclude the player information and standardize the data
# prior to performing cluster
player <- box_data[['Player']]
box_data[, Player := NULL]
standardize <- preProcess(box_data, method = c('center', 'scale'))
box_data_scaled <- predict(standardize, box_data)

# we decided to use 6 clusters, not because there is a clear elbow,
# but because the interpretation made more sense
set.seed(SEED)
fit <- kmeans(box_data_scaled, centers = 6, iter.max = ITER_MAX, nstart = N_START)
# cluster interpretation
# 1. facilitator, distributors
# 2. bad games
# 3. defensive players
# 4. three point shooters
# 5. power forward/centers
# 6. all stars that's carrying their team !!!!!!


# --------------------------------------------------------------------------------------
# obtain players that have similar play styles (not performance)

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
    # given the pairwise distance and the index of a player
    # compute the similar play style score

    # convert from cosine distance to angle 
    # (acos gives the radian .........................)
    cosine_distance <- acos(distance[query, ]) * 180 / pi

    # convert the cosine angle (0 ~ 180) to a single normalized score
    # ranging from 0 to 100
    # and cut the decimal places to prevent cluttering the page
    score <- ( (180 - cosine_distance) / 180 ) * 100
    score <- round(score, 1)
    dt_score <- data.table(score = score, player = player)
    return(dt_score)
}
# example
# query <- which(player == 'Stephen Curry')
# result <- compute_score(distance, query)


# --------------------------------------------------------------------------------------
# dashboard

body <- dashboardBody(
    # changing dashboard background color to white
    # https://github.com/rstudio/shinydashboard/issues/31
    includeCSS('www/custom.css'),
    tabItems(
        tabItem(tabName = 'DashBoard',
            fluidPage(
                titlePanel('Similarity-O-Meter'),
                sidebarLayout(
                    sidebarPanel(
                        helpText('Find Players with Similar Playstyle'),
                        hr(),
                        selectInput(inputId = 'choose_player',
                                    label = 'Choose Player:',
                                    choices = player),
                        br(),
                        actionButton(inputId = 'start_analysis',
                                     label = 'Start Analysis',
                                     style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
                    ),                   
                    mainPanel(
                        dataTableOutput(outputId = 'output_table')
                    )
                )
            )
        )
    )
)


ui <- dashboardPage(
    dashboardHeader(title = 'Sports Analytics'),
    dashboardSidebar(
        sidebarMenu(
            menuItem('DashBoard', tabName = 'DashBoard', icon = icon('dashboard'))
        )
    ),
    body
)


server <- function(input, output) {
    output$output_table <- renderDataTable({
        # the output should only be dependent on 
        # the action button
        input$start_analysis

        # compute the most similar players, include their
        # positions sorted by the normalized score (scale 0 ~ 100)
        # and remove itself from the result
        query <- which(player == isolate(input$choose_player))
        result <- compute_score(distance, query)
        result <- merge(result, position)[order(-score),]
        result <- result[2:nrow(result), ]
        return(result)

    # options: display 10 rows per page
    }, options = list(pageLength = 10))
}

shinyApp(ui, server)

