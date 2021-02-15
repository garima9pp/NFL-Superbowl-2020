library(dplyr)
#install.packages("rlang")
setwd("E:/Bana 290/Superbowl")
#install.packages("skellam")
library(skellam)
library(readxl)
X2020_Season_Data_with_Playoffs <- read_excel("2020 Season Data with Playoffs.xlsx")
View(X2020_Season_Data_with_Playoffs)

Teams_2020_Stats <- X2020_Season_Data_with_Playoffs
head(Teams_2020_Stats)
colnames(Teams_2020_Stats)[3] ="GameNumber"
colnames(Teams_2020_Stats)[4] ="GameLocation"
head(Teams_2020_Stats)
str(Teams_2020_Stats)

Teams_2020_Stats$GameLost <- as.integer(ifelse(Teams_2020_Stats$Net_Score < 0, 1, 0))
Teams_2020_Stats$GameTie <- as.integer(ifelse(Teams_2020_Stats$Net_Score == 0, 1, 0))
Teams_2020_Stats$GameWon <- as.integer(ifelse(Teams_2020_Stats$Net_Score > 0, 1, 0))
str(Teams_2020_Stats)
Teams_2020_Stats$Home <- ifelse(Teams_2020_Stats$GameLocation=="HOME", 1, 0)
Teams_2020_Stats$Away <- ifelse(Teams_2020_Stats$GameLocation=="AWAY", 1, 0)
Teams_2020_Stats$HomeGoals <- ifelse(Teams_2020_Stats$Home==1, Teams_2020_Stats$Team_Score, 0)
Teams_2020_Stats$AwayGoals <- ifelse(Teams_2020_Stats$Away==1, Teams_2020_Stats$Team_Score, 0)
str(Teams_2020_Stats)
# Create two data frames for running Poisson regressions
set.seed(123)


library(ggplot2)
library(plotly)
library(dplyr)

# Plot 1: Time-series interactive line plot by Team

plot1 <- Teams_2020_Stats %>% filter(Team=="Tampa Bay Buccaneers" | Team=="Kansas City Chiefs")

NFL_TimeSeriesLine <- ggplot(plot1, aes(GameNumber, Team_Score, group=Team, colour=Team)) +
  geom_line() + geom_point() + ylab("Points Scored") + xlab("Game Number") +
  scale_x_continuous(breaks=seq(1,18,1)) + ggtitle("Points Scored by Team (2020 NFL Season)")
Plot1 <- ggplotly(NFL_TimeSeriesLine)
Plot1

hist(Teams_2020_Stats$Net_Score)

Teams_2020_Stats <- na.omit(Teams_2020_Stats)

Home_data <- Teams_2020_Stats %>% filter(Home==1) %>% select(Team,HomeGoals,Opponent)
Away_data <-  Teams_2020_Stats %>% filter(Away==1) %>% select(Team,AwayGoals,Opponent)

#Avg home goals
mean(Home_data$HomeGoals)
#24.8097

#Avg away goals
mean(Away_data$AwayGoals)
#24.71642



NFL_Poisson <- rbind(
  data.frame(Points=Home_data$HomeGoals,
             Team=Home_data$Team,
             Opponent=Home_data$Opponent,
             Home=1),
  data.frame(Points=Away_data$AwayGoals,
             Team=Away_data$Opponent,
             Opponent=Away_data$Team,
             Home=0)) %>%
  glm(Points ~ Home + Team + Opponent, family=poisson(link=log), data=.)
View(NFL_Poisson)
summary(NFL_Poisson)

plot(NFL_Poisson)


predict(NFL_Poisson,
        data.frame(Home=1, Team="Tampa Bay Buccaneers",
                   Opponent="Kansas City Chiefs"), type="response")
#Score 28.77544 
predict(NFL_Poisson,
        data.frame(Home=0, Team="Kansas City Chiefs",
                   Opponent="Tampa Bay Buccaneers"), type="response")

#Score 26.57493

# Create function (and prepare underlying data frames) for simulation
SuperBowl_Simulate <- function(NFL_Model, HomeTeam, AwayTeam, MaxPoints=40){
  HomePointsAvg <- predict(NFL_Model,
                           data.frame(Home=1, Team=HomeTeam,
                                      Opponent=AwayTeam), type="response")
  AwayPointsAvg <- predict(NFL_Model,
                           data.frame(Home=0, Team=AwayTeam,
                                      Opponent=HomeTeam), type="response")
  dpois(0:MaxPoints, HomePointsAvg) %o% dpois(0:MaxPoints, AwayPointsAvg) 
}

# Simulate for the teams at Super Bowl 53, listing the Home-advantage team first.

KCvsTB <- SuperBowl_Simulate(NFL_Poisson, "Kansas City Chiefs", "Tampa Bay Buccaneers", MaxPoints=40)

# Chances of a Kansas City Chiefs win
sum(KCvsTB[lower.tri(KCvsTB)])
#0.3746924
# Chances of a Tampa Bay Buccaneers win
sum(KCvsTB[upper.tri(KCvsTB)])
# 0.55032
# Chances of a Tie, though this is an impossibility for the Super Bowl
sum(diag(KCvsTB))
#0.05227325


#Ref:
#https://dashee87.github.io/data%20science/football/r/predicting-football-results-with-statistical-modelling/


























