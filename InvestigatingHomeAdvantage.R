#### SET UP ####
setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

Results1516 <- read.csv("Results_2015-16.csv")
Results1617 <- read.csv("Results_2016-17.csv")
Results1718 <- read.csv("Results_2017-18.csv")

##### OVERALL IMPACT OF HOME ADVANTAGE #####
a <- length(which(Results1516$HomeScore > Results1516$AwayScore))/length(Results1516$HomeScore)
b <- length(which(Results1617$HomeScore > Results1617$AwayScore))/length(Results1617$HomeScore)
c <- length(which(Results1718$HomeScore > Results1718$AwayScore))/length(Results1718$HomeScore)

##### IMPACT OF HOME ADVANTAGE PER TEAM #####
HomeAdv <- c()
TotalWins <- c()

for (team in length(unique(Results1516$HomeTeam))){
  HomeAdv <- append(HomeAdv, length(which(Results1516$HomeTeam == team & Results1516$HomeScore > Results1516$AwayScore)))
  
  
  HomeAdv[team] <- HomeAdv[team] / (HomeAdv[team] + length(which(Results1718$AwayTeam == team & Results1718$AwayScore > Results1718$HomeScore)) + length(which(Results1617$AwayTeam == team & Results1617$AwayScore > Results1617$HomeScore)) + length(which(Results1516$AwayTeam == team & Results1516$AwayScore > Results1516$HomeScore)))
}

print(HomeAdv)

TotalWins <- c()
for (team in names(HomeAdv)){
    TotalWins <- append(TotalWins, length(which(Results1718$HomeTeam == team & Results1718$HomeScore > Results1718$AwayScore)) + length(which(Results1718$AwayTeam == team & Results1718$AwayScore > Results1718$HomeScore)))
}

plot(TotalWins, HomeAdv)

# Work out how much correlation

# PEARSON'S PRODUCT MOMENT CORRELATION COEFFICIENT (pg 81 in stats textbook)
Sxy <- sum((TotalWins - mean(TotalWins)) * (HomeAdv - mean(HomeAdv)))
Sxx <- sum((TotalWins - mean(TotalWins))^2)
Syy <- sum((HomeAdv - mean(HomeAdv))^2)

Correlation <- Sxy/sqrt(Sxx*Syy)

print(Correlation)