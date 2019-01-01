#### SET UP ####
setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

Results1516 <- read.csv("Results_2015-16.csv")
Results1617 <- read.csv("Results_2016-17.csv")
Results1718 <- read.csv("Results_2017-18.csv")

##### IMPACT OF HOME ADVANTAGE PER TEAM #####
HomeAdv <- c()
TotalWins <- c()

for (team in unique(Results1516$HomeTeam)){
    HomeAdv <- append(HomeAdv, length(which(Results1516$HomeTeam == team & Results1516$HomeScore > Results1516$AwayScore)))
    HomeAdv[length(HomeAdv)] <- HomeAdv[length(HomeAdv)] / (HomeAdv[length(HomeAdv)] + length(which(Results1516$AwayTeam == team & Results1516$AwayScore > Results1516$HomeScore)))
    TotalWins <- append(TotalWins, length(which(Results1516$HomeTeam == team & Results1516$HomeScore > Results1516$AwayScore)) + length(which(Results1516$AwayTeam == team & Results1516$AwayScore > Results1516$HomeScore)))
}

for (team in unique(Results1617$HomeTeam)){
    HomeAdv <- append(HomeAdv, length(which(Results1617$HomeTeam == team & Results1617$HomeScore > Results1617$AwayScore)))
    HomeAdv[length(HomeAdv)] <- HomeAdv[length(HomeAdv)] / (HomeAdv[length(HomeAdv)] + length(which(Results1617$AwayTeam == team & Results1617$AwayScore > Results1617$HomeScore)))
    TotalWins <- append(TotalWins, length(which(Results1617$HomeTeam == team & Results1617$HomeScore > Results1617$AwayScore)) + length(which(Results1617$AwayTeam == team & Results1617$AwayScore > Results1617$HomeScore)))
}

for (team in unique(Results1718$HomeTeam)){
    HomeAdv <- append(HomeAdv, length(which(Results1718$HomeTeam == team & Results1718$HomeScore > Results1718$AwayScore)))
    HomeAdv[length(HomeAdv)] <- HomeAdv[length(HomeAdv)] / (HomeAdv[length(HomeAdv)] + length(which(Results1718$AwayTeam == team & Results1718$AwayScore > Results1718$HomeScore)))
    TotalWins <- append(TotalWins, length(which(Results1718$HomeTeam == team & Results1718$HomeScore > Results1718$AwayScore)) + length(which(Results1718$AwayTeam == team & Results1718$AwayScore > Results1718$HomeScore)))
}

plot(TotalWins, HomeAdv)

# Work out how much correlation (pmmc pg 81 in stats textbook)

Correlation <- cor(TotalWins, HomeAdv)

print(Correlation)