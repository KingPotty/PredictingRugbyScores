#### SET UP ####
setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

Results <- read.csv("Results_2016-17.csv")
NextResults <- read.csv("Results_2017-18.csv")

TotalTries <- c()
for (i in 0:max(Results$HomeTries, Results$AwayTries))
{
    TotalTries <- append(TotalTries, 0)
}

TotalPenalties <- c()
for (i in 0:max(Results$HomePenalties, Results$AwayPenalties))
{
    TotalPenalties <- append(TotalPenalties, 0)
}

#### TRIES ####
for (n in 0:max(c(Results$HomeTries, Results$AwayTries)))
{
    TotalTries[n + 1] = length(which(Results$HomeTries == n)) + length(which(Results$AwayTries == n))
}

#### PENALTIES ####
for (n in 0:max(c(Results$HomePenalties, Results$AwayPenalties)))
{
    TotalPenalties[n + 1] = length(which(Results$HomePenalties == n)) + length(which(Results$AwayPenalties == n))
}

#### Calculate the mean tries and penalties for each team ####
MeanTries <- c()
MeanPenalties <- c()

for (team in 1:length(unique(Results$HomeTeam))){
    MeanTries <- append(MeanTries, 0)
    MeanTries[team] <- mean(c(Results$HomeTries[which(Results$HomeTeam == team)], Results$AwayTries[which(Results$AwayTeam == team)]))
    
    MeanPenalties <- append(MeanTries, 0)
    MeanPenalties[team] <- mean(c(Results$HomePenalties[which(Results$HomeTeam == team)], Results$AwayPenalties[which(Results$AwayTeam == team)]))
}

names(MeanTries) <- unique(Results$HomeTeam)
names(MeanPenalties) <- names(MeanTries)

#### Predict Each Result ####
for (match in 1:length(NextResults$HomeTeam)){
    HomeTeam <- NextResults$HomeTeam[match]
    AwayTeam <- NextResults$AwayTeam[match]
    if (HomeTeam %in% Results$HomeTeam && AwayTeam %in% Results$AwayTeam){
        MatchPrediction <- c(0, 0, 0)
        TriesProbGrid <- outer(dpois(0:20, MeanTries[HomeTeam]), dpois(0:20, MeanTries[AwayTeam]))
    }
}