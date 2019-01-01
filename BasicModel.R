# This model is based on the Poisson Distribution using tries and penalties

#### SET UP ####
setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

Results <- read.csv("Results_2016-17.csv")
NextResults <- read.csv("Results_2017-18.csv")

MeanTries <- c()
MeanPens <- c()
for (team in unique(Results$HomeTeam)){
    MeanTries <- append(0, MeanTries)
    MeanPens <- append(0, MeanPens)
}

names(MeanTries) <- unique(Results$HomeTeam)
names(MeanPens) <- unique(Results$HomeTeam)

for (team in unique(Results$HomeTeam)){
    MeanTries[team] <- sum(Results$HomeTries[which(Results$HomeTeam == team)]) + sum(Results$AwayTries[which(Results$AwayTeam == team)])
    MeanTries[team] <- MeanTries[team]/length(which(Results$HomeTeam == team | Results$AwayTeam == team))
    
    MeanPens[team] <- sum(Results$HomePenalties[which(Results$HomeTeam == team)]) + sum(Results$AwayPenalties[which(Results$AwayTeam == team)])
    MeanPens[team] <- MeanPens[team]/length(which(Results$HomeTeam == team | Results$AwayTeam == team))
}


AllPredictions <- c()
for (match in 1:length(NextResults$HomeTeam)){
    print(match)
    Prediction <- c(0, 0, 0)
    if (NextResults$HomeTeam[match] %in% unique(Results$HomeTeam) & NextResults$AwayTeam[match] %in% unique(Results$HomeTeam)){
        for (t1 in 0:10){
            for (t2 in 0:10){
                for (p1 in 0:10){
                    for (p2 in 0:10){
                        print(t1*1000+t2*100+p1*10+p2)
                        Prob <- dpois(t1, MeanTries[NextResults$HomeTeam[match]]) * dpois(t2, MeanTries[NextResults$AwayTeam[match]]) * dpois(p1, MeanPens[NextResults$HomeTeam[match]]) * dpois(p2, MeanPens[NextResults$AwayTeam[match]])
                        
                       
                        if (t1*5+p1*3 > t2*5+p2*3){
                            Prediction[1] <- Prediction[1] + Prob
                        } else if (t1*5+p1*3 < t2*5+p2*3){
                            Prediction[3] <- Prediction[3] + Prob
                        } else {
                            Prediction[2] <- Prediction[2] + Prob
                        }
                    }
                }
            }
        }
        AllPredictions <- append(Prediction, AllPredictions)
    } else {
        AllPredictions <- append(c(NaN, NaN, NaN), AllPredictions)
    }
}


HomeWinProb <- c()
DrawProb <- c()
AwayWinProb <- c()

for (p in 1:(length(AllPredictions)/3)){
    HomeWinProb <- append(AllPredictions[3*p-2][1], HomeWinProb)
    DrawProb <- append(AllPredictions[3*p-1], DrawProb)
    AwayWinProb <- append(AllPredictions[3*p], AwayWinProb)
}

NextResults$HomeWinProb <- HomeWinProb
NextResults$DrawProb <- DrawProb
NextResults$AwayWinProb <- AwayWinProb

#### Evaluate Method ####

Evaluation = 1
for (match in 1:length(NextResults$HomeTeam)){
    if (NextResults$HomeTeam[match] %in% unique(Results$HomeTeam) & NextResults$AwayTeam[match] %in% unique(Results$HomeTeam)){
        if (NextResults$HomeScore[match] > NextResults$AwayScore[match]){
            Evaluation <- Evaluation * NextResults$HomeWinProb[match]
        } else if (NextResults$HomeScore[match] < NextResults$AwayScore[match]){
            Evaluation <- Evaluation * NextResults$AwayWinProb[match]
        } else {
            Evaluation <- Evaluation * NextResults$DrawProb[match]
        }
    }
}

print(Evaluation)