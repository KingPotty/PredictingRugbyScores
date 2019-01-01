#### SET UP ####
setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

Results1516 <- read.csv("Results_2015-16.csv")
Results1617 <- read.csv("Results_2016-17.csv")
Results1718 <- read.csv("Results_2017-18.csv")

Conversions <- c(Results1617$HomeConversions, Results1617$AwayConversions)
Tries <- c(Results1617$HomeTries, Results1617$AwayTries)

plot(Conversions, Tries)

print(mean(Conversions/Tries, na.rm = TRUE))
print(var(Conversions/Tries, na.rm = TRUE))

print(cor.test(Conversions, Tries))