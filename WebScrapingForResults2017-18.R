setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scrapped
url <- 'http://en.espn.co.uk/scrum/rugby/series/296394.html?template=results'

#Reading the HTML code from the website
webpage <- read_html(url)

#Get only the sections with the scores in
data_html <- html_nodes(webpage, '.fixtureTablePreview')

Results <- html_text(data_html)

Results <- gsub("\u00A0", "~", Results, fixed = TRUE)

for (i in 1:length(Results))
{
  Results[i] <- gsub('  ', '~', Results[i])
  Results[i] <- strsplit(as.character(Results[i]), '~')
}

remove <- "Table"
Results <- setdiff(Results, remove)

Results <- strsplit(Results, '~')

HomeTeam <- c()
AwayTeam <- c()
HomeScore <- c()
AwayScore <- c()



for (i in 1:length(Results))
{
  HomeTeam <- append(HomeTeam, unlist(Results[i])[1])
  AwayTeam <- append(AwayTeam, unlist(Results[i])[3])
  Scores <- unlist(strsplit(unlist(Results[i])[2], " - "))
  HomeScore <- append(HomeScore, Scores[1])
  AwayScore <- append(AwayScore, Scores[2])
}

ResultDF <- data.frame(HomeTeam, AwayTeam, HomeScore, AwayScore)

write.csv(ResultDF, "Results_2017-18.csv", row.names = FALSE)