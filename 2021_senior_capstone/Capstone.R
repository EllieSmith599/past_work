library(readr); library(readxl);library(ggplot2); library(dplyr); library(corrplot);
library(reshape); library(reshape2); library(rgdal); library(leaflet); library(RColorBrewer)
library(plyr); library(Dict)

##Data Files
athlete2016 <- read_csv("https://storage.googleapis.com/kagglesdsdata/datasets/583/1212/athletes.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210303%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210303T154402Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=5d65bc4e18aa8014113a6b4fe895a02edd1003d63f46b84cbde183f8fc19d91e581eb50f523f7a4e6888b844e257aa5969309b8a5fb5a96f64e2bcc3d09ab2e0715e2b34f11c1459e1b0d1203ddffd48fb0c55269c684befb17156c08ae0deb34fb7deb6e02597e8f4e1fdf46e6b3839ac630845fadb96e888b9ef769038de398e4f766823cc7e44b1f9d4b6ee7428faf6431f8382f77b665192a9fb3d00298cbaf88d0079eb49fade458a490335c022771adae7f4406469e18627dc61ed32076de82426e6eb7da0476c4b2fc14647192a02b6f560b3a65d60542db13091a49b3e2fcff9e442ca91ff61fe738586a5db74a484c9471258e5bb5f24980af22929")
countries <- read_csv("https://storage.googleapis.com/kagglesdsdata/datasets/583/1212/countries.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210303%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210303T154507Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=018a22d861148268f6a1685d96e8eb911f6c4385757baa1e36319d16c6557ad7e9e22ab329d25acc40eb6467cbc2f9b1c200e99d05ece16cb825e37a24b604d7ed2128fc6b47e27e50eb595df5e42c54fa42b17baa0af3b45d049f932db119c5c0bcdd24ccdc617b4f3864d96bad4626329426b08f1b282102ea5509a66937a5c3a7a6c2abc2d623922116c1d80f1f95071bc2c70b3148fa48b756660414a5c2fac80f02f475a3ac139fc1e6d2c134e87bdca168bca9d61280279fbd68991c6043d14dee6b90c95e69fbf8f8978c0c85ebeb80748765c6afb20690b8ed21d91ca7a360cf1768315a45fd87f770f4bb721be22f892b92a4ad5d64f8abf6105e27")
events2016 <- read_csv("https://storage.googleapis.com/kagglesdsdata/datasets/583/1212/events.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210303%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210303T154552Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=4b0ee4316fcd4824adc49c51a603be042f23c86878e8212fad3166123bf1940f56cb8fa7feb22080aa98e139e8bc48009881bc9c30e33c5aa6e5a1346ab711da2dec7addef1dff07eaa58520f3768101d9948ef8ba60546140122a3b6b3088a05a4d613a424f3c0fa968e2273f4d3a57e9981f5a17ae91352b2a34c4415b4a6594f604b812b869eb11b816d23d6bf0f41f7f95f3dfa6c7bd5ea4a88caeb39c0873e5c4d9825e86d2d84c24803c45ed6f24b47c3a542acb9457b0db61fa72a84131965b54becde43a3e4825da1ff145ff7ae069c35ba81a1a11d7fefc5e6c19d88921c8bd2554346e7791332405ef9536fa914eacba7f376f53e3aa7447322f5d")

athletesAll <- read.csv("~/Downloads/athlete_events.csv")
athletesAll <- subset(athletesAll, Season == "Summer")

rankedCountries <- data.frame(read_excel("Downloads/RankedCountries.xlsx")) #https://wfdf.sport/world-rankings/ultimate-rankings/
gender2019 <- data.frame(read_excel("Downloads/2019Frisbee.xlsx", sheet = "GenderBalance"))
playerByDivision <- data.frame(read_excel("Downloads/2019Frisbee.xlsx", sheet = "PlayersByDivision"))
teamsPerCountry <- data.frame(read_excel("Downloads/2019Frisbee.xlsx", sheet = "TeamsPerCountry"))
nonPlayerTurn <- data.frame(read_excel("Downloads/2019Frisbee.xlsx", sheet = "NonPlayerTurnout"))
popularity <- data.frame(read_excel("Downloads/Popularity.xlsx")) #https://www.statista.com/statistics/237289/least-favorite-olympic-events-among-americans-in-2012/
leastPopular <- data.frame(read_excel("Downloads/LeastPopular.xlsx")) #https://fivethirtyeight.com/features/best-worst-olympic-sports-survey-ranked/
videoData2020 <- data.frame(read_excel("Downloads/YouTubeVideoData.xlsx", sheet = "y2020"))
videoData2015 <- data.frame(read_excel("Downloads/YouTubeVideoData.xlsx", sheet = "y2015"))
videoData2010 <- data.frame(read_excel("Downloads/YouTubeVideoData.xlsx", sheet = "y2010"))
videoYears <- data.frame(read_excel("Downloads/YouTubeVideoData.xlsx", sheet = "multYears"))
names(videoData2020)[names(videoData2020) == "VideoId"] <- "videoID"
names(teamsPerCountry)[names(teamsPerCountry) == "County"] <- "Country"
names(teamsPerCountry)[names(teamsPerCountry) == "Number.of.Teams"] <- "NumTeams"
names(teamsPerCountry)[names(teamsPerCountry) == "Number.of.Players"] <- "NumPlayers"

allVideoData <- rbind(videoData2020, videoData2015, videoData2010)

#########################################
#### Modifying and Cleaning the Data ####
#########################################

rankedCountries$New[(rankedCountries$Change == "New")] <- 1
rankedCountries$New[(rankedCountries$Rank == "NR")] <- 1
rankedCountries$New[(rankedCountries$Rank != "NR")] <- 0

teamsPerCountry <- teamsPerCountry[-55, ]
teamsPerCountry$Country[teamsPerCountry$Country == "USA"] <- "United States of America"
ranked2019 <- subset(rankedCountries, Year == 2019)
teamsPerCountry <- right_join(teamsPerCountry, ranked2019, by = "Country")
teamsPerCountry <- teamsPerCountry[, -6]
teamsPerCountry[is.na(teamsPerCountry)] <- 0
teamsPerCountry$Rank[teamsPerCountry$Rank == "NR"] <- 0
teamsPerCountry$Rank <- as.integer(teamsPerCountry$Rank)
#teamsPerCountry$Change <- as.integer(teamsPerCountry$Change)


##########################
#### Regression Model ####
##########################

test <- teamsPerCountry[1:42, -c(1, 4, 7)]
corr <- cor(teamsPerCountry[, -c(1, 4, 7)])
corrplot(corr, method = "number")
pairs(test, panel = panel.smooth)

nPeopleFull.0 <- lm(NumPlayers ~ ., teamsPerCountry[, -c(1, 7)])
summary(nPeopleFull.0)
numPeople.0 <- step(nPeopleFull.0, direction = "backward", trace = 0)
summary(numPeople.0)
plot(numPeople.0)[2]

nPeopleFull <- lm(NumPlayers ~ ., teamsPerCountry[1:42, -c(1, 7)])
summary(nPeopleFull)
numPeople <- step(nPeopleFull, direction = "backward", trace = 0)
summary(numPeople)
plot(numPeople)

plot(test$NumPlayers, (test$NumTeams+ test$New))

abline(numPeople)

nPeople.Pre <- data.frame(predict(numPeaple, data = teamsPerCountry, interval = "confidence"))
shapiro.test(nPeople.Pre$fit) ##Not normal 

#####################################
#### Predicting Future Countries ####
#####################################

numYear <- c(nrow(subset(rankedCountries, Year == 2020)),
nrow(subset(rankedCountries, Year == 2019)),
nrow(subset(rankedCountries, Year == 2018)),
nrow(subset(rankedCountries, Year == 2017)),
nrow(subset(rankedCountries, Year == 2016)))


df<- data.frame(Year = c(2020, 2019, 2018, 2017, 2016), 
                numTeams = numYear)
reg <- lm(numTeams ~ Year, data = df)
summary(reg)

plot(df$Year, df$numTeams, main = "Number of Countries by Year",
     xlab = "Year", ylab = "Total Countries") + abline(reg) #line is: -7182.4000 + 3.6*Year

Year2024 <- c(2024, round(-7182.4000 + 3.6*(2024), 0))
Year2028 <- c(2028, round(-7182.4000 + 3.6*(2028), 0))
Year2032 <- c(2032, round(-7182.4000 + 3.6*(2032), 0))

df <- rbind(df, Year2024, Year2028, Year2032)

plot(df$Year, df$numTeams, main = "Number of Future Countries by Year",
     xlab = "Year", ylab = "Total Countries") + abline(reg) #line is: -7182.4000 + 3.6*Year

################################
#### Categorical Statistics ####
################################

gen.m <- melt(gender2019, id.vars = "Team")
gen.m <- gen.m[1:27, ]

ggplot(gen.m, aes(Team, value)) + xlab("Team") + ylab("Number of Players") +
  geom_bar(aes(fill = variable), stat = "identity", position = "dodge") + labs(fill = "Gender") +
  ggtitle("Gender of Players in Leagues")


teams.m <- melt(teamsPerCountry[, c(1:2)], id.vars = "Country")
teams.m <- teams.m[order(teams.m$value, decreasing = TRUE), ]

ggplot(teams.m[1:10, ], aes(Country, value)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Teams") +
  ggtitle("Number of Teams by Country (Top 10)")

ggplot(teams.m[11:20, ], aes(Country, value)) +
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Teams") +
  ggtitle("Number of Teams by Country (Top 20)")

players.m <- melt(teamsPerCountry[, c(1,3)], id.vars = "Country")
players.m <- players.m[order(players.m$value, decreasing = TRUE),]

ggplot(players.m[1:10, ], aes(Country, value)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players (109-725)") + 
  ggtitle("Number of Players by Country (Top 10)")

ggplot(players.m[11:20, ], aes(Country, value)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players (73-108)") + 
  ggtitle("Number of Players by Country (Top 20)")

#####################
#### Viewer Data ####
#####################

#### Creating statistics and graphs 
mean(videoData2020$viewCount)
mean(videoData2015$viewCount) #In 2018 there was the world games for frisbee that were broadcasted through Youtube, Data will be skewed
mean(videoData2010$viewCount)

median(videoData2020$viewCount)
median(videoData2015$viewCount)
median(videoData2010$viewCount)

boxplot(viewCount ~ Year, data = allVideoData, ylab = "View Count", main = "View Count by Year")
boxplot(viewCount ~ Year, data = allVideoData, ylab = "View Count", main = "View Count by Year", outline = FALSE)

mean(videoYears$viewCount[videoYears$Year == "2020"])
mean(videoYears$viewCount[videoYears$Year == "2018"])
mean(videoYears$viewCount[videoYears$Year == "2016"])
mean(videoYears$viewCount[videoYears$Year == "2014"])
mean(videoYears$viewCount[videoYears$Year == "2012"])

max(videoYears$viewCount[videoYears$Year == "2020"])
max(videoYears$viewCount[videoYears$Year == "2018"])
max(videoYears$viewCount[videoYears$Year == "2016"])
max(videoYears$viewCount[videoYears$Year == "2014"])
max(videoYears$viewCount[videoYears$Year == "2012"])

median(videoYears$viewCount[videoYears$Year == "2020"])
median(videoYears$viewCount[videoYears$Year == "2018"])
median(videoYears$viewCount[videoYears$Year == "2016"])
median(videoYears$viewCount[videoYears$Year == "2014"])
median(videoYears$viewCount[videoYears$Year == "2012"])

mean(videoYears$viewCount[videoYears$Year == "2016" & videoYears$viewCount != 30225886])

boxplot(viewCount ~ Year, data = videoYears, ylab = "View Count", main = "View Count by Year")
boxplot(viewCount ~ Year, data = videoYears, ylab = "View Count", main = "View Count by Year", outline = FALSE)

nrow(subset(videoYears, Year == 2020))
nrow(subset(videoYears, Year == 2018))
nrow(subset(videoYears, Year == 2016))
nrow(subset(videoYears, Year == 2014))
nrow(subset(videoYears, Year == 2012))

######################
#### Animated Map ####
######################

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
system("unzip world_shape_file.zip")

world_spdf <- readOGR( 
  dsn= getwd() , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

country.data <- rankedCountries[, c(1, 3, 7)]

#country.data$order[country.data$Year == 2016] <- 1
#country.data$order[country.data$Year == 2017] <- 2
#country.data$order[country.data$Year == 2018] <- 3
#country.data$order[country.data$Year == 2019] <- 4
#country.data$order[country.data$Year == 2020] <- 5

country.data$fill[country.data$New == 0] <- "A"
country.data$fill[country.data$New == 1] <- "B"

####################
### Olympic Data ###
####################

equestrian <- subset(athletesAll, Sport == "Equestrianism")
equestrian <- equestrian[order(equestrian$Year),]
View(equestrian)

length(unique(athletesAll$Sport))
unique(athletesAll$Sport[athletesAll$Year == 2016])
unique(athletesAll$Sport[athletesAll$Year == 2012])
unique(athletesAll$Sport[athletesAll$Year == 2008])
unique(athletesAll$Sport[athletesAll$Year == 2004])
unique(athletesAll$Sport[athletesAll$Year == 2000])

athletesAllSub <- subset(athletesAll, Year >= 2000)
sports.m <- melt(athletesAllSub[, c(7, 10, 13)])

unique(unique(athletesAllSub$Sport))
length(unique(unique(athletesAllSub$Sport)))

OlyYears <- c(2016, 2012, 2008, 2004, 2000, 1996, 1992, 1988, 1984, 1980)

freqAthlete <- table(athletesAllSub[, c(10, 13)])
data.frame(freqAthlete) ## Badminton, baseball (?), boxing, canoeing (?), diving
### Equestrianism, rowing, sailing, shooting, Table Tennis

newSports <- c("Badminton", 'Baseball', "Canoeing", "Diving", "Equestrianism",
               "Rowing", "Sailing", "Shooting", "Table Tennis")

moreYears <- subset(athletesAll, Year >= 1980)
newSportsFreq <- subset(moreYears, Sport == "Badminton" | Sport ==  "Baseball" | Sport == "Canoeing" |
                          Sport == "Diving" | Sport == "Equestrianism"  | Sport == "Rowing" | Sport == "Sailing" |
                          Sport == "Shooting" | Sport == "Table Tennis" )
newFreqAthlete <- as.data.frame(table(newSportsFreq[, c(10, 13)]))
subset(newFreqAthlete, Sport == "Badminton")
BCS <- newFreqAthlete[c(1:10, 21:30, 61:70),]

ggplot(BCS, aes(Year, Freq, group = Sport)) + geom_line(aes(color = Sport)) + 
  geom_point(aes(color = Sport)) + ggtitle("Number of Players over Last 10 Olympics") +
  ylab("Number of Players")

ggplot(newFreqAthlete[1:10, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Badminton Players")

ggplot(newFreqAthlete[11:20, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Baseball Players")

ggplot(newFreqAthlete[21:30, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Canoeing Players")

ggplot(newFreqAthlete[31:40, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Diving Players")

ggplot(newFreqAthlete[41:50, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Equestrianism Players")

ggplot(newFreqAthlete[51:60, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Rowing Players")

ggplot(newFreqAthlete[61:70, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Sailing Players")

ggplot(newFreqAthlete[71:80, ], aes(Year, Freq)) +
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Shooting Players")

ggplot(newFreqAthlete[81:90, ], aes(Year, Freq)) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("Number of Players") + 
  ggtitle("Number of Table Tennis Players")

###########################
#### Comparing Results ####
###########################

leastPopular <- leastPopular[order(leastPopular$Percent, decreasing = TRUE), ]

ggplot(leastPopular[1:10, ], aes(Sport, Percent)) + 
  geom_bar(stat = "identity", position = "dodge") + ggtitle("Least Popular Sports")

################################################
#### Finding a Year Using Number of Players ####
################################################
df$Sport <- "Ultimate"

### Badminton
badminton <- subset(athletesAll, Sport == "Badminton")

View(badminton)

badYear <- c(length(badminton$NOC[badminton$Year == 1992]),
length(badminton$NOC[badminton$Year == 1996]),
length(badminton$NOC[badminton$Year == 2000]),
length(badminton$NOC[badminton$Year == 2004]),
length(badminton$NOC[badminton$Year == 2008]),
length(badminton$NOC[badminton$Year == 2012]),
length(badminton$NOC[badminton$Year == 2016]))
years <- c(1992, 1996, 2000, 2004, 2008, 2012, 2016)

badDF <- data.frame(Year = years, 
                    numTeams = badYear)
bad.lm <- lm(numTeams ~ Year, badDF)
summary(bad.lm)

bad2024 <- c(2024, round((6470.6429 - 3.1250*(2024)), 0))
bad2028 <- c(c(2028, round((6470.6429 - 3.1250*(2028)), 0)))
bad2032 <- c(2032, round((6470.6429 - 3.1250*(2032)), 0))
badDF <- rbind(badDF, bad2024, bad2028, bad2032)


badDF$Sport <- "Badminton"

### Canoeing

Canoe <- subset(athletesAll, Sport == "Canoeing")

canYear <- c(length(Canoe$NOC[Canoe$Year == 1992]),
             length(Canoe$NOC[Canoe$Year == 1996]),
             length(Canoe$NOC[Canoe$Year == 2000]),
             length(Canoe$NOC[Canoe$Year == 2004]),
             length(Canoe$NOC[Canoe$Year == 2008]),
             length(Canoe$NOC[Canoe$Year == 2012]),
             length(Canoe$NOC[Canoe$Year == 2016]))
years <- c(1992, 1996, 2000, 2004, 2008, 2012, 2016)

canDF <- data.frame(Year = years, 
                    numTeams = canYear)
can.lm <- lm(numTeams ~ Year, canDF)
summary(can.lm)

can2024 <- c(2024, round((14147.143 - 6.821*(2024)), 0))
can2028 <- c(c(2028, round((14147.143 - 6.821*(2028)), 0)))
can2032 <- c(2032, round((14147.143 - 6.821*(2032)), 0))
canDF <- rbind(canDF, can2024, can2028, can2032)


canDF$Sport <- "Canoeing"
### Sailing

Sail <- subset(athletesAll, Sport == "Sailing")

sailYear <- c(length(Sail$NOC[Sail$Year == 1992]),
             length(Sail$NOC[Sail$Year == 1996]),
             length(Sail$NOC[Sail$Year == 2000]),
             length(Sail$NOC[Sail$Year == 2004]),
             length(Sail$NOC[Sail$Year == 2008]),
             length(Sail$NOC[Sail$Year == 2012]),
             length(Sail$NOC[Sail$Year == 2016]))
years <- c(1992, 1996, 2000, 2004, 2008, 2012, 2016)

sailDF <- data.frame(Year = years, 
                    numTeams = sailYear)
sail.lm <- lm(numTeams ~ Year, sailDF)
summary(sail.lm)

sail2024 <- c(2024, round((6545.9643 - 3.0625*(2024)), 0))
sail2028 <- c(c(2028, round((6545.9643 - 3.0625*(2028)), 0)))
sail2032 <- c(2032, round((6545.9643 - 3.0625*(2032)), 0))
sailDF <- rbind(sailDF, sail2024, sail2028, sail2032)

sailDF$Sport <- "Sailing"

### Combining and find the years
combDF <- rbind(df, badDF, canDF, sailDF)

ggplot(combDF, aes(x = Year, y = numTeams, color = Sport)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) + ylab("Number of Teams") + 
  ggtitle("Popularity of Sports for Future Olympics")

summary(reg)
