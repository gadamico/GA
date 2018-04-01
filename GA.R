library(readr)
DSI <- read.csv("~/Desktop/Dossier materials/GeneralAssembly/DSI_kickstarterscrape_dataset.csv")
View(DSI)
mean(na.omit(DSI$pledged))
library(ggplot2)
ggplot(DSI, aes(x = DSI$backers)) + geom_histogram()

max(DSI$backers)
backersOrdered <- sort.int(DSI$backers)
length(backersOrdered)

DSIBackersOrdered <- DSI[order(DSI$backers),]
DSIClipped <- DSIBackersOrdered[1:45000,]
ggplot(DSIClipped, aes(x = DSIClipped$backers))
  + geom_histogram(fill = "red")

library(e1071)
skewness(DSI$backers)

library(dplyr)
set.seed(47)
DSIsample <- dplyr::sample_n(DSI, 10)
shapiro.test(DSIsample$duration)

## Part 2

train <- DSI[1:40000,]
test <- DSI[40001:45957,]
model1 <- glm(status ~ goal + duration, data = train,
              family = binomial())
summary(model1)

DSI$statusAsInt <- 1:45957
for (i in 1:45957){
  if (DSI$status[i] == "successful"){
    DSI$statusAsInt[i] <- 1}
  else if (DSI$status[i] == "failed"){
     DSI$statusAsInt[i] <- 0}
  else {DSI$statusAsInt[i] <- NA}
}

train <- DSI[1:40000,]
test <- DSI[40001:45957,]

model2 <- lm(statusAsInt ~ goal + duration, data = train)

## The negative coefficients on the goal and duration
## variables indicate that smaller goals and smaller
## durations are correlated with success.

fittingResults <- predict(model2, newdata = test,
                          type = "response")
fittingResults <- ifelse(fittingResults > 0.5, 1, 0)
Error <- mean(na.omit(fittingResults != test$statusAsInt))
Error

library(rpart)
rpart(statusAsInt ~ goal + duration, data = train,
      method = "class")

## This tree predicts that a campaign will typically
## be unsuccessful if the goal >= 8082 and will typically
## be successful if the goal < 3455. For intermediate
## goal values the campaigns are more successful when
## their durations are less than 58.93 (days?).


unique(DSI$category)

install.packages("dummies")
library(dummies)

dummies <- dummy(DSI$category)
DSIplusDums <- cbind(DSI, dummies)
model3 <- lm(statusAsInt ~ categoryArt + categoryComics
             + categoryDance + categoryDesign + categoryFashion
             + `categoryFilm & Video` + categoryFood + categoryGames
             + categoryMusic + categoryPhotography + categoryPublishing
             + categoryTechnology+ categoryTheater, data = DSIplusDums)
summary(model3)


library(tidyr)

DSIsepDates <- separate(DSI, "funded.date", c("funded day", "funded date",
                        "funded month", "funded year", "funded time"), sep = " ",
                        extra = "merge")

model4 <- lm(statusAsInt ~ `funded year`, data = DSI$sepDates)


monthdums <- dummy(DSIsepDates$`funded month`)
colnames(monthdums) <- c("Apr", "Aug", "Dec", "Feb", "Jan",
                         "Jul", "Jun", "Mar", "May", "Nov",
                         "Oct", "Sep")
DSIsepDatesPlusDums <- cbind(DSIsepDates, monthdums)

model5 <- lm(statusAsInt ~ Jan + Feb + Mar + Apr + May
             + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
             data = DSIsepDatesPlusDums)
summary(model5)



