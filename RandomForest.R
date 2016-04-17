library(dplyr)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
setwd("C:/Users/Anindita/Documents/ALDA/Project")

Train <- read.csv("train.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)

Train$Category <- as.factor(Train$Category)
Train$DayOfWeek <- as.factor(Train$DayOfWeek)
Train$PdDistrict <- as.factor(Train$PdDistrict)
Train$Resolution <- as.factor(Train$Resolution)
Train$Address <- as.character(Train$Address)
Train$Dates <- as.POSIXct(Train$Dates, format = "%Y-%m-%d %H:%M:%S")
Train$Date <- factor(format(Train$Dates, "%d"))
Train$Year <- factor(format(Train$Dates, "%Y"))
Train$months <- factor(format(Train$Dates, "%m"))
Train$Hours <- factor(format(Train$Dates, "%H"))

Train <- Train[,c(-1, -3, -6)]

Train <- na.omit(Train)

Category <- data.frame(table(Train$Category))
arrange(Category, desc(Freq))
CategoryFreqPercentage <- (Category$Freq/nrow(Train))*100

CategoryNew <- data.frame(Category, round(CategoryFreqPercentage))
arrange(CategoryNew, desc(Freq))

Train$Category <- as.character(Train$Category)

Train <- filter(Train, Train$Category != "TREA" & Train$Category != "PORNOGRAPHY/OBSCENE MAT" & Train$Category != "SEX OFFENSES NON FORCIBLE" & Train$Category != "GAMBLING" &
                  Train$Category != "EXTORTION" & Train$Category != "BRIBERY" & Train$Category != "BAD CHECKS" & Train$Category != "FAMILY OFFENSES" &
                  Train$Category != "SUICIDE" & Train$Category != "EMBEZZLEMENT" & Train$Category != "LOITERING" & Train$Category != "ARSON" & Train$Category != "LIQUOR LAWS" & 
                  Train$Category != "RUNAWAY" & Train$Category != "DRIVING UNDER THE INFLUENCE" & Train$Category != "KIDNAPPING" & Train$Category != "RECOVERED VEHICLE" &
                  Train$Category != "DRUNKENNESS" & Train$Category != "DISORDERLY CONDUCT" & Train$Category != "SEX OFFENSES FORCIBLE" &
                  Train$Category != "STOLEN PROPERTY" & Train$Category != "TRESPASS" & Train$Category != "PROSTITUTION" &
                  Train$Category != "WEAPON LAWS" & Train$Category != "SECONDARY CODES" & Train$Category != "FORGERY/COUNTERFEITING" &
                  Train$Category != "FRAUD" & Train$Category != "ROBBERY" & Train$Category != "MISSING PERSON" & Train$Category != "SUSPICIOUS OCC"
                  & Train$Category != "BURGLARY" & Train$Category != "WARRANTS" & Train$Category != "VANDALISM" & Train$Category != "VEHICLE THEFT"
                  & Train$Category != "DRUG/NARCOTIC" & Train$Category != "NON-CRIMINAL" & Train$Category != "ASSAULT")


Train$Category <- as.factor(Train$Category)
Train$CategoryMap <- Train$Category

levels(Train$CategoryMap) <- gsub("LARCENY/THEFT", 1, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("OTHER OFFENSES", 2, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("NON-CRIMINAL", 3, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("ASSAULT", 4, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("DRUG/NARCOTIC", 5, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("VEHICLE THEFT", 6, levels(Train$CategoryMap))


Train$DayOfWeekMap <- Train$DayOfWeek

levels(Train$DayOfWeekMap) <- gsub("Sunday", 1, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Saturday", 1, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Monday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Tuesday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Wednesday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Thursday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Friday", 0, levels(Train$DayOfWeekMap))

Train$AddressMap <- Train$Address

Train$AddressMap[!grepl("/", Train$Address)] = "0"
Train$AddressMap[grepl("/", Train$Address)] = "1"

Train$AddressMap <- as.factor(Train$AddressMap)
Train$DateMap <- as.factor(Train$Date)

levels(Train$DateMap) <- gsub("01", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("02", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("03", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("04", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("05", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("06", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("07", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("08", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("09", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("10", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("11", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("12", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("13", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("14", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("15", 1, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("16", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("17", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("18", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("19", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("20", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("21", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("22", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("23", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("24", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("25", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("26", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("27", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("28", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("29", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("30", 2, levels(Train$DateMap))
levels(Train$DateMap) <- gsub("31", 2, levels(Train$DateMap))

Train$DateMap <- as.factor(Train$DateMap)

Train$HoursMap <- Train$Hours

levels(Train$HoursMap) <- gsub("01", 0, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("02", 0, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("03", 0, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("04", 0, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("05", 0, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("06", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("07", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("08", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("09", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("10", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("11", 1, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("12", 2, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("13", 2, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("14", 2, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("15", 2, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("16", 3, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("17", 3, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("18", 3, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("19", 3, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("20", 3, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("21", 4, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("22", 4, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("23", 4, levels(Train$HoursMap))
levels(Train$HoursMap) <- gsub("00", 4, levels(Train$HoursMap))

FinalTrain <- Train[, c(-1, -2, -4, -7, -10)]


trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]


##Iteration 1
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
#################################################################
# creating the dataTest from rest 30% but not changing in sebsequent iterations
##################################################################
dataTest <- dataTrain[-trainIndex,]
############# end #####################################
tree.NT1 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred1 = predict(tree.NT1, dataTest[,-6], type = "class")

##Iteration 2
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT2 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred2 = predict(tree.NT2, dataTest[,-6], type = "class")

##Iteration 3
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT3 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred3 = predict(tree.NT3, dataTest[,-6], type = "class")

##Iteration 4
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT4 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred4 = predict(tree.NT4, dataTest[,-6], type = "class")

##Iteration 5
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT5 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred5 = predict(tree.NT5, dataTest[,-6], type = "class")

##Iteration 6
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT6 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred6 = predict(tree.NT6, dataTest[,-6], type = "class")

##Iteration 7
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT7 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred7 = predict(tree.NT7, dataTest[,-6], type = "class")

##Iteration 8
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT8 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred8 = predict(tree.NT8, dataTest[,-6], type = "class")

##Iteration 6
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT9 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred9 = predict(tree.NT9, dataTest[,-6], type = "class")

##Iteration 6
trainIndex <- createDataPartition(FinalTrain$CategoryMap, p = 0.10, list = FALSE, times = 1)
dataTrain <- FinalTrain[trainIndex,]
trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
dataTrainNew <- dataTrain[trainIndex,]
tree.NT10 <- rpart(CategoryMap ~ ., data = dataTrainNew, method = "class", parms = list(split = "gini"))
tree.pred10 = predict(tree.NT10, dataTest[,-6], type = "class")
#######################################
# finding the mode
#############################################

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

pred.final <- vector()

for( i in 1:9032){
  pred.final[i] <- getmode(c(as.numeric(tree.pred1)[i],as.numeric(tree.pred2)[i],as.numeric(tree.pred3)[i], as.numeric(tree.pred4)[i], as.numeric(tree.pred5)[i], as.numeric(tree.pred6)[i], as.numeric(tree.pred7)[i], as.numeric(tree.pred8)[i], as.numeric(tree.pred9)[i], as.numeric(tree.pred10)[i]))
}

pred.RandomForest <- table(pred.final, dataTest$CategoryMap)

confusionMatrix(pred.RandomForest)

randomForest(CategoryMap~., data = dataTest)
