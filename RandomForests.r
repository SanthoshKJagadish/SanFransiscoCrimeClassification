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
Train$Date <- (format(Train$Dates, "%d"))
Train$Year <- format(Train$Dates, "%Y")
Train$months <- (format(Train$Dates, "%m"))
Train$Hours <- (format(Train$Dates, "%H"))

Train$Year <- as.numeric(Train$Year)
Train$Date <- as.numeric(Train$Date)
Train$months <- as.numeric(Train$months)
Train$Hours <- as.numeric(Train$Hours)

summary(Train)
str(Train)

Train <- na.omit(Train)

Train <- Train[,c(-1, -3, -6)]

Category <- data.frame(table(Train$Category))
arrange(Category, desc(Freq))
CategoryFreqPercentage <- (Category$Freq/nrow(Train))*100

CategoryNew <- data.frame(Category, round(CategoryFreqPercentage))
arrange(CategoryNew, desc(Freq))

Train$Category <- as.character(Train$Category)

Train <- filter(Train, Train$Category != "TREA" & Train$Category != "PORNOGRAPHY/OBSCENE MAT" & Train$Category != "SEX OFFENSES NON FORCIBLE" & Train$Category != "GAMBLING" &
                  Train$Category != "EXTORTION" & Train$Category != "BRIBERY" & Train$Category != "BAD CHECKS" & Train$Category != "FAMILY OFFENSES" &
                  Train$Category != "SUICIDE" & Train$Category != "EMBEZZLEMENT" & Train$Category != "LOITERING" & Train$Category != "ARSON" & Train$Category != "LIQUOR LAWS" & 
                  Train$Category != "RUNAWAY" & Train$Category != "DRIVING UNDER THE INFLUENCE" & Train$Category != "KIDNAPPING" & Train$Category != "RECOVERED VEHICLE"
                & Train$Category != "DRUNKENNESS" & Train$Category != "DISORDERLY CONDUCT" & Train$Category != "SEX OFFENSES FORCIBLE"
                & Train$Category != "STOLEN PROPERTY" & Train$Category != "TRESPASS" & Train$Category != "PROSTITUTION" & Train$Category != "WEAPON LAWS" 
                & Train$Category != "SECONDARY CODES" & Train$Category != "FORGERY/COUNTERFEITING"
                & Train$Category != "FRAUD" & Train$Category != "ROBBERY" & Train$Category != "MISSING PERSON" & Train$Category != "SUSPICIOUS OCC"
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
levels(Train$CategoryMap) <- gsub("VANDALISM", 7, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("WARRANTS", 8, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("BURGLARY", 9, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("SUSPICIOUS OCC", 10, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("MISSING PERSON", 11, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("ROBBERY", 12, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("FRAUD", 13, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("FORGERY/COUNTERFEITING", 14, levels(Train$CategoryMap))
levels(Train$CategoryMap) <- gsub("SECONDARY CODES", 15, levels(Train$CategoryMap))

Train <- Train[,-1]

Train$DayOfWeekMap <- Train$DayOfWeek

levels(Train$DayOfWeekMap) <- gsub("Sunday", 1, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Saturday", 1, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Monday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Tuesday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Wednesday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Thursday", 0, levels(Train$DayOfWeekMap))
levels(Train$DayOfWeekMap) <- gsub("Friday", 1, levels(Train$DayOfWeekMap))

Train$AddressMap <- Train$Address

Train$AddressMap[!grepl("/", Train$Address)] = "0"
Train$AddressMap[grepl("/", Train$Address)] = "1"

Train$AddressMap <- as.factor(Train$AddressMap)

TrainNew <- Train[,c(-1,-3)]


str(TrainNew)
###########################################################
# Sampling
#############################################################


############################ for loop #####################################
tree.pred <- matrix(nrow = 2999, ncol = 10)
TrainNew <- TrainNew[sample(1:nrow(TrainNew), 100000,
                            replace=FALSE),]
for ( i in 1:10){
  trainIndex <- createDataPartition(TrainNew$CategoryMap, p = 0.10, list = FALSE, times = 1)
  dataTrain <- TrainNew[trainIndex,]
  
  trainIndex <- createDataPartition(dataTrain$CategoryMap, p = 0.70, list = FALSE, times = 1)
  dataTrainNew <- dataTrain[trainIndex,]
  dataTest <- dataTrain[-trainIndex,]
  
  tree.NT <- rpart(CategoryMap~PdDistrict+X+Y+Date+months+Year, data = dataTrainNew, method = "class", parms = list(split = "gini"))
  
  tree.pred[,i] = predict(tree.NT, dataTest, type = "class")
  #pred.tree[i] <- table(tree.pred[i], dataTest$CategoryMap)
  
}


#######################################
# finding the mode
#############################################

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

pred.final <- vector()

#for( i in 1:length(tree.pred1)){
# pred.final[i] <- getmode(c(as.numeric(tree.pred1)[i],as.numeric(tree.pred2)[i],as.numeric(tree.pred3)[i], as.numeric(tree.pred4)[i], as.numeric(tree.pred5)[i], as.numeric(tree.pred6)[i], as.numeric(tree.pred7)[i], as.numeric(tree.pred8)[i], as.numeric(tree.pred9)[i], as.numeric(tree.pred10)[i]))
#}

vector.pred <- vector(length = 10)
for( i in 1:length(dataTest)){
  
  pred.final[i] <- getmode(c(as.numeric(tree.pred)[i,1],as.numeric(tree.pred)[i,2],as.numeric(tree.pred)[i,3], as.numeric(tree.pred)[i,4], as.numeric(tree.pred)[i,5], as.numeric(tree.pred6)[i,6], as.numeric(tree.pred)[i,7], as.numeric(tree.pred)[i,8], as.numeric(tree.pred)[i,9], as.numeric(tree.pred)[i,10]))
}


pred.RandomForest <- table(pred.final, dataTest$CategoryMap)

confusionMatrix(pred.RandomForest)

model<- randomForest(CategoryMap~., data = dataTrainNew)
pred.RF.inbuilt <- predict(model, dataTest[,-8])
tab<- table(pred.RF.inbuilt, dataTest$CategoryMap)
tab
accuracy <- (tab[1,1]+tab[2,2])/sum(tab)
accuracy