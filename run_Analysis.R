library("dplyr")

##Loading all the needed data into tbl_df
subject.test <- tbl_df(read.table("test/subject_test.txt"))
x.test <- tbl_df(read.table("test/X_test.txt"))
y.test <- tbl_df(read.table("test/y_test.txt"))

subject.train <- tbl_df(read.table("train/subject_train.txt"))
x.train <- tbl_df(read.table("train/X_train.txt"))
y.train <- tbl_df(read.table("train/y_train.txt"))

features <- read.table("features.txt")

##Transforming y.train and y.test into the corresponding activity labels provided in activity_lables.txt
y.train[y.train=="1"] <- "Walking"
y.train[y.train=="2"] <- "Walking_Upstairs"
y.train[y.train=="3"] <- "Walking_Downstairs"
y.train[y.train=="4"] <- "Sitting"
y.train[y.train=="5"] <- "Standing"
y.train[y.train=="6"] <- "Laying"

y.test[y.test=="1"] <- "Walking"
y.test[y.test=="2"] <- "Walking_Upstairs"
y.test[y.test=="3"] <- "Walking_Downstairs"
y.test[y.test=="4"] <- "Sitting"
y.test[y.test=="5"] <- "Standing"
y.test[y.test=="6"] <- "Laying"

##Binding the "train" dataset together
trainCombined <- cbind(subject.train,y.train,x.train)

##Binding the "test" dataset together
testCombined <- cbind(subject.test,y.test,x.test)

##Combining both sets
dataCombined <- rbind(trainCombined,testCombined)

##Inputting Column Names
columnNames <- as.character(features[,2])
columnNames <- make.names(columnNames, unique=TRUE)
colnames(dataCombined)<- c("subjectNumber","activityType",columnNames)

##The combined dataset is below
dataCombined <- tbl_df(dataCombined)

##A grep search is used to only extract the columns for subject, activityType and the std deviations and means of each measurement.
##mean of Frequecies (meanFreq) has been left out
dataCombined <- dataCombined[,grep("subjectNumber|activityType|std()|mean()", names(dataCombined))]
dataCombined <- dataCombined[,grep("meanFreq",invert=TRUE, names(dataCombined))]

##Descriptive Activity Names have already been inputted as below.
unique(dataCombined$activityType)

##Descriptive variable names inputted
columnNames <- names(dataCombined)
columnNames <- gsub("()", "", columnNames, fixed = TRUE)
columnNames <- gsub("-", "", columnNames, fixed = TRUE)
columnNames <- gsub("X", ".Xaxis", columnNames, fixed = TRUE)
columnNames <- gsub("Y", ".Yaxis", columnNames, fixed = TRUE)
columnNames <- gsub("Z", ".Zaxis", columnNames, fixed = TRUE)
columnNames <- gsub("tBody", "Time.Body", columnNames, fixed = TRUE)
columnNames <- gsub("tGravity", "Time.Gravity", columnNames, fixed = TRUE)
columnNames <- gsub("fBody", "Frequency.Body", columnNames, fixed = TRUE)
columnNames <- gsub("fGravity", "Frequency.Gravity", columnNames, fixed = TRUE)
columnNames <- gsub("Acc", ".Accelerometer.", columnNames, fixed = TRUE)
columnNames <- gsub("Gyro", ".Gyroscope.", columnNames, fixed = TRUE)
columnNames <- gsub("mean", "Mean", columnNames, fixed = TRUE)
columnNames <- gsub("std", "Standard.Deviation", columnNames, fixed = TRUE)
columnNames <- gsub("..", "", columnNames, fixed = TRUE)
names(dataCombined) <- columnNames

##Creating a secound, independent dataset with just average of each variable
AvgVariableSet <- group_by(dataCombined, subjectNumber, activityType)
AvgVariableSet <- summarise_each(AvgVariableSet, funs(mean))
write.table(AvgVariableSet, file="CleanTidyDataSet.txt", row.names=FALSE)
