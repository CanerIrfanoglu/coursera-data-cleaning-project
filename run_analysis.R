path<- file.path("/Users/Caner/Desktop/UCI HAR Dataset") #Setting the path variable
#where files are located
####  READING THE DATA FILES TO R  ####
Activitytest  <- read.table(file.path(path, "test","y_test.txt"),header = FALSE) ##Reads activity variables for test section(6)
Activitytrain <- read.table(file.path(path,"train","Y_train.txt"),header = FALSE)##Reads activity variables for train section(6)

Subjecttrain <- read.table(file.path(path,"train","subject_train.txt"),header = FALSE)##Reads subject variables for test section(30)
Subjecttest  <- read.table(file.path(path,"test","subject_test.txt"),header = FALSE)##Reads subject variables for train section(30)

Featurestest  <- read.table(file.path(path,"test", "X_test.txt"),header = FALSE)##Reads feature variables for test section
Featurestrain <- read.table(file.path(path,"train","X_train.txt"),header = FALSE)##Reads feature variables for train section

featurevariablenames<-read.table(file.path(path,"features.txt"),header = FALSE) #Names of the features to be added for naming the cols

Activitylabels <- read.table(file.path(path,"activity_labels.txt"),header=FALSE)

####  COMBINING TRAIN & TEST DATA ####
Activity <- rbind(Activitytest,Activitytrain)
Subject <- rbind(Subjecttrain,Subjecttest)
Features <- rbind (Featurestest,Featurestrain)
####  Replacing the COLUMN NAMES DESCRIPTIVELY  ####
names(Activity) <- c("activity")
names(Subject) <- c("subject")
names(Features) <- c(as.character(featurevariablenames[,2]))
####  PUTTING ALL TOGETHER  ####
alldata <- cbind(Activity, Subject,Features)
####  EXTRACTING MEAN and STD ####
featurenamesfiltered <- featurevariablenames$V2[grep("[Mm]ean\\(\\)|[Ss]td\\(\\)", as.character(featurevariablenames$V2))] 
selectednames<-c(as.character(featurenamesfiltered), "subject", "activity" )
filtereddata<-subset(alldata,select=selectednames)
####  DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN FILTERED DATA  ####
filtereddata$activity <- factor(filtereddata$activity, levels = Activitylabels[,1], labels = as.character(Activitylabels[,2]))
filtereddata$subject <- as.factor(filtereddata$subject)
####  ORDERING THE DATASET PROPERLY ####
columnshiftData<-filtereddata[,c(67,68,1:66)]
columnordered<- columnshiftData[order(columnshiftData$subject,as.character(columnshiftData$activity)),]
#### GETTING THE AVERAGE AND CREATING THE TIDY DATAFRAME
library(reshape2)
columnordered_melt <- melt(columnordered, id = c("subject", "activity"))
casted<- dcast(columnordered_melt, subject + activity ~ variable, mean)
tidy<-casted[order(casted$subject,as.character(casted$activity)),]
write.table(tidy, file="tidy.txt")