require(plyr)
run_analysis <- function(){
  #read data from the files
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  X_labels <- read.table("UCI HAR Dataset/features.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

  #combine test and train data (Step 1)
  X <- rbind(X_test, X_train)
  Y <- rbind(Y_test, Y_train)
  subject <- rbind(subject_test, subject_train)
  
  #extract X variables of interest (Step 2)
  X_labels <- X_labels[grep("(mean|std)\\(", X_labels[[2]]),]
  X <- X[,X_labels[[1]]]
  
  #give descriptive activity names (step 3)
  activities <- character(length = length(Y[[1]]))
  for (i in 1:length(activities)){
    activities[i] <- as.character(activity_labels[[2]][Y[[1]][i]])
  }
  Y[[1]] <- factor(activities)
  
  #name the variables (step 4)
  names(X) <- make.names(X_labels[[2]])
  names(Y) <- "activity"
  names(subject) <- "subject"
  
  #combine data into one data frame
  allData <- cbind(X, Y, subject)
  
  #create "second tidy data set" (step 5)
  allData <- ddply(allData, .(activity, subject), colwise(mean))
  write.table(allData, "tidySet.txt", row.names = FALSE)
}