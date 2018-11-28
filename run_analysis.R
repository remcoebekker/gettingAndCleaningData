createTidyDataSet <- function() {
  ## First we validate the basic environment needed for this script.
  validateCorrectEnvironment()
  
  ## Next we read in the testData and the trainData.
  print("Reading in testData set...")
  testData <- readInTestData()
  print("Reading in trainData set...")
  trainData <- readInTrainData()

  ## Step 1 of the assignment "Merge the training and the test set to create 
  ## one data set" can now be performed.
  print("Step 1 of the assignment: merging the training and test set...")
  mergedData <- rbind(testData, trainData)
  
  ## For step 2 of the assignment "Extract only the measurements on the mean and 
  ## standard deviation for each measurement." we first need the determine the
  ## relevant mean and standard deviation features so we can use this information 
  ## to remove all other feature columns from the mergedData set. The 
  ## relevantFeatures object will contain a "relevant" column with TRUE for every 
  ## relevant feature.
  relevantFeatures <- determineRelevantFeatures()
  
  ## Now we can determine the column numbers that need to be kept from mergedData
  ## We need to keep column 1 and 2 as those contain the subjects and the activities.
  ## And we need the keep the columns for which the relevant value is set to
  ## TRUE. However since the first column of the features is the third column
  ## in the mergedData set we need to raise all relevant numbers with 2.
  columnsToKeep <- c(1, 2, which(relevantFeatures$relevant == TRUE) + 2)

  ## Step 2 of the assignment "Extracts only the measurements on the mean and 
  ## standard deviation for each measurement" can now be performed.
  print("Step 2 of the assignment: extract only the measurement on the mean and standard deviation.")
  mergedData <- mergedData[, columnsToKeep]
  
  ## Step 3 of the assignment "Uses descriptive activity names to name the activities in the data set"
  ## requires that we first read in the activity labels.
  activityLabels <- readInActivityLabels()
  print("Step 3 of the assignment: use descriptive activity names to name the activities in the data set...")
  ## Join the data set with the activity labels matching on the identifier.
  mergedData <- merge(x = mergedData, y = activityLabels, by.x = "V1.1", by.y = "V1", all.x = TRUE)
  ## The activity labels column has now been added at the right behind all the
  ## features, furthermore the activity identifier column still exists. 
  ## So we move the activity label column to the second place, remove the 
  ## activity identifier column and move the subject identifier column to the 
  ## first place. 
  ActivityLabelColumnIndex <- grep("V2.y", names(mergedData))
  mergedData <- mergedData[, c(2, ActivityLabelColumnIndex, (3:(ncol(mergedData)-1)))]
  
  print("Step 4 of the assignment: appropriately label the data set with descriptive variable names...")
  ## When we determined the relevant features we also saved the feature names as
  ## contained in the "features.txt" file. So we use those feature names
  ## to turn the column names into descriptive variable names.
  relevantFeatureNames <- createDescriptiveFeatureNames(relevantFeatures[which(relevantFeatures$relevant == TRUE),c("V2")])
  ## We just name the first and second column "Subject" and "Activity" and as
  ## the other column names we used the "features.txt" feature names. 
  descriptiveVariableNames <- c("Subject", "Activity", as.vector(relevantFeatureNames))
  names(mergedData) <- descriptiveVariableNames
  
  print("Step 5 of the assignment: create a second, independent tidy data set with the average of each variable for each activity and each subject.")
  ## We aggregate the adat on Subject and Activity and calculate the mean
  ## for every feature. 
  tidyDataSet <- aggregate(. ~ Subject + Activity, data = mergedData, mean)
  ## The variable names of features of this aggregated tidy data set need to
  ## be changed as they are averages now...
  names(tidyDataSet) <- addAverageToVariableNames(names(tidyDataSet))
  
  ## Finally we write a file containing the tidy data set as this needs to 
  ## be uploaded.
  print("Writing the tidy data set into the file 'tidy_data.txt'...")
  write.table(x = tidyDataSet, file = "tidy_data.txt", row.name = FALSE, quote = FALSE)
  
  ## For convenience we also return the tidy data set...
  return(tidyDataSet)
}

validateCorrectEnvironment <- function () {
  ## Check whether the basic environment is as expected...we could be 
  ## in the wrong working directory for example

  if (!file.exists("activity_labels.txt")) {
    stop("File activity_labels.txt not found, please check the working directory.")
  }
  
  if (!file.exists("features.txt")) {
    stop("File features.txt not found, please check the working directory.")
  }

  if (!dir.exists("test")) {
    stop("Folder test not found, please check the working directory.")
  }
  
  if (!dir.exists("train")) {
    stop("Folder train not found, please check the working directory.")
  }
  
  ## We need the "stringr" package later on...is it already loaded?
  if (!"stringr" %in% loadedNamespaces()) {
    ## Nope, it's not loaded, has it been installed?
    if (!"stringr" %in% installed.packages()[, "Package"]) { 
      ## Nope, tell the user that it needs to be installed.
      stop("This script requires the \"stringr\" package to be installed, please do so first.")
    }
    
    ## Load the library...
    library("stringr")
  }
}

readInTestData <- function() {
  ## The test folder should contain 3 text files: "subject_test.txt", 
  ## "X_test.txt"and "y_test.txt". It should also contain one folder 
  ## "Inertial Signals" but we won't be needing this folder as the necessary 
  ## features for the assignment (mean and standard deviation) have already been 
  ## calculated and are placed in "X_test.txt". The text file  "X_test.txt" 
  ## contains 2947 feature vectors. Each feature vector pertains to one subject
  ## row in the textfile "subject_test.txt" and one activity in the text file
  ## "y_test.txt". 
    
  ## "subject_test.txt" contains the 2947 identifiers of the subjects. There are
  ## 9 unique subject identifiers in this file (that is 30% of 30 subjects)
  testSubjects <- read.table("test/subject_test.txt", header=FALSE)
  testSubjects$V1 <- as.factor(testSubjects$V1)
  
  ## "y_test.txt" contains the 2947 identifiers of the activities. The meaning
  ## of these identifiers is explained in the file "activity_labels.txt"
  ## located in the parent folder of the test folder.
  testActivities <- read.table("test/y_test.txt", header=FALSE)

  ## "X_test.txt" contains the 2947 feature vectors. Each feature vector consists 
  ## of 561 features. The meaning of these 561 features has been defined in the 
  ## "features.txt" file located in the parent folder of the test folder.
  ## this file is a fixed column width text file where each column is 16 wide. 
  ## Per feature vector we read in 561 times 16 characters.
  testFeatureVector <- read.fwf("test/x_test.txt", widths = rep.int(16L, 561), header=FALSE, sep = "")

  ## Now that we have read in the 3 test files we need to merge them together.
  ## The row numbers correspond to each other, so we just bind the dataframes
  ## together. We start with the subjects, then the activitities and lastly
  ## the feature vectors as the subject and activity can be seen as the keys that
  ## provide access to the feature vectors. 
  testData <- cbind(testSubjects, testActivities, testFeatureVector)
}

readInTrainData <- function() {
  ## The train folder should contain 3 text files: "subject_train.txt", 
  ## "X_train.txt"and "y_train.txt". It should also contain one folder 
  ## "Inertial Signals" but we won't be needing this folder as the necessary 
  ## features for this assignment (means and standard deviation) have already 
  ## been calculated and are placed in "X_train.txt". The text file  
  ## "X_train.txt" contains 7352 feature vectors. Each feature vector pertains
  ## to one subject row in the text file "subject_train.txt" and one activity 
  ## in the text file"y_train.txt". 
  
  ## "subject_train.txt" contains the 7352 identifiers of the subjects. There 
  ## are 21 unique subject identifiers in this file (that is 70% of 30 subjects)
  trainSubjects <- read.table("train/subject_train.txt", header=FALSE)
  trainSubjects$V1 <- as.factor(trainSubjects$V1)
  
  ## "y_train.txt" contains the 7352 identifiers of the activities. The meaning
  ## of these identifiers is explained in the file "activity_labels.txt"
  ## located in the parent folder of the test folder.
  trainActivities <- read.table("train/y_train.txt", header=FALSE)
  
  ## "X_train.txt" contains the 7352 feature vectors. Each feature vector 
  ## consists of 561 features. The meaning of these 561 features has been defined 
  ## in the "features.txt" file located in the parent folder of the test folder.
  ## this file is a fixed column width text file where each column is 16 wide. 
  ## Per feature vector we read in 561 times 16 characters.
  trainFeatureVector <- read.fwf("train/x_train.txt", widths = rep.int(16L, 561), header=FALSE, sep = "")
  
  ## Now that we have read in the 3 test files we need to merge them together.
  ## The row numbers correspond to each other, so we just bind the dataframes
  ## together. We start with the subjects, then the activitities and lastly
  ## the feature vectors as the subject and activity can be seen as the keys that
  ## provide access to the feature vetors. 
  trainData <- cbind(trainSubjects, trainActivities, trainFeatureVector)
}

determineRelevantFeatures <- function() {
  ## We read in the file "features.txt" containing a list of the 561 features.
  features <- read.table("features.txt", header=FALSE)
  ## We create a new column "relevant" in the features deta frame to 
  ## indicate if the features contains the phrase "mean()" or the phrase "std()"
  ## as per the assignment we need to single out the mean and standard deviation
  ## features. I choose here to NOT include meanFreq() features. The assignment
  ## is not clear on this so you could go either way. 
  features$relevant <- str_detect(features$V2, pattern = "mean\\(\\)|std\\(\\)")
  return(features)
}

readInActivityLabels <- function() {
  ## The file "activity_labels.txt" contains the activity labels related to 
  ## each activity identifier.
  activities <- read.table("activity_labels.txt", header=FALSE)
}

createDescriptiveFeatureNames <- function(features) {
  ## The feature names as stated in the "features.txt" file are rather
  ## cryptic, so we transform the names. Even though it preference
  ## is to have all lower case variable names but with these
  ## long names I feel this makes it less readable. So I opt for camel
  ## case.
  
  features <- gsub(pattern = "^t", replacement = "time", x = features)
  features <- gsub(pattern = "^f", replacement = "frequency", x = features)
  features <- gsub(pattern = "Acc", replacement = "Acceleration", x = features)
  features <- gsub(pattern = "Mag", replacement = "Magnitude", x = features)
  features <- gsub(pattern = "\\-mean\\(\\)", replacement = "Mean", x = features)
  features <- gsub(pattern = "\\-std\\(\\)", replacement = "Standarddeviation", x = features)
  features <- gsub(pattern = "Gyro", replacement = "Angularvelocity", x = features)
  features <- gsub(pattern = "\\-X", replacement = "XAxis", x = features)
  features <- gsub(pattern = "\\-Y", replacement = "YAxis", x = features)
  features <- gsub(pattern = "\\-Z", replacement = "ZAxis", x = features)
  ## In some features the word Body seems to be double. So, we fix this.
  features <- gsub(pattern = "BodyBody", replacement = "Body", x = features)
}

addAverageToVariableNames <- function(variableNames) {
  variableNames <- sub(pattern = "^time", replacement = "averageTime", x = variableNames)
  variableNames <- sub(pattern = "^frequency", replacement = "averageFrequency", x = variableNames)
}