# CodeBook for the Getting and cleaning data course project
Author: Remco Bekker  
Date:   November 28th, 2018  

## Description of the project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 

* A tidy data set as described below
* A link to a Github repository with your script for performing the analysis
* A code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md
* You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

* Merges the training and the test sets to create one data set.
* Extract only the measurements on the mean and standard deviation for each measurement.
* Use descriptive activity names to name the activities in the data set
* Appropriately label the data set with descriptive variable names.
* From the data set in step 4, create a second, independent tidy data set with the average of each variable for   each activity and each subject.

## Description of the data
### Context of the original data set ("the input")
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

### Data description that comes with the original data set ("the input")
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* Maxine's(): index of the frequency component with largest magnitude
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'

The acceleration signals from the smartphone accelerometer X, Y and Z axes in standard gravity units 'g'.   
The angular velocity measured by the gyroscope are in units of radians/second.  
Features are normalized and bounded within [-1,1].

### Data description of the fields in the tidy data set file that is produced by the "run_analysis.R" script ("the output")
This "run_analysis.R" script will transform the input data into a tidy data file called "tidy_data.txt" which
contains the columns as shown below. 

    > str(tidyData)
    'data.frame':	180 obs. of  68 variables:
     $ Subject                                                          : Factor w/ 30 levels "2","4","9","10",..: 1 2 3 4 5 6 7 8 9 10 ...
     $ Activity                                                         : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ averageTimeBodyAccelerationMeanXAxis                             : num  0.281 0.264 0.259 0.28 0.26 ...
     $ averageTimeBodyAccelerationMeanYAxis                             : num  -0.0182 -0.015 -0.0205 -0.0243 -0.0175 ...
     $ averageTimeBodyAccelerationMeanZAxis                             : num  -0.107 -0.111 -0.108 -0.117 -0.108 ...
     $ averageTimeBodyAccelerationStandarddeviationXAxis                : num  -0.974 -0.954 -0.942 -0.968 -0.955 ...
     $ averageTimeBodyAccelerationStandarddeviationYAxis                : num  -0.98 -0.942 -0.916 -0.946 -0.949 ...
     $ averageTimeBodyAccelerationStandarddeviationZAxis                : num  -0.984 -0.963 -0.941 -0.959 -0.948 ...
     $ averageTimeGravityAccelerationMeanXAxis                          : num  -0.51 -0.421 -0.58 -0.453 -0.379 ...
     $ averageTimeGravityAccelerationMeanYAxis                          : num  0.753 0.915 -0.119 -0.139 0.803 ...
     $ averageTimeGravityAccelerationMeanZAxis                          : num  0.6468 0.3415 0.9579 -0.0311 0.275 ...
     $ averageTimeGravityAccelerationStandarddeviationXAxis             : num  -0.959 -0.921 -0.922 -0.955 -0.936 ...
     $ averageTimeGravityAccelerationStandarddeviationYAxis             : num  -0.988 -0.97 -0.97 -0.967 -0.974 ...
     $ averageTimeGravityAccelerationStandarddeviationZAxis             : num  -0.984 -0.976 -0.971 -0.963 -0.96 ...
     $ averageTimeBodyAccelerationJerkMeanXAxis                         : num  0.0826 0.0934 0.0881 0.0738 0.0854 ...
     $ averageTimeBodyAccelerationJerkMeanYAxis                         : num  0.01225 0.00693 0.01156 0.0157 0.00774 ...
     $ averageTimeBodyAccelerationJerkMeanZAxis                         : num  -0.0018 -0.00641 -0.00705 0.00717 -0.00437 ...
     $ averageTimeBodyAccelerationJerkStandarddeviationXAxis            : num  -0.986 -0.978 -0.965 -0.978 -0.969 ...
     $ averageTimeBodyAccelerationJerkStandarddeviationYAxis            : num  -0.983 -0.942 -0.964 -0.967 -0.963 ...
     $ averageTimeBodyAccelerationJerkStandarddeviationZAxis            : num  -0.988 -0.979 -0.959 -0.976 -0.971 ...
     $ averageTimeBodyAngularvelocityMeanXAxis                          : num  -0.01848 -0.00923 -0.01363 -0.01956 -0.01465 ...
     $ averageTimeBodyAngularvelocityMeanYAxis                          : num  -0.1118 -0.093 -0.1589 -0.077 -0.0836 ...
     $ averageTimeBodyAngularvelocityMeanZAxis                          : num  0.145 0.17 0.101 0.105 0.145 ...
     $ averageTimeBodyAngularvelocityStandarddeviationXAxis             : num  -0.988 -0.973 -0.942 -0.962 -0.966 ...
     $ averageTimeBodyAngularvelocityStandarddeviationYAxis             : num  -0.982 -0.961 -0.927 -0.954 -0.954 ...
     $ averageTimeBodyAngularvelocityStandarddeviationZAxis             : num  -0.96 -0.962 -0.962 -0.972 -0.95 ...
     $ averageTimeBodyAngularvelocityJerkMeanXAxis                      : num  -0.102 -0.105 -0.104 -0.1 -0.099 ...
     $ averageTimeBodyAngularvelocityJerkMeanYAxis                      : num  -0.0359 -0.0381 -0.0276 -0.0389 -0.0411 ...
     $ averageTimeBodyAngularvelocityJerkMeanZAxis                      : num  -0.0702 -0.0712 -0.0569 -0.0591 -0.0679 ...
     $ averageTimeBodyAngularvelocityJerkStandarddeviationXAxis         : num  -0.993 -0.975 -0.945 -0.966 -0.967 ...
     $ averageTimeBodyAngularvelocityJerkStandarddeviationYAxis         : num  -0.99 -0.987 -0.962 -0.967 -0.966 ...
     $ averageTimeBodyAngularvelocityJerkStandarddeviationZAxis         : num  -0.988 -0.984 -0.977 -0.984 -0.97 ...
     $ averageTimeBodyAccelerationMagnitudeMean                         : num  -0.977 -0.955 -0.931 -0.957 -0.948 ...
     $ averageTimeBodyAccelerationMagnitudeStandarddeviation            : num  -0.973 -0.931 -0.915 -0.94 -0.937 ...
     $ averageTimeGravityAccelerationMagnitudeMean                      : num  -0.977 -0.955 -0.931 -0.957 -0.948 ...
     $ averageTimeGravityAccelerationMagnitudeStandarddeviation         : num  -0.973 -0.931 -0.915 -0.94 -0.937 ...
     $ averageTimeBodyAccelerationJerkMagnitudeMean                     : num  -0.988 -0.97 -0.963 -0.976 -0.97 ...
     $ averageTimeBodyAccelerationJerkMagnitudeStandarddeviation        : num  -0.986 -0.961 -0.955 -0.968 -0.963 ...
     $ averageTimeBodyAngularvelocityMagnitudeMean                      : num  -0.95 -0.93 -0.907 -0.938 -0.931 ...
     $ averageTimeBodyAngularvelocityMagnitudeStandarddeviation         : num  -0.961 -0.947 -0.899 -0.927 -0.936 ...
     $ averageTimeBodyAngularvelocityJerkMagnitudeMean                  : num  -0.992 -0.985 -0.965 -0.971 -0.971 ...
     $ averageTimeBodyAngularvelocityJerkMagnitudeStandarddeviation     : num  -0.99 -0.983 -0.953 -0.96 -0.962 ...
     $ averageFrequencyBodyAccelerationMeanXAxis                        : num  -0.977 -0.959 -0.947 -0.969 -0.956 ...
     $ averageFrequencyBodyAccelerationMeanYAxis                        : num  -0.98 -0.939 -0.934 -0.954 -0.951 ...
     $ averageFrequencyBodyAccelerationMeanZAxis                        : num  -0.984 -0.968 -0.946 -0.964 -0.955 ...
     $ averageFrequencyBodyAccelerationStandarddeviationXAxis           : num  -0.973 -0.952 -0.941 -0.968 -0.955 ...
     $ averageFrequencyBodyAccelerationStandarddeviationYAxis           : num  -0.981 -0.946 -0.913 -0.946 -0.951 ...
     $ averageFrequencyBodyAccelerationStandarddeviationZAxis           : num  -0.985 -0.962 -0.942 -0.96 -0.948 ...
     $ averageFrequencyBodyAccelerationJerkMeanXAxis                    : num  -0.986 -0.979 -0.964 -0.979 -0.969 ...
     $ averageFrequencyBodyAccelerationJerkMeanYAxis                    : num  -0.983 -0.944 -0.964 -0.968 -0.963 ...
     $ averageFrequencyBodyAccelerationJerkMeanZAxis                    : num  -0.986 -0.975 -0.956 -0.973 -0.967 ...
     $ averageFrequencyBodyAccelerationJerkStandarddeviationXAxis       : num  -0.987 -0.98 -0.969 -0.979 -0.973 ...
     $ averageFrequencyBodyAccelerationJerkStandarddeviationYAxis       : num  -0.985 -0.944 -0.967 -0.968 -0.965 ...
     $ averageFrequencyBodyAccelerationJerkStandarddeviationZAxis       : num  -0.989 -0.98 -0.96 -0.979 -0.973 ...
     $ averageFrequencyBodyAngularvelocityMeanXAxis                     : num  -0.986 -0.967 -0.93 -0.954 -0.957 ...
     $ averageFrequencyBodyAngularvelocityMeanYAxis                     : num  -0.983 -0.972 -0.935 -0.955 -0.953 ...
     $ averageFrequencyBodyAngularvelocityMeanZAxis                     : num  -0.963 -0.961 -0.96 -0.97 -0.946 ...
     $ averageFrequencyBodyAngularvelocityStandarddeviationXAxis        : num  -0.989 -0.975 -0.946 -0.965 -0.969 ...
     $ averageFrequencyBodyAngularvelocityStandarddeviationYAxis        : num  -0.982 -0.956 -0.923 -0.953 -0.955 ...
     $ averageFrequencyBodyAngularvelocityStandarddeviationZAxis        : num  -0.963 -0.966 -0.966 -0.975 -0.956 ...
     $ averageFrequencyBodyAccelerationMagnitudeMean                    : num  -0.975 -0.939 -0.927 -0.951 -0.944 ...
     $ averageFrequencyBodyAccelerationMagnitudeStandarddeviation       : num  -0.975 -0.937 -0.922 -0.944 -0.942 ...
     $ averageFrequencyBodyAccelerationJerkMagnitudeMean                : num  -0.985 -0.962 -0.954 -0.969 -0.962 ...
     $ averageFrequencyBodyAccelerationJerkMagnitudeStandarddeviation   : num  -0.985 -0.958 -0.955 -0.965 -0.962 ...
     $ averageFrequencyBodyAngularvelocityMagnitudeMean                 : num  -0.972 -0.962 -0.919 -0.938 -0.945 ...
     $ averageFrequencyBodyAngularvelocityMagnitudeStandarddeviation    : num  -0.961 -0.947 -0.903 -0.934 -0.94 ...
     $ averageFrequencyBodyAngularvelocityJerkMagnitudeMean             : num  -0.99 -0.984 -0.956 -0.961 -0.964 ...
     $ averageFrequencyBodyAngularvelocityJerkMagnitudeStandarddeviation: num  -0.989 -0.983 -0.952 -0.961 -0.962 ...     

* Subject: This is an identifier ranging from 1 to 30 indicating one of the subjects that participated in the endeavour to create the
original data set
* Activity: This is a label that contains 1 of 6 activities that was undertaken in the endeavour to create the original dat set: 
    * "WALKING"
    * "WALKING_UPSTAIRS"
    * "WALKING_DOWNSTAIRS"
    * "SITTING"
    * "STANDING"
    * "LAYING"
* Then there are 66 averages that have been calculated on 66 features from the original data set for each Subject identifier for each activity:
    * Averages of the time body accleration means for the X, Y and Z axes
    * Averages of the time body acceleration standard deviations for the X, Y and Z axes
    * Averages of the time gravity acceleration means for the X, Y and Z axes
    * Averages of the time gravity acceleration standard deviations for the X, Y and Z axes
    * Averages of the time body acceleration jerk means for the X, Y and Z axes
    * Averages of the time body acceleration jerk standard deviations for the X, Y and Z axes
    * Averages of the time body angular velocity means for the X, Y and Z axes
    * Averages of the time body angular velocity standard deviations for the X, Y and Z axes
    * Averages of the time body angular velocity jerk means for the X, Y and Z axes    
    * Averages of the time body angular velocity jerk standard deviations for the X, Y and Z axes
    * Averages of the time body acceleration magnitude means and standard deviations
    * Averages of the time gravity acceleration magnitude means and standard deviations
    * Averages of the time body acceleration jerk magnitude means and standard deviations
    * Averages of the time body angular velocity magnitude means and standard deviations
    * Averages of the time body angular velocity jerk magnitude means and standard deviations
    * Averages of the frequency body acceleration means for the X, Y and Z axes
    * Averages of the frequency body acceleration standard deviations for the X, Y and Z axes
    * Averages of the frequency body acceleration jerk means for the X, Y and Z axes
    * Averages of the frequency body acceleration jerk standard deviations for the X, Y and Z axes
    * Averages of the frequency body angular velocity means for the X, Y and Z axes
    * Averages of the frequency body angular velocity standard deviations for the X, Y and Z axes
    * Averages of the frequency body acceleration magnitude means and standard deviations
    * Averages of the frequency body acceleration jerk magnitude means and standard deviations
    * Averages of the frequency body angular velocity magnitude means and standard deviations
    * Averages of the frequency body angular velocity jerk magnitude means and standard deviations

Since the input data is for the acceleration signals for the X, Y and Z axes in standard gravity units 'g' the same applies to the corresponding calculated averages.
Since the input data for angular velocity is measured in units of radians/second the same applies to the corresponding calculated averages.
Since the features have been normalized and bounded within [-1,1] the same applies to the corresponding calculated averages.

## Performed transformations 
### Introduction
The R script "run_analysis.R" contains the required transformations as indicated in the project description
to go from the downloaded data to the required tidy data set that is ready for analysis purposes.
In the following sections each transformation step is explained.

The "run_analysis.R" script contains one control function "createTidyDataSet()" that needs to be run to
execute the script. This control function calls various helper functions contained within the same R script. It starts out by checking
whether the environment is as expected (expected files/folders for the script are present and the required library is loaded). This happens in "validateCorrectEnvironment()". The script ends with the tidy data set data frame being returned. In between the various transformations steps are performed which will be explained in the next sections.  

    createTidyDataSet <- function() {
      ## First we validate the basic environment needed for this script.
      validateCorrectEnvironment()
      
      ....
       
      ## For convenience we also return the tidy data set...
      return(tidyDataSet)
    }

### 1. Merge the training and the test sets to create one data set
In the createTidyDataSet() control function the following code snippet performs this transformation:

      ## Next we read in the testData and the trainData.
      print("Reading in testData set...")
      testData <- readInTestData()
      print("Reading in trainData set...")
      trainData <- readInTrainData()
      
      ## Step 1 of the assignment "Merge the training and the test set to create 
      ## one data set" can now be performed.
      print("Step 1 of the assignment: merging the training and test set...")
      mergedData <- rbind(testData, trainData)
  
  The helper function readInTestData() is described below:
  
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

The helper function readInTrainData() is described below:

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

### 2. Extracts only the measurements on the mean and standard deviation for each measurement
In the createTidyDataSet() control function the following code snippet performs this transformation:

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
      
The helper function "determineRelevantFeatures()" works as follows:

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

### 3. Use descriptive activity names to name the activities in the data set
In the createTidyDataSet() control function the following code snippet performs this transformation:

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

The helper function "readInActivityLabels() works as follows:

      readInActivityLabels <- function() {
        ## The file "activity_labels.txt" contains the activity labels related to 
        ## each activity identifier.
        activities <- read.table("activity_labels.txt", header=FALSE)
      }

### 4. Appropriately label the data set with descriptive variable names.
In the createTidyDataSet() control function the following code snippet performs this transformation:

      print("Step 4 of the assignment: appropriately label the data set with descriptive variable names...")
      ## When we determined the relevant features we also saved the feature names as
      ## contained in the "features.txt" file. So we use those feature names
      ## to turn the column names into descriptive variable names.
      relevantFeatureNames <- createDescriptiveFeatureNames(relevantFeatures[which(relevantFeatures$relevant == TRUE),c("V2")])
      ## We just name the first and second column "Subject" and "Activity" and as
      ## the other column names we used the "features.txt" feature names. 
      descriptiveVariableNames <- c("Subject", "Activity", as.vector(relevantFeatureNames))
      names(mergedData) <- descriptiveVariableNames

The helper function "createDescriptiveFeatureNames()" works as follows:

    createDescriptiveFeatureNames <- function(features) {
      ## The feature names as stated in the "features.txt" file are rather
      ## cryptic, so we transform the names. Even though the preference
      ## is to have all lower case variable names, I feel these long names
      ## make it less readable. So I opt for camel case.
      
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

### 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject
In the createTidyDataSet() control function the following code snippet performs this transformation:

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

The helper function "addAverageToVariableNames()" works as follows:

    addAverageToVariableNames <- function(variableNames) {
      variableNames <- sub(pattern = "^time", replacement = "averageTime", x = variableNames)
      variableNames <- sub(pattern = "^frequency", replacement = "averageFrequency", x = variableNames)
    }