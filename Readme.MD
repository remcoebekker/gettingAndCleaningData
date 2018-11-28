# Readme for the Getting and cleaning data course project
Author: Remco Bekker  
Date:   November 28th, 2018  

## Introduction
This readme is the starting point for my elaboration of the final project for the "Getting and cleaning data" course which is part of the
"Data Science Specialization" track.

This readme is accompanied by the following 3 files:

* CodeBook ("CodeBook.MD") in which the following is explained:
    * It is explained what the project entails
    * A description of the data both (both the input data as well as the output data)
    * An elaborate explanation of the transformations that are performed by the run analysis R script to get from the input data to the output data
* Run analysis R script ("run_analysis.R") that contains the actual code for performing the the transformations to get from the input data to the output data. This script uses the input data (which can be downloaded using the download file for analysis R script) to product an output file named "tidy_data.txt".
* Download file for analysis R script ("download_file_for_analysis.R") that helps in downloading the input data file and setting up the environment needed for running the run analysis R script. 

In the following the sections the various steps that need to be undertaken to run the run analysis R script are explained.

## Step 1 Setting the working directory
The dowload file for analysis R script creates a folder that will contain the input data. Please first set the working directory to whatever folder you want this folder to be created in. 

For instance you could set the working directory as follows:

    > getwd()
    [1] "/Users/rbekker/Documents/OU/Data science specialization"
    > setwd("./03 Getting and cleaning data")

## Step 2 Running the download file for analysis R script
Once the working directory has been set to the desired location you can run the download file for analysis R script to download the data file and set up the environment. In order to run it you first have to source the script into your R environment. Depending on where the script resides on your machine you could source it like this:

    > source('~/Documents/OU/Data science specialization/03 Getting and cleaning data/download_file_for_analysis.R')

The download file for analysis R script contains one function called "downloadFileForAnalysis()" which takes no parameters.
Now you can run this script like this:

    > downloadFileForAnalysis()
    [1] "Checking if a 'projectData' folder already exists..."
    [1] "It doesn't exist yet, so we create it..."
    [1] "One moment, downloading the zip file with the data into the 'projectData' folder..."
    trying URL 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    Content type 'application/zip' length 62556944 bytes (59.7 MB)
    ==================================================
    downloaded 59.7 MB
    
    [1] "The working directory is set to the 'projectData' folder..."
    [1] "And the data zip file is unzipped into the 'projectData' folder..."
    [1] "Within the 'projectData' folder a new folder called 'UCI HAR Dataset' has been created by unzipping."
    [1] "As a last step the working directory is changed to this folder as the run analysis R script expects that as the working directory."
    > getwd()
    [1] "/Users/rbekker/.Trash/projectData 19.49.24/UCI HAR Dataset/projectData/UCI HAR Dataset"

## Step 3 Checking whether the required library has been installed
The run analysis R script requires the "stringr" package to be installed. You can check with the following command whether the package
has already been installed:

    > "stringr" %in% installed.packages()[, "Package"]
    [1] TRUE

If the package has not been installed yet, then please do so. Don't worry about loading the library after installing it, since the 
run analysis script will take care of this.

## Step 4 Read the CodeBook 
Now that the environment has been set-up, it's time to understand what the run analysis script will actually do. Please refer to the CodeBook for this.

## Step 5 Run the run analysis R script
Now that the environment has been set-up and you have read the CodeBook to see what the input data is and how the run analysis R script will
produce the tidy data output text file. In order to be able to run it, you first need to source the run analysis R script into you R environment. Depending on where the script resides on your machine you could source it like this:

    > source('~/Documents/OU/Data science specialization/03 Getting and cleaning data/UCI HAR Dataset/run_analysis.R')

The run analysis R script contains one control function called "createTidyDataSet()" which takes no parameters. This function calls various other
helper functions which are also located inside the run analysis R script. 

Now you can run this script like this:

    > tidyData <- createTidyDataSet()
    [1] "Reading in testData set..."
    [1] "Reading in trainData set..."
    [1] "Step 1 of the assignment: merging the training and test set..."
    [1] "Step 2 of the assignment: extract only the measurement on the mean and standard deviation."
    [1] "Step 3 of the assignment: use descriptive activity names to name the activities in the data set..."
    [1] "Step 4 of the assignment: appropriately label the data set with descriptive variable names..."
    [1] "Step 5 of the assignment: create a second, independent tidy data set with the average of each variable for each activity and each subject."
    [1] "Writing the tidy data set into the file 'tidy_data.txt'..."

## Step 6 Inspect the tidy data set produced
The run analysis R script has produced in the working directory (that is inside './projectData/UCI HAR Dataset') an output file
called 'tidy_data.txt'. You can inspect this file. Also for convenience the tidyData data set is also returned by the run analysis R script so you can assign the tidyData data frame to a variable and immediately inspect the data frame from your R environment. 
In the previous step the output of the createTidyDataSet() has been assigned to a variable named tidyData.


