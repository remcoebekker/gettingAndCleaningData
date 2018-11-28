downloadFileForAnalysis <- function() {
  ## First we try to create a "projectData" folder if it doesn't exist already.
  print("Checking if a 'projectData' folder already exists...")
  if (!dir.exists("projectData")) {
    print("It doesn't exist yet, so we create it...")
    dir.create("projectData")
  }
  
  ## Next we download the zip file with the data...
  print("One moment, downloading the zip file with the data into the 'projectData' folder...")
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./projectData/data.zip")
  
  ## Then we change the working directory to the "projectData" folder just created.
  print("The working directory is set to the 'projectData' folder...")
  setwd("./projectData")
  
  ## Now we unzip the downloaded file.
  print("And the data zip file is unzipped into the 'projectData' folder...")
  unzip(zipfile = "data.zip")
  
  ## last step is changing the working directory to the "UCI HAR Dataset" folder to which the unzipping has taken place.
  print("Within the 'projectData' folder a new folder called 'UCI HAR Dataset' has been created by unzipping.")
  print("As a last step the working directory is changed to this folder as the run analysis R script expects that as the working directory.")
  setwd("./UCI HAR Dataset")
}