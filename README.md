Getting And Cleaning Data
==========================

This repo contains `run_analysis.R`, and its codebook `CodeBook.md`.
> The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

Data Source
-----------
The data for the project is obtained from
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The R script will automatically download and work with the file.

Guidelines
----------
> You should create one R script called `run_analysis.R` that does the following. 
> 
> 1. Merges the training and the test sets to create one data set.
> 2. Extracts only the measurements on the mean and standard deviation for each measurement.
> 3. Uses descriptive activity names to name the activities in the data set.
> 4. Appropriately labels the data set with descriptive activity names.
> 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Steps to run this project
-------------------
1. Download `run_analysis.R` and `pkgCkeck.R` scripts to the working directory of R.
2. > source("run_analysis.R")
3. > run_analysis()

Output
------
A tidy data set `HARUsingSmartphones.txt` will be generated.

CodeBook.md
---------------
This file is the CodeBook for `HARUsingSmartphones.txt` generated using the R script `run_analysis.R`
