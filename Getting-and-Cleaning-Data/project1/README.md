# CourseraDSS-GACD-Quizzes
Coursera Data Science Specialization, Getting and Cleaning Data Quiz Solutions

Synopsis

This repository implements solutions for the course project for the Coursera Course Getting and Cleaning Data.

Code Example
run_analysis.R:  This script takes no input variables and expects only the working directory to be set (via setwd())
1. If the download file (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) doesn’t exist in the working directory it will download it and check against a known hash code for version control.  The specifically desired files for the test and train datasets are extracted into data frames and combined into a single data frame.
2. Only the feature variables including *mean() and *std() in the variable name are kept.
3. Descriptive names are given to each activity.
4. Column names are cleaned up to remove “_” and “()” which are difficult for processing.
5. A tidy data frame is created according to the following:
	—This is a wide data frame with column names for each of the relevant features
	—For each subject and activity combination the features column is averaged and collapsed to a mean.

The resulting summary data frame is outputed to a local file in the working directory
