# getting_cleaning_data_assignment

Data Science specialization on Coursera - Course "Getting and Cleaning Data" - Assignment

## Datasets

- Data were downloaded from UCI Machine Learning website :(https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

- Information about the initial experiment can be found here : (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Goal

The purpose of this assignment was to demonstrate our ability to collect, work with, and clean a data set. The goal was to prepare tidy data that can be used for later analysis.

## In this folder

- README.md
- code_book.txt : description of variables and 
- UCI HAR Dataset repository containing :
* README.txt file of the initial experiment. Contains information about test and train folders
* features : features in test and train datasets
* features_info : features code book
* activity_labels : describes activities
* run_analysis.R : script to perform the analysis
* summarized_dataset_table.txt : tidy data corresponding of one summary for all the measurements per subject and activity
* index_summarized_data.txt : list column names in summarized_dataset_table.txt

## How to run
In order to get the summarized dataset 
1- download this repo
2- Go at the root of this repo in R with setwd() command
3- source(run_analysis.R)
4-  run the script with run_analysis()
