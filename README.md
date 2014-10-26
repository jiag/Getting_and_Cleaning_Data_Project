# Getting and Cleaning Data Project

## Project Description
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## What you c
CodeBook.md: explain how the analysis is performed
Readme.md: this file, describing how to use files in the repo
run_analysis.R: R script contains code to create a tidy data set

## How to create the required tidy data set
- Create a empty folder called ```project``` and download the data from provided URL. Put the  ```UCI HAR Dataset``` folder into the ```project``` folder
- Download the ```run_analysis.R``` file and put it in the ```project``` folder. Set you working directory as ```~/pathtoprojectfolder/project```.
- Source the ```run_analysis.R``` file(the ```plyr``` package is required) and the tidy_data.txt file containing the tidy data set will be created in the ```project``` folder.