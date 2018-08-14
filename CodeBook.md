Coursera - Getting & Cleaning Data
Course Project

08/13/2018

Marc Reitz

Based on the original work of Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.  Full citation is as follows:

==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 



The intent of this analysis is to refine my R programming skills by reducing the original 561 attribute data frame to a smaller data frame of length 68 comprised of:

======================================

- A combined data set comprised of the original "test" and "train" data sets.
- A subset of the original measurements that directly cited either mean or standard deviation in the title of their column
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'Question_5_Answer.txt':  A tidy data set that includes the average of each variable for each activity and each subject.

- 'run_analysis.R':  A R script that:
    - Downloads and unzips the data set from the source
    - Reads in all relevent data files and labels.
    - Merges the test and training files
    - Extracts only the measurements of the mean and standard deviation.
    - Relabels the data set with descriptive names
    - Creates the tidy data file referred to above.

- 'CodeBook.md':  This file

- 'Readme.md':  A description of the repo and details process used to construct the analysis.


=================================
Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


As noted previously, the list of columns included in the tidy data set were reduced to focus on only the Mean and Standard Deviation metrics.  The attributes included were:

Subjects  - identifier of the individual being tested
Activity  - activity being performed at the time

Measures:
tBodyAcc-Mean-X
tBodyAcc-Mean-Y
tBodyAcc-Mean-Z
tBodyAcc-Standard_Deviation-X
tBodyAcc-Standard_Deviation-Y
tBodyAcc-Standard_Deviation-Z
tGravityAcc-Mean-X
tGravityAcc-Mean-Y
tGravityAcc-Mean-Z
tGravityAcc-Standard_Deviation-X
tGravityAcc-Standard_Deviation-Y
tGravityAcc-Standard_Deviation-Z
tBodyAccJerk-Mean-X
tBodyAccJerk-Mean-Y
tBodyAccJerk-Mean-Z
tBodyAccJerk-Standard_Deviation-X
tBodyAccJerk-Standard_Deviation-Y
tBodyAccJerk-Standard_Deviation-Z
tBodyGyro-Mean-X
tBodyGyro-Mean-Y
tBodyGyro-Mean-Z
tBodyGyro-Standard_Deviation-X
tBodyGyro-Standard_Deviation-Y
tBodyGyro-Standard_Deviation-Z
tBodyGyroJerk-Mean-X
tBodyGyroJerk-Mean-Y
tBodyGyroJerk-Mean-Z
tBodyGyroJerk-Standard_Deviation-X
tBodyGyroJerk-Standard_Deviation-Y
tBodyGyroJerk-Standard_Deviation-Z
tBodyAccMag-Mean
tBodyAccMag-Standard_Deviation
tGravityAccMag-Mean
tGravityAccMag-Standard_Deviation
tBodyAccJerkMag-Mean
tBodyAccJerkMag-Standard_Deviation
tBodyGyroMag-Mean
tBodyGyroMag-Standard_Deviation
tBodyGyroJerkMag-Mean
tBodyGyroJerkMag-Standard_Deviation
fBodyAcc-Mean-X
fBodyAcc-Mean-Y
fBodyAcc-Mean-Z
fBodyAcc-Standard_Deviation-X
fBodyAcc-Standard_Deviation-Y
fBodyAcc-Standard_Deviation-Z
fBodyAccJerk-Mean-X
fBodyAccJerk-Mean-Y
fBodyAccJerk-Mean-Z
fBodyAccJerk-Standard_Deviation-X
fBodyAccJerk-Standard_Deviation-Y
fBodyAccJerk-Standard_Deviation-Z
fBodyGyro-Mean-X
fBodyGyro-Mean-Y
fBodyGyro-Mean-Z
fBodyGyro-Standard_Deviation-X
fBodyGyro-Standard_Deviation-Y
fBodyGyro-Standard_Deviation-Z
fBodyAccMag-Mean
fBodyAccMag-Standard_Deviation
fBodyBodyAccJerkMag-Mean
fBodyBodyAccJerkMag-Standard_Deviation
fBodyBodyGyroMag-Mean
fBodyBodyGyroMag-Standard_Deviation
fBodyBodyGyroJerkMag-Mean
fBodyBodyGyroJerkMag-Standard_Deviation





Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

