#Human Activity Recognition Using Smartphones Dataset
***
One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data in this analysis was collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

This analysis:

1. Merges the training and test data sets
1. Extracts only the mean and standard deviation measurements
1. Computes the average of each variable for each activity and each subject

***
## The raw data
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

###For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

###The dataset includes the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

###Notes: 
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

###License:
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.


***
## The tidy data set

In this analysis, the training and test data sets are merged, and only the mean and standard deviation measurements are extracted. The average of each variable for each subject-activity is being computed. The output data set is written to the file tidyData.txt. 

The tidy data comprises 88 columns and 180 rows.


***
## The code book

Refer to CodeBook.md for details on the tidy data set.


***
## The instruction script

Refer to run_analysis.R for the script to reproduce the tidy data set.

1. Download the raw data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Place the dataset folder "UCI HAR Dataset" in your working directory
3. Place the script run_analysis.R in your working directory
4. Set your working directory in R
5. Run the script run_analysis.R in R
6. Upon completion, find the output data tidyData.txt in your working directory.

### Steps in run_analysis.R

1. Set directory paths
2. Reads features from features.txt and finds rows that contain the string mean or std
3. A1 - Reads the test data set from X_test.txt and extracts only means and std columns
4. A2 - Reads test data subjects from subject_test.txt
5. A3 - Reads test data labels from y_test.txt
6. Combines A1, A2, and A3 into a data table for test data
7. B1 - Reads the train data set from X_train.txt and extracts only means and std columns
8. B2 - Reads train data subjects for subject_train.txt
9. B3 - Reads train data labels from y_train.txt
10. Combines B1, B2, and B3 into a data table for train data
11. Merges all rows in test data and train data
12. Reads activity labels from activity_labels.txt
13. Introduce ActivityDescription to the merged dataset by merging activity labels
14. Remove activity column, replace with ActivityDescription, rename it to Activity and move to 2nd column
15. Aggregates data and computes average
16. Writes tidy data to text file
