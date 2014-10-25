### Tidy Data Set Code Book: Human Activity Recognition Using Smartphones Dataset

The tidy data set contains records for 30 subjects. Each subject has 6 records corresponding to each of the activities 

* LAYING
* SITTING
* STANDING
* WALKING
* WALKING_DOWNSTAIRS
* WALKING_UPSTAIRS

Each subject-activity has 86 features corresponding to the mean and standard deviation measurements. The tidy data set contains the average of each feature for each subject-activity.

***

### Code Book

Item  | Description
------------- | -------------
`Field 1` | |
Field label:  | Subject
Variable:  | person identifier
Variable type:  | numeric
Allowable values: | [1,30]
Comments: | |
`Field 2` | |
Field label:  | Activity
Variable:  | description of activity 
Variable type:  | character
Allowable values: | LAYING
| | SITTING
| | STANDING
| | WALKING
| | WALKING_DOWNSTAIRS
| | WALKING_UPSTAIRS
Comments: | |
`Field 3 - 88` | |
Field label:  | **refer to feature list below**
Variable:  | average value of the feature
Variable type:  | numeric
Allowable values: | [-1,1]
Comments: | Each item in the feature list below corresponds to a field. 
| | All 86 features are numeric and bounded within [-1,1]. |
| | |


### Feature List
Field No. | Feature Field Label
------------- | -------------
3 | tBodyAcc-mean()-X  
4 | tBodyAcc-mean()-Y	
5 | tBodyAcc-mean()-Z	
6 | tBodyAcc-std()-X	
7 | tBodyAcc-std()-Y	
8 | tBodyAcc-std()-Z	
9 | tGravityAcc-mean()-X
10 | tGravityAcc-mean()-Y
11 | tGravityAcc-mean()-Z
12 | tGravityAcc-std()-X
13 | tGravityAcc-std()-Y
14 | tGravityAcc-std()-Z
15 | tBodyAccJerk-mean()-X
16 | tBodyAccJerk-mean()-Y
17 | tBodyAccJerk-mean()-Z
18 | tBodyAccJerk-std()-X
19 | tBodyAccJerk-std()-Y
20 | tBodyAccJerk-std()-Z
21 | tBodyGyro-mean()-X
22 | tBodyGyro-mean()-Y
23 | tBodyGyro-mean()-Z
24 | tBodyGyro-std()-X
25 | tBodyGyro-std()-Y
26 | tBodyGyro-std()-Z
27 | tBodyGyroJerk-mean()-X
28 | tBodyGyroJerk-mean()-Y
29 | tBodyGyroJerk-mean()-Z
30 | tBodyGyroJerk-std()-X
31 | tBodyGyroJerk-std()-Y
32 | tBodyGyroJerk-std()-Z
33 | tBodyAccMag-mean()
34 | tBodyAccMag-std()
35 | tGravityAccMag-mean()
36 | tGravityAccMag-std()
37 | tBodyAccJerkMag-mean()
38 | tBodyAccJerkMag-std()
39 | tBodyGyroMag-mean()
40 | tBodyGyroMag-std()
41 | tBodyGyroJerkMag-mean()
42 | tBodyGyroJerkMag-std()
43 | fBodyAcc-mean()-X
44 | fBodyAcc-mean()-Y
45 | fBodyAcc-mean()-Z
46 | fBodyAcc-std()-X
47 | fBodyAcc-std()-Y
48 | fBodyAcc-std()-Z
49 | fBodyAcc-meanFreq()-X
50 | fBodyAcc-meanFreq()-Y
51 | fBodyAcc-meanFreq()-Z
52 | fBodyAccJerk-mean()-X
53 | fBodyAccJerk-mean()-Y
54 | fBodyAccJerk-mean()-Z
55 | fBodyAccJerk-std()-X
56 | fBodyAccJerk-std()-Y
57 | fBodyAccJerk-std()-Z
58 | fBodyAccJerk-meanFreq()-X
59 | fBodyAccJerk-meanFreq()-Y
60 | fBodyAccJerk-meanFreq()-Z
61 | fBodyGyro-mean()-X
62 | fBodyGyro-mean()-Y
63 | fBodyGyro-mean()-Z
64 | fBodyGyro-std()-X
65 | fBodyGyro-std()-Y
66 | fBodyGyro-std()-Z
67 | fBodyGyro-meanFreq()-X
68 | fBodyGyro-meanFreq()-Y
69 | fBodyGyro-meanFreq()-Z
70 | fBodyAccMag-mean()
71 | fBodyAccMag-std()
72 | fBodyAccMag-meanFreq()
73 | fBodyBodyAccJerkMag-mean()
74 | fBodyBodyAccJerkMag-std()
75 | fBodyBodyAccJerkMag-meanFreq()
76 | fBodyBodyGyroMag-mean()
77 | fBodyBodyGyroMag-std()
78| fBodyBodyGyroMag-meanFreq()
79 | fBodyBodyGyroJerkMag-mean()
80 | fBodyBodyGyroJerkMag-std()
81 | fBodyBodyGyroJerkMag-meanFreq()
82 | angle(tBodyAccMean,gravity)
83 | angle(tBodyAccJerkMean),gravityMean)
84 | angle(tBodyGyroMean,gravityMean)
85 | angle(tBodyGyroJerkMean,gravityMean)
86 | angle(X,gravityMean)
87 | angle(Y,gravityMean)
88 | angle(Z,gravityMean)



***
###Feature Selection 

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

`In tidy data set`

* mean(): Mean value
* std(): Standard deviation
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency

`Not in tidy data set`

* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* maxInds(): index of the frequency component with largest magnitude
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.  
  

`In tidy data set`

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean


The complete list of variables of each feature vector is available in 'features.txt' found in the raw data package.

