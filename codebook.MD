# Codebook for Getting and Cleaning Data Course Project
## May 23, 2014

### The objective of this project is to produce an analysis ready dataset containing only mean and standard deviation information using existing data from the Human Activity Recognition Using 
### Smartphones Dataset. 

## Study Design

### Data used in the course project came from Human Activity Recognition Using Smartphones Dataset created by Smartlab - Non Linear Complex Systems Laboratory
### The experiments was carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities 
### (walking, walking upstairs, walking downstairs, sitting, standing, and laying) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its 
### embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have 
### been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for 
### generating the training data and 30% the test data. For each record, there is a 561-feature vector with time and frequency domain variables. 

### The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap 
### (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body 
### acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, 
### a vector of features was obtained by calculating variables from the time and frequency domain.  

### The data was broken down into several smaller datasets:

* 'features.txt': List of all features.
* 'activity_labels.txt': Links the class labels with their activity name.
* 'train/X_train.txt': Training set.
* 'train/y_train.txt': Training labels.
* 'test/X_test.txt': Test set.
* 'test/y_test.txt': Test labels.
* 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
* 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

### There were two additional informative files that helped explain the data:

* 'README.txt': provided the background of the data
* 'features_info.txt": explained the creation of the variables in the original dataset

### The following is the original explanation of variable creation and naming scheme:

##Feature Selection 

### The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were 
### captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, 
### the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency 
### of 0.3 Hz. 

### Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional 
### signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

### Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' 
### to indicate frequency domain signals). 

### These signals were used to estimate variables of the feature vector for each pattern:  
### '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

*tBodyAcc-XYZ
*tGravityAcc-XYZ
*tBodyAccJerk-XYZ
*tBodyGyro-XYZ
*tBodyGyroJerk-XYZ
*tBodyAccMag
*tGravityAccMag
*tBodyAccJerkMag
*tBodyGyroMag
*tBodyGyroJerkMag
*fBodyAcc-XYZWi
*fBodyAccJerk-XYZ
*fBodyGyro-XYZ
*fBodyAccMag
*fBodyAccJerkMag
*fBodyGyroMag
*fBodyGyroJerkMag

### The set of variables that were estimated from these signals are: 

*mean(): Mean value
*std(): Standard deviation
*mad(): Median absolute deviation 
*max(): Largest value in array
*min(): Smallest value in array
*sma(): Signal magnitude area
*energy(): Energy measure. Sum of the squares divided by the number of values. 
*iqr(): Interquartile range 
*entropy(): Signal entropy
*arCoeff(): Autorregresion coefficients with Burg order equal to 4
*correlation(): correlation coefficient between two signals
*maxInds(): index of the frequency component with largest magnitude
*meanFreq(): Weighted average of the frequency components to obtain a mean frequency
*skewness(): skewness of the frequency domain signal 
*kurtosis(): kurtosis of the frequency domain signal 
*bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
*angle(): Angle between to vectors.

### Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

*gravityMean
*tBodyAccMean
*tBodyAccJerkMean
*tBodyGyroMean
*tBodyGyroJerkMean

## Data Creation

### All data manipulation was handled in R 3.0.2. 

### First, the training and test datasets from X, Y, and Subject data were row bound together and created x.data, y.data, and sub.data respectively. Secondly, the newly created data where column bound
### in the order of x.data, sub.data, and y.data to create all.data that contained all features, subject, and activity information. The all.data columns were named using the features.txt table. The last two
### columns containing the subject and activity information respectively, had to be named manual due to the features.txt only containing the names of the 561 features. Next, the column containing the 
### information on activities performed ("activities"), was recalculated as a factor variables with more descriptive names using the activity_labels.txt table. Using the GREPL command, the names of the data
### columns were searched for any names that contained either "mean" or "std" (an short hand name for standard deviation). Eighty-six columns were identified using this method and all were pulled from the 
### original data along with the activities and subject id columns. This is our subset of interest. From here, the subject id column ("subject") was converted to a factor variable. The data was used to 
### calculate the aggregate values for each of the 86 features per activity per subject, resulting in data sized 180 x 88 (6 activities x 30 subjects, by 86 grepped features plus subject and activity identifiers.)
### This aggregate data was saved in final.data. Finally, the column names of the were reworded to include more recognizable names. The methods sub and gsub were used to replace short hand names in the column
### names with complete words. Below is the replacement scheme and a list of the final variable names. Please note: All of the final column names are lowercase and without any separators as recommended in the
### course. The final.data was exported from R as a .txt table named tidydata.txt.

## Variable Naming Scheme

* "tBodyAcc" was replaced with "timebodyacceleration"
* "tGravityAcc" was replaced with "timegravityacceleration"
* "tBodyGyro" was replaced with "timebodygyroscope"
* "fBodyAcc" was replaced with "frequencybodyacceleration"
* "fBodyGyro" was replaced with "frequencybodygyroscope"
* "fBodyBodyAcc" was replaced with "frequencybodyacceleration"
* "fBodyBodyGyro" was replaced with "frequencybodygyroscope"
* "Mag" was replaced with "magnitude"
* "-meanFreq()-" was replaced with "wieghtedfreqmean"
* "-meanFreq()" was replaced with "wieghtedfreqmean"
* "angle" was replaced with "anglebetween"

## Final Variables Included in Tidy Data, Explanation of variables, and Creation Details as Applicable

* "subject" : Contains the subject ID, ranges from 1-30                                               
* "activity": Contains the activity ID, values include: laying, sitting, standing, walking, walkingdownstairs, walkingupstairs                                              
* "timebodyaccelerationmeanx": the grand-mean for time domain signals for body acceleration by person by activity in direction X                             
* "timebodyaccelerationmeany": the grand-mean for time domain signals for body acceleration by person by activity in direction Y                             
* "timebodyaccelerationmeanz": the grand-mean for time domain signals for body acceleration by person by activity in direction Z
* "timebodyaccelerationstdx" : the overall standard deviation for time domain signals for body acceleration by person by activity in direction X  
* "timebodyaccelerationstdy" : the overall standard deviation for time domain signals for body acceleration by person by activity in direction Y                            
* "timebodyaccelerationstdz" : the overall standard deviation for time domain signals for body acceleration by person by activity in direction Z
* "timegravityaccelerationmeanx" : the grand-mean for time domain signals for gravity acceleration by person by activity in direction X                           
* "timegravityaccelerationmeany" : the grand-mean for time domain signals for gravity acceleration by person by activity in direction Y                          
* "timegravityaccelerationmeanz" : the grand-mean for time domain signals for gravity acceleration by person by activity in direction Z                                
* "timegravityaccelerationstdx" : the overall standard deviation for time domain signals for gravity acceleration by person by activity in direction X                             
* "timegravityaccelerationstdy" : the overall standard deviation for time domain signals for gravity acceleration by person by activity in direction Y                           
* "timegravityaccelerationstdz" : the overall standard deviation for time domain signals for gravity acceleration by person by activity in direction Z                           
* "timebodyaccelerationjerkmeanx" : the grand-mean for time domain signals for body acceleration jerk signals by person by activity in direction X                         
* "timebodyaccelerationjerkmeany" : the grand-mean for time domain signals for body acceleration jerk signals by person by activity in direction Y                       
* "timebodyaccelerationjerkmeanz" : the grand-mean for time domain signals for body acceleration jerk signals by person by activity in direction Z                        
* "timebodyaccelerationjerkstdx"  : the overall standard deviation for time domain signals for body acceleration jerk signals by person by activity in direction X                        
* "timebodyaccelerationjerkstdy"  : the overall standard deviation for time domain signals for body acceleration jerk signals by person by activity in direction Y                         
* "timebodyaccelerationjerkstdz"  : the overall standard deviation for time domain signals for body acceleration jerk signals by person by activity in direction Z                           
* "timebodygyroscopemeanx" : the grand-mean for time domain signals for body gyroscope by person by activity in direction X                                
* "timebodygyroscopemeany" : the grand-mean for time domain signals for body gyroscope by person by activity in direction Y                              
* "timebodygyroscopemeanz" : the grand-mean for time domain signals for body gyroscope by person by activity in direction Z
* "timebodygyroscopestdx"  : the overall standard deviation for time domain signals for body gyroscope by person by activity in direction X                                 
* "timebodygyroscopestdy"  : the overall standard deviation for time domain signals for body gyroscope by person by activity in direction Y                              
* "timebodygyroscopestdz"  : the overall standard deviation for time domain signals for body gyroscope by person by activity in direction Z
* "timebodygyroscopejerkmeanx" : the grand-mean for time domain signals for body gyroscope jerk signals by person by activity in direction X                            
* "timebodygyroscopejerkmeany" : the grand-mean for time domain signals for body gyroscope jerk signals by person by activity in direction Y                           
* "timebodygyroscopejerkmeanz" : the grand-mean for time domain signals for body gyroscope jerk signals by person by activity in direction Z
* "timebodygyroscopejerkstdx"  : the overall standard deviation for time domain signals for body gyroscope jerk signals by person by activity in direction X                            
* "timebodygyroscopejerkstdy"  : the overall standard deviation for time domain signals for body gyroscope jerk signals by person by activity in direction Y                            
* "timebodygyroscopejerkstdz"  : the overall standard deviation for time domain signals for body gyroscope jerk signals by person by activity in direction Z
* "timebodyaccelerationmagnitudemean" : the grand-mean of time signal body acceleration magnitude                    
* "timebodyaccelerationmagnitudestd"  : the overall standard deviation of time signal body acceleration magnitude                     
* "timegravityaccelerationmagnitudemean" : the grand-mean of time signal gravity acceleration magnitude                   
* "timegravityaccelerationmagnitudestd" : the overall standard deviation of time signal gravity acceleration magnitude                   
* "timebodyaccelerationjerkmagnitudemean" : the grand-mean of time signal body acceleration jerk signals magnitude                  
* "timebodyaccelerationjerkmagnitudestd" : the overall standard deviation of time signal body acceleration jerk signals magnitude                  
* "timebodygyroscopemagnitudemean" : the grand-mean of time signal body gyroscope magnitude                         
* "timebodygyroscopemagnitudestd" : the overall standard deviation of time signal body gyroscope magnitude                         
* "timebodygyroscopejerkmagnitudemean" : the grand-mean of time signal body gyroscope jerk signals magnitude                      
* "timebodygyroscopejerkmagnitudestd" : the overall standard deviation of time signal body gyroscope jerk signals magnitude                     
* "frequencybodyaccelerationmeanx" : the grand-mean for frequency domain signals for body acceleration by person by activity in direction X                       
* "frequencybodyaccelerationmeany" : the grand-mean for frequency domain signals for body acceleration by person by activity in direction Y                       
* "frequencybodyaccelerationmeanz" : the grand-mean for frequency domain signals for body acceleration by person by activity in direction Z                        
* "frequencybodyaccelerationstdx"  : the overall standard deviation for frequency domain signals for body acceleration by person by activity in direction X                        
* "frequencybodyaccelerationstdy"  : the overall standard deviation for frequency domain signals for body acceleration by person by activity in direction Y                       
* "frequencybodyaccelerationstdz"  : the overall standard deviation for frequency domain signals for body acceleration by person by activity in direction Z                       
* "frequencybodyaccelerationwieghtedfreqmeanx" : the grand-mean of Weighted average of the frequency components to obtain a mean frequency in direction X           
* "frequencybodyaccelerationwieghtedfreqmeany" : the grand-mean of Weighted average of the frequency components to obtain a mean frequency in direction Y       
* "frequencybodyaccelerationwieghtedfreqmeanz" : the grand-mean of Weighted average of the frequency components to obtain a mean frequency in direction Z           
* "frequencybodyaccelerationjerkmeanx" : the grand-mean for frequency domain signals for body acceleration jerk signals by person by activity in direction X                      
* "frequencybodyaccelerationjerkmeany" : the grand-mean for frequency domain signals for body acceleration jerk signals by person by activity in direction Y                  
* "frequencybodyaccelerationjerkmeanz" : the grand-mean for frequency domain signals for body acceleration jerk signals by person by activity in direction Z
* "frequencybodyaccelerationjerkstdx"  : the overall standard deviation for frequency domain signals for body acceleration jerk signals by person by activity in direction X                     
* "frequencybodyaccelerationjerkstdy"  : the overall standard deviation for frequency domain signals for body acceleration jerk signals by person by activity in direction Y                  
* "frequencybodyaccelerationjerkstdz"  : the overall standard deviation for frequency domain signals for body acceleration jerk signals by person by activity in direction Z                   
* "frequencybodyaccelerationjerkwieghtedfreqmeanx" : the grand-mean of Weighted average of the frequency components in body acceleration and jerk signals to obtain a mean frequency in direction X        
* "frequencybodyaccelerationjerkwieghtedfreqmeany" : the grand-mean of Weighted average of the frequency components in body acceleration and jerk signals to obtain a mean frequency in direction Y       
* "frequencybodyaccelerationjerkwieghtedfreqmeanz" : the grand-mean of Weighted average of the frequency components in body acceleration and jerk signals to obtain a mean frequency in direction Z
* "frequencybodygyroscopemeanx" : the grand-mean for frequency domain signals for body gyroscope by person by activity in direction X                            
* "frequencybodygyroscopemeany" : the grand-mean for frequency domain signals for body gyroscope by person by activity in direction Y                          
* "frequencybodygyroscopemeanz" : the grand-mean for frequency domain signals for body gyroscope by person by activity in direction Z                            
* "frequencybodygyroscopestdx"  : the overall standard deviation for frequency domain signals for body gyroscope by person by activity in direction X                             
* "frequencybodygyroscopestdy"  : the overall standard deviation for frequency domain signals for body gyroscope by person by activity in direction Y                           
* "frequencybodygyroscopestdz"  : the overall standard deviation for frequency domain signals for body gyroscope by person by activity in direction Z                           
* "frequencybodygyroscopewieghtedfreqmeanx" : the grand-mean of Weighted average of the frequency components in body gyroscope and jerk signals to obtain a mean frequency in direction X               
* "frequencybodygyroscopewieghtedfreqmeany" : the grand-mean of Weighted average of the frequency components in body gyroscope and jerk signals to obtain a mean frequency in direction Y              
* "frequencybodygyroscopewieghtedfreqmeanz" : the grand-mean of Weighted average of the frequency components in body gyroscope and jerk signals to obtain a mean frequency in direction Z
* "frequencybodyaccelerationmagnitudemean" : the grand-mean of frequency signal body acceleration magnitude                   
* "frequencybodyaccelerationmagnitudestd" : the overall standard deviation of frequency signal body acceleration magnitude                
* "frequencybodyaccelerationmagnitudewieghtedfreqmean" : the grand-mean of Weighted average of the magnitude in frequency components in body acceleration
* "frequencybodyaccelerationjerkmagnitudemean"  : the grand-mean of frequency signal body acceleration jerk signals magnitude            
* "frequencybodyaccelerationjerkmagnitudestd"  : the overall standard deviation of frequency signal body acceleration jerk signals magnitude            
* "frequencybodyaccelerationjerkmagnitudewieghtedfreqmean" : the grand-mean of Weighted average of the magnitude in frequency components in body acceleration and jerk signals
* "frequencybodygyroscopemagnitudemean" : the grand-mean of frequency signal body gyroscope magnitude                   
* "frequencybodygyroscopemagnitudestd" : the overall standard deviation of frequency signal body gyroscope magnitude                   
* "frequencybodygyroscopemagnitudewieghtedfreqmean" : the grand-mean of Weighted average of the magnitude in frequency components in body gyroscope   
* "frequencybodygyroscopejerkmagnitudemean" : the grand-mean of frequency signal body gyroscope jerk signals magnitude                 
* "frequencybodygyroscopejerkmagnitudestd" : the overall standard deviation of frequency signal body gyroscope jerk signals magnitude               
* "frequencybodygyroscopejerkmagnitudewieghtedfreqmean"  : the grand-mean of Weighted average of the magnitude in frequency components in body gyroscope and jerk signals 
* "anglebetweentimebodyaccelerationmeanandgravity" : the angle between the vectors timebodyaccelerationmean and gravity         
* "anglebetweentimebodyaccelerationjerkmeanandgravitymean": the angle between the vectors timebodyaccelerationjerkmean and gravitymean
* "anglebetweentimebodygyroscopemeanandgravitymean" : the angle between the vectors timebodygyroscopemean and gravitymean
* "anglebetweentimebodygyroscopejerkmeanandgravitymean" : the angle between the vectors timebodygyroscopejerkmean and gravitymean
* "anglebetweenxandgravitymean" : the angle between the vectors x and gravitymean                         
* "anglebetweenyandgravitymean" : the angle between the vectors y and gravitymean                          
* "anglebetweenzandgravitymean" : the angle between the vectors z and gravitymean

#### Note: gravitymean is not included in the dataset
#### It is defined as gravitymean: vector obtained by averaging the signals in a signal window sample. This is solely used in the "anglebetween" variables

#### Grand-Means were calculated by taking the average of the average value for each feature, grouped by person and activity. This means, each combination of activity and person has their own
#### grand-mean. Standard deviations were calculated in a similar manner; as the average standard deviation for each feature by person and activity. 
#### Person and subject are interchangeable terms in the context. 

## Code can be found in the run_analysis.R file








