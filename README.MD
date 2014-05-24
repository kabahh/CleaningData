# ReadME for Getting and Cleaning Data Course Project
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

## Code can be found in the run_analysis.R file

