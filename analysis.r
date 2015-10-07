# install (if necessary) and load data manipulation packages
install.packages("plyr")
install.packages("dplyr")
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)

# read in the features
features <- read.table("UCI HAR Dataset\\features.txt")
# read in the test data
test_x <- read.table("UCI HAR Dataset\\test\\X_test.txt")
test_y <- read.table("UCI HAR Dataset\\test\\y_test.txt")
test_subjects <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
train_x <- read.table("UCI HAR Dataset\\train\\X_train.txt")
train_y <- read.table("UCI HAR Dataset\\train\\y_train.txt")
train_subjects <- read.table("UCI HAR Dataset\\train\\subject_train.txt")

# 1) merge the training and test sets to create one data set
combined_data <- rbind(test_x, train_x)
combined_y <- rbind(test_y, train_y)
combined_subjects <- rbind(test_subjects, train_subjects)

# apply the features V2 column as the headers for the combined_data data frame
# clean up the names first to remove hyphens and parentheses
listOfNames <- as.character(features$V2)
listOfNames <- make.names(listOfNames, unique=TRUE)
names(combined_data) <- listOfNames

# add combined_y and combined_subjects as the first columns in the combined_data data frame
combined_data <- cbind(combined_y, combined_data)
names(combined_subjects) = c('subjects')
combined_data <- cbind(combined_subjects, combined_data)

# 2) extract only the measurements on the mean and standard deviation for each measurement
sub_data <- select(combined_data, subjects, V1, contains(".mean."), contains(".std."))

# convert the subject and activities columns to factors
sub_data$subjects <- as.factor(sub_data$subjects)
sub_data$V1 <- as.factor(sub_data$V1)

# 3) use descriptive activity names to name the activites in the data set
sub_data$V1 <- mapvalues(sub_data$V1, from = c("1", "2", "3", "4", "5", "6"), 
                         to = c("Walking", "WalkingUpStairs", "WalkingDownStairs", "Sitting", "Standing", "Lying"))

# 4) appropriately label the data set with descriptive variable names
# (See codebook.md for an explanation of the variable names.)
names(sub_data) <- str_replace_all(names(sub_data), "[.][.]", "")
names(sub_data) <- str_replace_all(names(sub_data), "BodyBody", "Body")
names(sub_data) <- str_replace_all(names(sub_data), "tBody", "Body")
names(sub_data) <- str_replace_all(names(sub_data), "fBody", "FFTBody")
names(sub_data) <- str_replace_all(names(sub_data), "tGravity", "Gravity")
names(sub_data) <- str_replace_all(names(sub_data), "fGravity", "FFTGravity")
names(sub_data) <- str_replace_all(names(sub_data), "Acc", "Acceleration")
names(sub_data) <- str_replace_all(names(sub_data), "Gyro", "AngularVelocity")
names(sub_data) <- str_replace_all(names(sub_data), "Mag", "Magnitude")
for(i in 3:68) {if (str_detect(names(sub_data)[i], "[.]std")) 
                {names(sub_data)[i] <- paste0("StandardDeviation", str_replace(names(sub_data)[i], "[.]std", ""))}}
for(i in 3:68) {if (str_detect(names(sub_data)[i], "[.]mean")) 
                {names(sub_data)[i] <- paste0("Mean", str_replace(names(sub_data)[i], "[.]mean", ""))}}
names(sub_data) <- str_replace_all(names(sub_data), "[.]X", "XAxis")
names(sub_data) <- str_replace_all(names(sub_data), "[.]Y", "YAxis")
names(sub_data) <- str_replace_all(names(sub_data), "[.]Z", "ZAxis")

# 5) from the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# Use a split/apply/combine method. First, split the data by the subject and activity factors.
split_set <- split(select(sub_data, 3:68), list(sub_data$subjects, sub_data$V1))
# Next, use lapply to iterate over each item in the resulting list, and use apply to calculate the mean of each column.
mean_set <- lapply(split_set, function(x) apply(x, 2, mean, na.rm=TRUE))
# The output from lapply is a list. Convert this back to a data frame.
tidy_set <- data.frame(t(sapply(mean_set,c)))
# The subject and activity factors are still combined, and are now row names instead of columns. Split them 
# using strsplit, then add them to a separate data frame that can be combined with the tidy data set using cbind.
factors <- data.frame(t(sapply(strsplit(rownames(tidy_set), "[.]"),c)))
tidy_set <- cbind(factors, tidy_set)
# Give the subject and activity columns friendly names, and convert them to factors.
tidy_set <- dplyr::rename(tidy_set,TestSubject = X1, Activity = X2)
tidy_set$TestSubject <- as.factor(tidy_set$TestSubject)
tidy_set$Activity <- as.factor(tidy_set$Activity)
rownames(tidy_set) <- NULL

# DATA VERIFICATION - "manually" generate a couple test variables to verify that the calculated average values are correct.
# pull all of the data for subject = 1, activity = walking, variable = tBodyAcc.mean...X
test_set <- select(filter(sub_data, V1=="Walking" & subjects==1), MeanBodyAccelerationXAxis)
# calculate the mean, and compare it to the same calculation from the result set.
tidy_set_val <- select(filter(tidy_set, TestSubject==1 & Activity=="Walking"), MeanBodyAccelerationXAxis)$MeanBodyAccelerationXAxis
result <- all.equal(mean(test_set$MeanBodyAccelerationXAxis), tidy_set_val)
print("Data calculation verification--TRUE indicates the verification passed:")
print(result)

# second verification, with data from the middle of the matrix
test_set <- select(filter(sub_data, V1=="Sitting" & subjects==5), StandardDeviationFFTBodyAccelerationXAxis)
tidy_set_val <- select(filter(tidy_set, TestSubject==5 & Activity=="Sitting"), StandardDeviationFFTBodyAccelerationXAxis)$StandardDeviationFFTBodyAccelerationXAxis
result <- all.equal(mean(test_set$StandardDeviationFFTBodyAccelerationXAxis), tidy_set_val)
print(result)

# write the tidy data set to a file for project submission
write.table(tidy_set, "tidy_data_set.txt", row.names=FALSE)

# Environment clean up - tidy up the global environment by removing temporary variables to save memory
rm(combined_data)
rm(combined_subjects)
rm(combined_y)
rm(factors)
rm(features)
rm(sub_data)
rm(test_subjects)
rm(test_x)
rm(test_y)
rm(train_subjects)
rm(train_x)
rm(train_y)
rm(listOfNames)
rm(mean_set)
rm(split_set)
