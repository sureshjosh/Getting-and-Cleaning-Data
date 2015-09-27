library(dplyr)

getfiles <- function(dir, source, dest) {
  if (!file.exists(dir)) {
    dir.create(dir)
  }
  download.file(
    source, dest, 
    method = "curl"
  )
  unzip(dest, exdir = dir)
  list.files(path = dir)  
}

# create project directory and unzip the files
getfiles(
  'project','https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', './project/Dataset.zip'
)

#read activity level 
activity_labels <-
  read.table('project/UCI HAR Dataset/activity_labels.txt')
activity_labels <- as.character(activity_labels[,2])

#read train data set 
train_ds <- read.table('project/UCI HAR Dataset/train/X_train.txt')
train_activity <-
  read.table('project/UCI HAR Dataset/train/y_train.txt')
train_subject <-
  read.table('project/UCI HAR Dataset/train/subject_train.txt')

#read test data set
test_ds <- read.table('project/UCI HAR Dataset/test/X_test.txt')
test_activity <-
  read.table('project/UCI HAR Dataset/test/y_test.txt')
test_subject <-
  read.table('project/UCI HAR Dataset/test/subject_test.txt')

#read variable description 
variables <- read.table('project/UCI HAR Dataset/features.txt')
var_names <- as.character(variables[,2])

#convert activity into factor variable and label it with activity level
train_activity <-
  train_activity %>% rename(activity = V1) %>% mutate(activity = factor(activity, labels = activity_labels))
test_activity <-
  test_activity %>% rename(activity = V1) %>% mutate(activity = factor(activity, labels = activity_labels))

#convert subject into factor variable
train_subject <-
  train_subject %>% rename(subject = V1) %>% mutate(subject = factor(subject))
test_subject <-
  test_subject %>% rename(subject = V1) %>% mutate(subject = factor(subject))


#find only the measurements on the mean and standard deviation for each measurement
std_mean_variables <-
  grep('mean|std', var_names, value = T, ignore.case = T)

#add description of the variable
setnames(train_ds,var_names)
setnames(test_ds,var_names)

# Extract only the measurements on the mean and standard deviation for each measurement
train_ds <- train_ds[,std_mean_variables]
test_ds <- test_ds[,std_mean_variables]

#merge activity and subject into data set
train_ds <- cbind(train_activity, train_subject, train_ds)
test_ds <- cbind(test_activity, test_subject, test_ds)

#combine test and training datasets
ds <- rbind(train_ds, test_ds)

#creates a data set 'subject_averages' with the average of each variable for each activity and each subject
subject_averages <-
  ds %>% group_by(activity,subject) %>%  summarise_each(funs(mean))
write.table(subject_averages, file = "subject_averages.txt", row.name = FALSE)



