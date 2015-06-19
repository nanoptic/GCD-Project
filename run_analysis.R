getData = function() {
  message("Checking and if needed, creating data directory.")
  if (!file.exists("data")) {
    dir.create("data")
  }
  if (!file.exists("data/UCI HAR Dataset")) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipfile="data/UCI_HAR_data.zip"
    download.file(fileURL, destfile=zipfile, method="curl")
    unzip(zipfile, exdir="data")
  }
}

mergeData = function() {
  message("Reading input data files.")
  training.x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
  training.y <- read.table("data/UCI HAR Dataset/train/y_train.txt")
  training.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
  test.x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
  test.y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
  test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")

  merged.x <- rbind(training.x, test.x)
  merged.y <- rbind(training.y, test.y)
  merged.subject <- rbind(training.subject, test.subject)
  list(x=merged.x, y=merged.y, subject=merged.subject)
}

setNames = function(df) {
  message("Providing descriptive activity names.")
  colnames(df) <- "activity"
  df$activity[df$activity == 1] = "WALKING"
  df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
  df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
  df$activity[df$activity == 4] = "SITTING"
  df$activity[df$activity == 5] = "STANDING"
  df$activity[df$activity == 6] = "LAYING"
  df
}

getStats = function(df) {
  message("Calculating mean and standard deviation.")
  # Read the features file
  features <- read.table("data/UCI HAR Dataset/features.txt")
  mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
  std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))

  edf <- df[, (mean.col | std.col)]
  colnames(edf) <- features[(mean.col | std.col), 2]
  edf
}


library(plyr)
getData()
merged <- mergeData()
mx <- getStats(merged$x)
my <- setNames(merged$y)
colnames(merged$subject) <- c("subject")
combined <- cbind(mx, my, merged$subject)
tidy <- ddply(combined, .(subject, activity), function(x) colMeans(x[,1:60]))
message("Writing output file.")
write.table(tidy, "UCI_HAR_TIDY.txt", row.names=FALSE)
