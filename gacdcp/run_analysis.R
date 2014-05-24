# Coursera Getting and Cleaning Data Course Project

temp_train <- read.table("train/X_train.txt")

temp_test <- read.table("test/X_test.txt")

X_col <- rbind(temp_train, temp_test)

temp_train <- read.table("train/subject_train.txt")

temp_test <- read.table("test/subject_test.txt")

subject_col <- rbind(temp_train, temp_test)

temp_train <- read.table("train/y_train.txt")

temp_test <- read.table("test/y_test.txt")

Y_col <- rbind(temp_train, temp_test)

features <- read.table("features.txt")

index <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

X_col <- X_col[, index]

nom(X_col) <- features[index, 2]

nom(X_col) <- gsub("\\(|\\)", "", nom(X_col))

nom(X_col) <- tolower(nom(X_col))  

activities <- read.table("activity_labels.txt")

activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))

Y_col[,1] = activities[Y_col[,1], 2]

nom(Y_col) <- "activity"

nom(subject_col) <- "subject"

clean <- cbind(subject_col, Y_col, X_col)

write.table(clean, "merged_data.txt")

subjs = unique(subject_col)[,1]

numsubjs = length(unique(subject_col)[,1])

numacts = length(activities[,1])

cols = dim(clean)[2]

averaged = clean[1:(numsubjs*numacts), ]

rowindex = 1

for (k in 1:numsubjs) {

	for (a in 1:numacts) {

		averaged[rowindex, 1] = subjs[k]

		averaged[rowindex, 2] = activities[a, 2]

		tmp <- clean[clean$subject==k & clean$activity==activities[a, 2], ]

		averaged[rowindex, 3:cols] <- colMeans(tmp[, 3:cols])

		rowindex = rowindex+1
	}
}

write.table(averaged, "averages.txt")


