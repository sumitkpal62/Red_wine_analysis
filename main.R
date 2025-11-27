# Load the dataset from the CSV file.

my_data <- read.csv('./dataset/winequality-red.csv')

# Check how many columns have null value(empty)

total_null_value_col<-colSums(is.na(my_data))
print(total_null_value_col)

# Check total number of missing value

missing_value<-sum(is.na(my_data))
print(missing_value)

# Check the percentage of missing value

missing_value<-sum(is.na(my_data))
total_values<-ncol(my_data)*nrow(my_data)

missing_percentage<-(missing_value/total_values)*100

print(missing_percentage)

# Check the number of duplicate rows present in dataset

duplicate_rows<-sum(duplicated(my_data))

print(duplicate_rows)

# Calculating the mean of the dataset by feature.

mean_col<-colMeans(my_data, na.rm = TRUE)
print(round(mean_col,2))

## Other ways to find mean of columns

# 1. Using sapply => sapply(list, function)

sapply_mean<-round(sapply(my_data, mean),2)
print(sapply_mean)

# 2. Summarise with across()

library(dplyr)

mean_dplyr<-my_data %>% summarise(across(where(is.numeric), ~ mean(., na.rm=TRUE)))

print(round(mean_dplyr,2))

# Calculation the median of the dataset by feature

sapply_median<-sapply(my_data, median)

print(round(sapply_median,2))

# Other method to find median

median_dplyr<-my_data %>% summarise(across(where(is.numeric), ~ median(., na.rm = TRUE)))

print(round(median_dplyr, 2))



# Calculation of mode of each feature

get_mode<- function(x) {
  # Remove NA values first, if desired (optional)
  x<-x[!is.na(x)]
  
  # Check if there's any data left
  if(length(x)==0) return(NA)
  
  # Calculate freq table
  tbl<-table(x)
  
  modes<-names(tbl)[tbl==max(tbl)]
  
  # returns the modes
  if(is.numeric(x)) return(as.numeric(modes))
  return(modes)
}

mode_values<- my_data %>% summarise(across(everything(), get_mode))

print(round(mode_values, 2))






















