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


# Using desctools to get mode

library(DescTools)

mode_values_descTools=mode(my_data)
print(mode_values_descTools)


# summary of the statistics

summary_stats_df<-bind_rows(
  "Mean"= round(mean_dplyr,2),
  "Median"= round(median_dplyr,2),
  "Mode"= round(mode_values,2),
  .id= "Statistics"
)

print(summary_stats_df)

## This can be done by base R method (rbind)

rownames(mean_dplyr)<-"Mean"
rownames(median_dplyr)<-"Median"
rownames(mode_values)<-"Mode"

summary_stats_rbind<-rbind(
  mean_dplyr,
  median_dplyr,
  mode_values,
)

print(summary_stats_rbind)


# Saving this data as a CSV file.


write.csv(summary_stats_df, file = './r_output/mean_median_mode_output.csv', row.names = FALSE)


#====================================================

# Calculating min and max of the feature and check the spreadness of data

min_values<- my_data %>% summarise(across(is.numeric, ~ min(., na.rm = TRUE)))
max_values<- my_data %>% summarise(across(is.numeric, ~ max(., na.rm=TRUE)))

print(min_values, max_values)


range_values=max_values-min_values

print(range_values)

# creating dataframe with the above details

min_max_df<-bind_rows(
  'Min'=round(min_values),
  'Max'=round(max_values),
  'Range'=round(range_values),
  .id = "Attributes"
)

print(min_max_df)


# Calculate the variance, standard variance and  Q1, Q3 and IQR values of the dataset

var_value<- my_data %>% summarise(across(is.numeric, ~ var(., na.rm=TRUE)))
std_value<- my_data %>% summarise(across(is.numeric, ~ sd(., na.rm=TRUE)))

print(var_value)
print(std_value)




Q1_value<- my_data %>% reframe(across(is.numeric, ~ quantile(., 0.25, na.rm=TRUE)))
Q3_value<- my_data %>% reframe(across(is.numeric, ~ quantile(., 0.75, na.rm=TRUE)))

IQR_value<- Q3_value-Q1_value

Q_df<- bind_rows(
  "Q1"=Q1_value,
  "Q3"=Q3_value,
  "IQR"=IQR_value,
  .id = "Quartile"
)

print(Q_df)


# Calculate the coefficient of variance

CV<- (std_value/mean_dplyr)*100
print(round(CV,2))


# Other method

CV2<- my_data %>% summarise(across(is.numeric, ~ CoefVar(., na.rm=TRUE)))
print(round(CV2*100,2))


# calculating skewness of all the feature

library(e1071)

skewness_value<- my_data %>% summarise(across(is.numeric, ~ skewness(., na.rm = TRUE)))

rownames(skewness_value)<-"Skewness"

skewness_df<- rbind(
  skewness_value
)

print(skewness_value)


# calculate the kurtosis


kurtosis_value<- my_data %>% summarise(across(is.numeric, ~ kurtosis(., na.rm = TRUE)))

print(kurtosis_value)


# Visualize skewness and kurtosis using histogram and kernal density

library('ggplot2')
library('patchwork')
library('tidyverse')

df_long<- my_data %>% 
  pivot_longer(
    cols = everything(),
    names_to = 'Feature',
    values_to = 'Value'
  )

print(df_long)


p_all_features <- ggplot(df_long, aes(x=Value)) +
  geom_histogram(aes(y=after_stat(density)), bins = 30, fill='lightblue', color='black', alpha=0.7) +
  geom_density(color='red', linewidth=1) +
  facet_wrap(~ Feature, scales='free', ncol=3) +
  labs(
    title = "Distribution of All Features",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

p_all_features


# Visualize the outlier nature of each feature using boxplot


p_all_boxplot<-ggplot(df_long, mapping = aes(x=Feature, y=Value)) +
  geom_boxplot(fill='lightblue', color='darkblue', outlier.color = 'red') +
  coord_flip() +
  facet_wrap(~ Feature, scales='free', ncol=3) +
  labs(
    title = "Boxplots of All Features",
    x = NULL, 
    y = "Value Distribution"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    strip.text = element_text(face = "bold")
  )

p_all_boxplot


# Visualize the data using violin plot

p_all_violin<-ggplot(df_long, aes(x=Feature, y=Value)) +
  geom_violin(fill='lightblue',draw_quantiles = c(0.25,0.75) ,quantile.colour = 'red') +
  geom_boxplot(fill='blue', box.color = 'red', width = 0.1)+
  facet_wrap(~ Feature, scales='free' ,ncol = 3) +
  coord_flip() +
  labs(
    title = "Violinplot of all features",
    x=NULL,
    y="Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    strip.text = element_text(face = 'bold')
  )

p_all_violin



# Check relationship between two numeric feature using correlation, scatterplot

regplot_feature<-ggplot(my_data, aes(x=fixed.acidity, y=volatile.acidity)) + 
  geom_point(color='darkblue', alpha=0.6) +
  geom_smooth(method = 'lm', se = FALSE, color='red', linewidth=1.2) +
  labs(
    title="Scatter plot with linear regression line",
    x='Fixed Acidity',
    y='Volatile Acidity',
    
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(colour = 'darkblue', size = 12),
    axis.text.y = element_text(colour = 'darkblue', size = 12),
    axis.ticks = element_line(colour = 'red'),
    axis.line.x = element_line(color='red', linewidth = 1),
    axis.line.y = element_line(color='red', linewidth = 1),
    title = element_text(face = 'bold', hjust = 0.5),
    plot.title = element_text(face='bold', hjust = 0.5)
  )

regplot_feature



