# Brandon Ban Kai Xian
# TP067094

# Install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("stringr")

# Load packages
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)

# Import data by reading csv file
data <- read.csv('C:/Users/bbkx2/Downloads/project/R/PFDA_Assignment/employee_attrition.csv', header = TRUE, sep = ",")

# Data Exploration
dataExploration <- function(data) {
  
  # Check number of columns
  cat("The total no. of columns is", ncol(data),"and the total no. of rows is", nrow(data),"\n") # 18 columns, 49653 rows
  
  # Check number of missing values
  cat("The number of missing values in data:", sum(is.na(data)), "\n\n") # 0 missing values in data
  
  # Check data structure
  cat(str(data), "\n")
  
  # Check summary of the data
  print(summary(data))
}

# Data Cleaning
dataCleaning <- function(data) {

  # Check for duplicates
  sum(duplicated(data))
  
  # If there's any, remove the duplicates
  data <- distinct(data)
  
  # Delete any rows that contains NA
  data <- na.omit(data)
  
  # Remove unnecessary column
  data <- select(data, -c("gender_short"))
  # data <- data[data$terminationdate_key > data$orighiredate_key, ]
  
  # Renaming column names
  colnames(data) <- c("employee_ID", "record_date", "birth_date", "hired_date", "termination_date", 
                      "age", "service_count", "city_name", "department_name", "job_title", 
                      "store_name", "gender", "termination_reason", "termination_type", "status_year", "status", "business_unit")
  return(data)
}

# Data Pre-processing
dataPreprocessing <- function(data) {
  
  # Converted to POSIXct (a date/time data type in R) using the specified date format strings
  data$record_date <- as.POSIXct(data$record_date, format = "%m/%d/%Y %H:%M")
  data$birth_date <- as.POSIXct(data$birth_date, format = "%m/%d/%Y")
  data$hired_date <- as.POSIXct(data$hired_date, format = "%m/%d/%Y")
  data$termination_date <- as.POSIXct(data$termination_date, format = "%m/%d/%Y")
  
  # Splits the job title string by the comma character and returns the first element of the resulting string vector
  data$job_title <- sapply(data$job_title, function(f) {strsplit(f, ",")[[1]]}[1])
  
  # Converted to character format
  data$job_title <- as.character(data$job_title)
  
  return(data)
}

dataExploration(data)
data <- data %>%
  dataCleaning() %>%
  dataPreprocessing()

View(data)
#-------------------------------------------------------------------------------
# What factors are associated with employee turnover?

# Analysis 1.1 - Analyze the relationship between job position and termination type
# Filter the data to only include terminated employees
data_terminated <- filter(data, status == "TERMINATED")

# Create a histogram of the termination types
ggplot(data_terminated, 
       aes(y = termination_type)) + # Aesthetic mappings describe how variables in the data are mapped
        geom_histogram(stat = "count") + 
          facet_wrap( ~job_title) # Wraps a 1d sequence of panels into 2d

# Create a table of the termination types and the total number of employees
# terminated for each job position and termination type
analysis1.1 <- data_terminated %>% 
  group_by(job_title, termination_type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

View(analysis1.1)

# Analysis 1.2 - Analyze the relationship between employee age and termination reason
# Create a histogram of employee age based on the reason for termination.
ggplot(data_terminated, 
       aes(y = age)) + 
        geom_histogram(stat = "count") + 
        facet_wrap( ~ termination_reason) + 
        scale_x_log10()

# Create a summary of termination reasons and types to count the number of employees 
analysis1.2 <- data_terminated %>% 
  group_by(age, termination_reason) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

View(analysis1.2)

#Analysis 1.3 - Compare the length of service of employees who left the company versus those who stayed
# Filter the data to only include employees who have left the company or are still working in the company
data_filtered <- data %>%
  filter(status %in% c("ACTIVE", "TERMINATED"))
print(data_filtered)
# Calculate the length of service for each employee
data_filtered$service_length <- as.numeric(difftime(data$termination_date, data$hired_date, units = "days"))/365

# Create a new column to label employees as 'left' or 'stayed'
data_filtered <- data_filtered %>%
  mutate(left_stayed = ifelse(status == "TERMINATED", "left", "stayed"))

# Create a bar plot to compare the length of service of employees who left the company versus those who stayed
ggplot(data_filtered, aes(x = left_stayed, y = service_length, fill = left_stayed)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge") +
  scale_fill_manual(values = c("#619CFF", "#FF6B6B")) +
  labs(x = "", y = "Average length of service (years)", fill = "") +
  theme_classic()
