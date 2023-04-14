# Brandon Ban Kai Xian
# TP067094

# Install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")

# Load packages
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(tidyverse)
library(lubridate)

# Import data by reading csv file
setwd("C:/Users/bbkx2/Downloads/project/R/PFDA_Assignment")
data <- read.csv('employee_attrition.csv', header = TRUE, sep = ",")

# Data Exploration
dataExploration <- function(data) {
  # Check number of columns
  cat("The total no. of columns is", ncol(data),"and the total no. of rows is", nrow(data),"\n")
  # Check number of missing values
  cat("The number of missing values in data:", sum(is.na(data)), "\n\n")
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
  # Replace missing values with NA, if any
  data[data == ""] <- NA
  # Remove unnecessary column
  data <- select(data, -c("gender_short"))
  # Renaming column names
  colnames(data) <- c("employee_ID", "record_date", "birth_date", "hired_date", "termination_date", 
                      "age", "service_count", "city_name", "department_name", "job_title", 
                      "store_name", "gender", "termination_reason", "termination_type", "status_year", 
                      "status", "business_unit")
  return(data)
}

# Data Pre-processing
dataPreprocessing <- function(data) {
  # Replace default termination date (still working) to "NA"
  data$termination_date = ifelse(data$termination_date == "1/1/1900", NA, data$termination_date)
  # Converted to POSIXct (a date/time data type in R) using the specified date format strings
  data$record_date <- as.POSIXct(data$record_date, format = "%m/%d/%Y %H:%M")
  data$birth_date <- as.POSIXct(data$birth_date, format = "%m/%d/%Y")
  data$hired_date <- as.POSIXct(data$hired_date, format = "%m/%d/%Y")
  data$termination_date <- as.POSIXct(data$termination_date, format = "%m/%d/%Y")
  # Replace typo "resignaton" -> "resignation"
  data$termination_reason <- str_replace(data$termination_reason, "Resignaton", "Resignation")
  # Splits the job title string by the comma character and returns the first element of the resulting string vector
  data$job_title <- sapply(data$job_title, function(f) {strsplit(f, ",")[[1]]}[1])
  # Converted to character format
  data$job_title <- as.character(data$job_title)
  # Converted character type data into factor
  data <- data %>%
    mutate(
      employee_ID <- as.factor(employee_ID),
      city_name <- as.factor(city_name),
      department_name <- as.factor(department_name),
      job_title <- as.factor(job_title),
      store_name <- as.factor(store_name),
      gender <- as.factor(gender),
      termination_reason <- as.factor(termination_reason),
      termination_type <- as.factor(termination_type),
      status <- as.factor(status),
      business_unit <- as.factor(business_unit)
    )
  
  return(data)
}

dataExploration(data)
data <- data %>%
  dataCleaning() %>%
  dataPreprocessing()

View(data)
#+==============================================================================+
#| Data Exploration for Ideas of questions to ask                               |
#+==============================================================================+
# 1st Idea - Trends in employee termination within 10 years
firstIdea <- function(data) {
  # create a new column for termination year
  data$termination_year <- format(data$termination_date, "%Y")
  
  # group the data by termination year and termination reason
  termination_by_year_reason <- data %>%
    filter(status == "TERMINATED") %>% # remove rows with missing termination date
    group_by(termination_year, termination_reason) %>%
    summarise(n = n(), .groups = 'drop')
  
  # create a stacked bar chart
  ggplot(termination_by_year_reason, aes(x = termination_year, y = n, fill = termination_reason)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))) + # specify color for each termination reason
    labs(title = "Employee Termination by Year and Reason",
         x = "Year of Termination",
         y = "Number of Terminations",
         fill = "Termination Reason") +
    theme_bw() + # set theme to black and white
    theme(plot.title = element_text(hjust = 0.5), # center the title
          axis.text.x = element_text(angle = 90, vjust = 0.5)) # rotate x-axis labels
}
firstIdea(data)
# Findings:
# Layoff only happened in 2014 and 2015
# many employee leave the company in 2014 and most common reason is layoff

#  How has the number of terminated employees changed over the past 10 years?
#  What is the overall trend in employee termination over the past 10 years?
#  Are there any seasonal patterns in employee termination?
#  Is there any correlation between employee termination and company performance?
#  How has the distribution of termination reasons changed over time?

#--------------------------------------------------------------------------------
# 2nd Idea - Analysis of employee termination by gender within ten years
secondIdea <- function(data) {
  # Create a new column for the year of termination
  data$termination_year <- as.integer(substr(data$termination_date, 1, 4))
  # Filter out any rows where the termination year is missing
  data <- data %>%
    filter(!is.na(termination_year))

  # Create the bar chart
  ggplot(data, aes(x = factor(termination_year), group = gender, fill = gender)) +
    geom_bar(position = "dodge", color = "black", stat = "count") +
    labs(title = "Number of Terminated Employees by Gender and Year",
         x = "Year of Termination", y = "Count of Employees", fill = "Gender") +
    theme_minimal()
}
secondIdea(data)

#  How does the number of terminated employees differ by gender?
#  What is the distribution of termination reasons by gender?
#  Is there a significant difference in the percentage of male and female employees who are terminated?
#  How has the gender ratio of terminated employees changed over time?
#  Are there any differences in the length of service between male and female terminated employees?
#-------------------------------------------------------------------------------

#===============================================================================
# Q1. What factors contributed to the high rate of employee layoffs?
# Analysis 1.1 - Find the relationship between employees' job and layoff
Q1_1 = function() {
  # Subset the data to only include employees terminated due to layoff
  layoff_data <- subset(data, termination_reason == "Layoff")
  
  # Group the data by job_title and termination_reason, and count the number of employees in each group
  job_layoff_count <- aggregate(layoff_data$employee_ID, by=list(layoff_data$job_title, layoff_data$termination_reason), FUN=length)
  names(job_layoff_count) <- c("job_title", "termination_reason", "count")
  
  # Order the job titles by the count of layoffs
  job_layoff_count <- job_layoff_count[order(job_layoff_count$count, decreasing=TRUE),]
  
  # Create a stacked bar plot
  ggplot(job_layoff_count, aes(x=job_title, y=count, fill=termination_reason)) +
    geom_bar(stat="identity") +
    labs(x="Job Title", y="Count", fill="Termination Reason") +
    ggtitle("Relationship between Job Title and Layoff") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
}
Q1_1()
# According to the bar graph, the job category with the highest number of employees who were laid off is Cashier, followed by Dairy Person and Meat Cutter
# This could be attributed to the advancements in technology and automation, which have resulted in the gradual replacement of human labor with machinery in 
# industries that are more easily replaceable, such as those represented by the aforementioned job categories.
#-------------------------------------------------------------------------------
# Q2. Was there any correlation between the length of service of an employee and their likelihood of being laid off?

# Q1. What factors are associated with employee termination?
# Analysis 1.1 - Analyze the relationship between job title and termination type
Q2_1 = function() {
  # filter only the terminated employees
  data_terminated <- filter(data, status == "TERMINATED")
  # show the list of termination reasons
  unique(data_terminated$termination_reason) 
  # create a bar chart for each job title
  ggplot(data_terminated, aes(x = termination_type), fill = termination_type) + 
    stat_count() +
    facet_wrap( ~ job_title) +
    scale_fill_manual(values = c("Voluntary" = "#0072B2", "Involuntary" = "#E69F00")) +
    labs(x = "Termination Type", y = "Count", fill = "Termination Type") +
    ggtitle("Termination Types by Job Title")
}
Q2_1()
# The number of employees who worked as cashiers, bakers, dairy persons, meat cutters, 
# and produce clerks who left the job is greater than the number of employees who left from other jobs.
# However, the number of employees who left voluntarily is greater than the number of employees who left involuntarily.

# Analysis 1.2 - Analyze the relationship between employee age and termination reason
Q1_2 = function() {
  # Filter data for termination reasons "layoff", "resignation" and "retirement"
  data_filtered <- data %>% filter(termination_reason %in% c("Layoff", "Resignation", "Retirement"))
  
  # Visualize relationship between employee age and termination reason using ggplot2
  ggplot(data_filtered, aes(x = age, fill = termination_reason)) + 
    geom_density(alpha = 0.5) +
    labs(title = "Relationship between Employee Age and Termination Reason",
         x = "Employee Age",
         y = "Density",
         fill = "Termination Reason") +
    theme_minimal()
}
Q1_2()
# Employees aged 60 and above are more likely to retire, while those aged between 20 and 35 are more likely to resign. 
# It’s important to note that layoffs can happen across all age ranges and are not specific to any particular age group.

# Analysis 1.3 - Analyze the relationship between length of service and employees who left or stayed
Q1_3 = function() {
  # Visualize length of service of employees who left vs stayed using histogram
  data_terminated = filter(data)
  ggplot(data_terminated, aes(x = service_count, fill = status)) + 
    geom_histogram(binwidth = 1) +
    facet_wrap( ~ status) +
    labs(title = "Length of Service of Employees Who Left vs Stayed",
         x = "Length of Service (Years)",
         y = "Employees Count (Person)",
         fill = "Status") +
    theme_minimal()
}
Q1_3()

# Subset data for employees who left and those who stayed
left <- data[data$status == "TERMINATED",]
stay <- data[data$status == "ACTIVE",]

# Create histogram for length of service
hist(left$service_count, col="red", breaks=20, main="Length of Service Distribution (Left)")
hist(stay$service_count, col="blue", breaks=20, main="Length of Service Distribution (Stay)")

# Add legend
legend("topright", c("Left", "Stay"), fill=c("red", "blue"))

# From the range of 0 ~ 25, the employees who worked for 1, 8, and 13 years left the job
#-------------------------------------------------------------------------------
