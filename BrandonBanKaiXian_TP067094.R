# Brandon Ban Kai Xian
# TP067094

# Install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")
install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("plotrix")
install.packages("openair")
install.packages("ggrepel")

# Load packages
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(plotrix)
library(openair)
library(ggrepel)
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
      employee_ID = as.factor(employee_ID),
      city_name = as.factor(city_name),
      department_name = as.factor(department_name),
      job_title = as.factor(job_title),
      store_name = as.factor(store_name),
      gender = as.factor(gender),
      termination_reason = as.factor(termination_reason),
      termination_type = as.factor(termination_type),
      status = as.factor(status),
      business_unit = as.factor(business_unit)
    )
  
  # Extract the year from the birth date
  data$birth_year <- year(ymd(data$birth_date))
  
  data$generation <- ifelse(data$birth_year >= 1997, "Gen Z",
                    ifelse(data$birth_year >= 1981, "Millennials",
                    ifelse(data$birth_year >= 1965, "Gen X",
                    ifelse(data$birth_year >= 1946, "Baby Boomers", "The Silent Generation"))))
  
  return(data)
}

dataExploration(data)
data <- data %>%
  dataCleaning() %>%
  dataPreprocessing()

View(data)
#+============================================================================+
#| Data Exploration for Ideas of questions to ask                             |
#+============================================================================+
#---- 1st Idea - Trends in employee termination within 10 years ----
firstIdea <- function() {
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
firstIdea()
# Observation:
# Layoff only happened in 2014 and 2015
# Many employee leave the company in 2014 and most common reason is layoff

# Questions that can be asked:
# What factors are associated with employee termination?
# What factors contributed to the high rate of employee layoffs?
# What is the average length of service for terminated employees? Does this vary by department or job title?

#---- 2nd Idea - Analysis of employee termination by gender within ten years ----
secondIdea <- function() {
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
secondIdea()
# Observation:
# Large number of female employees are terminated in 2014 and 2015
# The termination of female employees are greater than male employees

# Questions that can be asked:
# How does the number of terminated employees differ by gender?

#---- 3rd Idea - Understand the age distribution of the workforce ----
thirdIdea <- function() {
  # Count number of employees in each generation
  generation_count <- data %>%
    group_by(status_year, generation) %>%
    summarize(count = n(), .groups = 'drop') %>%
    filter(!is.na(generation)) %>%
    ungroup()
  
  # Plot number of employees in each age group by year
  ggplot(generation_count, aes(x = status_year, y = count, color = generation)) +
    geom_line(size = 0.75) +
    labs(x = "Year", y = "Number of Employees", title = "Number of Employees by Age Generation Over Time") +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_x_continuous(breaks = seq(min(generation_count$status_year), max(generation_count$status_year), 1)) +
    theme_minimal()
}
thirdIdea()
# Observation:
# The number of millennial employees has been increasing year by year
# Starting from 2013, there appears to be a decline in the total number of employees across all generations.

# Questions that can be asked:
# Why has the total number of employees been decreasing since 2013? 
#==============================================================================

#==== Q1. What factors are associated with employee termination? ===========

#---- Analysis 1.1 - Analyze the relationship between employee age and termination reason ----
Q1_1 = function() {
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
Q1_1()

# Explanation:
# Employees aged 60 and above are more likely to retire, while those aged between 20 and 35 are more likely to resign. 
# It’s important to note that layoffs can happen across all age ranges and are not specific to any particular age group.
# Employees aged 60 and above may be more likely to retire due to reaching retirement age or wanting to reduce their workload
# employees aged between 20 and 35 may be more likely to resign due to exploring other career opportunities, 
# seeking higher pay or wanting to further their education
#---- Analysis 1.2 - Analyze the relationship between job title and termination reason ----
Q1_2 = function() {
  # filter only the terminated employees
  data_terminated <- filter(data, status == "TERMINATED")
  # show the list of termination reasons
  unique(data_terminated$termination_reason) 
  # create a bar chart for each job title
  ggplot(data_terminated, aes(x = termination_reason, fill = termination_reason)) + 
    stat_count() +
    facet_wrap( ~ job_title) +
    labs(x = "Termination Reason", y = "Count", fill = "Termination Reason") +
    scale_fill_manual(values = c("Layoff" = "#0072B2", "Resignation" = "#E69F00", "Retirement" = "#09DDFA")) +
    ggtitle("Termination Reasons by Job Title")
}
Q1_2()

# Explanation:
# The occupation of meat cutter has the highest proportion of employees who retire, with produce clerk following closely behind, 
# while the cashier role has the highest proportion of employees who resigned.
# Meat cutting is a physically demanding job that requires a certain level of strength and stamina. 
# As employees age, it may become more difficult for them to perform the tasks required of them, leading to retirement. 
# On the other hand, cashiering involves standing for long periods and repetitive tasks, which can lead to boredom and burnout, 
# especially for younger employees who may be seeking more challenging and fulfilling work
#---- Analysis 1.3 - Analyze the relationship between length of service and termination reason for cashiers and meat cutters -----
# 
Q1_3 = function() {
  # Select only the data for employees who have been terminated
  cashier_terminated = filter(data, status == "TERMINATED", job_title == "Cashier")
  meat_cutter_terminated = filter(data, status == "TERMINATED", job_title == "Meat Cutter")
  
  # Group data by length of service and termination reason, and calculate the count of employees
  cashier_count = cashier_terminated %>% 
    group_by(service_count, termination_reason) %>% 
    summarize(count = n(), .groups = "drop")
  
  meat_cutter_count = meat_cutter_terminated %>% 
    group_by(service_count, termination_reason) %>% 
    summarize(count = n(), .groups = "drop")
  
  # Visualize stacked bar chart for terminated employees who worked as cashier
  a <- ggplot(cashier_count, aes(x = service_count, y = count, fill = termination_reason)) + 
    geom_col() +
    labs(title = "Cashier",
         x = "Length of Service (Years)",
         y = "Terminated Employees (Person)",
         fill = "Termination Reason") +
    scale_x_continuous(breaks = seq(0, max(cashier_terminated$service_count), by = 1)) +
    theme_minimal()
  # Visualize stacked bar chart for terminated employees who worked as meat cutter
  b <- ggplot(meat_cutter_count, aes(x = service_count, y = count, fill = termination_reason)) + 
    geom_col() +
    labs(title = "Meat Cutter",
         x = "Length of Service (Years)",
         y = "Terminated Employees (Person)",
         fill = "Termination Reason") +
    scale_x_continuous(breaks = seq(0, max(meat_cutter_terminated$service_count), by = 1)) +
    theme_minimal()
  
  # Arrange the plots side by side
  grid.arrange(a, b, ncol=2)
}
Q1_3()

# Explanation:
# Based on the graph shown above,  we can conclude that meat cutters tend to retire after a longer period of service compared to cashiers, who are more likely to resign after a shorter period of work
# The reason behind this might be jobs, such as cashiering, may be mentally demanding due to the need to interact with customers and handle money accurately
# This could lead to burnout or stress, which could also result in employees leaving the job earlier or being terminated.
# The job of meat cutter demands a particular skill set and expertise, which suggests that it may take a considerable amount of time (usually 2-5 years) for them to attain the required level of proficiency. 
# This factor could be one of the reasons behind the higher retirement rate among meat cutters after serving for a longer duration.
#------------------------------- Conclusion ---------------------------------------
# In conclusion, the factors associated with employee termination can be attributed to various reasons based on age, occupation, and job demands. 
# Employees aged 60 and above are more likely to retire, while those between 20 and 35 are more likely to resign due to exploring other career opportunities, 
# seeking higher pay, or furthering their education. Layoffs, on the other hand, can happen across all age ranges and are not specific to any particular age group. 
# Occupation-wise, the meat cutter role has the highest proportion of employees who retire, while cashiers are more likely to resign. 
# This could be due to the physically demanding nature of meat cutting, which requires a particular skill set and expertise that may take time to develop, leading to retirement after a longer period of service. 
# In contrast, cashiering involves standing for long periods and repetitive tasks that can lead to burnout or stress, resulting in employees leaving the job earlier or being terminated. 
# Understanding these factors is essential for employers to develop strategies to retain employees and reduce the cost associated with high turnover rates.

#==============================================================================

#==== Q2. What factors contributed to the high rate of employee layoffs? =====

#---- Analysis 2.1 - Analyze the relationship between departments and layoff ----
Q2_1 = function() {
  # Create a subset of data containing only relevant columns
  data_sub <- data %>% select(department_name, termination_reason)
  
  # Calculate the count of layoffs in each department
  data_sub_agg <- data_sub %>% 
    filter(termination_reason == "Layoff") %>% 
    group_by(department_name) %>% 
    summarise(count = n())
  
  # Create a table showing the count of layoffs in each department
  table <- data_sub_agg %>% 
    spread(department_name, count, fill = 0)

  # Create a heat map
  ggplot(data = data_sub_agg, aes(x = department_name, y = "")) + 
    geom_tile(aes(fill = count), color = "white", size = 1.5) + 
    scale_fill_gradient(low = "pink", high = "red") + 
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(color = "black", size = 1, fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggtitle("Count of Layoffs by Department") +
    labs(x = "Department", y = "") +
    theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))
}
Q2_1()

# Explanation:
# In terms of layoffs, customer service had the highest number of affected employees, 
# potentially due to the ease of outsourcing or automation of these jobs. 
# Conversely, store management positions had the lowest number of layoffs, 
# possibly because these roles are crucial to the success of the company, and cuts to this department could have a more significant impact on the business.

#---- Analysis 2.2 - Analyze the relationship between employees' job and layoff ----
Q2_2 = function() {
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
Q2_2()

# Explanation:
# According to the bar graph, the job category with the highest number of employees who were laid off is cashier, followed by dairy person
# This could be attributed to the advancements in technology and automation, which have resulted in the gradual replacement of human labor with machinery in 
# industries that are more easily replaceable, such as those represented by the aforementioned job categories.

#---- Analysis 2.3 - Analyze the relationship between employees' generations and employees' gender ----
Q2_3 = function() {
  # summarize the data and store it in a new object
  laid_off_summary <- data %>%
    filter(termination_reason == "Layoff") %>%
    group_by(generation, gender) %>%
    summarise(num_laid_off = n(), .groups = 'drop')
  
  # plot stacked bar chart using the summarized data
  ggplot(laid_off_summary, aes(x=generation, y=num_laid_off, fill=gender)) +
    geom_bar(stat="identity") +
    xlab("Age Generation") +
    ylab("Number of Laid Off Employees") +
    ggtitle("Number of Laid Off Employees by Age Generation and Gender") +
    theme(plot.title = element_text(hjust = 0.5))
}
Q2_3()

# Explanation:
# Millennial employees, irrespective of gender, have been laid off in the largest numbers. 
# This could be attributed to companies laying off employees as a cost-saving measure, 
# and younger employees being more vulnerable due to their lower salaries and less experience. 
# The adoption of new technologies is leading to the automation of many jobs, which may be a contributing factor to layoffs. 
# As millennial are often in entry-level positions, they may be more impacted by these changes.

#------------------------------- Conclusion ------------------------------------
# Based on the analysis, it can be concluded that several factors contributed to the high rate of employee layoffs. 
# Customer service and job categories such as cashier and dairy person have been hit the hardest due to the ease of outsourcing or automation. 
# On the other hand, store management positions, which are critical to the success of the company, had the lowest number of layoffs.
# Furthermore, millennial employees, who are often in entry-level positions, have been laid off in the largest numbers. 
# Companies may lay off employees as a cost-saving measure, and younger employees are more vulnerable due to their lower salaries and less experience. 
# The adoption of new technologies and the resulting automation of many jobs could also be a contributing factor to layoffs.
# Overall, these findings suggest that companies are likely looking to cut costs by automating jobs or outsourcing them to other countries. 
# This trend is likely to continue, impacting entry-level employees and those in job categories that are more easily replaceable. 
# To mitigate the impact of these changes, companies may need to invest in retraining programs and support for affected employees to transition to new roles or industries.

#==============================================================================

#==== Q3. How does the number of terminated employees differ by gender? =========

#---- Analysis 3.1 - Find the correlation between the age of the employee and the likelihood of termination by gender ----
Q3_1 = function() {
  # Filter the data that only applicable for those who terminated in 2014 & 2015
  data$record_year <- year(ymd(data$record_date))
  
  data_2014_2015 <- data %>% 
    filter(record_year >= 2014 & record_year <= 2015)
  
  # Create the histogram for females
  data_terminated <- filter(data_2014_2015, status == "TERMINATED")
  female_data <- subset(data_terminated, gender == "Female")
  female_hist <- ggplot(female_data, aes(x = age, fill = termination_reason)) +
    geom_histogram(binwidth = 2) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Female Employee Termination by Age and Reason", 
         x = "Age", y = "Frequency")
  
  # Create the histogram for males
  male_data <- subset(data_terminated, gender == "Male")
  male_hist <- ggplot(male_data, aes(x = age, fill = termination_reason)) +
    geom_histogram(binwidth = 2) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Male Employee Termination by Age and Reason", 
         x = "Age", y = "Frequency")
  
  # Combine the histograms
  gridExtra::grid.arrange(female_hist, male_hist, ncol = 2)
}
Q3_1()

# Explanation
# There is a higher likelihood for older female employees to retire compared to older male employees, 
# and no retirement occurred for male employees in 2014 and 2015. 
# This trend may be attributed to the fact that female employees are more likely to be employed in job roles 
# that have different retirement patterns compared to those of male employees.

#---- Analysis 3.2 - Identify the job roles with the highest number of employees above the age of 60, categorized by gender ----
Q3_2 <- function() {
  data_filtered <- data %>%
    filter(age > 60) %>%
    group_by(gender, job_title, status) %>%
    summarise(count = n(), .groups = "drop")
  
  ggplot(data_filtered, aes(x = job_title, y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack", color = "white", size = 0.2) +
    labs(x = "Job Title", y = "Count of Terminated Employees", fill = "Status") +
    facet_wrap(~ gender) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q3_2()

# Explanation:
# Based on the analysis, it was found that the job role with the highest number of employees above the age of 60, for both male and female, was meat cutter. 
# However, it was observed that a higher number of female employees had left the job compared to male employees. 
# One possible explanation for this trend could be that the physical demands of the job, which require stamina and strength, become more challenging for female employees with increasing age, 
# resulting in a higher likelihood of termination.

#---- Analysis 3.3 - Find the relationship between city and employees' termination ----
Q3_3 <- function() {
  # Filter the data that only applicable for those who terminated in 2014 & 2015
  data$record_year <- year(ymd(data$record_date))
  
  data_filtered <- data %>%
    filter(status == "TERMINATED", record_year %in% c(2014, 2015))
  
  # Counting terminated employees by gender and city
  terminated_by_city <- data_filtered %>%
    group_by(gender, city_name) %>%
    summarise(count = n(), .groups = "drop")
  
  # Plotting the graph
  ggplot(terminated_by_city, aes(x = city_name, y = count, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "City", y = "Count of Terminated Employees", fill = "Gender") +
    ggtitle("Distribution of Terminated Employees by City and Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q3_3()

# Explanation:
# According to the graph, Vancouver had the highest number of terminated female employees. 
# This may be attributed to a possible decline in the industries where female employees were more represented in Vancouver, resulting in higher termination rates.

#---- Analysis 3.4 - Identify the job roles with the highest number of female employees in the city, Vancouver ----
Q3_4 <- function() {
  # Filter data for Vancouver and group by gender and job title
  data_filtered <- data %>% 
    filter(city_name == "Vancouver", gender == "Female") %>% 
    group_by(gender, job_title) %>% 
    summarise(count = n(), .groups = "drop")
  
  # Order job titles by descending count
  data_filtered <- data_filtered %>% 
    arrange(desc(count))
  
  # Plot stacked bar chart
  ggplot(data_filtered, aes(x = job_title, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    labs(x = "Jobs in Vancouver city", y = "Number of Employees", fill = "Gender") +
    coord_flip() +
    theme(legend.position = "bottom")
}
Q3_4()

# Explanation:
# The analysis revealed that the occupation of meat cutter had the highest number of female workers in Vancouver. 
# This observation is in line with the earlier analysis that identified meat cutter as the job role with the highest number of female employees 
# who were terminated above the age of 60.

#------------------------------- Conclusion ------------------------------------
# Based on the analyses conducted, it can be concluded that there are differences in the number of terminated employees by gender. 
# Specifically, older female employees are more likely to retire compared to older male employees, which may be due to the fact that 
# female employees are more likely to be employed in job roles with different retirement patterns compared to those of male employees.
# Moreover, meat cutter is the job role with the highest number of employees above the age of 60 for both male and female, but a higher number of female 
# employees had left the job compared to male employees, possibly due to the physical demands of the job becoming more challenging with increasing age.
# It is noteworthy that Vancouver had the highest number of terminated female employees, which may be attributed to a decline in the jobs, such as meat cutter, where female employees were more represented, 
# resulting in higher termination rates.

#==============================================================================

#==== Q4. Why has the total number of employees been decreasing since 2013?
#---- Analysis 4.1 - Find the correlation between employees' generation, job title, and job termination ----
Q4_1 <- function() {
  termination_count <- data %>%
    filter(status == "TERMINATED", status_year >= 2013)

  # Plot grouped bar chart
  ggplot(termination_count, aes(x = generation, fill = generation)) +
    stat_count() +
    facet_wrap( ~ job_title) +
    labs(x = "Generation", y = "Number of Terminated Employees", fill = "Status") +
    scale_fill_manual(values = c("Baby Boomers" = "#DD4321", "Gen X" = "#0F2199", "Millennials" = "#AF3376")) +
    ggtitle("Employees' Job by Generation")
}
Q4_1()

# Explanation:
# The cashier job appears to be the most commonly terminated position among millennials and gen X, while meat cutter is the most commonly terminated job among baby boomers. 
# Additionally, the dairy person job is another position that appears to have a high termination rate among gen X.
# This trend may be attributed to changes in the retail industry and organizational policies, resulting in a decrease in demand for these positions. 
# Moreover, employees may have opted for better job opportunities offering higher pay, benefits, and career advancement prospects.

#---- Analysis 4.2 - Determine the termination reason of different generation employees ----
Q4_2 <- function() {
  # Filter data for terminated employees after 2013
  millennials_termination_count <- data %>%
    filter(status == "TERMINATED", generation == "Millennials", job_title == "Cashier", status_year >= 2013)
  gen_x_termination_count <- data %>%
    filter(status == "TERMINATED", generation == "Gen X", job_title == "Cashier" | job_title == "Dairy Person", status_year >= 2013)
  baby_boomers_termination_count <- data %>%
    filter(status == "TERMINATED", generation == "Baby Boomers", job_title == "Meat Cutter", status_year >= 2013)
  
  # Count termination reasons
  millennials_reason_count <- millennials_termination_count %>%
    group_by(generation, termination_reason) %>%
    summarize(count = n(), .groups = 'drop') %>%
    ungroup()
  gen_x_reason_count <- gen_x_termination_count %>%
    group_by(generation, termination_reason) %>%
    summarize(count = n(), .groups = 'drop') %>%
    ungroup()
  baby_boomers_reason_count <- baby_boomers_termination_count %>%
    group_by(generation, termination_reason) %>%
    summarize(count = n(), .groups = 'drop') %>%
    ungroup()
  
  # Create pie chart
  a <- ggplot(millennials_reason_count, aes(x = generation, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(fill = "Termination Reason") +
    ggtitle("Count of terminated Millennial \nCashier employees after 2013") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5))
  b <- ggplot(gen_x_reason_count, aes(x = generation, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(fill = "Termination Reason") +
    ggtitle("Termination count of Gen X employees working as Cashiers \nand Dairy Persons after 2013.") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5))
  c <- ggplot(baby_boomers_reason_count, aes(x = generation, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(fill = "Termination Reason") +
    ggtitle("Count of terminated Baby Boomers \nMeat Cutter employees after 2013") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5))
  
  grid.arrange(a, b, c, ncol=3)
}
Q4_2()
# Explanation:
# The high termination rates observed among millennials in the cashier job, gen X in the dairy person and cashier jobs, and baby boomers in the meat cutter job may be attributed to various factors. 
# Millennials may have left the cashier job to seek better employment opportunities with improved compensation, benefits, and career growth prospects. 
# Gen X may have experienced job loss due to automation and technological advancements in the industry. 
# Baby boomers, on the other hand, may have retired due to age or personal preferences for a reduced workload.

#------------------------------- Conclusion ------------------------------------
# In conclusion, the decrease in total employee count since 2013 varies across different generations. Millennials may have resigned from cashier jobs to 
# seek better employment opportunities, while gen X may have faced job loss due to automation and technological advancements. 
# Baby boomers may have opted to retire due to age or personal preferences for a reduced workload.

#==============================================================================

#==== Q5. What is the average length of service for terminated employees? Does this vary by department or city? ====
#---- Analysis 5.1 - Determine the average length of service by year ----
Q5_1 <- function() {
  # Filter for terminated employees
  term_data <- data %>%
    filter(status == "TERMINATED")
  
  # Calculate average length of service by year
  service_by_year <- term_data %>%
    group_by(status_year) %>%
    summarize(avg_service = mean(service_count))
  
  # Plot the line chart
  ggplot(service_by_year, aes(x = status_year, y = avg_service)) +
    geom_line(color = "blue", size = 1) +
    labs(x = "Year", y = "Average length of service (years)",
         title = "Average Length of Service for Terminated Employees by Year") +
    scale_x_continuous(breaks = seq(min(term_data$status_year), max(term_data$status_year), 1))
}
Q5_1()

# Explanation:
# The trend of the average length of service shows a decline from 2009 to 2011, 
# possibly due to the global recession leading to higher termination rates and shorter service periods for terminated employees.
# However, from 2011 to 2015, there was a rapid increase in the average length of service, 
# which may be attributed to the economic recovery and increased hiring during that period.

#---- Analysis 5.2 - Find the relationship between average length of service and departments ----
Q5_2 <- function() {
  # Group the data by department and calculate the average length of service
  avg_service_dept <- aggregate(data$service, by=list(data$department_name), FUN=mean)
  
  # Plot the bar graph
  ggplot(avg_service_dept, aes(x=Group.1, y=x)) +
    geom_bar(stat="summary", fun = "mean", fill="palegreen") +
    ggtitle("Average Length of Service by Department") +
    xlab("Department") +
    ylab("Average Length of Service") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q5_2()

# Explanation:
# Based on the graph, it can be observed that the average length of service is the shortest for the customer service department, 
# followed by the dairy and processed foods departments. 
# This trend may be attributed to the possibility that these departments have a relatively larger number of entry-level positions 
# that tend to attract younger and less experienced employees who may be more prone to leaving their jobs sooner.

#---- Analysis 5.3 - Find the relationship between average length of service and cities ----
Q5_3 <- function() {
  # Group the data by city and calculate the average length of service
  avg_service_dept <- aggregate(data$service, by=list(data$city_name), FUN=mean)
  # Plot the bar graph
  ggplot(avg_service_dept, aes(x=Group.1, y=x)) +
    geom_bar(stat="summary", fun = "mean", fill="magenta") +
    ggtitle("Average Length of Service by Department") +
    xlab("Department") +
    ylab("Average Length of Service") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q5_3()

# Explanation:
# Based on the graph, it can be observed that "Ocean Falls" is the city with the longest average length of service.
# rephrase professionally, " the average length of service in the city "Ocean Falls" is the longest, 
# this may be that the company in this city may have a strong presence and good reputation in the community, 
# leading to higher job satisfaction and loyalty among employees.

#------------------------------- Conclusion ------------------------------------
# The average length of service for terminated employees varies significantly depending on the year, department, and city. 
# By analyzing these variations, companies can gain valuable insights into factors that contribute to employee retention and 
# implement strategies to improve employee loyalty and reduce turnover rates.