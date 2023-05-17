# Brandon Ban Kai Xian
# TP067094

#---- Install packages ----
install.packages("gridExtra")
install.packages("plotrix")
install.packages("tidyverse")

#---- Load packages ----
library(gridExtra)          # grid.arrange()
library(plotrix)            # legend
library(tidyverse)          # included library readr, dplyr, ggplot2, stringr, forcats, lubridate, magrittr

# Import data by reading csv file
setwd("C:/Users/bbkx2/Downloads/project/R/PFDA_Assignment")
data <- read.csv('employee_attrition.csv', header = TRUE, sep = ",")

# Data Exploration
# This function takes in the data and displays the number of columns and rows in the data,
# the number of missing values in the data, the data structure, and the summary of the data
# @param data: The data to be explored
explore_data <- function() {

  # Check number of rows & columns
  cat("The total no. of columns is", ncol(data),"and the total no. of rows is", nrow(data),"\n")

  # Check number of missing values
  cat("The number of missing values in data:", sum(is.na(data)), "\n\n")

  # Check data structure
  cat(str(data), "\n")

  # Check summary of the data
  print(summary(data))
}

# Data Cleaning
# This code is used to clean the data by removing duplicates, replacing missing values, 
# removing unnecessary columns, renaming columns, and returning the cleaned data.
# @param data: The data to be cleaned
clean_data <- function(data) {

  # Check for duplicates
  sum(duplicated(data))

  # If there's any, remove the duplicates
  unique(data)

  # Replace missing values with NA, if any
  data[data == ""] <- NA
  
  # Remove unnecessary column
  data <- select(data, -c("gender_short", "recorddate_key", "orighiredate_key"))

  # Renaming column names
  colnames(data) <- c("employee_ID", "birth_date", "termination_date", "age", "service_count", "city_name", 
                      "department_name", "job_title", "store_name", "gender", "termination_reason", 
                      "termination_type", "status_year", "status", "business_unit")
  
  return(data)
}

# Data Pre-processing
# This function pre-processes the input data by cleaning and transforming it.
# Finally, it returns the pre-processed data.
# @param data: The data to be processed
preprocess_data <- function(data) {

  # Replace default termination date (still working) to "NA"
  data$termination_date = ifelse(data$termination_date == "1/1/1900", NA, data$termination_date)

  # Converted to POSIXct (a date/time data type in R) using the specified date format strings
  data$birth_date <- as.POSIXct(data$birth_date, format = "%m/%d/%Y")
  data$termination_date <- as.POSIXct(data$termination_date, format = "%m/%d/%Y")

  # Replace typo "resignaton" to "resignation"
  data$termination_reason <- str_replace(data$termination_reason, "Resignaton", "Resignation")

  # Splits the job title string (e.g <Director, Any> to <Director> only) by the comma character 
  # and returns the first element of the resulting string vector
  data$job_title <- sapply(data$job_title, function(f) {strsplit(f, ",")[[1]]}[1])

  # Converted job title to character format
  data$job_title <- as.character(data$job_title)
  
  # Converted character type data into factor
  data <- data %>%
    mutate(
      city_name = as.factor(city_name),
      department_name = as.factor(department_name),
      job_title = as.factor(job_title),
      store_name = as.factor(store_name),
      gender = as.factor(gender),
      termination_reason = as.factor(termination_reason),
      termination_type = as.factor(termination_type),
      status = as.factor(status),
      business_unit = as.factor(business_unit),
    )
  
  # Extract the year from the birth date
  data$birth_year <- year(ymd(data$birth_date))

  # This code creates a new variable generation based on the birth_year variable
  # The generation variable categorizes the different birth years into five different groups
  # The first group is the Silent Generation, the second group is the Baby Boomers, 
  # the third group is Gen X, the fourth group is the Millennial and the fifth group is Gen Z
  data$generation <- ifelse(data$birth_year >= 1997, "Gen Z",
                     ifelse(data$birth_year >= 1981, "Millennials",
                     ifelse(data$birth_year >= 1965, "Gen X",
                     ifelse(data$birth_year >= 1946, "Baby Boomers", "The Silent Generation"))))
  
  return(data)
}

# Explore, Clean, and Pre-process the data all at once
# The data is first explored and then cleaned and pre-processed.
explore_data()

data <- data %>%
  clean_data() %>%
  preprocess_data()

# View the processed data in table format
View(data)

#+============================================================================+
#| Data Exploration for Ideas of questions to ask                             |
#+============================================================================+

#---- 1st Idea - Trends in employee termination within 10 years ----
first_idea <- function() {
  
  # create a new column for termination year
  data$termination_year <- format(data$termination_date, "%Y")
  
  # group the data by termination year and termination reason
  termination_by_year_reason <- data %>%
    filter(status == "TERMINATED") %>% # Filter to only terminated employees
    group_by(termination_year, termination_reason) %>% # Group by termination year and termination reason
    summarize(count = n(), .groups = 'drop') # Summarize number of employees

  # create a stacked bar chart
  # specifies the data and the variables to be used for the x and y axes and for color
  ggplot(termination_by_year_reason, aes(x = termination_year, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity") + # `stat = "identity"` argument tells ggplot to use the actual values in "count"
    scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a")) + # specify color for each termination reason
    labs(title = "Employee Termination by Year and Reason", # title for the plot
          x = "Year of Termination", # label for the x axis
          y = "Number of Terminations", # label for the y axis
          fill = "Termination Reason") + # label for the legend
    theme_bw() + # set theme to black and white
    theme(plot.title = element_text(hjust = 0.5), # center the title
          axis.text.x = element_text(vjust = 0.5)) # move the x axis labels closer to the axis
}
first_idea()

# Observation:
# The layoff only occurred during the years 2014 and 2015
# A significant number of employees left the company in 2014, 
# and the most frequently cited reason for leaving was the layoff.

# Questions that can be asked:
# What factors are associated with employee termination?
# What factors contributed to the high rate of employee layoffs?
# What is the average length of service for terminated employees? Does this vary by department or job title?

#---- 2nd Idea - Analysis of employee termination by gender within ten years ----
second_idea <- function() {
  
  # Create a new column for the year of termination
  data$termination_year <- as.integer(substr(data$termination_date, 1, 4))
  
  # Filter to use only the terminated employees' data
  data <- data %>%
    filter(status == "TERMINATED")

  # Create the bar chart
  # factor function is used to ensure that the X-axis shows all years 
  # even if there is no data for a particular year
  ggplot(data, aes(x = termination_year, group = gender, fill = gender)) +
    # dodge the bars so that they do not overlap
    geom_bar(position = "dodge", color = "black", stat = "count") +
    # Sets the breaks on the x-axis 
    scale_x_continuous(breaks = seq(min(data$termination_year), 
                                    max(data$termination_year), 1)) +
    labs(title = "Number of Terminated Employees by Gender and Year",
         x = "Year of Termination", 
         y = "Count of Employees", 
         fill = "Gender") +
    theme_minimal()
}
second_idea()

# Observation:
# Most number of female employees are terminated in 2014
# The termination of female employees are greater than male employees

# Questions that can be asked:
# How does the number of terminated employees differ by gender in 2014?

#---- 3rd Idea - Understand the age distribution of the workforce ----
third_idea <- function() {
  
  # Count number of employees in each generation
  generation_count <- data %>%
    group_by(status_year, generation) %>%
    summarize(count = n(), .groups = 'drop') %>%
    filter(!is.na(generation)) %>%
    ungroup() 
    # After grouping the data and summarizing it, 
    #remove the grouping structure of the data frame
  
  # Plot number of employees in each age group by year using line graph
  ggplot(generation_count, aes(x = status_year, y = count, color = generation)) +
    geom_line(linewidth = 0.75, linejoin = "mitre", lineend = "butt") +
    labs(x = "Year", 
         y = "Number of Employees", 
         title = "Number of Employees by Age Generation Over Time") +
    # Sets the qualitative color palette for the different generations.
    scale_color_brewer(type = "qual", 
                       palette = "Dark2") +
    # Sets the breaks on the x-axis 
    scale_x_continuous(breaks = seq(min(generation_count$status_year), 
                                    max(generation_count$status_year), 1)) +
      
    theme_minimal()
}
third_idea()

# Observation:
# The number of millennial employees has been increasing year by year
# Starting from 2013, there appears to be a decline in the total number of employees across all generations.
# The silent generation disappeared after 2010

# Questions that can be asked:
# Why has the total number of employees been decreasing since 2013? 
#==============================================================================

#==== Q1. What factors are associated with employee termination? ===========

#---- Analysis 1.1 - Analyze the relationship between employee age and termination reason ----
Q1_1 = function() {
  
  # Filter data for terminated employees
  data_filtered <- data %>% filter(status == "TERMINATED")
  
  # Create the density graph
  ggplot(data_filtered, aes(x = age, fill = termination_reason)) +
    # Adjust the visibility of the overlapping densities
    geom_density(alpha = 0.5, outline.type = "upper",adjust = 2) + 
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
  
  # create a bar chart for each job title
  ggplot(data_terminated, aes(x = termination_reason, fill = termination_reason)) + 
    # This adds the count of terminated employees for each termination reason to the plot  
    stat_count() +
    # This divides the plot into facets (multiple plots arranged in a grid) based
    facet_wrap( ~ job_title) +
    labs(x = "Termination Reason", 
         y = "Count", 
         fill = "Termination Reason") +
    scale_fill_manual(values = c("Layoff" = "#0072B2", 
                                 "Resignation" = "#E69F00", 
                                 "Retirement" = "#09DDFA")) +
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
Q1_3 = function() {
  
  # Select only the data for employees who have been terminated and worked as cashier or meat cutter
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
  cashier_bar_plot <- ggplot(cashier_count, aes(x = service_count, y = count, fill = termination_reason)) + 
    # Creates vertical bars to represent the data
    geom_col(color = "black") +
    labs(title = "Cashier",
         x = "Length of Service (Years)",
         y = "Terminated Employees (Person)",
         fill = "Termination Reason") +
    scale_x_continuous(breaks = seq(0, max(cashier_terminated$service_count), by = 1)) +
    theme_minimal()
  
  # Visualize stacked bar chart for terminated employees who worked as meat cutter
  meat_cutter_bar_plot <- ggplot(meat_cutter_count, aes(x = service_count, y = count, fill = termination_reason)) + 
    # Creates vertical bars to represent the data
    geom_col(color = "black") +
    labs(title = "Meat Cutter",
         x = "Length of Service (Years)",
         y = "Terminated Employees (Person)",
         fill = "Termination Reason") +
    scale_x_continuous(breaks = seq(0, max(meat_cutter_terminated$service_count), by = 1)) +
    theme_minimal()
  
  # Arrange the plots side by side
  grid.arrange(cashier_bar_plot, meat_cutter_bar_plot, ncol=2)
}
Q1_3()

# Explanation:
# Based on the graph shown above,  we can conclude that meat cutters tend to retire after a longer period of service compared to cashiers, who are more likely to resign after a shorter period of work
# The reason behind this might be jobs, such as cashiering, may be mentally demanding due to the need to interact with customers and handle money accurately
# This could lead to burnout or stress, which could also result in employees leaving the job earlier or being terminated.
# The job of meat cutter demands a particular skill set and expertise, which suggests that it may take a considerable amount of time (usually 2-5 years) for them to attain the required level of proficiency. 
# This factor could be one of the reasons behind the higher retirement rate among meat cutters after serving for a longer duration.

#---- Analysis 1.4 - Analyze the distribution of termination reasons across different departments----
Q1_4 = function() {
  
  data <- filter(data, status == "TERMINATED")
  
  # Calculate the count of termination reasons by department
  termination_counts <- data %>%
    group_by(department_name, termination_reason) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(department_name = fct_reorder(department_name, count, .desc = TRUE))
  
  # Create the bar plot
  ggplot(termination_counts, aes(x = department_name, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity") +
    labs(x = "Department", y = "Count", fill = "Termination Reason") +
    ggtitle("Distribution of Termination Reasons across Departments") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")

}
Q1_4()
#---- Analysis 1.5 - Investigate if there is a correlation between employee age and termination type ----
Q1_5 = function() {
  
  # Filter the terminated employees data
  terminated_data <- filter(data, status == "TERMINATED")
  
  # Create the box plot
  ggplot(terminated_data, aes(x = termination_type, y = age)) +
    geom_boxplot() +
    labs(x = "Termination Type", y = "Employee Age") +
    ggtitle("Correlation between Employee Age and Termination Type") +
    scale_y_continuous(limits = c(15, 66), breaks = seq(15, 66, by = 2)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
}
Q1_5()
#---- Analysis 1.6 - Compare the average length of service for terminated employees across different termination reasons ----
Q1_6 = function() {
  
  # Filter the terminated employees data
  terminated_data <- filter(data, status == "TERMINATED")
  
  # Calculate the average length of service by termination reason
  avg_service_length <- terminated_data %>%
    group_by(termination_reason) %>%
    summarise(avg_length_of_service = mean(service_count), .groups = 'drop') %>%
    arrange(desc(avg_length_of_service))
  
  # Create the bar plot
  ggplot(avg_service_length, aes(x = termination_reason, y = avg_length_of_service)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Termination Reason", y = "Average Length of Service") +
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 1))+
    ggtitle("Average Length of Service for Terminated Employees by Termination Reason") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
}
Q1_6()

#---- Analysis 1.7 - Examine the relationship between termination reasons and gender ----
Q1_7 = function() {
  
  # Filter the terminated employees data
  terminated_data <- filter(data, status == "TERMINATED")
  
  # Calculate the count of termination reasons by gender
  termination_counts <- terminated_data %>%
    group_by(termination_reason, gender) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the stacked bar plot
  ggplot(termination_counts, aes(x = termination_reason, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 50))+
    labs(x = "Termination Reason", y = "Count", fill = "Gender") +
    ggtitle("Relationship between Termination Reasons and Gender") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom")
}
Q1_7()

#------------------------------- Conclusion -----------------------------------
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
    summarize(count = n()) %>%
    ungroup()
  
  # Create a table showing the count of layoffs in each department
  table <- data_sub_agg %>% 
    spread(department_name, count, fill = 0)

  # Create a heat map
  ggplot(data_sub_agg, aes(x = department_name, y = "")) + 
  geom_tile(aes(fill = count), color = "white", size = 1.5) + 
  scale_fill_gradient(low = "#FFFF00", high = "#FF0000") + 
  # element_blank() removes the text labels
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1, 1, 1, 5), "cm"),
        plot.title = element_text(hjust = 0.5), # centers the plot title horizontally 
        panel.background = element_rect(color = "black", linewidth = 1, fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Count of Layoffs by Department") +
  labs(x = "Department", y = "") 
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
  
  # Group the data by job_title and termination_reason, and `aggregate()` count the number of employee layoffs by job title and termination reason.
  # `FUN` specifies the function to be used to collapse the data
  # `length` function is being used to count the number of employee IDs for each combination of job title and termination reason.
  job_layoff_count <- aggregate(layoff_data$employee_ID, by = list(layoff_data$job_title, layoff_data$termination_reason), FUN=length)

  # Create each column's name
  names(job_layoff_count) <- c("job_title", "termination_reason", "count")
  

  # Create a stacked bar plot
  ggplot(job_layoff_count, aes(job_title, y=count, fill = termination_reason)) +
    geom_bar(stat = "identity") +
    labs(x = "Job Title", 
         y = "Count", 
         fill = "Termination Reason") +
    ggtitle("Relationship between Job Title and Layoff") +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom", 
          axis.text.x = element_text(angle = 45, hjust = 1))
}
Q2_2()

# Explanation:
# According to the bar graph, the job category with the highest number of employees who were laid off is cashier, followed by dairy person
# This could be attributed to the advancements in technology and automation, which have resulted in the gradual replacement of human labor with machinery in 
# industries that are more easily replaceable, such as those represented by the aforementioned job categories.

#---- Analysis 2.3 - Analyze the correlation among employees' generations, employees' gender and layoffs----
Q2_3 = function() {
  # summarize the data and store it in a new object
  laid_off_summary <- data %>%
    filter(termination_reason == "Layoff") %>%
    group_by(generation, gender) %>%
    summarize(num_laid_off = n(), .groups = 'drop')
  
  # plot stacked bar chart using the summarized data
  ggplot(laid_off_summary, aes(x=generation, y=num_laid_off, fill=gender)) +
    geom_bar(stat = "identity", position="stack") +
    labs(x="Age Generation", 
         y="Number of Laid Off Employees",
         fill="Gender") +
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
# Many members of the Silent Generation are retired and not actively participating in the workforce. 
# This could result in them being less affected by layoffs compared to other generations.

#---- Analysis 2.4 - Examine if there are any specific departments or locations that experienced a higher number of layoffs ----
Q2_4 = function() {
  
  # Filter the terminated employees data for layoffs
  layoffs_data <- filter(data, termination_reason == "Layoff")
  
  # Count the number of layoffs by department and location
  layoffs_counts <- layoffs_data %>%
    group_by(department_name, city_name) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Sort the data in descending order by count
  layoffs_counts <- layoffs_counts %>%
    arrange(desc(count))
  
  # Create the bar plot
  ggplot(layoffs_counts, aes(x = department_name, y = count, fill = city_name)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Department", y = "Number of Layoffs", fill = "Location") +
    ggtitle("Number of Layoffs by Department and Cities") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}
Q2_4()

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

#==== Q3. How does the number of terminated employees differ by gender in year 2014? =========

#---- Analysis 3.1 - Find the correlation between the age of the employee and the likelihood of termination by gender in year 2014 ----
Q3_1 = function() {
  
  # Filter out the year of termination date
  data$termination_year <- year(ymd(data$termination_date))
  
  # Filter the data that only applicable for those who terminated in 2014
  data_2014 <- data %>%
    filter(termination_year %in% c(2014), status == "TERMINATED")

  # Create the histogram for females
  female_data <- subset(data_2014, gender == "Female")
  female_hist <- ggplot(female_data, aes(x = age, fill = termination_reason)) +
    geom_histogram(binwidth = 2, color = "black", width = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Female Employee Termination by Age and Reason", 
         x = "Age", 
         y = "Frequency")
  
  # Create the histogram for males
  male_data <- subset(data_2014, gender == "Male")
  male_hist <- ggplot(male_data, aes(x = age, fill = termination_reason)) +
    geom_histogram(binwidth = 2, color = "black", width = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Male Employee Termination by Age and Reason", 
         x = "Age", y = "Frequency")
  
  # Combine the histograms to show them all in a graph
  grid.arrange(female_hist, male_hist, ncol = 2)
}
Q3_1()

# Explanation
# There is a higher likelihood for older female employees to retire compared to older male employees, 
# and no retirement occurred for male employees in 2014. 
# This trend may be attributed to the fact that old female employees are more likely to be employed in job roles 
# that have different retirement patterns compared to those of male employees.

#---- Analysis 3.2 - Identify the job roles with the highest number of employees equals to or greater than the age of 60 in year 2014, categorized by gender ----
Q3_2 <- function() {
  
  # Filter out the year of termination date
  data$termination_year <- year(ymd(data$termination_date))
  
  # Filter the data based on specific conditions, group by gender, job_title, and status, 
  # calculate the count of terminated employees for each group, and reorder job titles based on count.
  data_filtered <- data %>%
    filter(age >= 60, termination_year %in% c(2014)) %>%
    group_by(gender, job_title, status) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(job_title = fct_reorder(job_title, count, .desc = FALSE))
  
  # Generate the bar chart using ggplot library, where x-axis is job_title, y-axis is count, 
  # fill is based on status, and facets are based on gender.
  ggplot(data_filtered, aes(x = job_title, y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.4) +
    labs(x = "Job Title", 
         y = "Count of Terminated Employees", 
         fill = "Status") +
    facet_wrap(~ gender) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q3_2()

# Explanation:
# The analysis revealed that the job role with the greatest number of female employees over the age of 60 in 2014 was that of a storage manager. 
# This trend may be attributed to the physically demanding nature of the job, which requires endurance and strength that may become more challenging 
# for female employees as they age, leading to a higher probability of termination. 
# Additionally, it was observed that there were only a little numbers of male employees aged 60 in 2014, 
# possibly due to a retirement or layoff program that targeted male employees in this age group. 

#---- Analysis 3.3 - Find the relationship between city and employees' termination in year 2014, categorized by gender ----
Q3_3 <- function() {
  
  # Filter out the year of termination date
  data$termination_year <- year(ymd(data$termination_date))
  
  # Filter the data that only applicable for those who terminated in 2014
  data_filtered <- data %>%
    filter(status == "TERMINATED", termination_year %in% c(2014))
  
  # Counting terminated employees by gender and city
  terminated_by_city <- data_filtered %>%
    group_by(gender, city_name) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(city_name = fct_reorder(city_name, count, .desc = TRUE))
  
  # Plotting the graph
  ggplot(terminated_by_city, aes(x = city_name, y = count, fill = gender)) +
    geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.75), binwidth=1) +
    labs(x = "City", 
         y = "Count of Terminated Employees") +
    scale_fill_manual(values=c("red", "blue")) +
    ggtitle("Distribution of Terminated Employees by City and Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q3_3()


# Explanation:
# According to the graph, Vancouver had the highest number of terminated female employees, followed by Fort Nelson. 
# This may be attributed to a possible decline in the industries where female employees were more represented in Vancouver and Fort Nelson, resulting in higher termination rates.

#---- Analysis 3.4 - Identify the job roles with the highest number of female employees in the city, Vancouver and Fort Nelson in year 2014 ----
Q3_4 <- function() {
  
  # Filter out the year of termination date
  data$termination_year <- year(ymd(data$termination_date))
  
  # Filter data for Vancouver & Fort Nelson and group by gender and job title
  data_filtered_vancouver <- data %>% 
    filter(city_name == "Vancouver", gender == "Female", termination_year %in% c(2014)) %>% 
    group_by(gender, job_title) %>% 
    summarize(count = n(), .groups = "drop")
  
  data_filtered_fort_nelson <- data %>% 
    filter(city_name == "Fort Nelson", gender == "Female", termination_year %in% c(2014)) %>% 
    group_by(gender, job_title) %>% 
    summarize(count = n(), .groups = "drop")
  
  # Order job titles by descending count
  data_filtered_vancouver <- data_filtered_vancouver %>% 
    mutate(job_title = fct_reorder(job_title, count, .desc = FALSE))

  data_filtered_fort_nelson <- data_filtered_fort_nelson %>% 
    mutate(job_title = fct_reorder(job_title, count, .desc = FALSE))
  
  # Plot stacked bar chart
  vancouver_bar_plot <- ggplot(data_filtered_vancouver, aes(x = job_title, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    labs(x = "Jobs in Vancouver city", 
         y = "Number of Terminated Employees in year 2014", 
         fill = "Gender") +
    # Flip the coordinates of x & y
    coord_flip() +
    theme(legend.position = "bottom")
  
  fort_nelson_bar_plot <- ggplot(data_filtered_fort_nelson, aes(x = job_title, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    labs(x = "Jobs in Fort Nelson city", 
         y = "Number of Terminated Employees in year 2014", 
         fill = "Gender") +
    # Flip the coordinates of x & y
    coord_flip() +
    theme(legend.position = "bottom")
  
  grid.arrange(vancouver_bar_plot, fort_nelson_bar_plot, ncol = 2)
}
Q3_4()

# Explanation:
# The graph suggests that female employees in Vancouver were mostly terminated from their roles as recruiters, whereas in Fort Nelson, 
# female employees were predominantly terminated from their positions as meat cutters and dairy persons. 
# This discrepancy in termination rates could potentially be due to gender bias, 
# where female employees in these industries may be subject to negative stereotypes and be viewed as less capable than their male counterparts, 
# leading to higher rates of termination.

#---- Analysis 3.5 - Calculate the total number of terminated employees in 2014 and compare it by gender ----
Q3_5 = function() {
  
  # Filter the terminated employees data for the year 2014
  terminated_2014 <- filter(data, year(termination_date) == 2014)
  
  # Calculate the total number of terminated employees by gender
  terminated_counts <- terminated_2014 %>%
    group_by(gender) %>%
    summarise(total_terminated = n())
  
  # Create the bar plot
  ggplot(terminated_counts, aes(x = gender, y = total_terminated, fill = gender)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 50)) +
    labs(x = "Gender", y = "Total Terminated Employees", fill = "Gender") +
    ggtitle("Total Terminated Employees in 2014 by Gender") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
}
Q3_5()

#---- Analysis 3.6 - Investigate if there is a correlation between gender and the length of service for terminated employees in 2014 ----
Q3_6 = function() {
  # Filter the terminated employees data for the year 2014
  terminated_2014 <- filter(data, year(termination_date) == 2014)
  
  # Create the violin plot
  ggplot(terminated_2014, aes(x = gender, y = service_count, fill = gender)) +
    geom_violin() +
    labs(x = "Gender", y = "Length of Service", 
         title = "Correlation between Gender and Length of Service for Terminated Employees in 2014") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
}
Q3_6()

#---- Analysis 3.7 - Examine if there are any gender disparities in termination rates across different departments in 2014 ----
Q3_7 = function() {
  
  # Filter the terminated employees data for the year 2014
  terminated_2014 <- filter(data, year(termination_date) == 2014)
  
  # Calculate the termination counts by gender and department
  termination_counts <- terminated_2014 %>%
    group_by(department_name, gender) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the stacked bar plot
  ggplot(termination_counts, aes(x = department_name, y = count, fill = gender)) +
    geom_bar(stat = "identity") +
    labs(x = "Department", y = "Termination Count", fill = "Gender") +
    ggtitle("Termination Rates by Gender and Department in 2014") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}
Q3_7()

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
#---- Analysis 4.1 - Find the relationship between department and terminated employee after 2013 ----
Q4_1 <- function() {
  
  # Filter data for terminated employees since 2013 and group by department and year
  termination_count <- data %>% 
    filter(status == "TERMINATED", status_year >= 2013) %>%
    group_by(department_name, status_year) %>%
    summarize(count = n(), .groups = "drop")
  
  # Generate the stacked bar chart using ggplot library, where x-axis is department name,
  # y-axis is count, fill is based on year, and departments are reordered based on count.
  ggplot(termination_count, aes(x = reorder(department_name, -count), y = count)) +
    geom_col(aes(fill = factor(status_year))) +
    labs(x = "Department", 
         y = "Number of Terminated Employees", 
         fill = "Year") +
    # Manually set the fill colors for each year using scale_fill_manual()
    scale_fill_manual(values = c("2013" = "#DD4321", 
                                 "2014" = "#0F2199", 
                                 "2015" = "#AF3376")) +
    # Rotate the x-axis text by 45 degrees for better readability
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # Add a title to the chart
    ggtitle("Jobs termination after year 2013")
}
Q4_1()

# Explanation:
# The customer service department had the most terminations after 2013, while the audit, compensation, investment, and legal departments had almost none. 
# This may be due to customer service jobs can be demanding, and employees may need to deal with angry or frustrated customers. 
# This can lead to high levels of stress and burnout, which may contribute to turnover. 
# Employees in the audit, compensation, investment, and legal departments may be more satisfied with their work, which could lead to lower turnover rates.

#---- Analysis 4.2 - Determine the percentage of different generation employees in each department before and after year 2013 ----
Q4_2 <- function() {
  
  # Filter data for active employees after 2013, group by department and generation and summarize count
  data_after_2013 <- data %>%
    filter(status_year >= 2013, status == "ACTIVE") %>%
    group_by(department_name, generation) %>%
    summarize(count = n(), .groups = 'drop') %>%
    group_by(department_name) %>%
    mutate(pct = count / sum(count) * 100)
  
  # Filter data for active employees before 2013, group by department and generation and summarize count
  data_before_2013 <- data %>%
    filter(status_year < 2013, status == "ACTIVE") %>%
    group_by(department_name, generation) %>%
    summarize(count = n(), .groups = 'drop') %>%
    group_by(department_name) %>%
    mutate(pct = count / sum(count) * 100)
  
  # Filter departments with more than two generations
  data_before_2013_filtered <- data_before_2013 %>%
    group_by(department_name) %>%
    summarize(num_generations = n_distinct(generation)) %>%
    filter(num_generations > 2) %>%
    select(department_name)
  
  data_after_2013_filtered <- data_after_2013 %>%
    group_by(department_name) %>%
    summarize(num_generations = n_distinct(generation)) %>%
    filter(num_generations > 2) %>%
    select(department_name)
  
  # Join filtered data with department names to keep only relevant departments
  data_before_2013_filtered <- semi_join(data_before_2013, data_before_2013_filtered, by = "department_name")
  data_after_2013_filtered <- semi_join(data_after_2013, data_after_2013_filtered, by = "department_name")
  
  # Create the grouped bar chart for before 2013
  plot_before_2013 <- ggplot(data_before_2013_filtered, aes(x = department_name, y = pct, fill = generation)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("#FDB813", "#00A6ED", "#E21F5D", "#1F9433")) +
    labs(x = "Department", 
         y = "Percentage of Employees", 
         fill = "Generation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Percentage of Employees in Each Department by Generation before year 2013")
  
  # Create the grouped bar chart for after 2013
  plot_after_2013 <- ggplot(data_after_2013_filtered, aes(x = department_name, y = pct, fill = generation)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("#FDB813", "#00A6ED", "#E21F5D", "#1F9433")) +
    labs(x = "Department", 
         y = "Percentage of Employees", 
         fill = "Generation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Percentage of Employees in Each Department by Generation after year 2013")
  
  # Create the grouped bar chart for after 2013
  grid.arrange(plot_before_2013, plot_after_2013, ncol = 2)
}
Q4_2()

# Explanation:
# According to Figure 4.4.5, the composition of employees in the customer service department evolved from before to after 2013 
# with a decline in Gen X staff and a rise in Millennial staff. 
# One possible explanation for this transition is that Gen X employees who worked in customer service for an extended period of time 
# may have become exhausted and stressed, pushing them to choose other career opportunities inside or outside the company. 
# Customer service, on the other hand, may be interesting to millennial employees in their early stages of employment 
# due to its ability to strengthen communication and interpersonal skills. 

#---- Analysis 4.3 - Examine the distribution of termination reasons for employees who left after 2013 to identify if there is a dominant reason contributing to the decrease ----
Q4_3 <- function() {
  
  # Filter data for employees who left after 2013
  terminated_after_2013 <- data %>%
    filter(termination_date > as.Date("2013-01-01"), status == "TERMINATED")
  
  # Count the frequency of each termination reason
  termination_reason_counts <- terminated_after_2013 %>%
    count(termination_reason)
  
  # Sort termination reasons by frequency in descending order
  termination_reason_counts <- termination_reason_counts %>%
    arrange(desc(n))
  
  # Create a lollipop plot
  ggplot(termination_reason_counts, aes(x = termination_reason, y = n)) +
    geom_segment(aes(xend = termination_reason, yend = 0), color = "darkblue", size = 1.5) +
    geom_point(size = 3, color = "darkblue") +
    labs(x = "Termination Reason", y = "Frequency", title = "Distribution of Termination Reasons\n(For Employees Who Left After 2013)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
}
Q4_3()
#---- Analysis 4.4 - Investigate the average length of service for terminated employees after 2013 and compare it to previous years to determine if there is a change in employee retention ----
Q4_4 <- function() {
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees after 2013
  terminated_after_2013 <- data %>%
    filter(termination_date > as.Date("2013-01-01"), status == "TERMINATED")
  
  # Calculate average length of service by year
  average_service_length <- terminated_after_2013 %>%
    group_by(year = termination_year) %>%
    summarize(avg_service_length = mean(length_of_service))
  
  # Create bar plot
  ggplot(average_service_length, aes(x = year, y = avg_service_length)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    geom_text(aes(label = round(avg_service_length, 2)), vjust = -0.5, color = "white") +
    labs(x = "Year", y = "Average Length of Service", title = "Average Length of Service for Terminated Employees\n(After 2013)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

Q4_4()

#---- Analysis 4.5 - Investigate whether there are any differences in the gender distribution of terminated employees after 2013 compared to previous years ----
Q4_5 <- function() {
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Create a variable to identify the time period (after 2013 or previous years)
  data$termination_period <- ifelse(data$termination_year > 2013, "After 2013", "Previous Years")
  
  # Filter data for terminated employees
  terminated_employees <- data %>%
    filter(status == "TERMINATED")
  
  # Calculate the proportion of terminated employees by gender and time period
  gender_distribution <- terminated_employees %>%
    group_by(gender, termination_period) %>%
    summarize(count = n(), .groups = 'drop') %>%
    mutate(prop = count / sum(count))
  
  # Create Grouped Bar Plot
  ggplot(gender_distribution, aes(x = termination_period, y = prop, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Time Period", y = "Proportion", title = "Gender Distribution of Terminated Employees") +
    scale_fill_manual(values = c("darkblue", "darkred"), labels = c("Male", "Female")) +
    theme_minimal() +
    theme(legend.position = "top")
}
Q4_5()

#---- Analysis 4.6 - Analyze the age distribution of terminated employees after 2013 and compare it to previous years to identify if there are any age-related factors contributing to the decrease ----
Q4_6 <- function() {
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Create a variable to identify the time period (after 2013 or previous years)
  data$termination_period <- ifelse(data$termination_year > 2013, "After 2013", "Previous Years")
  
  # Filter data for terminated employees
  terminated_employees <- data %>%
    filter(status == "TERMINATED")
  
  # Create a density plot for age distribution
  ggplot(terminated_employees, aes(x = age, fill = termination_period)) +
    geom_density(alpha = 0.5) +
    labs(x = "Age", y = "Density", title = "Age Distribution of Terminated Employees") +
    scale_fill_manual(values = c("darkblue", "darkred"), labels = c("After 2013", "Previous Years")) +
    theme_minimal()
}
Q4_6()

#------------------------------- Conclusion ------------------------------------
# In conclusion, the decline in total employees since 2013 can be linked to high turnover rates in the customer service department, as well as a shift in workforce makeup in this area. 
# The hard nature of customer service professions, which sometimes requires dealing with problematic clients and can lead to burnout and stress, may contribute to the department's high turnover rates. 
# Moreover, the decline in Gen X staff and the rise in Millennial staff in the customer service department suggest that the appeal of this department varies by generation. 
# While Gen X employees may have become exhausted and stressed from working in customer service for an extended period of time, Millennial employees may find this department interesting due to its ability to strengthen communication and interpersonal skills. 
# The transition in staff typifies how workforce dynamics can change over time with generational shifts.

#==============================================================================

#==== Q5. How does the average length of service vary among terminated employees? ====
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
  labs(x = "Year", 
       y = "Average length of service (years)",
       title = "Average Length of Service for Terminated Employees by Year") +
  scale_x_continuous(breaks = seq(min(term_data$status_year), max(term_data$status_year), 1))
}
Q5_1()

# Explanation:
# The trend of the average length of service shows a decline from 2009 to 2011, 
# possibly due to the global recession leading to higher termination rates and shorter service periods for terminated employees.
# However, from 2011 to 2015, there was a rapid increase in the average length of service, 
# which may be attributed to the economic recovery and increased hiring during that period

#---- Analysis 5.2 - Find the relationship between average length of service and departments ----
Q5_2 <- function() {
  
  # Group the data by department and calculate the average length of service
  avg_service_dept <- aggregate(data$service, by=list(data$department_name), FUN=mean)
  
  # Rename the columns name
  names(avg_service_dept) <- c("department_name", "avg_service_length")
  
  # Reorder the departments by average service length
  avg_service_dept <- avg_service_dept %>%
    mutate(department_name = fct_reorder(department_name, avg_service_length, .desc = FALSE))
  
  # Set theme options
  my_theme <- theme_minimal() +
    theme(plot.background = element_rect(fill = "#f2f2f2"),
          panel.background = element_rect(fill = "#f2f2f2"),
          axis.line = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "#f2f2f2"),
          panel.grid.major = element_line(color = "white", linewidth = 0.25),
          panel.grid.minor = element_blank())
  
  # Plot the scatter plot with line of best fit
  ggplot(avg_service_dept, aes(x = department_name, y = avg_service_length)) +
    geom_point(size = 3, color = "navy") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
    ggtitle("Average Length of Service by Department") +
    labs(x = "Department", 
         y = "Average Length of Service") +
    scale_y_continuous(limits = c(5, 25), breaks = seq(5, 25, by = 1)) +
    scale_color_manual(values = "navy", name = "") +
    my_theme
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
  avg_service_dept <- aggregate(data$service_count, by=list(data$city_name), FUN=mean)
  
  # Rename the columns name
  names(avg_service_dept) <- c("service_count", "city_name")
  
  # Reorder the departments by average service length in descending order
  avg_service_dept <- avg_service_dept %>%
    mutate(service_count = fct_reorder(service_count, city_name, .desc = TRUE))
  
  # Plot the bar graph
  ggplot(avg_service_dept, aes(x = service_count, y = city_name)) +
  geom_bar(stat="summary", fun = "mean", fill="maroon") +
  ggtitle("Average Length of Service by Department") +
  labs(x = "City", 
       y = "Average Length of Service") +
  coord_cartesian(ylim = c(7, 15)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q5_3()

# Explanation:
# Based on the graph, it can be observed that "Ocean Falls" is the city with the longest average length of service.
# this may be that the company in this city may have a strong presence and good reputation in the community, 
# leading to higher job satisfaction and loyalty among employees.

#---- Analysis 5.4 - Determine the average length of service by gender ----
Q5_4 <- function() {
  
  # Filter for terminated employees
  term_data <- data %>%
    filter(status == "TERMINATED")
  
  # Calculate average length of service by gender
  service_by_gender <- term_data %>%
    group_by(gender) %>%
    summarize(avg_service = mean(service_count))
  
  # Plot the bar chart
  ggplot(service_by_gender, aes(x = gender, y = avg_service, fill = gender)) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(x = "Gender",
         y = "Average Length of Service (years)",
         title = "Average Length of Service for Terminated Employees by Gender") +
    scale_fill_manual(values = c("#FF6F61", "#8E8E93")) +
    theme_minimal()
}
Q5_4()

#---- Analysis 5.5 - Analyse average length of service by business unit ----
Q5_5 <- function() {
  # Calculate average length of service by business unit
  avg_service_by_unit <- data %>%
    group_by(business_unit) %>%
    summarize(avg_service = mean(service_count))
  
  # Sort business units by average service length in descending order
  avg_service_by_unit <- avg_service_by_unit %>%
    arrange(desc(avg_service))
  
  # Create a bar plot with average length of service by business unit
  ggplot(avg_service_by_unit, aes(x = reorder(business_unit, avg_service), y = avg_service)) +
    geom_bar(stat = "identity", fill = "#0077B5", width = 0.5) +
    labs(x = "Business Unit",
         y = "Average Length of Service",
         title = "Average Length of Service by Business Unit") +
    theme_minimal()
}
Q5_5()

#---- Analysis 5.6 - Investigate average length of service by stores name ----
Q5_6 <- function() {
  # Calculate average length of service by store
  avg_service_by_store <- data %>%
    group_by(store_name) %>%
    summarize(avg_service = mean(service_count),
              std_service = sd(service_count))
  
  # Create a bar plot with error bars
  ggplot(avg_service_by_store, aes(x = store_name, y = avg_service, fill = store_name)) +
    geom_bar(stat = "identity", color = "black", width = 0.6) +
    labs(x = "Store",
         y = "Average Length of Service",
         title = "Average Length of Service by Store") +
    theme_minimal()
}
Q5_6()
#------------------------------- Conclusion -----------------------------------
# The average length of service for terminated employees varies significantly depending on the year, department, and city. 
# By analyzing these variations, companies can gain valuable insights into factors that contribute to employee retention and 
# implement strategies to improve employee loyalty and reduce turnover rates.

#==============================================================================

#==== Q6. Are there any specific cities, business unit or store locations with a higher rate of employee termination? ====
#---- Analysis 6.1 - Analyze the termination rate in different cities and store name ----
Q6_1 <- function() {
  
  # Filter data for terminated employees
  terminated_employees <- data %>%
    filter(status == "TERMINATED")
  
  # Calculate termination rate and count by city and store location
  termination_data <- terminated_employees %>%
    group_by(city_name, store_name) %>%
    summarize(termination_rate = n() / nrow(data),
              termination_count = n(), .groups = 'drop')
  
  # Create bubble plot
  ggplot(termination_data, aes(x = city_name, y = store_name, size = termination_count, fill = termination_rate)) +
    geom_point(shape = 21, color = "black") +
    scale_size_continuous(range = c(2, 10)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(x = "City", y = "Store Name", title = "Termination Rate in Different Cities and Store Locations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q6_1()

#---- Analysis 6.2 - Identify the cities or store locations with the highest number of terminated employees. ----
Q6_2 <- function() {
  
  # Filter data for terminated employees
  terminated_employees <- data %>%
    filter(status == "TERMINATED")
  
  # Calculate the count of terminated employees by city and store location
  termination_counts <- terminated_employees %>%
    count(city_name, store_name)
  
  # Sort the termination counts in descending order
  termination_counts <- termination_counts %>%
    arrange(desc(n))
  
  # Create bar plot
  ggplot(termination_counts, aes(x = reorder(city_name, -n), y = n, fill = store_name)) +
    geom_bar(stat = "identity") +
    labs(x = "City", y = "Termination Count", title = "Cities with Highest Number of Terminated Employees") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")
  
}
Q6_2()

#---- Analysis 6.3 - Examine the distribution of terminated employees across different business units ----
Q6_3 <- function() {
  # Filter data for terminated employees
  terminated_employees <- data %>% filter(status == "TERMINATED")
  
  # Create a violin plot for business unit distribution
  ggplot(terminated_employees, aes(x = business_unit, y = age, fill = business_unit)) +
    geom_violin(trim = FALSE) +
    scale_fill_discrete(guide = FALSE) +
    labs(x = "Business Unit", y = "Age", title = "Distribution of Terminated Employees Across Business Units") +
    theme_minimal() +
    theme(legend.position = "none")
}
Q6_3()

#---- Analysis 6.4 - Examine the trend of employee termination rates over time for Vancouver city ----
Q6_4 <- function() {
  
  city <- "Vancouver"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific city
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & city_name == city)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))

  # Create a stacked bar plot
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate)) +
    geom_bar(stat = "identity", fill = "#3399FF", color = "black", width = 0.6) +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for", city)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_4()

#---- Analysis 6.5 - Determine the trend of employee termination rates over time for Victoria city ----
Q6_5 <- function() {
  
  city <- "Victoria"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific city
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & city_name == city)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Create a point plot with lines connecting the points
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate)) +
    geom_point(color = "#3399FF", size = 3) +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for", city)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_5()

#---- Analysis 6.6 - Analyse the trend of employee termination rates over time for Nanaimo city ----
Q6_6 <- function() {
  
  city <- "Nanaimo"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific city
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & city_name == city)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Create a bar plot
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate, fill = termination_year)) +
    geom_bar(stat="identity", position = "dodge", color = "black") +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for", city)) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_6()

#---- Analysis 6.7 - Trend Analysis of Employee Termination Rates over Time for the Stores Business Unit ----
Q6_7 <- function() {
  
  unit <- "STORES"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific business_unit
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & business_unit == unit)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Create a line plot
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate, color = termination_year)) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for", unit)) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_7()

#---- Analysis 6.8 - Analysis of Employee Termination Rate Trends Over Time for Store 35 ----
Q6_8 <- function() {
  
  unit <- "35"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific business_unit
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & store_name == unit)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Create a bar plot
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate, fill = termination_year)) +
    geom_bar(stat="identity", position = "dodge", color = "black") +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for Store", unit)) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_8()

#---- Analysis 6.9 - Exploring the Trend of Employee Termination Rates over Time for Store 37 ----
Q6_9 <- function() {
  
  unit <- "37"
  
  data$termination_year <- format(data$termination_date, "%Y")
  
  # Filter data for terminated employees in the specific business_unit
  terminated_employees <- data %>%
    filter(status == "TERMINATED" & store_name == unit)
  
  # Calculate the termination rates by year
  termination_rates <- terminated_employees %>%
    group_by(termination_year) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Create a stacked bar chart with custom colors
  ggplot(termination_rates, aes(x = termination_year, y = termination_rate, fill = termination_year)) +
    geom_bar(stat="identity", width = 0.7, color = "black") +
    scale_fill_manual(values = c("#F9C74F", "#90BE6D", "#F94144", "#F8961E", "#8CCE3F", 
                                 "#4D4D4D", "#3D405B", "#FFC107", "#5E35B1", "#EC407A")) +
    labs(x = "Termination Year", y = "Termination Rate", title = paste("Termination Rate Trend for Store", unit)) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
Q6_9()

#==============================================================================

#==== Q7. How does the age generation impact employee termination? ====

#---- Analysis 7.1 - Analyze the proportion of different generations within each department ----
Q7_1 <- function() {
  
  # Subset data
  generation_dept <- data[, c("department_name", "generation")]
  
  # Calculate proportion of each generation within each department
  generation_prop <- prop.table(table(generation_dept$department_name, generation_dept$generation), margin = 1)
  
  # Reshape data for plotting
  generation_prop <- as.data.frame(generation_prop)
  generation_prop$Var1 <- as.character(generation_prop$Var1)
  generation_prop$Var2 <- as.character(generation_prop$Var2)
  
  # Create stacked bar plot
  ggplot(generation_prop, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity") +
    labs(x = "Department", y = "Proportion", fill = "Generation") +
    ggtitle("Proportion of Generations within Each Department") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}
Q7_1()

#---- Analysis 7.2 - Identify the departments with the highest representation of each generation ----
Q7_2 <- function() {
  
  # Group the data by age generation and termination reason and calculate the count
  termination_reasons <- data %>%
    filter(status == "TERMINATED", !is.na(generation)) %>%
    group_by(generation, termination_reason) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup()
  
  # Set the order of termination reasons for better readability
  termination_reasons$termination_reason <- factor(termination_reasons$termination_reason, 
                                                   levels = c("Resignation", "Layoff", "Retirement"))
  
  # Create the bar plot
  plot <- ggplot(termination_reasons, aes(x = generation, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(title = "Distribution of Termination Reasons by Age Generation",
         x = "Age Generation",
         y = "Frequency",
         fill = "Termination Reason") +
    scale_fill_manual(values = c("#FFB74D", "#4DB6AC", "#FF7043"),
                      labels = c("Resignation", "Layoff", "Retirement")) +
    theme_minimal()
  
  # Adjust the plot's appearance for better readability and design
  plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
Q7_2()

#---- Analysis 7.3 - Analyze the length of service by age generation and termination reason ----
Q7_3 <- function() {
  
  # Filter the data for terminated employees and relevant variables
  terminated_data <- data %>%
    filter(status == "TERMINATED", !is.na(generation), generation != "The Silent Generation") %>%
    select(generation, termination_reason, service_count)
  
  # Create the boxplot
  plot <- ggplot(terminated_data, aes(x = generation, y = service_count, fill = termination_reason)) +
    geom_boxplot() +
    labs(title = "Length of Service by Age Generation and Termination Reason",
         x = "Age Generation",
         y = "Length of Service",
         fill = "Termination Reason") +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 1)) +
    scale_fill_manual(values = c("#FFB74D", "#4DB6AC", "#FF7043"),
                      labels = c("Resignation", "Layoff", "Retirement")) +
    theme_minimal()
  
  # Adjust the plot's appearance for better readability and design
  plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
Q7_3()

#---- Analysis 7.4 - Analyse the termination rate by age generation ----
Q7_4 <- function() {
  
  # Calculate the termination rate by age generation
  termination_rate <- data %>%
    filter(status == "TERMINATED", !is.na(generation)) %>%
    group_by(generation) %>%
    summarize(termination_count = n()) %>%
    mutate(termination_rate = termination_count / sum(termination_count))
  
  # Sort the age generations in ascending order
  termination_rate$generation <- factor(termination_rate$generation, 
                                        levels = c("The Silent Generation", "Baby Boomers", "Gen X", "Millennials", "Gen Z"))
  
  # Create the bar plot
  plot <- ggplot(termination_rate, aes(x = generation, y = termination_rate)) +
    geom_bar(stat = "identity", fill = "#4DB6AC") +
    labs(title = "Termination Rate by Age Generation",
         x = "Age Generation",
         y = "Termination Rate") +
    theme_minimal()
  
  # Adjust the plot's appearance for better readability and design
  plot + theme(axis.text.x = element_text(angle = 45, hjust = 1),
               plot.title = element_text(hjust = 0.5))
  
}
Q7_4()

#---- Analysis 7.5 - Comparison of Termination Types by Age Generation ----
Q7_5 <- function() {
  
  # Group the data by age generation and termination type and calculate the count
  termination_types <- data %>%
    filter(status == "TERMINATED", !is.na(generation)) %>%
    group_by(generation, termination_type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup()
  
  # Set the order of termination types for better readability
  termination_types$termination_type <- factor(termination_types$termination_type, 
                                               levels = c("Voluntary", "Involuntary"))
  
  # Create the bar plot
  plot <- ggplot(termination_types, aes(x = generation, y = count, fill = termination_type)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(title = "Comparison of Termination Types by Age Generation",
         x = "Age Generation",
         y = "Frequency",
         fill = "Termination Type") +
    scale_fill_manual(values = c("#FFB74D", "#4DB6AC"),
                      labels = c("Voluntary", "Involuntary")) +
    theme_minimal()
  
  # Adjust the plot's appearance for better readability and design
  plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
Q7_5()

#---- Analysis 7.6 - Comparison of Termination reasons by age generation and gender ----
Q7_6 <- function() {
  # Filter the data for terminated employees and relevant variables
  terminated_data <- data %>%
    filter(status == "TERMINATED", !is.na(generation), !is.na(gender)) %>%
    select(generation, termination_reason, gender)
  
  # Group the data by age generation, termination reason, and gender, and calculate the count
  termination_counts <- terminated_data %>%
    group_by(generation, termination_reason, gender) %>%
    summarise(count = n(), .groups = "drop")
  
  # Create the bar plot
  plot <- ggplot(termination_counts, aes(x = generation, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid(gender ~ ., scales = "free_y", space = "free_y") +
    labs(title = "Termination Reasons by Age Generation and Gender",
         x = "Age Generation",
         y = "Frequency",
         fill = "Termination Reason") +
    scale_fill_manual(values = c("#FFB74D", "#4DB6AC", "#FF7043"),
                      labels = c("Resignation", "Layoff", "Retirement")) +
    theme_minimal()
  
  # Adjust the plot's appearance for better readability and design
  plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q7_6()

#---- Analysis 7.7 - Analyze Age Generation and Termination Reasons by Department ----
Q7_7 <- function() {
  
  # Filter the data for terminated employees and relevant variables
  terminated_data <- data %>%
    filter(status == "TERMINATED", !is.na(generation), !is.na(department_name)) %>%
    select(generation, termination_reason, department_name)
  
  # Calculate the count of terminations by age generation, termination reason, and department
  termination_counts <- terminated_data %>%
    group_by(generation, termination_reason, department_name) %>%
    summarise(count = n(), .groups = "drop")
  
  # Create the stacked bar plot
  ggplot(termination_counts, aes(x = department_name, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity") +
    facet_grid(. ~ generation) +
    labs(title = "Termination Reasons by Age Generation and Department",
         x = "Department",
         y = "Count",
         fill = "Termination Reason") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}
Q7_7()

#---- Analysis 7.8 - Identify Age Generation and Termination Reasons by City ----
Q7_8 <- function() {

  # Filter the data for terminated employees and relevant variables
  terminated_data <- data %>%
  filter(status == "TERMINATED", !is.na(generation), !is.na(city_name)) %>%
  select(generation, termination_reason, city_name)
  
  # Calculate the count of terminations by age generation, termination reason, and city
  termination_counts <- terminated_data %>%
  group_by(generation, termination_reason, city_name) %>%
  summarise(count = n(), .groups = "drop")
  
  # Create the stacked bar plot
  ggplot(termination_counts, aes(x = city_name, y = count, fill = termination_reason)) +
    geom_bar(stat = "identity") +
    facet_grid(generation ~ ., space = "free_y") +
    labs(title = "Termination Reasons by Age Generation and City",
    x = "City",
    y = "Count",
    fill = "Termination Reason") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom")
  
}
Q7_8()

#---- Analysis 7.9 - Find the termination reasons over time for different age generations ----
Q7_9 <- function() {
  
  # Filter the data for terminated employees and relevant variables
  terminated_data <- data %>%
    filter(status == "TERMINATED", !is.na(generation), !is.na(termination_date)) %>%
    select(generation, termination_reason, termination_date)
  
  # Extract the year from the termination date
  terminated_data$year <- lubridate::year(terminated_data$termination_date)
  
  # Calculate the count of terminations by age generation, termination reason, and year
  termination_counts <- terminated_data %>%
    group_by(generation, termination_reason, year) %>%
    summarise(count = n(), .groups = "drop")
  
  # Create the line plot
  ggplot(termination_counts, aes(x = year, y = count, color = termination_reason, group = termination_reason)) +
    geom_line(size = 1) +
    facet_grid(generation ~ .) +
    scale_x_continuous(breaks = seq(min(terminated_data$year), max(terminated_data$year), 1)) +
    labs(title = "Termination Reasons Over Time by Age Generation",
         x = "Year",
         y = "Count",
         color = "Termination Reason") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}
Q7_9()
#==============================================================================

#==== Q8. Has the proportion of employee termination been increasing over the years? ====
#---- Analysis 8.1 - Determine the proportion of terminated employees by year ----
Q8_1 <- function() {
  
  library(ggplot2)
  
  # Calculate the termination rate by year
  termination_rate <- data %>%
    group_by(status_year) %>%
    summarize(termination_rate = sum(status == "TERMINATED") / n())
  
  # Create the line plot using ggplot2
  ggplot(termination_rate, aes(x = status_year, y = termination_rate)) +
    geom_line(color = "magenta", linewidth = 1) +
    geom_point(color = "magenta", size = 3) +
    scale_x_continuous(breaks = seq(min(data$status_year), max(data$status_year), 1)) +
    labs(x = "Year", y = "Termination Rate", title = "Termination Rate Over the Years") +
    theme_minimal()
  
}
Q8_1()

#---- Analysis 8.2 - Find the termination rates by gender and year ----
Q8_2 <- function() {

  # Calculate the termination rates by gender and year
  termination_rates <- data %>%
    group_by(status_year, gender) %>%
    summarize(termination_rate = sum(status == "TERMINATED") / n(), .groups = 'drop')
  
  # Create the bar plot using ggplot2
  ggplot(termination_rates, aes(x = status_year, y = termination_rate, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = seq(min(data$status_year), max(data$status_year), 1)) +
    labs(x = "Year", y = "Termination Rate", title = "Termination Rates by Gender and Year") +
    scale_fill_manual(values = c("magenta", "blue"), labels = c("Male", "Female")) +
    theme_minimal()
}
Q8_2()

#---- Analysis 8.3 - Analyse the termination types by year ----
Q8_3 <- function() {
  
  # Calculate the count of termination types by year
  termination_types <- data %>%
    filter(status == "TERMINATED") %>%
    group_by(status_year, termination_type) %>%
    summarize(termination_count = n(), .groups = 'drop')
  
  # Create the stacked bar plot using ggplot2
  ggplot(termination_types, aes(x = status_year, y = termination_count, fill = termination_type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_x_continuous(breaks = seq(min(data$status_year), max(data$status_year), 1)) +
    labs(x = "Year", y = "Termination Count", title = "Termination Types by Year") +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) +
    theme_minimal()
}
Q8_3()

#---- Analysis 8.4 - Analyse the termination Rates by Department ----
Q8_4 <- function() {
  
  # Calculate the termination rates by department
  termination_rates <- data %>%
    group_by(department_name) %>%
    summarize(termination_rate = sum(status == "TERMINATED") / n(), .groups = 'drop')
  
  # Sort the termination rates in descending order
  termination_rates <- termination_rates %>%
    arrange(desc(termination_rate))
  
  # Create the bar plot using ggplot2
  ggplot(termination_rates, aes(x = department_name, y = termination_rate, fill = department_name)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(x = "Department", y = "Termination Rate", title = "Termination Rates by Department") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Q8_4()

#---- Analysis 8.5 - Analyse the termination Rates by Business Unit ----
Q8_5 <- function() {
  
  # Calculate the termination rates by business unit and year
  termination_rates <- data %>%
    group_by(status_year, business_unit) %>%
    summarize(termination_rate = sum(status == "TERMINATED") / n(), .groups = 'drop')
  
  # Create the line plot using ggplot2
  ggplot(termination_rates, aes(x = status_year, y = termination_rate, color = business_unit)) +
    geom_line() +
    geom_point(size = 3) +
    scale_x_continuous(breaks = seq(min(data$status_year), max(data$status_year), 1)) +
    labs(x = "Year", y = "Termination Rate", title = "Termination Rates by Business Unit and Year") +
    scale_color_manual(values = c("#E41A1C", "#377EB8")) +
    theme_minimal()
}
Q8_5()

#==============================================================================