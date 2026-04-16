

# Analyzes "Education Inequality" across 1,000 schools in the United States

#Complete until Q4. 

# 1.1  import data

library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)

# Read in CSV file 

edu_data <- read_csv("education_inequality_data.csv")
View(edu_data)



## 1.2 Data Cleaning 


# Check the data set's column and roles
glimpse(edu_data)

# check the data set's column and roles
dim(edu_data)


## Convert school_type and grade_level to factors
edu_data <- edu_data %>%
  mutate(
    school_type = as.factor(school_type),
    grade_level = as.factor(grade_level)
  )

## check the data type
str(edu_data)

# checking total missing values
sum(is.na(edu_data))

# checking missing values of columns
colSums(is.na(edu_data))

# Check for duplicates ids
sum(duplicated(edu_data$id))


  
  
#1.3 Summary statistics including mean, median, mode, and other basic statistics.
  
# 1.3 Summary Statistics
  
# edu data set overall summary
summary(edu_data)

# mode function
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_mode(edu_data$funding_per_student_usd)
get_mode(edu_data$avg_test_score_percent)
get_mode(edu_data$percent_low_income)
get_mode(edu_data$dropout_rate_percent)


## 1.4 Groped Summary by State, School Type, and Grade Level

# Fine Interest school types, grade level, and school and state
# Grouped summary particularly state, school_type, and grade_level with including min, mean, max, and sd

test_score_summary_by_state_type_level <- edu_data %>%
  group_by(state, school_type, grade_level) %>%
  summarise(
    Lower = min(avg_test_score_percent),
    Average = mean(avg_test_score_percent),
    Upper = max(avg_test_score_percent),
    SD = sd(avg_test_score_percent),
    Difference = max(avg_test_score_percent) - min(avg_test_score_percent),
    .groups = "drop"
  ) %>%
  arrange(Average)

view(test_score_summary_by_state_type_level)
print(test_score_summary_by_state_type_level)
summary(test_score_summary_by_state_type_level)



## 1.5 Summary by School Type and Grade Level Only

# Finding min, mean, max, sd, and difference in school type and grade level

test_score_summary_by_level <- edu_data %>%
  group_by(school_type, grade_level) %>%
  summarise(
    Lower = min(avg_test_score_percent),
    Average = mean(avg_test_score_percent),
    Upper = max(avg_test_score_percent),
    SD = sd(avg_test_score_percent),
    Difference = max(avg_test_score_percent) - min(avg_test_score_percent),
    .groups = "drop"
  ) %>%
  arrange(Average) %>% 
  view



summary(test_score_summary_by_level)
print(test_score_summary_by_level)



#1.6 Summary Statistics of the Key Variables

# Find the most interesting three variables Funding, avg_test score and percent low income
# Combined Summary of the most interesting three variables' Mean, SD, and Range

combined_stats <- edu_data %>%
  summarise(
    # Funding per Student
    Mean_Funding = mean(funding_per_student_usd),
    SD_Funding = sd(funding_per_student_usd),
    Range_Funding = max(funding_per_student_usd) - min(funding_per_student_usd),
    
    # Average Test Score
    Mean_Score = mean(avg_test_score_percent),
    SD_Score = sd(avg_test_score_percent),
    Range_Score = max(avg_test_score_percent) - min(avg_test_score_percent),
    
    # Percent Low Income
    Mean_LowIncome = mean(percent_low_income),
    SD_LowIncome = sd(percent_low_income),
    Range_LowIncome = max(percent_low_income) - min(percent_low_income)
  )

view(combined_stats)
print(combined_stats, width = Inf)




# 2. Basic Visualization

## 2.1 Histogram of Funding per Student
  
ggplot(edu_data, aes(x = funding_per_student_usd)) +
  geom_histogram(binwidth = 2000, color = "black", fill = "turquoise") +
  labs(
    title = "Distribution of Funding per student",
    x = "Funding per student (USD) ",
    y = "Count of schools "
  ) +
  theme_minimal()



  
#2.2 Histogram of test score 

ggplot (edu_data, aes (x = avg_test_score_percent))+
  geom_histogram(binwidth = 3, color = "white", fill = "steelblue") +
  labs (title = "Distribution of Average Test Scores",
        x = "Averge Test Score (%) ",
        y = "count of schools") +
  theme_gray()   


## 2.3 Histogram of Percent Low Income
  
ggplot(edu_data, aes(x = percent_low_income)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(
    title = "Distribution of Percent Low Income Students",
    x = "Low Income Students (%)",
    y = "Count of Schools"
  ) +
  theme_minimal()


## 2.4 Bar Chart - Schools by Grade Level
# Build Bar chart which is the numbers of Schools by grade level or School Type
# finding no of schools

edu_data %>%
  count(grade_level)

ggplot(edu_data, aes(x = grade_level, fill = grade_level)) +
  geom_bar() +
  labs(
    title = "The number of Schools by Grade level",
    x = "Grade level",
    y = "Count",
    fill = "Grade Level" 
  ) +
  theme_minimal()



## 2.5 Scatter Plot - Funding vs Test Scores by School Type

# Scatter for funding vs test scores

edu_data %>%
  ggplot(aes(x = funding_per_student_usd,
             y = avg_test_score_percent,
             colour = school_type)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ school_type) +
  labs(
    title = "Funding per student vs Average Test Score",
    subtitle = "Panels show each school type with a trend line",
    x = "Funding per student (USD)",
    y = "Average test score (%)",
    colour = "School type"
  ) +
  theme_classic()


## 2.6 Interactive 3D Scatter Plot
  
# Create the interactive 3D scatter plot

p_3d <- edu_data %>%
  # Define the 3 dimensions (X, Y, Z) and the color dimension
  plot_ly(
    x = ~funding_per_student_usd,
    y = ~avg_test_score_percent,
    z = ~percent_low_income,
    color = ~school_type, # Color points by school type
    marker = list(size = 5, opacity = 0.7)
  ) %>%
  add_markers() %>%
  layout(
    scene = list(
      xaxis = list(title = 'Funding per Student (USD)'),
      yaxis = list(title = 'Average Test Score (%)'),
      zaxis = list(title = 'Low Income Students (%)')
    ),
    title = "3D Analysis of Inequality: Funding, Scores, and Poverty by School Type"
  )

# Display the plot (This line is essential for RStudio/R Markdown)
p_3d



# 3. Exploratory Data Analysis (EDA)
  
## 3.1 Descriptive Statistics
  
# Display first 15 rows
head(edu_data, n = 15)



# ordering school types for creating plots
edu_data$school_type <- factor((edu_data$school_type),
                               levels = c("Public",
                                          "Charter",
                                          "Private"))

levels(edu_data$school_type)
class(edu_data$school_type)

# checking total columns excluding IDS

edu_data %>%
  select(state,
         school_name,
         school_type,
         ends_with(c("level", "usd", "percent", "ratio", "income", "minority"))) %>%
  names()

# check all column
names(edu_data)

  
## 3.2 Test Scores by School Type
# find min, mean, max, and difference of school type and average test score percent in USA

test_score_by_school_type <- edu_data %>%
  group_by(school_type) %>%
  summarise(
    lower = min(avg_test_score_percent),
    Average = mean(avg_test_score_percent),
    upper = max(avg_test_score_percent),
    SD_average = sd(avg_test_score_percent),
    Difference = max(avg_test_score_percent) - min(avg_test_score_percent),
    .groups = "drop"
  ) %>%
  arrange(Average)

view(test_score_by_school_type)

print(test_score_by_school_type)



## 3.3 State Level Funding per Student
  
# State level funding per student (min, mean, max, sd and range)

states_funding_summary <- edu_data %>%
  group_by(state) %>%
  summarise(
    Lower = min(funding_per_student_usd),
    Average = mean(funding_per_student_usd),
    Upper = max(funding_per_student_usd),
    SD_Funding = sd(funding_per_student_usd),
    Difference = max(funding_per_student_usd) - min(funding_per_student_usd),
    School_Count = n(),
    .groups = "drop"
  ) %>%
  arrange(Average) %>%
  select(state, School_Count, Lower, Average, Upper, SD_Funding, Difference)

summary(states_funding_summary)
view(states_funding_summary)


print(states_funding_summary)




# 4. High-Achieving Schools Analysis
  
# Filter observation 
  
## 4.1 Filtering High Achieving Schools
  
# create a data that demonstrate schools in high school achieve high scores

high_achieved_schools <- edu_data %>%
  group_by(state) %>%
  mutate(state_score_sd = sd(avg_test_score_percent)) %>%
  ungroup() %>%
  filter(grade_level == "High", avg_test_score_percent > 80) %>%
  select(state, school_name,
         school_type, grade_level,
         avg_test_score_percent,
         ends_with("low_income"),
         ends_with("access_percent"),
         state_score_sd) %>%
  arrange(desc(avg_test_score_percent))

view(high_achieved_schools)

# View first rows
head(high_achieved_schools, n = 10)

#  Check structure
str(high_achieved_schools)

summary(high_achieved_schools)





## 4.2 Best Performance by School Type
  
# Evaluate best performance school type among school types

best_performance_by_school_type <- high_achieved_schools %>%
  group_by(school_type) %>%
  summarise(
    school_count = n(),
    average_score = round(mean(avg_test_score_percent, na.rm = TRUE), 2),
    sd_score = round(sd(avg_test_score_percent, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(average_score))
view(best_performance_by_school_type)
print(best_performance_by_school_type)

summary(best_performance_by_school_type)



  
## 4.3 Visualizations High Achieving Schools
  
###  School Type's Test Scores 
ggplot(high_achieved_schools, aes(x = school_type, y = avg_test_score_percent, fill = school_type)) +
  geom_boxplot() +
  labs(
    title = "Test Score Distribution by School Type (High Schools with Score > 80%)",
    x = "School Type",
    y = "Test Score (%)"
  ) +
  theme_get()


  
  
  
### Visualizaton Low Income versus Test Score
  
ggplot(high_achieved_schools, aes(x = percent_low_income, y = avg_test_score_percent)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Low Income % vs Test Score (High-Performing High Schools)",
    x = "Low Income Students (%)",
    y = "Test Score (%)"
  ) +
  theme_gray()


### Count of High-Performing Schools by Type
### Showing by Bar Charts
  
ggplot(high_achieved_schools, aes(x = school_type, fill = school_type)) +
  geom_bar() +
  labs(
    title = "Count of High-Performing Schools by Type",
    x = "School Type", 
    y = "Count"
  ) +
  theme_minimal()


## 4.4 Overall Summary of High-Achieving Schools

overall_summary <- high_achieved_schools %>% 
  group_by(school_type) %>%
  summarise(
    count = n(),
    avg_score = round(mean(avg_test_score_percent, na.rm= TRUE), 2),
    avg_low_income = round(mean(percent_low_income, na.rm =TRUE), 2),
    .groups = 'drop'
  )
view(overall_summary)
print(overall_summary)


#4.5 Summary table for high-achieving schools by type

# Summary table for high-achieving schools by type
high_achievers_table <- data.frame(
  School_Type = c("Private", "Charter", "Public"),
  Count = c(35, 37, 43),
  Avg_Score = c(91.2, 90.0, 87.8),
  SD = c(6.07, 5.46, 5.73),
  Avg_Low_Income = c(57.1, 56.1, 49.4)
)

view(high_achievers_table)
print(high_achievers_table)


# 5. Simple Linear Regression
  
## 5.1 Model Building
  
model <- lm(avg_test_score_percent ~ funding_per_student_usd, data = edu_data)

# Model summary
summary(model)

  
## 5.2 Residual Diagnostics
  

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


  
## 5.3 Base R Regression  plot
plot(edu_data$funding_per_student_usd, edu_data$avg_test_score_percent,
     main = "Linear Regression: Funding vs Test Scores",
     xlab = "Funding per Student (USD)",
     ylab = "Average Test Score (%)",
     pch = 19)
abline(model, col = "blue", lwd = 2)


##5.4 Multiple Linear Regression
model_full <- lm(avg_test_score_percent ~ 
                   funding_per_student_usd +
                   percent_low_income +
                   percent_minority +
                   dropout_rate_percent +
                   student_teacher_ratio +
                   school_type +
                   grade_level,
                 data = edu_data)

summary(model_full)



## 5.5 Correlation Matrix
cor(edu_data[, sapply(edu_data, is.numeric)])

# 6. Data Manipulation with dplyr
  
## 6.1 Filter Operations

high_funding <- edu_data %>% filter(funding_per_student_usd > 10000)

## 6.2 Select Specific Columns
  
selected_data_subset <- edu_data %>% 
  select(school_name, funding_per_student_usd, avg_test_score_percent)


## 6.3 Create New Variables
edu_data <- edu_data %>%
  mutate(score_category = ifelse(avg_test_score_percent >= 70, "High", "Low"))

## 6.4 Summarize data
  
summary_table <- edu_data %>%
  summarize(
    mean_funding = mean(funding_per_student_usd),
    mean_score = mean(avg_test_score_percent)
  )
## 6.5 Sort Data by Funding

sorted_data <- edu_data %>% arrange(desc(funding_per_student_usd))

## 6.6 Group by School Type and Calculate Averages


result <- edu_data %>%
  filter(funding_per_student_usd > 8000) %>%
  select(school_type, funding_per_student_usd, avg_test_score_percent) %>%
  group_by(school_type) %>%
  summarize(avg_funding = mean(funding_per_student_usd),
            avg_score = mean(avg_test_score_percent))



view(result)
print(result)


  
  
# 7. Additional  Visualizations for Report
  
## 7.1 Scatter Plot Colored by Score Category
  
# 1. Create the column
edu_data <- edu_data %>%
  mutate(score_category = ifelse(avg_test_score_percent >= 70, "High", "Low"))

# 2. Create the plot
ggplot(edu_data, aes(x = funding_per_student_usd, y = avg_test_score_percent, color = score_category)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Funding vs Test Scores by Score Category",
       x = "Funding per Student (USD)",
       y = "Average Test Score (%)") +
  theme_minimal()




## 7.2 Facet Plot by School Type
  
# Facet plot by school type

ggplot(edu_data, aes(x = funding_per_student_usd, y = avg_test_score_percent)) +
  geom_point() +
  facet_wrap(~ school_type) +
  theme_minimal()


## 7.3 Alternative Linear Regression Model
  
# Model Building

# Build linear regression model
model <- lm(avg_test_score_percent ~ funding_per_student_usd, data = edu_data)

# Model summary
summary(model)



## 7.4 Base R Regression Visualization

par(mfrow = c(1, 1))

plot(edu_data$funding_per_student_usd, edu_data$avg_test_score_percent,
     main = "Linear Regression: Funding vs Test Scores",
     xlab = "Funding per Student (USD)",
     ylab = "Average Test Score (%)",
     pch = 19)

abline(model, col = "blue", lwd = 2)


## 7.5 Filter High Funding Schools

high_funding <- edu_data %>%
  filter(funding_per_student_usd > 15000)

## 7.6 Select and Mutate Operations Revisited
# Select specific columns

selected_data <- edu_data %>%
  select(school_name, funding_per_student_usd, avg_test_score_percent)

# Create new column: score category

new_data <- edu_data %>%
  mutate(score_category = ifelse(avg_test_score_percent >= 70, "High", "Low"))


view(new_data)


print(new_data)

## 7.7 Summary and Sorting

# Summarize data

summary_table <- edu_data %>%
  summarize(
    mean_funding = mean(funding_per_student_usd),
    mean_score = mean(avg_test_score_percent)
  )

summary_table

# Sort data by funding descending

sorted_data <- edu_data %>% 
  arrange(desc(funding_per_student_usd))
print(sorted_data)



## 7.8 Final Group Results
# Group by school type and calculate averages

final_result <- edu_data %>%
  filter(funding_per_student_usd > 8000) %>%
  select(school_type, funding_per_student_usd, avg_test_score_percent) %>%
  group_by(school_type) %>%
  summarize(
    avg_funding = mean(funding_per_student_usd),
    avg_score = mean(avg_test_score_percent),
    .groups = "drop"
  )

view(final_result)
print(final_result)


#=========================================Done Project===============================================

