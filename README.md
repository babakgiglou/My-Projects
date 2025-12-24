# # 🔔 Bellabeat Consumer Data Strategy & Segmentation | Capstone Project

This project is a complete, end-to-end data analysis capstone for **Bellabeat**, transforming raw user behavior data into a strategic business strategy.

---

## 🎯 Business Task & Objective

**Objective:** Analyze complex FitBit user data to identify critical usage patterns, develop strategic customer segmentation, and provide actionable marketing recommendations to optimize user engagement and conversion.

**Data Source:** Public FitBit Fitness Tracker Data (33 users, 31 days).

---

## 🛠️ Tools & Advanced Skills Demonstrated

| Tool / Technique | Technical Application (Validated by Code) |
| :--- | :--- |
| **Data Programming** | **R (tidyverse, lubridate, ggplot2):** Used for complex data cleaning, time series analysis, and visualization. |
| **Data Merging & ETL** | **SQL** principles applied via `left_join()` across **multiple granular files** (Daily, Hourly, Minute, Sleep data) to create the `df_master_daily_data`. |
| **Data Cleaning** | **Engineering-driven** approach to QA: Checked for NAs, Duplicates, Unique IDs (N=33 confirmed), and performed type conversion (`mdy_hms()` function) across all 18 files. |
| **Data Visualization** | **ggplot2/Tableau:** Created a **Dual-Axis Line Plot** to show **24-Hour Activity Cycles** (Steps vs. Calories) and a Segment Bar Plot. |
| **Business Acumen** | **MBA-level** Strategic Segmentation and translation of analytical findings into a targeted marketing plan. |

---
##example of codes used for this task 
##Load and Clean Daily Data
library(tidyverse)
library(lubridate)
df_daily <- read_csv("dailyActivity_merged.csv")
df_daily_clean <- df_daily %>%
  rename(Date = ActivityDate) %>%
  mutate(Id = as.character(Id),Date = mdy(Date))
df_daily_clean %>% summarise(across(everything(), ~sum(is.na(.))))
##Aggregate Hourly Steps to Daily
df_hourly_steps <- read_csv("hourlySteps_merged.csv")
df_hourly_steps_daily <- df_hourly_steps %>%
  mutate(Id = as.character(Id),
    ActivityDateTime = mdy_hms(ActivityHour),
    Date = as.Date(ActivityDateTime)) %>%
  group_by(Id, Date) %>%
  summarise(DailySteps = sum(StepTotal, na.rm = TRUE), .groups = "drop")
  
  ##Merge Daily Activity with Sleep
  
  df_sleep <- read_csv("minuteSleep_merged.csv") %>%
  mutate(Id = as.character(Id),
    ActivityDateTime = mdy_hms(date),
    SleepDate = as.Date(ActivityDateTime)
  ) %>%group_by(Id, logId) %>%summarise(
    TotalMinutesAsleep = sum(value %in% c(1,2)),
    MinutesAwake = sum(value == 3),
    SleepEfficiency = TotalMinutesAsleep / n(),
    .groups = "drop"
  )

df_master_daily <- df_daily_clean %>%
  left_join(df_hourly_steps_daily, by = c("Id", "Date")) %>%
  left_join(df_sleep, by = c("Id", "Date" = "SleepDate"))

##User Segmentation & Summary

df_user_summary <- df_master_daily %>%
  group_by(Id) %>%
  summarise(AvgTotalSteps = mean(TotalSteps, na.rm = TRUE),
    AvgSleepEfficiency = mean(SleepEfficiency, na.rm = TRUE),
    .groups = "drop")df_user_segments <- df_user_summary %>%
  mutate(UserSegment = case_when(
    AvgTotalSteps < 5000 ~ "Sedentary",
    AvgTotalSteps < 7500 ~ "Lightly Active",
    AvgTotalSteps < 10000 ~ "Active",
    TRUE ~ "Highly Active"))

##Example Plots

User Segmentation: distribution of participants by activity level.
24-Hour Activity Cycle: average steps and calories per hour.
Correlation: average daily steps vs. sleep efficiency.

library(ggplot2)

# 1. Prepare data with Percentage calculation (if not already done)
segment_counts_for_plot <- df_user_segments %>%
    group_by(UserSegment) %>%
    summarise(
        UserCount = n(),
        .groups = 'drop'
    ) %>%
    mutate(
        Percentage = UserCount / sum(UserCount) # Calculate the percentage
    )

# Define the correct order for the bars
segment_order <- c("Sedentary", "Lightly Active", "Active", "Highly Active")

# 2. Optimized ggplot Code
segment_plot <- ggplot(segment_counts_for_plot,
                       # Order the X-axis and map fill color to the segment
                       aes(x = factor(UserSegment, level = segment_order),
                           y = UserCount,
                           fill = UserSegment)) +
    # Use geom_col (same as geom_bar(stat="identity")) for defined y-values
    geom_col(width = 0.7) +
    # Add text labels for Count (and Percentage)
    geom_text(aes(label = paste0(UserCount, " (", round(Percentage * 100), "%)")),
              vjust = -0.5, # Position labels slightly above the bars
              size = 4) +
    # Add clear labels and title
    labs(
        title = "User Segmentation by Average Daily Steps",
        subtitle = "The Sedentary Segment Represents 42% of Users.",
        x = "Activity Level",
        y = "Number of Users"
    ) + # Use a professional theme
    theme_minimal() +
    theme(legend.position = "none") # Hide the redundant legend

## 📊 Key Findings & Strategic Insights

The analysis leveraged segmentation and correlation techniques to uncover patterns crucial for Bellabeat's strategy:

### **1. The $42 Sedentary Segment (The Core Finding)**

* **Methodology:** User segmentation was applied based on the average daily steps (using standard public health thresholds), dividing users into Sedentary (< 5k steps), Lightly Active, Active, and Highly Active.
* **Key Finding:** The **Sedentary Segment** comprised the largest group of users (approx. **$42\%$** of total participants).
* **Insight:** This confirmed that the primary marketing focus should be on *activation* and *engagement* features for this large, untapped user base.

### **2. Critical Behavioral Correlations**

* **Hourly Trends:** Identified peak activity windows (e.g., late afternoon/evening hours, confirmed by `peak_steps` analysis) which are ideal times for push notifications.
* **Activity vs. Sleep Correlation:** Calculated the **Pearson Correlation Coefficient ($r$)** between Average Daily Steps and Average Sleep Efficiency. The result (a **Weak Positive Trend**) suggests that while more activity is slightly beneficial for sleep quality, Bellabeat should market Sleep and Activity features **together**.

---

## 💡 Strategic Recommendation (The "Act" Phase)

The final strategy targets the $\approx 42\%$ sedentary segment and optimizes app timing:

1.  **Targeted Nudges:** Implement personalized push notifications during low-activity periods (based on the hourly trend analysis) to encourage movement.
2.  **Product Integration:** Emphasize product features that connect steps and sleep quality, reinforcing the calculated correlation (e.g., "Hit your step goal today? Check your sleep efficiency score!").
3.  **Community Challenges:** Develop social challenges within the app to convert sedentary users into the lightly active category.

---

## 📁 Repository Contents

* `fitbit markdown.Rmd` / `fitbit markdown.html`: The source R Markdown script and/or final report.
* `df_master_daily_data.csv`: The final, merged dataset used for the main analysis.
* `user_segmentation_summary.csv`: Table showing the final user segmentation counts.
* `hourly_activity_trends_final.csv`: Data supporting the **24-Hour Activity Cycle** analysis.

**[www.linkedin.com/in/babak-agahi-giglou-0a15a2109**
