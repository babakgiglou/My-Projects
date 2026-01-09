##  Bellabeat Consumer Data Strategy & Segmentation | Capstone Project

This project is a complete, end-to-end data analysis capstone for **Bellabeat**, transforming raw user behavior data into a strategic business strategy.

## Business Task & Objective

**Objective:** Analyze complex FitBit user data to identify critical usage patterns, develop strategic customer segmentation, and provide actionable marketing recommendations to optimize user engagement and conversion.

**Data Source:** Public FitBit Fitness Tracker Data (33 users, 31 days).

### Load and Clean Daily Data

```r
library(tidyverse)
library(lubridate)

df_daily <- read_csv("dailyActivity_merged.csv")

df_daily_clean <- df_daily %>%
  rename(Date = ActivityDate) %>%
  mutate(
    Id = as.character(Id),
    Date = mdy(Date)
  )

df_daily_clean %>% summarise(across(everything(), ~sum(is.na(.))))

df_hourly_steps <- read_csv("hourlySteps_merged.csv")

df_hourly_steps_daily <- df_hourly_steps %>%
  mutate(
    Id = as.character(Id),
    ActivityDateTime = mdy_hms(ActivityHour),
    Date = as.Date(ActivityDateTime)
  ) %>%
  group_by(Id, Date) %>%
  summarise(
    DailySteps = sum(StepTotal, na.rm = TRUE),
    .groups = "drop"
  )
df_sleep <- read_csv("minuteSleep_merged.csv") %>%
  mutate(
    Id = as.character(Id),
    ActivityDateTime = mdy_hms(date),
    SleepDate = as.Date(ActivityDateTime)
  ) %>%
  group_by(Id, logId) %>%
  summarise(
    TotalMinutesAsleep = sum(value %in% c(1,2)),
    MinutesAwake = sum(value == 3),
    SleepEfficiency = TotalMinutesAsleep / n(),
    .groups = "drop"
  )

df_master_daily <- df_daily_clean %>%
  left_join(df_hourly_steps_daily, by = c("Id", "Date")) %>%
  left_join(df_sleep, by = c("Id", "Date" = "SleepDate"))

df_user_summary <- df_master_daily %>%
  group_by(Id) %>%
  summarise(
    AvgTotalSteps = mean(TotalSteps, na.rm = TRUE),
    AvgSleepEfficiency = mean(SleepEfficiency, na.rm = TRUE),
    .groups = "drop"
  )

df_user_segments <- df_user_summary %>%
  mutate(
    UserSegment = case_when(
      AvgTotalSteps < 5000 ~ "Sedentary",
      AvgTotalSteps < 7500 ~ "Lightly Active",
      AvgTotalSteps < 10000 ~ "Active",
      TRUE ~ "Highly Active"
    )
  )

library(ggplot2)

segment_counts_for_plot <- df_user_segments %>%
  group_by(UserSegment) %>%
  summarise(
    UserCount = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = UserCount / sum(UserCount)
  )

segment_order <- c("Sedentary", "Lightly Active", "Active", "Highly Active")

segment_plot <- ggplot(
  segment_counts_for_plot,
  aes(
    x = factor(UserSegment, level = segment_order),
    y = UserCount,
    fill = UserSegment
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(UserCount, " (", round(Percentage * 100), "%)")),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "User Segmentation by Average Daily Steps",
    subtitle = "The Sedentary Segment Represents 42% of Users.",
    x = "Activity Level",
    y = "Number of Users"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

 Key Findings & Strategic Insights

The analysis leveraged segmentation and correlation techniques to uncover patterns crucial for Bellabeat's strategy:

1. The $42 Sedentary Segment (The Core Finding)

Methodology: User segmentation was applied based on the average daily steps (using standard public health thresholds), dividing users into Sedentary (< 5k steps), Lightly Active, Active, and Highly Active.

Key Finding: The Sedentary Segment comprised the largest group of users (approx. 42% of total participants).

Insight: This confirmed that the primary marketing focus should be on activation and engagement features for this large, untapped user base.

2. Critical Behavioral Correlations

Hourly Trends: Identified peak activity windows (e.g., late afternoon/evening hours, confirmed by peak_steps analysis) which are ideal times for push notifications.

Activity vs. Sleep Correlation: Calculated the Pearson Correlation Coefficient (r) between Average Daily Steps and Average Sleep Efficiency. The result (a Weak Positive Trend) suggests that while more activity is slightly beneficial for sleep quality, Bellabeat should market Sleep and Activity features together.

 Strategic Recommendation (The "Act" Phase)

The final strategy targets the ≈42% sedentary segment and optimizes app timing:

Targeted Nudges: Implement personalized push notifications during low-activity periods (based on the hourly trend analysis) to encourage movement.

Product Integration: Emphasize product features that connect steps and sleep quality, reinforcing the calculated correlation.

Community Challenges: Develop social challenges within the app to convert sedentary users into the lightly active category.

 Repository Contents

fitbit markdown.Rmd / fitbit markdown.html

df_master_daily_data.csv

user_segmentation_summary.csv

hourly_activity_trends_final.csv


www.linkedin.com/in/babak-giglou


    
