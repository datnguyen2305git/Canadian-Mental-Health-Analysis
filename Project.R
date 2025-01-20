# Install and load required packages
install.packages(c("dplyr", "ggplot2", "corrplot", "knitr", "rmarkdown"))
install.packages("e1071")
install.packages("car")
library(e1071)
library(dplyr)
library(ggplot2)
install.packages("ggplot2")
library(ggplot2)

# Load data
perceived_mental_health <- read.csv("C:/Users/owner/OneDrive/Documents/Humber/R practice/Project/Perceived mental health.csv")

# Filter rows where "Indicators" column contains "Excellent or very good perceived mental health"
mental_df <- perceived_mental_health[perceived_mental_health$Indicators == "Excellent or very good perceived mental health", ]

# Remove rows with "Total" in any relevant column
mental_df <- mental_df %>%
  filter(!grepl("Total", Gender, ignore.case = TRUE) &
           !grepl("Total", Sociodemographic.characteristics, ignore.case = TRUE))

# Select specific columns to analyze
mental_df <- mental_df[, c("REF_DATE", "Gender", "Sociodemographic.characteristics", "VALUE")]

# EDA on filtered mental_df
# 1. Explore structure
str(mental_df)

# 2. Summary statistics
summary(mental_df)

unique(mental_df$Sociodemographic.characteristics)

# 3. Check for missing values
missing_data_mental_df <- data.frame(
  Column_Name = names(mental_df),
  NA_value_count = colSums(is.na(mental_df)),
  number_of_rows = nrow(mental_df),
  percentage_of_missing = round((colSums(is.na(mental_df)) / nrow(mental_df)) * 100, 2)
)
print(missing_data_mental_df)

# 4. Check for outliers using IQR
identify_outliers <- function(series) {
  series <- series[!is.na(series)]
  Q1 <- quantile(series, 0.25, na.rm = TRUE)
  Q3 <- quantile(series, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- series[(series < lower_bound) | (series > upper_bound)]
  return(outliers)
}
percentage_of_outlier_mental_df <- round((length(identify_outliers(mental_df$VALUE)) / length(mental_df$VALUE)) * 100, 2)
print(percentage_of_outlier_mental_df)

# 5. Check skewness
skewness_value_mental <- skewness(mental_df$VALUE, na.rm = TRUE)
print(skewness_value_mental)

# Clean dataset
# 1. Handle REF_DATE column
mental_df$month <- substr(mental_df$REF_DATE, 6, 7)
mental_df$quarter_label <- ifelse(mental_df$month == "01", "Q1",
                                  ifelse(mental_df$month == "04", "Q2",
                                         ifelse(mental_df$month == "07", "Q3",
                                                ifelse(mental_df$month == "10", "Q4", NA))))

# 2. Remove duplicate rows
mental_df <- mental_df[!duplicated(mental_df), ]

# 3. Handle missing values in VALUE column with the median value
mental_df$VALUE <- ifelse(is.na(mental_df$VALUE), mean(mental_df$VALUE, na.rm = TRUE), mental_df$VALUE)

# 4. Convert character columns to factors
mental_df <- mental_df %>% mutate_if(is.character, as.factor)
write.csv(mental_df, "mental_df.csv", row.names = FALSE)
# Analyze dataset
mental_df$year <- substr(mental_df$REF_DATE, 1, 4)

#1. Overview by year
#Calculate mean, growth, variance for VALUE
summarise_data <- mental_df %>%
  group_by(year) %>%
  summarise(mean_VALUE = mean(VALUE, na.rm = TRUE),
            variance_VALUE = var(VALUE, na.rm = TRUE) # Calculate variance
  ) %>%
  mutate(growth = (mean_VALUE - lag(mean_VALUE)) / lag(mean_VALUE) * 100, # Calculate growth
         growth = ifelse(is.na(growth), 0, growth)) %>% # Replace NA (baseline year) with 0%
  arrange(as.numeric(year))
print(summarise_data)

# Mean VALUE by year chart
ggplot(summarise_data, aes(x = as.numeric(year), y = mean_VALUE)) +
  geom_bar(stat = "identity") + # Use stat = "identity" for bar height based on mean_VALUE
  labs(title = "Mean VALUE by Year",
       x = "Year",
       y = "Mean VALUE") +
  theme_minimal()

# Format and print the data with growth
formatted_mean_data <- summarise_data %>%
  mutate(growth = round(growth, 2)) # Round growth to 2 decimal places

# Display the table
print(formatted_mean_data)

# Plot the scatter plot with mean value and growth
ggplot(mean_data, aes(x = mean_VALUE, y = growth, color = as.factor(year))) +
  geom_point(size = 8) +
  labs(title = "Scatter Plot of Growth vs Mean VALUE",
       x = "Mean VALUE",
       y = "Growth (%)",
       color = "Year") + # Add legend title for year
  theme_minimal()

# Plot the scatter plot with mean value and variance
ggplot(summarise_data, aes(x = mean_VALUE, y = variance_VALUE, color = as.factor(year))) +
  geom_point(size = 8) +
  labs(
    title = "Scatter Plot of Variance vs Mean VALUE by Year",
    x = "Mean VALUE",
    y = "Variance",
    color = "Year"
  ) +
  theme_minimal()

#2. Examine 2023
# Group by Sociodemographic.characteristics and year, then calculate mean and variance
summarise_data <- mental_df %>%
  group_by(year, Sociodemographic.characteristics) %>%
  summarise(
    mean_VALUE = mean(VALUE, na.rm = TRUE),
    variance_VALUE = var(VALUE, na.rm = TRUE), # Calculate variance
    .groups = "drop"
  ) %>%
  arrange(Sociodemographic.characteristics, as.numeric(year)) # Ensure sorting

# Filter the summarise_data table for 2023
scatter_data_2023 <- summarise_data %>%
  filter(year == "2023")

# Plot the scatter plot
ggplot(scatter_data_2023, aes(x = mean_VALUE, y = variance_VALUE, label = Sociodemographic.characteristics)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Add labels for groups
  labs(
    title = "Scatter Plot of Mean Value and Variance (2023)",
    x = "Mean Value",
    y = "Variance"
  ) +
  theme_minimal()


# Calculate growth rate for each Sociodemographic.characteristics group
growth_data <- summarise_data %>%
  group_by(Sociodemographic.characteristics) %>%
  mutate(
    growth = (mean_VALUE - lag(mean_VALUE)) / lag(mean_VALUE) * 100, # Calculate growth
    growth = ifelse(is.na(growth), 0, growth) # Replace NA (baseline year) with 0%
  ) %>%
  filter(year == "2023") # Focus only on 2023 for the plot

# Plot scatter plot: Mean VALUE in 2023 vs Growth Rate from 2022 to 2023
ggplot(growth_data, aes(x = mean_VALUE, y = growth, color = Sociodemographic.characteristics, label = Sociodemographic.characteristics)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Add text labels
  labs(
    title = "Scatter Plot of Mean VALUE in 2023 and Growth Rate (2022-2023)",
    x = "Mean VALUE in 2023",
    y = "Growth Rate (%)",
    color = "Sociodemographic Group"
  ) +
  theme_minimal()

# Filter data for 2022 and 2023 separately
data_2022 <- mental_df %>%
  filter(year == "2022") %>%
  group_by(Sociodemographic.characteristics) %>%
  summarise(mean_value_2022 = mean(VALUE, na.rm = TRUE), .groups = "drop")

data_2023 <- mental_df %>%
  filter(year == "2023") %>%
  group_by(Sociodemographic.characteristics) %>%
  summarise(mean_value_2023 = mean(VALUE, na.rm = TRUE), .groups = "drop")

# Join the data for 2022 and 2023
group_means <- left_join(data_2022, data_2023, by = "Sociodemographic.characteristics")

# Print the table
print(group_means)

library(dplyr)

# Calculate the impact value for each group
group_impacts <- group_means %>%
  mutate(
    absolute_change = mean_value_2023 - mean_value_2022,       # Absolute change for each group
    total_change = sum(abs(absolute_change), na.rm = TRUE),   # Total absolute change across all groups
    impact_value = (absolute_change / total_change) * 100     # Impact value as a percentage
  ) %>%
  arrange(desc(abs(impact_value)))  # Sort by the absolute impact value

# Print the impact table
print(group_impacts)

# Plot the bar chart for impact values
ggplot(group_impacts, aes(x = reorder(Sociodemographic.characteristics, impact_value), y = impact_value)) +
  geom_bar(stat = "identity", fill = ifelse(group_impacts$impact_value < 0, "red", "green")) +
  coord_flip() + # Flip the coordinates for better readability
  labs(
    title = "Impact Value by Sociodemographic Group",
    x = "Sociodemographic Group",
    y = "Impact Value (%)"
  ) +
  theme_minimal()

# Adjust priority score to prioritize lower mean values
priorities <- group_impacts %>%
  filter(absolute_change < 0) %>%
  mutate(
    rescaled_impact = (impact_value - min(impact_value)) / (max(impact_value) - min(impact_value)), # Rescale impact
    reversed_rescaled_mean = 1 - (mean_value_2023 - min(mean_value_2023)) / (max(mean_value_2023) - min(mean_value_2023)), # Reverse rescale for mean value
    fixed_priority_score = rescaled_impact * reversed_rescaled_mean # Calculate new priority score
  ) %>%
  arrange(desc(fixed_priority_score)) # Sort by fixed priority score

# Print the updated table
print(fixed_priorities)

# Plot bar chart for fixed priority score
ggplot(fixed_priorities, aes(x = reorder(Sociodemographic.characteristics, fixed_priority_score), y = fixed_priority_score)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(
    title = "Priority Score by Sociodemographic Group",
    x = "Sociodemographic Group",
    y = "Fixed Priority Score"
  ) +
  theme_minimal()

write.csv(fixed_priorities, "fixed_priorities.csv", row.names = FALSE)












# 1. Build linear regression model
model <- lm(VALUE ~ Gender + Sociodemographic.characteristics + quarter_label, data = mental_df)
summary(model)

# 2. Extract significant factors
summary_df <- data.frame(
  Factor = names(coef(model)),
  Coefficient = coef(model),
  p.value = coef(summary(model))[, "Pr(>|t|)"]
) %>%
  filter(p.value < 0.05)

print(summary_df)

write.csv(summary_df, "original_summary_df1.csv", row.names = FALSE)

significant_factors_table <- read.csv("C:/Users/owner/OneDrive/Documents/Humber/R practice/original_summary_df1.csv")
print(significant_factors_table)

# 3. Filter dataset with combinations of significant levels only (cross variable analysis)
#a. Define significant levels
significant_sociodemographics <- c("55 to 64 years",
                                   "65 years and over",
                                   "Immigrants",
                                   "Indigenous identity",
                                   "LGBTQ2+ people",
                                   "No certificate, diploma or degree",
                                   "Non-immigrants",
                                   "Non-Indigenous identity",
                                   "Non-LGBTQ2+ people",
                                   "Non-permanent residents",
                                   "Not a visible minority",
                                   "Persons with a disability, difficulty or long-term condition",
                                   "Persons without a disability, difficulty or long-term condition",
                                   "Postsecondary certificate, diploma or degree",
                                   "Retired",
                                   "Rural areas",
                                   "Secondary (high) school diploma or equivalency certificate",
                                   "Urban areas",
                                   "Visible minority population",
                                   "Working at a paid job or business")

# Filter the dataset for combinations of significant levels (AND condition)
filtered_mental_df <- mental_df %>%
  filter(Sociodemographic.characteristics %in% significant_sociodemographics)
write.csv(filtered_mental_df, "filtered_mental_df.csv", row.names = FALSE)
#b. Find top 5 levels having lowest mean value 
# agrregate
filter <- filtered_mental_df %>%
  group_by(Sociodemographic.characteristics) %>%
  summarize(mean_VALUE = mean(VALUE)) %>%
  arrange(mean_VALUE) 
write.csv(filter, "aggregate.csv", row.names = FALSE)

# Select first 5 levels from Sociodemographic.characteristics
top_5_levels <- filter %>%
  slice_head(n = 5) %>%  # Select the first 5 rows
  pull(Sociodemographic.characteristics)  # Extract as a vector (series)

# Filter the cleaned dataset using the top_5_levels vector
filtered_dataset <- filtered_mental_df %>%
  filter(Sociodemographic.characteristics %in% top_5_levels)

#c. Calucalate residual for filtered dataset
# Calculate predicted value
filtered_dataset$Predicted_VALUE <- predict(model, newdata = filtered_dataset)

# Calculate residual
filtered_dataset$Residuals <- filtered_dataset$VALUE - filtered_dataset$Predicted_VALUE

print(filtered_dataset)

# Calculate average absolute residuals and average VALUE by Gender and Sociodemographic.characteristics
scatter_data <- filtered_dataset %>%
  group_by(Gender, Sociodemographic.characteristics) %>%
  summarize(
    avg_abs_residual = mean(abs(Residuals), na.rm = TRUE),  # Average absolute residual
    avg_VALUE = mean(VALUE, na.rm = TRUE)                  # Average VALUE
  )

# Create the scatterplot
ggplot(scatter_data, aes(x = avg_abs_residual, y = avg_VALUE, color = Gender)) +
  geom_point(size = 3) +
  geom_text(aes(label = Sociodemographic.characteristics), hjust = 0.5, vjust = -0.5, size = 3) +
  labs(
    title = "Scatterplot of Average Absolute Residuals vs. Average VALUE",
    x = "avg_abs_residual ",
    y = "avg_VALUE (Mental Health)",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
