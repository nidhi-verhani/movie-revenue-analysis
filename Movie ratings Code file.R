library(dplyr)
library(ggplot2)
library(readxl
        )

movies_clean <- read.csv("Movie_Data_300.csv")

head(movies_clean)


# Group by Release Year and calculate average rating and total revenue
movies_summary <- movies_clean %>%
  group_by(Release_Year) %>%
  summarise(
    Avg_Rating = mean(Rating),
    Total_Revenue = sum(Revenue)
  )

head(movies_summary)


#Boxplot for Ratings Distribution by Year
ggplot(movies_clean, aes(x = as.factor(Release_Year), y = Rating, fill = as.factor(Release_Year))) +
  geom_boxplot() +
  labs(title = "Movie Ratings Distribution by Release Year", x = "Release Year", y = "Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Histogram for Movie Revenue Distribution
ggplot(movies_clean, aes(x = Revenue)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Movie Revenue (in millions)", x = "Revenue (in millions)", y = "Frequency") +
  theme_minimal()


#Line plot for Average Rating over the years
ggplot(movies_summary, aes(x = Release_Year, y = Avg_Rating)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Average Rating Over the Years", x = "Year", y = "Average Rating") +
  theme_minimal()

#Line plot for Total Revenue over the years
ggplot(movies_summary, aes(x = Release_Year, y = Total_Revenue)) +
  geom_line(color = "green", linewidth = 1.2) +
  labs(title = "Total Revenue Over the Years", x = "Year", y = "Total Revenue (Millions)") +
  theme_minimal()


#Filter data for movies released in 2010 and 2015
ratings_data_clean <- movies_clean %>% filter(Release_Year %in% c(2010, 2015))

#Perform two-sample t-test for ratings between 2010 and 2015
t_test_ratings_clean <- t.test(Rating ~ Release_Year, data = ratings_data_clean)
print(t_test_ratings_clean)


#Filter data for movies released in 2010 and 2015
revenue_data_clean <- movies_clean %>% filter(Release_Year %in% c(2010, 2015))

#Perform two-sample t-test for revenue between 2010 and 2015
t_test_revenue_clean <- t.test(Revenue ~ Release_Year, data = revenue_data_clean)
print(t_test_revenue_clean)


#Calculate the correlation between Rating and Revenue
correlation_coefficient <- cor(movies_clean$Rating, movies_clean$Revenue)

#Print the correlation coefficient
print(paste("Correlation Coefficient between Rating and Revenue:", round(correlation_coefficient, 2)))



#Scatterplot of Rating vs Revenue
ggplot(movies_clean, aes(x = Rating, y = Revenue)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Correlation between Rating and Revenue",
       x = "Rating",
       y = "Revenue (in millions)") +
  theme_minimal()

#adding paired T-Test as suggested by professor during presentation

# Paired T-Test for Ratings (2010 vs. 2015)
ratings_data_clean <- movies_clean %>% filter(Release_Year %in% c(2010, 2015))
ratings_2010 <- ratings_data_clean$Rating[ratings_data_clean$Release_Year == 2010]
ratings_2015 <- ratings_data_clean$Rating[ratings_data_clean$Release_Year == 2015]

common_length_ratings <- min(length(ratings_2010), length(ratings_2015))
ratings_2010 <- ratings_2010[1:common_length_ratings]
ratings_2015 <- ratings_2015[1:common_length_ratings]

t_test_ratings_paired <- t.test(ratings_2010, ratings_2015, paired = TRUE)
print(t_test_ratings_paired)

# Paired T-Test for Revenue (2010 vs. 2015)
revenue_data_clean <- movies_clean %>% filter(Release_Year %in% c(2010, 2015))
revenue_2010 <- revenue_data_clean$Revenue[revenue_data_clean$Release_Year == 2010]
revenue_2015 <- revenue_data_clean$Revenue[revenue_data_clean$Release_Year == 2015]

common_length_revenue <- min(length(revenue_2010), length(revenue_2015))
revenue_2010 <- revenue_2010[1:common_length_revenue]
revenue_2015 <- revenue_2015[1:common_length_revenue]

t_test_revenue_paired <- t.test(revenue_2010, revenue_2015, paired = TRUE)
print(t_test_revenue_paired)

set.seed(123)  
movies_clean$Month <- sample(1:12, nrow(movies_clean), replace = TRUE)

movies_clean <- movies_clean %>%
  mutate(Half = ifelse(Month <= 6, "First", "Second"))

# Paired T-Test for Revenue by Half-Year
revenue_first <- movies_clean$Revenue[movies_clean$Half == "First"]
revenue_second <- movies_clean$Revenue[movies_clean$Half == "Second"]

common_length_half <- min(length(revenue_first), length(revenue_second))
revenue_first <- revenue_first[1:common_length_half]
revenue_second <- revenue_second[1:common_length_half]

t_test_half <- t.test(revenue_first, revenue_second, paired = TRUE)
print(t_test_half)

