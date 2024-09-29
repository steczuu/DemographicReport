library(dplyr)     
library(ggplot2)   
library(readr)
library(corrplot)



data <- read.csv("dane_demograficzne.csv")

head.matrix(data)

summary(data)

View(data)

print(colnames(data))

#calculation of average earnings by profession
average_income <- data %>%
  group_by(Zawód) %>%
  summarize(Average_income = mean(`Zarobki_PLN`, na.rm = TRUE)) %>%  
  arrange(desc(Average_income))

print(average_income)

ggplot(average_income, aes(x = Zawód, y = Average_income)) +  
  geom_col(fill = "skyblue", color = "black") +  
  labs(title = "Average Income by Profession", x = "Profession", y = "Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Linear regression analysis: earnings prediction
model_income <- lm(`Zarobki_PLN` ~ Wiek + Liczba_dzieci + Zawód, data = data)

summary(model_income)

ggplot(data, aes(x = Wiek, y = `Zarobki_PLN`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear regression: Age vs Earnings", x = "Age", y = "Earnings")


#calculation of average earnings by age
data <- data %>% 
  mutate(Age_group = cut(Wiek, breaks = c(18, 30, 40, 50, 60, 100), 
                         labels = c("18-29", "30-39", "40-49", "50-59", "60+")))
average_income_age_group <- data %>%
  group_by(Age_group) %>%
  summarize(Average_income = mean(`Zarobki_PLN`, na.rm = TRUE)) %>%
  arrange(desc(Average_income))

ggplot(average_income_age_group, aes(x = Age_group, y = Average_income)) +
  geom_col(fill = "coral", color = "black") +
  labs(title = "Average Income by Age Group", x = "Age Group", y = "Income")