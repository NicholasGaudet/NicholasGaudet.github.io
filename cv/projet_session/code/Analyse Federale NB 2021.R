install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("broom")
library(broom)
GARPH <- read_excel("C:/Users/nicho/OneDrive/Documents/Test.xlsx")

ggplot(GARPH, aes(x = Percent_French, y = Vote_Percentage, color = Party)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Nuage de points avec régression linéaire par parti",
       x = "Pourcentage de Francophones",
       y = "Pourcentage de Vote") +
  theme_minimal() +
  scale_color_manual(values = c("Lib" = "red", "Con" = "blue", "NPD" = "Orange"))

title = "Nuage de points avec régression linéaire par parti"
x = "Pourcentage de votes reçus"
y = "Pourcentage de Français"


correlation <- cor(GARPH$Percent_French, GARPH$Vote_Percentage)
print(paste("Correlation:", correlation))

# Fit the linear model
model <- lm(Vote_Percentage ~ Percent_French + Party, data = GARPH)

# Display the model summary
summary(model)


correlation_by_party <- GARPH %>%
  group_by(Party) %>%
  summarise(correlation = cor(Percent_French, Vote_Percentage))

# Print the results
print(correlation_by_party)