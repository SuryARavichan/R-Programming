install.packages('randomForest')
install.packages('leaflet')
install.packages('caret')


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(leaflet)
library(caret)  # Added for cross-validation
library(leaflet)  # Load the leaflet package


data <- read.csv('/Volumes/Surya/Github projects/R programming/natural_disaster_dataset.csv')

data

head(data, 10)

tail(data, 10)

summary(data)


data_cleaned <- data %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

data_cleaned$Date <- as.Date(data_cleaned$Date, format='%Y-%m-%d')
data_cleaned$Disaster_Type <- as.factor(data_cleaned$Disaster_Type)

ggplot(data_cleaned, aes(x=Disaster_Type)) +
  geom_bar(fill = 'purple')+
  labs(title = 'Distribution of the Disaster Types', x = 'Disaster Type', y = 'Count')+
  theme_minimal()


## magnitude of disaster over time

ggplot(data_cleaned, aes(x = Date, y = Magnitude))+
  geom_line(color = 'red')+
  labs(title = 'Magnitude of Diasters Over the Time', x = 'Date', y = 'Magnitude')+
  theme_minimal()


##geographical location of the disasters

# Location-Based Analysis
leaflet(data_cleaned) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, color = ~Disaster_Type, 
                   popup = ~paste(Disaster_Type, "<br>", Date))
