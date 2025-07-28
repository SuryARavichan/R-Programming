library(forecast)
library(ggplot2)
library(tidyverse)

hotel_data <- read.csv('/Volumes/Surya/Github projects/R programming/hotel_bookings.csv')

hotel_data

head(hotel_data)

summary(hotel_data)

str(hotel_data)

total_na_count <- sum(is.na(hotel_data))

na_counts_per_column <- colSums(is.na(hotel_data))

cat("Total NA values in the entire data frame:", total_na_count, "\n")
cat("NA values per column:\n")
print(na_counts_per_column)


hotel_data <- na.omit(hotel_data)

sum(is.na(hotel_data))

ggplot(hotel_data, aes(x = reservation_status, fill = hotel)) +
  geom_bar(position = 'dodge')+
  labs(title = 'reservation status by hotel type', x = 'Reservation Status', y = 'Count')



ggplot(hotel_data, aes(x = lead_time, y = adr, color = hotel)) +
  geom_point(alpha = 0.5) +
  labs(title = 'Lead Time Bookings Vs Average Daily Rate',
       x = 'Lead Time',
       y = 'Average Daily Rate',
       color = 'Hotel Type')+
  theme_minimal()


sample_size <- 1000
hotel_data_sample <- hotel_data %>% sample_n(sample_size)

theme_set(theme_dark())

ggplot(hotel_data_sample, aes(x = lead_time)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = 'purple', color = 'blue',
                 alpha = 0.7)+
  geom_density(color = 'green') +
  
  labs(title = "Hotel Booking Lead Time Distribution with Distribution Plot",
       x = "Lead Time (days)",
       y = "Density") +
  
  theme(panel.grid.major = element_line(color = "red", linetype = "dashed",size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none") +
  
  scale_x_continuous(breaks = seq(0, max(hotel_data_sample$lead_time), by = 20))


ggplot(hotel_data, aes(x=1, y=lead_time)) +
  geom_boxplot(fill = 'red', color='black')+
  labs(tile = 'Boxplot for Outlier Detection',
       x = '',
       y = 'Lead Time')+
  theme_minimal()


#monthly booking trends

hotel_data %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
  ggplot(aes(x = arrival_date_month, fill = hotel))+
  geom_bar(poistion = 'dodge', stat = 'count')+
  labs(title = 'Monthly Trends of the Booking',
       x = 'Month',
       y = 'Number of Bookings',
       fil = 'Hotel Type')+
  theme_minimal()


#average daily distribution rates

ggplot(hotel_data, aes(x = adr, fill = hotel))+
  geom_density(alpha = 0.9)+
  labs(title = 'ADR Distribution by the Hotel Type',
       x = 'ADR',
       y = 'Density',
       fill = 'Hotel Type')+
  theme_minimal()


#booking market distribution

hotel_data %>%
  ggplot(aes(x = market_segment, fill = hotel))+
  geom_bar(position = 'dodge', stat = 'count')+
  labs(title = 'Distribution of the Bookings by Market',
       x = 'Market Segment',
       y = 'Number of Bookings',
       fill = 'Hotel Type')+
  theme_minimal()

install.packages('ggthemes')
library(ggplot2)
library(ggthemes)

hotel_data %>%
  ggplot(aes(x = hotel, fill = hotel)) +
  geom_bar(stat = "count", color = "white", size = 0.7) + 
  labs(title = "Distribution of Bookings by Hotel Type",
       x = "Hotel Type",
       y = "Number of Bookings",
       fill = "Hotel Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
        legend.title = element_text(face = "italic"),  # Make legend title italic
        legend.position = "top",  # Position legend at the top
        panel.grid.major = element_line(colour = "lightgray", linetype = "dashed"))


#Cancellation patterns of hotel

# Visualization: Cancellation Patterns
hotel_data %>%
  ggplot(aes(x = hotel, fill = as.factor(is_canceled))) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Cancellation Patterns by Hotel Type",
       x = "Hotel Type",
       y = "Number of Bookings",
       fill = "Cancellation Status") +
  theme_minimal()


#booking distribution by month

# Visualization: Booking Distribution by Month
hotel_data %>%
  ggplot(aes(x = arrival_date_month, fill = hotel)) +
  geom_bar(stat = "count", position = "stack") +
  labs(title = "Booking Distribution by Month",
       x = "Month",
       y = "Number of Bookings",
       fill = "Hotel Type") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) formatC(as.numeric(x), width = 2, flag = "0"))



#customer types

ggplot(hotel_data, aes(x = customer_type, fill = customer_type))+
  geom_bar()+
  labs(title = 'Distribution of Bookings by Customer Type',
       x = 'Customer Type',
       y = 'Number of Bookings')+
  theme_minimal()

#bookings by channel

# Create a barplot for booking distribution by channels
ggplot(hotel_data, aes(x = distribution_channel, fill = distribution_channel)) +
  geom_bar() +
  labs(title = "Distribution of Bookings by Booking Channels",
       x = "Booking Channels",
       y = "Number of Bookings") +
  theme_minimal()


#heatmap correlation
install.packages('reshape2')
library(reshape2)

numeric_columns <- select_if(hotel_data, is.numeric)

correlation_matrix <- cor(numeric_columns)

ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = 'white')+
  scale_fill_gradient2(low ='purple',mid='grey', high='blue',midpoint = 0)+
  theme_minimal()+
  labs(title = 'Correlational matrix of Numerical Columns',
       x = 'Variables',
       y = 'Variables',
       fill = 'Correlation')

##weekday Vs weekend bookings

# Create a barplot for weekday vs. weekend bookings
hotel_data %>%
  mutate(booking_day_type = ifelse(stays_in_weekend_nights >0,"Weekend","Weekday"))%>%
  ggplot(aes(x = booking_day_type, fill = booking_day_type)) +
  geom_bar() +
  labs(title = "Distribution of Bookings between Weekdays and Weekends",
       x = "Day Type",
       y = "Number of Bookings") +
  theme_minimal()