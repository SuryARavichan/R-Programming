install.packages('ggplot2')
library(tidyverse)


list.files(path = "../input")

data<-read.csv('/Volumes/Surya/Github projects/R programming/all_stocks_5yr.csv')

data

head(data, 10)

dim(data)

colSums(is.na(data))

variable.names<-names(data)

variable.names


str(data)


data1<-na.omit(data)

colSums(is.na(data))

dim(data1)

summary(data1)

ggplot(data1, aes(x=open))+
  geom_histogram(bandwidth=1, fill='blue', color='white')+
  labs(title='histogram of opening prices', x='Opening Price', y='Count')

ggplot(data1, aes(x=open))+
  geom_histogram(bandwidth=1, fill='red', color='white')+
  labs(title='histogram of closing prices', x=' Closing Price', y='Count')


ggplot(data1, aes(x=low, y-high))+
  geom_point()+
  labs(title = 'Comparison of High Vs Low Prices', x = 'Low Price', y = 'High Price')

companies<-length(unique(data1$Name))

companies

min_price<-min(data1$close)
min_price

max_price<-max(data1$close)
max_price

maximum_value<-data1$Name[which.max(data1$volume)]
maximum_value

minimum_value<-data1$Name[which.min(data1$volume)]
minimum_value

mean_price<-mean(data1$close)
mean_price

total_volume<-sum(data1$volume)
total_volume


price_increase <- data1$Name[rle(data1$close > lag(data1$close))$values &
                               rle(data1$close > lag(data1$close))$lengths > 1][1]
price_increase


price_decrease <- data1$Name[rle(data1$close < lag(data1$close))$values &
                               rle(data1$close < lag(data1$close))$lengths > 1][1]
price_decrease



date1$date<-as.character(data1$date)

splitted<-strsplit(data1$date,'/')
df<-data.frame(
  date = data1$date,
  day = as.integer(sapply(splitted, '[',1)),
  month = as.integer(sapply(splitted, '[',2)),
  year = as.integer(sapply(splitted, '[',3)),
  stringsAsFactors = FALSE
)

head(df)


see_the_change <- data.frame(
  date = data1$date,
  day = day(data1$date),
  month = month(data1$date),
  year = year(data1$date),
  stringsAsFactors = FALSE
)

head(see_the_change)

data1$day <- day(data1$date)
data1$month <- month(data1$date)
data1$year <- year(data1$date)

head(data1)

data1$is_quarter_end <- ifelse(data1$month %% 3 == 0, 1, 0)
head(data1, 5)


nume_column <- c('open','high',
                 'low', 'close')
data_num <- data1[, c('date', nume_column)]
data_num <- data_num[apply(data_num[, nume_column],
                           1, function(x) all(is.numeric(x))), ]

data_num$year <- lubridate::year(data_num$date)

data_grouped <- data_num %>%
  group_by(year) %>%
  summarise(across(all_of(nume_column), mean))

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (i in 1:4) {
  col <- nume_column[i]
  barplot(data_grouped[[col]], main = col,
          xlab = "Year", ylab = "Mean")
}


# Select a few distinct companies for comparison
sample_companies <- unique(data1$Name)[1:5] # Adjust number as needed

ggplot(filter(data1, Name %in% sample_companies), aes(x = open, fill = Name)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.7, position = "identity") +
  facet_wrap(~ Name, scales = "free_y") +
  labs(title = "Distribution of Opening Prices by Company (Sample)",
       x = "Opening Price",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend as facets label companies


### Density Plots

ggplot(data1, aes(x = close)) +
  geom_density(fill = "lightgreen", color = "darkgreen", alpha = 0.6) +
  labs(title = "Density Plot of Closing Prices",
       x = "Closing Price",
       y = "Density") +
  theme_minimal()

# Compare Open vs. Close Price Distributions
ggplot(data1 %>%
         pivot_longer(cols = c(open, close), names_to = "price_type", values_to = "price_value"),
       aes(x = price_value, fill = price_type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot: Open vs. Close Price Distribution",
       x = "Price",
       y = "Density") +
  scale_fill_manual(values = c("open" = "lightblue", "close" = "salmon")) +
  theme_minimal()

#relationship plots

ggplot(data1, aes(x = open, y = close)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Add a linear regression line
  labs(title = "Open vs. Close Prices",
       x = "Opening Price",
       y = "Closing Price") +
  theme_minimal()

#box plots of prices by quater end

ggplot(data1, aes(x = factor(is_quarter_end, labels = c("Not Quarter End", "Quarter End")), y = close)) +
  geom_boxplot(fill = c("lightblue", "lightcoral"), alpha = 0.7) +
  labs(title = "Closing Price Distribution: Quarter End vs. Other Days",
       x = "Day Type",
       y = "Closing Price") +
  theme_minimal()

data_with_change <- data1 %>%
  group_by(Name) %>%
  arrange(date) %>%
  mutate(price_change = close - lag(close)) %>%
  ungroup()

#Average daily range of prices

data_range_by_year <- data1 %>%
  mutate(daily_range = high - low) %>%
  group_by(year) %>%
  summarise(mean_daily_range = mean(daily_range, na.rm = TRUE))

ggplot(data_range_by_year, aes(x = factor(year), y = mean_daily_range, group = 1)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred") +
  labs(title = "Average Daily Price Range Over Years",
       x = "Year",
       y = "Average (High - Low) Price") +
  theme_minimal()
