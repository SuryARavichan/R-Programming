install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("DT")


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

social_data <- read_csv('/Volumes/Surya/Github projects/R programming/twitter-archive-enhanced.csv')

social_data

social_data <- social_df %>%
  mutate(date = as.Date(timestamp)) %>%
  mutate(retweet_count = ifelse(!is.na(retweeted_status_id), 1, 0)) %>%
  rename(likes = rating_numerator)

daily_data <- social_data %>%
  group_by(date) %>%
  summarise(
    likes = sum(likes, na.rm = TRUE),
    retweets = sum(retweet_count, na.rm = TRUE),
    tweets = n()
  )

ui <- dashboardPage(
  dashboardHeader(title = "Social Media Analytics Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Details", tabName = "details", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Likes Over Time", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("likesPlot", height = 250)
                ),
                box(
                  title = "Retweets Over Time", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("retweetsPlot", height = 250)
                )
              ),
              fluidRow(
                box(
                  title = "Number of Tweets Over Time", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("tweetsPlot", height = 250)
                ),
                box(
                  title = "Distribution of Ratings", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("ratingsPlot", height = 250)
                )
              )
      ),
      tabItem(tabName = "details",
              fluidRow(
                box(
                  title = "Detailed Metrics", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  dataTableOutput("detailsTable")
                )
              )
      )
    )
  )
)



#### server logic


server <- function(input, output){
  output$likesPlot <- renderPlot({
    ggplot(daily_data, aes(x = date, y = likes)) +
      geom_line(color = 'blue') +
      labs(title = 'Likes Over The Time', x = 'Date', y = 'Likes') +
      theme_minimal()
  })
  
  output$retweetsPlot <- renderPlot({
    ggplot(daily_data, aes(x = date, y = retweets)) +
      geom_line(color = 'purple') +
      labs(title = 'Retweets Over The Time', x = 'Date', y = 'Retweets') +
      theme_minimal()
  
  })
  
  output$tweetsPlot <- renderPlot({
    ggplot(daily_data, aes(x = date, y = tweets)) +
      geom_line(color = 'green') +
      labs(title = 'Number of Tweets Over The Time', x = 'Date', y = 'Number of Tweets') +
      theme_minimal()
  })
  
  output$ratingsPlot <- renderPlot({
    ggplot(social_data, aes(x = likes)) +
      geom_histogram(binwidth = 1, fill = 'pink', color = 'black') +
      labs(title = 'Distribution Of The Ratings', x = 'Rating', y = 'Frequency') +
      theme_minimal()
    
  })
  
  output$detailsTable <- renderDataTable({
    datatable(social_data)
  })
}



shinyApp(ui = ui, server = server)