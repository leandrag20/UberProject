library(readxl)
library(tidyverse)
library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(shiny)
library(leaflet.extras)
library(caret)
library(magrittr)
library(corrplot)
library(terra)

rm(list=ls())

setwd("~/git_data/332/UberProject")

#increases the number of expressions that can be evaluated in a 
#single line to 10000 and sets the timeout limit to 300 seconds
options(expressions = 10000, timeout = 300)

aprilData <- read.csv('uber-raw-data-apr14.csv')
mayData <- read.csv('uber-raw-data-may14.csv')
juneData <- read.csv('uber-raw-data-jun14.csv')
julyData <- read.csv('uber-raw-data-jul14.csv')
augustData <- read.csv('uber-raw-data-aug14.csv')
septemberData <- read.csv('uber-raw-data-sep14.csv')

aprilData$Month <- "April"
mayData$Month <- "May"
juneData$Month <- "June"
julyData$Month <- "July"
augustData$Month <- "August"
septemberData$Month <- "September"

aprilData$monthNum <- 4
mayData$monthNum <- 5
juneData$monthNum <- 6
julyData$monthNum <- 7
augustData$monthNum <- 8
septemberData$monthNum <- 9

df_combined <- rbind(aprilData, mayData, juneData, julyData, augustData, septemberData)

df_combined$Time <- str_sub(df_combined$Date.Time, -8, -1)
df_combined$Date <- as.Date(df_combined$Date.Time, format = "%m/%d/%Y")
df_combined$Date <- as.numeric(str_sub(df_combined$Date, -2, -1))
df_combined$Hour <- as.numeric(str_sub(df_combined$Time, -8, -7))

# Create a new column for total day, dayName and weekNum
df_combined <- df_combined %>% 
  mutate(totalDay = NA, dayName = NA, weekNum = NA)

#creates a column of the total day the ride took place on
df_combined$totalDay <- ifelse(df_combined$Month == "May", df_combined$Date + 30, df_combined$Date)
df_combined$totalDay <- ifelse(df_combined$Month == "June", df_combined$Date + 61, df_combined$Date)
df_combined$totalDay <- ifelse(df_combined$Month == "July", df_combined$Date + 91, df_combined$Date)
df_combined$totalDay <- ifelse(df_combined$Month == "August", df_combined$Date + 122, df_combined$Date)
df_combined$totalDay <- ifelse(df_combined$Month == "September", df_combined$Date + 153, df_combined$Date)

#creates a column with the day name of the week
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 1, "Tuesday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 2, "Wednesday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 3, "Thursday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 4, "Friday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 5, "Saturday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 6, "Sunday", df_combined$dayName)
df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 0, "Monday", df_combined$dayName)

#creates a column with the number of the week the ride took place during
df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 31, 5, df_combined$weekNum)
df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 26, 4, df_combined$weekNum)
df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 19, 3, df_combined$weekNum)
df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 12, 2, df_combined$weekNum)
df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 5, 1, df_combined$weekNum)

df_combined$weekNum <- ifelse(df_combined$Month == "May" & df_combined$Date <= 31, 5, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "May" & df_combined$Date <= 24, 4, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "May" & df_combined$Date <= 17, 3, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "May" & df_combined$Date <= 10, 2, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "May" & df_combined$Date <= 3, 1, df_combined$weekNum)

df_combined$weekNum <- ifelse(df_combined$Month == "June" & df_combined$Date <= 30, 5, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "June" & df_combined$Date <= 28, 4, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "June" & df_combined$Date <= 21, 3, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "June" & df_combined$Date <= 14, 2, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "June" & df_combined$Date <= 7, 1, df_combined$weekNum)

df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 31, 6, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 30, 5, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 23, 4, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 16, 3, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 9, 2, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "August" & df_combined$Date <= 2, 1, df_combined$weekNum)

df_combined$weekNum <- ifelse(df_combined$Month == "September" & df_combined$Date <= 30, 5, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "September" & df_combined$Date <= 27, 4, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "September" & df_combined$Date <= 20, 3, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "September" & df_combined$Date <= 13, 2, df_combined$weekNum)
df_combined$weekNum <- ifelse(df_combined$Month == "September" & df_combined$Date <= 6, 1, df_combined$weekNum)


#chart that shows the amount of trips by month
month_counts <- table(df_combined$Month)
barplot(month_counts[], xlim=c(0.4, 7), xlab="Month",
        ylim=c(0, max(month_counts)), ylab="Number of Rides", main="Month Count Chart", col = "Green")

#pivot table and chart that shows the trips by hour
hour_counts <- df_combined %>% group_by(Hour) %>% summarise(Count = n())
ggplot(hour_counts, aes(x=Hour, y=Count, fill=Hour)) + 
  geom_bar(stat="identity") +
  ggtitle("Count of Rides by Hour") +
  xlab("Hour") + ylab("Count")

#pivot table and chart that shows the trips by day
day_counts <- df_combined %>% group_by(Date) %>% summarise(Count = n())
ggplot(day_counts, aes(x=Date, y=Count, fill=Date)) + 
  geom_bar(stat="identity") +
  ggtitle("Count of Rides by Date") +
  xlab("Date") + ylab("Trips")

#Chart that shows Trips by Hour and Month
hourly_monthly_trips <- df_combined %>% group_by(Hour, Month) %>% summarise(Count = n()) 
ggplot(hourly_monthly_trips, aes(x = Hour, y = Count, fill = Month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Count of Trips by Hour and Month") +
  xlab("Hour") + ylab("Trips")

#Plot data by trips taken during every day of the month.
daily_trips <- df_combined %>% group_by(Date) %>% summarise(Trips = n()) 
ggplot(daily_trips, aes(x = Date, y = Trips)) + 
  geom_line()

#day of week chart
day_of_week_counts <- df_combined %>% group_by(dayName) %>% summarise(Count = n())
ggplot(day_of_week_counts, aes(x=dayName, y=Count, fill=dayName)) + 
  geom_bar(stat="identity") +
  ggtitle("Count of Rides by Day of the Week") +
  xlab("Day") + ylab("Trips")

#Chart that shows Trips by Day and Month
dayName_monthly_trips <- df_combined %>% group_by(dayName, Month) %>% summarise(Count = n()) 
ggplot(dayName_monthly_trips, aes(x = dayName, y = Count, fill = Month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Count of Trips by Day Name and Month") +
  xlab("Day Name") + ylab("Trips")

#Chart Trips by Bases and Month (Base is the X axis and Month is your label)
base_monthly_trips <- df_combined %>% group_by(Base, Month) %>% summarise(Count = n()) 
ggplot(base_monthly_trips, aes(x = Base, y = Count, fill = Month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Count of Trips by Base and Month") +
  xlab("Base") + ylab("Trips")

#Heat Maps

#Heat map that displays by hour and day
ggplot(df_combined, aes(x = Hour, y = Date)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Hour and Date") +
  labs(x = "Hour of Day", y = "Day of Month", fill = "Number of Trips")
ggplot(df_combined, aes(x = Hour, y = dayName)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Hour and Day of Week") +
  labs(x = "Hour of Day", y = "Day of Week", fill = "Number of Trips")

#Heat map by month and day
ggplot(df_combined, aes(x = Month, y = Date)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Month and Date") +
  labs(x = "Month", y = "Day of Month", fill = "Number of Trips")
ggplot(df_combined, aes(x = Month, y = dayName)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Month and Day of Week") +
  labs(x = "Month", y = "Day of Week", fill = "Number of Trips")

#Heat map by month and week
ggplot(df_combined, aes(x = weekNum, y = Month)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Week of Month and Month") +
  labs(x = "Week of Month", y = "Month", fill = "Number of Trips")

#Heat map Bases and Day of Week
ggplot(df_combined, aes(x = Base, y = dayName)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Base and Day of Week") +
  labs(x = "Base", y = "Day of Week", fill = "Number of Trips")


#prediction model
df_cortable<-df_combined %>%
  select(Date, Hour, weekNum, Lat, Lon, monthNum)
head(df_cortable)
B<-cor(df_cortable)
head(round(B,2))
corrplot(B, method="color")


#Leaflet Shiny Geospatial Map this portion is subject to change. 
ui <- fluidPage(
  leafletOutput("map")
)

# Server
server <- function(input, output) {
  # Create a Leaflet map
  output$map <- renderLeaflet({
    leaflet(data = df_combined) %>% 
      # Add map tiles
      addTiles() %>% 
      # Add markers for each trip start location
      addMarkers(lng = df_combined$Lon, lat = df_combined$Lat)
  })
}

# Run the app
shinyApp(ui, server)


df_combined_subset <- select(df_combined, colnames(df_combined)[4:5], colnames(df_combined)[8:9], colnames(df_combined)[11:12])
ui <- fluidPage(
  titlePanel("Uber Rides April-September 2014"),
  sidebarLayout(
    sidebarPanel(
      selectInput("VariableX", label = "Select Variable X", choices = unique(colnames(df_combined_subset))),
      selectInput("VariableY", label = "Select Variable Y", choices = c("Date", "dayName", "Month")),
      selectInput("Fill", label = "Select Fill Variable", choices = unique(colnames(df_combined_subset)))
    ),
    mainPanel(
      tabPanel("Chart", plotOutput("plot1")),
      tabPanel("Heat Map", plotOutput("plot2"))
    )
  )
)

server <- function(input, output) {
  selected_variables <- reactive({
    df_combined %>% group_by(!!sym(input$VariableX), !!sym(input$Fill)) %>% summarise(Count = n())
  })
  
  output$plot1 <- renderPlot({
    ggplot(selected_variables(), aes(x = !!sym(input$VariableX), y = Count, fill = !!sym(input$Fill))) + 
      geom_bar(stat = "identity")
  })
  
  output$plot2 <- renderPlot({
    ggplot(selected_variables(), aes(x = !!sym(input$VariableX), y = !!sym(input$VariableY), fill = ..count..)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue")
  })
}

shinyApp(ui, server)

