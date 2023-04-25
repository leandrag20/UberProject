# UberProject

For this project I analyzed the data from 6 months during the year 2014 and the rides from Uber that took place. Throughout this project, I cleaned the data, extracted certain values into seperate columns and use that data to create graphs and models to help better understand how popular Uber was during this time. 
![image](https://user-images.githubusercontent.com/113047041/234376822-d2c5265f-f7ee-46b6-b71a-12a3651cc8cf.png)

# Dictionary📝
The main columns I used during this project were:
Base: the base where the ride was from
Lat: The latitude coordinate from the ride
Lon: The longitude coordinate from the ride
Month: The month in which the ride took place
Time: The time in which the ride took place
Date: The date in which the ride took place
Hour: The hour in which the ride took place
dayName: The day of the week in which the ride took place
weekNum: The week of the month in which the ride took place

# Data Cleaning🧹
To first clean the data, I put in the month name and number to make it easily accessible
```R
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
```
Next, I put all the different tables into one by rbinding and pulled out certain data from the Date.Time column that I would later need.
```R
df_combined <- rbind(aprilData, mayData, juneData, julyData, augustData, septemberData)

df_combined$Time <- str_sub(df_combined$Date.Time, -8, -1)
df_combined$Date <- as.Date(df_combined$Date.Time, format = "%m/%d/%Y")
df_combined$Date <- as.numeric(str_sub(df_combined$Date, -2, -1))
df_combined$Hour <- as.numeric(str_sub(df_combined$Time, -8, -7))
```
After that, I made three new columns and pulled out that information from different columns as briefly shown below
```R
df_combined <- df_combined %>% 
  mutate(totalDay = NA, dayName = NA, weekNum = NA)

df_combined$totalDay <- ifelse(df_combined$Month == "May", df_combined$Date + 30, df_combined$Date)

df_combined$dayName <- ifelse(df_combined$totalDay %% 7 == 1, "Tuesday", df_combined$dayName)

df_combined$weekNum <- ifelse((df_combined$Month == "April" | df_combined$Month == "July") & df_combined$Date <= 31, 5, df_combined$weekNum)
```

#Creating Pivot Tables, Heat Maps and Charts
The next step after cleaning the data was to put it into Pivot Tables and Charts so the images of what exactly was happening could become more clear. Almost every Chart and Pivot Table code followed as similar format to the ones below, but differs from what exactly was being plotted:
```R
month_counts <- table(df_combined$Month)
barplot(month_counts[], xlim=c(0.4, 7), xlab="Month",
        ylim=c(0, max(month_counts)), ylab="Number of Rides", main="Month Count Chart", col = "Green")

hour_counts <- df_combined %>% group_by(Hour) %>% summarise(Count = n())
ggplot(hour_counts, aes(x=Hour, y=Count, fill=Hour)) + 
  geom_bar(stat="identity") +
  ggtitle("Count of Rides by Hour") +
  xlab("Hour") + ylab("Count")
  
daily_trips <- df_combined %>% group_by(Date) %>% summarise(Trips = n()) 
ggplot(daily_trips, aes(x = Date, y = Trips)) + 
  geom_line()
```

After the Pivot Tables and Charts were created, I made Heat Maps. All heat maps followed a similar format to the one below, but again differed in what was being plotted. 
```R
ggplot(df_combined, aes(x = Hour, y = Date)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "blue") +
  ggtitle("Hour and Date") +
  labs(x = "Hour of Day", y = "Day of Month", fill = "Number of Trips")
```

#Prediction Model
In order to try and predict when rides would happen, I made a correlation chart. This chart sadly did not help me gain any knowledge in when rides would happen due to there being close to no correlation in every variable. This correlation chart was made through:
```R
df_cortable<-df_combined %>%
  select(Date, Hour, weekNum, Lat, Lon, monthNum)
head(df_cortable)
B<-cor(df_cortable)
head(round(B,2))
corrplot(B, method="color")
```
This table below shows how there was little to no correlation with all the variables:
<img width="362" alt="Screen Shot 2023-04-25 at 2 44 43 PM" src="https://user-images.githubusercontent.com/113047041/234386505-224a9b95-640c-4723-be6c-0ef295d2d7dd.png">

#Leaflet Shiny Geospatial
The next part of this project was the create a simple Leaflet Shiny Geospatial, which I did through the following code:
```R
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

shinyApp(ui, server)
```

#Shiny App
The last part that needed to be completed for this project was to make a Shiny App to show the graphs that were made through the project. This was completed through the following code:
```R
ui <- fluidPage(
  titlePanel("Uber Rides April-September 2014"),
  sidebarLayout(
    sidebarPanel(
      selectInput("VariableX", label = "Select Variable X", choices = colnames(df_combined)),
      selectInput("VariableY", label = "Select Variable Y", choices = colnames(df_combined)),
      selectInput("Fill", label = "Select Fill Variable", choices = colnames(df_combined)),
      ),
    mainPanel(
      tabPanel("Chart", plotOutput("plot1")),
      tabPanel("Heat Map", plotOutput("plot2"))
      )
    )
)
server <- function(input, output) {
  selected_variables <- reactive({
    df_combined %>% group_by(input$VariableX, Fill) %>% summarise(Trips = n()) 
})
  # Create reactive function for graph 1
  output$plot1 <- renderPlot({
    ggplot(selected_variables, aes(x= !!sym(input$VariableX), y=Count, fill= !!sym(input$Fill))) + 
      geom_bar(stat="identity")
  })
  
  # Create reactive function for graph 2
  output$plot2 <- renderPlot({
    ggplot(df_combined, aes(x = !!sym(input$VariableX), y = !!sym(input$VariableY))) +
      stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1, 1)) +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Base", y = "Day of Week", fill = "Number of Trips")
  })
}

shinyApp(ui, server)
```
