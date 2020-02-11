#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(plyr)

# read in the data

data <- read.csv("litterati challenge-65.csv", stringsAsFactors = FALSE)

# assign unique usernames

data$username[data$username == "litterati-73940"] <- "Baadshah"
data$username[data$username == "litterati-115453"] <- "kainat"
data$username[data$username == "litterati-117766"] <- "waqar"
data$username[data$username == "litterati-119389"] <- "rozina"
data$username[data$username == "litterati-126822"] <- "abdul"
data$username[data$username == "litterati-127490"] <- "haseeb"
data$username[data$username == "litterati-57379"] <- "latif"
data$username[data$username == "litterati-64263"] <- "zahid"

# marking untagged

data$tags[data$tags == ""]<- "untagged"

# remove unwanted points not around forest park

data <- data[(data$lat < 41.88751),]
data <- data[(data$lat > 41.849888),]
data <- data[(data$lon > -87.833789),]
data <- data[(data$lon < -87.804116),]

# conver the date to Chicago time

data$date <- ymd_hms(data$litterTimestamp, tz = "America/Chicago")

# add the days of the week

data$days <- wday(ymd_hms(data$date), label = TRUE, abbr = FALSE)

# extract hour and put it into a column

data$hour <- hour(data$date)

# num of litter 

num_litter <- length(data$user_id)

# top 10 litter picker

table1 <- count(data$username)
attach(table1)
table2 <- table1[order(-freq),]
detach(table1)
table3 <- table2[1:10,]
table3$User <- table3$x
table3$x <- NULL

# adding "default" in the top 10 users table

frame1 <- data.frame(table3$User)
names(frame1) <- c("User")

frame2 <- data.frame(toString("default"))
names(frame2) <- c("User")

frame3 <- rbind(frame1,frame2)

# top 10 tags

abdul <- toString(data$tags)
abdul <- strsplit(abdul,",")
abdul <- data.frame(abdul)
abdul <- trimws(abdul$c..wrapper....cigarette....plastic.....plastic....wrapper.....papertowel...)
abdul <- data.frame(abdul)
abdul <- count(abdul)
abdul <- abdul[order(-abdul$freq),]
abdul <- abdul[1:10,]

# adding default in the top 10 tags

frame4 <- data.frame(abdul$abdul)
names(frame4) <- c("tag")

frame5 <- data.frame(toString("default"))
names(frame5) <- c("tag")

frame6 <- rbind(frame4, frame5)

# Define UI for application that draws a histogram

ui <- dashboardPage(
  
  dashboardHeader(
    title = "title panel"
  ),
  
  dashboardSidebar(
    sidebarMenu(
    
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    selectInput("selectedUser", label = "Pick a User",frame3$User, selected = "default"),
    
    selectInput("selectedTag", label = "Pick a tag", frame6$tag, selected = "default"),
    
    selectInput("timeOfDay", label = "Time of Day", c("Morning","Afternoon","Evening","Night","default"), selected = "default")
    
  ),
  
  dashboardBody(
    
    fluidRow(
      column(12,
             fluidRow(
               box(title = "Litter picked up each day", solidHeader = TRUE, status = "primary", height = 500,
                   plotOutput("first_graph", height = 400)
               ),
               box(title = "Litter picked up each day", solidHeader = TRUE, status = "primary",
                   dataTableOutput("First_Graph")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             fluidRow(
               box(title = "Litter picked up by day of week", solidHeader = TRUE, status = "primary", height = 500,
                   plotOutput("second_graph", height = 400)
               ),
               box(title = "Litter picked up by day of weekday", solidHeader = TRUE, status = "primary",
                   dataTableOutput("Second_Graph")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             box(title = "Litter Location", solidHeader = TRUE, status = "primary", height = 500,
                 leafletOutput("map", height = 400)
             ),
             box(title = "About : Total", solidHeader = TRUE, status = "primary", height = 500,
                 textOutput("var")
             ) 
      )
    ),
    fluidRow(
      column(12,
             fluidRow(
               box(title = "Litter picked up by hour", solidHeader = TRUE, status = "primary", height = 500,
                   plotOutput("third_graph", height = 400)
               ),
               box(title = "Litter picked up by hour", solidHeader = TRUE, status = "primary", height = 400,
                   dataTableOutput("Third_Graph")
               )
             )
      )
    ),
    fluidRow(
      column(12,
             fluidRow(
               box(title = "Litter by Tag", solidHeader = TRUE, status = "primary", height = 500,
                   plotOutput("fourth_graph", height = 400)
               ),
               box(title = "Litter by Tag", solidHeader = TRUE, status = "primary",
                   dataTableOutput("Fourth_Graph")
               )
             ) 
      )
    ),
    fluidRow(
      box(title = "Top 10 Pickers", solidHeader = TRUE, status = "primary",
          tableOutput("top10picker")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$top10picker <- renderTable({table3})
  output$var <- renderText({ 
    paste("total litter picked up was", num_litter)
  })
  output$First_Graph <- renderDataTable({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      date_only <- as.character(date(data$date)) 
      date_count <- count(date_only)
      date_count
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      date_only <- as.character(date(singledata$date)) 
      date_count <- count(date_only)
      date_count
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      date_only <- as.character(date(singledata$date)) 
      date_count <- count(date_only)
      date_count
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, singledata$tags))
      date_only <- as.character(date(singledata$date)) 
      date_count <- count(date_only)
      date_count
    }
  })
  
  output$Second_Graph <- renderDataTable({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      days_count <- count(data$days)
      days_count
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      days_count <- count(singledata$days) 
      days_count
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      days_count <- count(singledata$days) 
      days_count
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, singledata$tags))
      days_count <- count(singledata$days) 
      days_count
    }
  })
  
  output$Third_Graph <- renderDataTable({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      hour_count <- count(data$hour)
      hour_count
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      hour_count <- count(singledata$hour)
      hour_count
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      hour_count <- count(singledata$hour)
      hour_count
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      hour_count <- count(singledata$hour)
      hour_count
    }
  })
  
  output$Fourth_Graph <- renderDataTable({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      abdul <- toString(data$tags)
      abdul <- strsplit(abdul,",")
      abdul <- data.frame(abdul)
      abdul <- trimws(abdul$c..wrapper....cigarette....plastic.....plastic....wrapper.....papertowel...)
      abdul <- data.frame(abdul)
      abdul <- count(abdul)
      abdul <- abdul[order(-abdul$freq),]
      abdul <- abdul[1:10,]
      abdul
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      abdul
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      abdul
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      abdul
    }
  })
  
  output$selected_var <- renderText({
    if(input$selectedUser == "default")
    {
      paste("data include every user")   
    }
    else
    {
      paste("you have selected user:", input$selectedUser)
    }
  })
  
  output$map <- renderLeaflet({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=data$lon, lat=data$lat, clusterOptions = markerClusterOptions())  
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=singledata$lon, lat=singledata$lat, clusterOptions = markerClusterOptions())
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=singledata$lon, lat=singledata$lat, clusterOptions = markerClusterOptions())
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=singledata$lon, lat=singledata$lat, clusterOptions = markerClusterOptions())  
    }
  })
  
  output$first_graph <- renderPlot({
    name <- input$selectedUser
    tag <- input$selectedTag
    timeofDay <- input$timeOfDay
    
    if(name == "default" && tag == "default"){
      date_only <- as.Date(data$date,"%Y%m%d")
      date_count <- count(date_only)
      first_graph <- ggplot(date_count, aes(x = x,y=freq)) + xlab("Dates")+ ylab("No. litter picked") + geom_bar(stat = "identity",fill="steelblue") + scale_x_date(date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      first_graph            
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      date_only <- as.Date(singledata$date,"%Y%m%d")
      date_count <- count(date_only)
      first_graph <- ggplot(date_count, aes(x = x,y=freq)) + xlab("Dates")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue") + scale_x_date(date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      first_graph
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      date_only <- as.Date(singledata$date,"%Y%m%d")
      date_count <- count(date_only)
      first_graph <- ggplot(date_count, aes(x = x,y=freq)) + xlab("Dates")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue") + scale_x_date(date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      first_graph
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      date_only <- as.Date(singledata$date,"%Y%m%d")
      date_count <- count(date_only)
      first_graph <- ggplot(date_count, aes(x = x,y=freq)) + xlab("Dates")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue") + scale_x_date(date_breaks = "1 day") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      first_graph
    }
    
  })
  output$second_graph <- renderPlot({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      days_count <- count(data$days) 
      second_graph <- ggplot(days_count, aes(x = x,y=freq)) + xlab("Days")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      second_graph
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      days_count <- count(singledata$days) 
      second_graph <- ggplot(days_count, aes(x = x,y=freq)) + xlab("Days")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      second_graph
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      days_count <- count(singledata$days) 
      second_graph <- ggplot(days_count, aes(x = x,y=freq)) + xlab("Days")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      second_graph
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, singledata$tags))
      days_count <- count(singledata$days) 
      second_graph <- ggplot(days_count, aes(x = x,y=freq)) + xlab("Days")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      second_graph 
    }
  })
  output$third_graph <- renderPlot({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      hour_count <- count(data$hour)
      third_graph <- ggplot(hour_count, aes(x = x,y=freq)) + xlab("Hour")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      third_graph
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      hour_count <- count(singledata$hour)
      third_graph <- ggplot(hour_count, aes(x = x,y=freq)) + xlab("Hour")+ ylab("No. litter picked") + geom_bar(stat = "identity",fill="steelblue")
      third_graph
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      hour_count <- count(singledata$hour)
      third_graph <- ggplot(hour_count, aes(x = x,y=freq)) +  xlab("Hour")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      third_graph
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      hour_count <- count(singledata$hour)
      third_graph <- ggplot(hour_count, aes(x = x,y=freq)) +  xlab("Hour")+ ylab("No. litter picked") +geom_bar(stat = "identity",fill="steelblue")
      third_graph
    }
  })
  output$fourth_graph <- renderPlot({
    name <- input$selectedUser
    tag <- input$selectedTag
    
    if(name == "default" && tag == "default"){
      abdul <- toString(data$tags)
      abdul <- strsplit(abdul,",")
      abdul <- data.frame(abdul)
      abdul <- trimws(abdul$c..wrapper....cigarette....plastic.....plastic....wrapper.....papertowel...)
      abdul <- data.frame(abdul)
      abdul <- count(abdul)
      abdul <- abdul[order(-abdul$freq),]
      abdul <- abdul[1:10,]
      fourth_graph <- ggplot(data = abdul, aes(x = abdul, y = freq)) +  xlab("Tags")+ ylab("No. litter picked") +geom_bar(stat = "identity", fill="steelblue")
      fourth_graph
    }
    else if(name != "default" && tag == "default"){
      singledata <- subset(data, data$username == name)
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      fourth_graph <- ggplot(data =abdul, aes(x = abdul, y = Freq)) +  xlab("Tags")+ ylab("No. litter picked") +geom_bar(stat = "identity", fill="steelblue")
      fourth_graph
    }
    else if(name == "default" && tag != "default"){
      singledata <- subset(data, grepl(tag, data$tags))
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      fourth_graph <- ggplot(data =abdul, aes(x = abdul, y = Freq)) +xlab("Tags")+ ylab("No. litter picked") + geom_bar(stat = "identity", fill="steelblue")
      fourth_graph
    }
    else{
      singledata <- subset(data, data$username == name)
      singledata <- subset(singledata, grepl(tag, data$tags))
      abdul <- strsplit(singledata$tags,",")
      abdul <- unlist(abdul)
      abdul <- table(abdul)
      abdul <- as.data.frame(abdul)
      abdul <- abdul[order(-abdul$Freq),]
      abdul <- head(abdul,10)
      fourth_graph <- ggplot(data =abdul, aes(x = abdul, y = Freq)) +xlab("Tags")+ ylab("No. litter picked") + geom_bar(stat = "identity", fill="steelblue")
      fourth_graph
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
