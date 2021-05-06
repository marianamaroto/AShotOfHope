# read latest data from Github

# load libraries
library(ggplot2)
library(xts)
library(dygraphs)
library(plotly)
library(reshape2)
library(readr)
library(shinydashboard)
library(dplyr)
library(data.table)
library(plotly)

urlfile="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
mydata<-read_csv(url(urlfile))

urlfile2="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv"
databymanu<-read_csv(url(urlfile2))

pop_data = read.csv('pop_figures.csv', stringsAsFactors = FALSE)

joined_data <- inner_join(pop_data, mydata, by = c("Country_Code" = "iso_code"))

# Define UI for app that draws a histogram ----
ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "A Shot of Hope"),
  
  dashboardSidebar(
      
      selectInput(inputId = "country",
                  label = "Select Country:",choices = unique(joined_data$location), selected = 'United States'),
      numericInput(inputId = "per_herd", min = 50, max = 100, value = 70, step = 1, label = '% To Reach Herd Immunity'),
      h5('Powered By: ', tags$a(href="https://marimarotodata.com/", "MariMarotoData.com")),
      h5('Data from: ',  tags$a(href="https://ourworldindata.org/covid-vaccinations/", 'Our World In Data'))
    ),
    
    dashboardBody(
      fluidRow(
        infoBox(width = 4, "Days Till Herd Immunity", textOutput(outputId = "days_herd"), icon = icon("calendar")),
        infoBox(width = 4, "% of Pop Vaccinated Daily", textOutput(outputId = "per_vaccinated_daily"), icon = icon("heart")),
        box(width = 4 , h6("Estimated based on country's population and its latest 7 day rolling average vaccinations/day.")
      )),
      fluidRow(
        tabBox(title = "COVID -19 Vaccination Rates", width = 12,
               tabPanel("At least 1 Dose",  dygraphOutput(outputId = "timeseries")),
               tabPanel("2 Doses", dygraphOutput(outputId = "timeseries_fully")))
      )
        
        
        #box(plotlyOutput("pie_brands"))
    ) # close dashboard body
  
)


# Define server 
server <- function(input, output) {
  
  output$timeseries <- renderDygraph({
    
    # select country
    plot_data <- mydata[ which(mydata$location == input$country),]
    
    # create time series object
    phones_timeSeries <- xts(x = plot_data$people_vaccinated_per_hundred,
                             order.by = plot_data$date)
    
    # create a basic interactive element
    interact_time <- dygraph(phones_timeSeries, xlab = 'Date', ylab = 'Vaccinated Rate', main = paste(input$country, 'Vaccination Rate (at least 1 Dose)')) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#3269a8") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
      dyAxis("y", valueRange = c(0,100))
    interact_time
    
  })
  
  output$timeseries_fully <- renderDygraph({
    
    # select country
    plot_data <- mydata[ which(mydata$location == input$country),]
    
    # create time series object
    phones_timeSeries <- xts(x = plot_data$people_fully_vaccinated_per_hundred,
                             order.by = plot_data$date)
    
    # create a basic interactive element
    interact_time <- dygraph(phones_timeSeries, xlab = 'Date', ylab = 'Fully Vaccinated Rate', main = paste(input$country,'Vaccination Rate (2-doses)')) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#3269a8") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
      dyAxis("y", valueRange = c(0,100))
      
    interact_time
    
  })
  
  output$pie_brands <- renderPlotly({
    
    plot_data <- databymanu[ which(databymanu$location == input$country),]
    latest_date <- max(plot_data$date)
    plot_data <- plot_data[ which(plot_data$date == latest_date),]
    
    fig <- plot_ly(plot_data, labels = ~vaccine, values = ~total_vaccinations, type = 'pie')
    fig <- fig %>% layout(title = 'Total Vaccinations by Manufacturer',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
  })
  
  output$days_herd <- renderText({
    # select country
    joined_data <- inner_join(pop_data, mydata, by = c("Country_Code" = "iso_code"))
    joined_data <- joined_data[ which(joined_data$location == input$country),]
    latest_date <- max(joined_data$date)
    population = joined_data$Year_2016[joined_data$date==latest_date]
    vacc_doses_delivered = joined_data$total_vaccinations[joined_data$date==latest_date]
    latest_daily_doses = joined_data$daily_vaccinations[joined_data$date==latest_date]
    days = ((population*(input$per_herd)/100)-(vacc_doses_delivered/2))/(latest_daily_doses/2)
    round(days, digits = 0)
  })

  output$per_vaccinated_daily <- renderText({
    # select country
    joined_data <- inner_join(pop_data, mydata, by = c("Country_Code" = "iso_code"))
    joined_data <- joined_data[ which(joined_data$location == input$country),]
    latest_date <- max(joined_data$date)
    population = joined_data$Year_2016[joined_data$date==latest_date]
    vacc_doses_delivered = joined_data$total_vaccinations[joined_data$date==latest_date]
    latest_daily_doses = joined_data$daily_vaccinations[joined_data$date==latest_date]
    per_vaccinated_daily = (latest_daily_doses/population)*100
    round(per_vaccinated_daily, digits = 4)
  })
  
}

shinyApp(ui, server)