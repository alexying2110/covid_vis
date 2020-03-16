library(shiny)
library(DT)
library(ggplot2)

today <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-15-2020.csv")
confirmed <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

data = confirmed %>% 
  inner_join(., deaths, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".confirmed", ".deaths")) %>%
  inner_join(., recovered, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".recovered", ".recovered"))

ui <- fluidPage(
  fluidRow(
    column(12, DT::dataTableOutput('tbl')),
  ), 
  fluidRow(
    column(4, plotOutput('confirmed', height = 500)),
    column(4, plotOutput('deaths', height = 500)),
    column(4, plotOutput('recovered', height = 500))
  )
)

server <- shinyServer(function(input, output, session) {
  output$tbl = DT::renderDataTable({
    datatable(today[c(1:6)], selection = "single")
    }, 
    server = FALSE
  )
  
  
  output$confirmed = renderPlot({
    selected <- input$tbl_rows_selected
    province <- as.character(today[selected,]$Province.State)
    country <- as.character(today[selected,]$Country.Region)
    selectedRow <- intersect(which(confirmed$Province.State == province), which(confirmed$Country.Region == country))
    
    dates <- gsub("X", "", names(confirmed)[-c(1:4)])
    
    confirmed_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), confirmed = as.numeric(confirmed[selectedRow,-c(1:4)]))
    ggplot(data = confirmed_data, aes(x=date, y=confirmed)) + geom_line(color="orange") + geom_point(color="orange")
  })
  
  output$deaths = renderPlot({
    selected <- input$tbl_rows_selected
    province <- as.character(today[selected,]$Province.State)
    country <- as.character(today[selected,]$Country.Region)
    selectedRow <- intersect(which(deaths$Province.State == province), which(deaths$Country.Region == country))
    
    dates <- gsub("X", "", names(deaths)[-c(1:4)])
    
    deaths_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), deaths = as.numeric(deaths[selectedRow,-c(1:4)]))
    ggplot(data = deaths_data, aes(x=date, y=deaths)) + geom_line(color="red") + geom_point(color="red")
  })
  
  output$recovered = renderPlot({
    selected <- input$tbl_rows_selected
    province <- as.character(today[selected,]$Province.State)
    country <- as.character(today[selected,]$Country.Region)
    selectedRow <- intersect(which(recovered$Province.State == province), which(recovered$Country.Region == country))
    
    dates <- gsub("X", "", names(recovered)[-c(1:4)])
    
    recovered_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), recovered = as.numeric(recovered[selectedRow,-c(1:4)]))
    ggplot(data=recovered_data, aes(x=date, y=recovered)) + geom_line(color="green") + geom_point(color="green")
  })
})