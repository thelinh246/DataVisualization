library(shiny)
library(ggplot2)
library(dplyr)

# Đọc dữ liệu
data <- read.csv("historytrial2037.csv")
data$Start.Time <- as.Date(data$Start.Time, format="%Y-%m-%d")

# UI
ui <- fluidPage(
  titlePanel("Enhanced History Log Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("search_type", "Select Search Type:", 
                  choices = c("Tất cả", unique(data$Search.Type))),
      dateRangeInput("date_range", "Select Date Range:", 
                     start = min(data$Start.Time), 
                     end = max(data$Start.Time)),
      selectInput("chart_type", "Select Chart Type:", 
                  choices = c("Bar Plot", "Line Chart", "Pie Chart"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", plotOutput("mainPlot")),
        tabPanel("Summary", tableOutput("summaryTable")),
        tabPanel("Detailed Data", tableOutput("detailedTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$search_type == "Tất cả") {
      data %>%
        filter(Start.Time >= input$date_range[1], 
               Start.Time <= input$date_range[2])
    } else {
      data %>%
        filter(Search.Type == input$search_type, 
               Start.Time >= input$date_range[1], 
               Start.Time <= input$date_range[2])
    }
  })
  
  output$mainPlot <- renderPlot({
    plot_data <- filtered_data() %>%
      group_by(Start.Time, User) %>%
      summarize(Searches = n(), Total_Results = sum(Result.Count), .groups = 'drop')
    
    if (input$chart_type == "Bar Plot") {
      ggplot(plot_data, aes(x = Start.Time, y = Searches, fill = User)) +
        geom_bar(stat = "identity") +
        labs(title = "Number of Searches per Day", x = "Date", y = "Searches") +
        theme_minimal()
    } else if (input$chart_type == "Line Chart") {
      ggplot(plot_data, aes(x = Start.Time, y = Total_Results, color = User, group = User)) +
        geom_line(size = 1.2) +
        labs(title = "Total Results over Time", x = "Date", y = "Total Results") +
        theme_minimal()
    } else if (input$chart_type == "Pie Chart") {
      pie_data <- filtered_data() %>%
        group_by(Search.Pattern) %>%
        summarize(Count = n(), .groups = 'drop')
      ggplot(pie_data, aes(x = "", y = Count, fill = Search.Pattern)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Search Pattern Distribution") +
        theme_void()
    }
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      group_by(Start.Time) %>%
      summarize(
        Num_of_Users = n_distinct(User),
        Num_of_Searches = n(),
        Total_Results = sum(Result.Count),
        Avg_Results = mean(Result.Count),
        .groups = 'drop'
      )
  })
  
  output$detailedTable <- renderTable({
    filtered_data()
  })
}

# Run App
shinyApp(ui = ui, server = server)
