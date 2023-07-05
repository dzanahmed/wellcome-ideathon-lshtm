# This is  a shiny app to present results of sentiment analysis and topic modelling

# Set working directory to script location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)

# Generate sample data
set.seed(123)
dates <- seq(as.Date("2020-08-01"), as.Date("2022-07-31"), by = "day")
negative_tweets <- sample(100:500, length(dates), replace = TRUE)
positive_tweets <- sample(200:800, length(dates), replace = TRUE)
countries <- sample(c("USA", "UK", "India"), length(dates), replace = TRUE)

# Define UI
ui <- page_sidebar(
  title = "Sentiment Analysis",
  sidebar = sidebar(
    title = "Dashboard controls",
    selectInput(
      inputId = "country_filter",
      label = "Select Country",
      choices = c("All", unique(countries)),
      selected = "All"
    ),
    sliderInput(
      inputId = "date_range",
      label = "Select Time Bounds",
      min = as.Date("2020-08-01"),
      max = as.Date("2022-07-31"),
      value = c(as.Date("2020-08-01"), as.Date("2022-07-31")),
      timeFormat = "%Y-%m-%d")
  ),
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    navbar_bg = "#343a40"
  ),
  card(
    width = 12,
    status = "primary",
    card_header("Sentiment Analysis"),
    plotOutput("sentimentPlot")
  ),
  card(
    width = 12,
    status = "primary",
    card_header("Filtered Data"),
    dataTableOutput("filteredData")
  )
)

# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default theme
theme_set(theme_dark())

# Define server
server <- function(input, output) {
  # Prepare data for plotting
  sentiment_data <- reactive({
    df <- data.frame(Date = dates, Negative = negative_tweets, Positive = positive_tweets, Country = countries)
    filtered_df <- if (input$country_filter == "All") {
      df
    } else {
      df %>%
        filter(Country == input$country_filter)
    }
    filtered_df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
  })
  
  # Render the plot
  output$sentimentPlot <- renderPlot({
    ggplot(data = sentiment_data()) +
      geom_line(aes(x = Date, y = Negative, color = "Negative"), size = 1) +
      geom_line(aes(x = Date, y = Positive, color = "Positive"), size = 1) +
      labs(x = "Date", y = "Count of Tweets") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851"),
        labels = c(Negative = "Negative", Positive = "Positive")
      ) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#343a40"),
        panel.background = element_rect(fill = "#343a40"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#343a40", color = "#343a40")
      )
  })
  
  # Render the filtered data table
  output$filteredData <- renderDataTable({
    sentiment_data()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




