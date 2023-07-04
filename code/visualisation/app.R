library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("COVID 19 Sentiment analysis"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select plot
      selectInput("plot_select", label = "Select type",
                  choices = c("Sentiment", "Topic"),
                  selected = "Sentiment")
    ),
    mainPanel(
      # Display selected plot
      plotOutput("selected_plot")
    )
  )
)

# Server
server <- function(input, output) {
  # Render selected plot
  output$selected_plot <- renderPlot({
    if (input$plot_select == "Sentiment") {
      # Generate plot 1
      ggplot(mtcars, aes(x = mpg, y = disp)) +
        geom_point() +
        labs(title = "Sentiment", x = "MPG", y = "Displacement")
    } else if (input$plot_select == "Topic") {
      # Generate plot 2
      ggplot(mtcars, aes(x = hp, y = wt)) +
        geom_point() +
        labs(title = "Topic", x = "Horsepower", y = "Weight")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)