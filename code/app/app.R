library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)
library(forcats)


tweets <- readRDS("tweets.rds")

# Prepare the transformed data
df <- tweets %>% 
  group_by(date, VADER_label) %>%
  summarise(count = n()) %>%
  ungroup()


topic_df <- tweets %>%
  group_by(date, topic) %>%
  summarise(count = n()) %>%
  ungroup()



events <- data.frame(
  event = c("WHO approve emergency BioNTech vaccine in Netherlands",
            "COVISHIELD approved for use",
            "Moderna vaccine approved for use in EU"
            ),
  date = as.Date(c(
    "2020-12-18",
    "2021-02-15",
    "2021-04-30"
  )
  )
)


# Define UI


ui <- page_navbar(
    title = "The Outbreak Outliars",
    sidebar = sidebar(
      title = "Dashboard controls",
      selectInput(
        inputId = "label_filter",
        label = "Select Sentiment Label",
        choices = c("All", "Negative", "Positive"),
        selected = "All"
      ),
      # selectInput(
      #   inputId = "topic_filter",
      #   label = "Select Topic",
      #   choices = c("All", unique(topic_df$topic)),
      #   selected = "All"
      # ),
      sliderInput(
        inputId = "date_range",
        label = "Select Time Bounds",
        min = min(ymd(tweets$date), na.rm = TRUE),
        max = max(ymd(tweets$date), na.rm = TRUE),
        value = c(min(ymd(tweets$date), na.rm = TRUE), max(ymd(tweets$date), na.rm = TRUE))
      )
    ),
   nav_panel("Sentiment",
   
    
    card(
      width = 12,
      status = "primary",
      card_header("Sentiment Analysis"),
      plotOutput("sentimentPlot")
    ),
    card(
      width = 12,
      status = "primary",
      card_header("Topic modelling"),
      plotOutput("topicPlot")
    )
  ),
  nav_panel("Topic", "Page 2 content"),
  
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    navbar_bg = "#2d2d2d"
  )
  )



# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default theme
theme_set(theme_dark())

# Define server
server <- function(input, output) {
  # Render the sentiment plot
  output$sentimentPlot <- renderPlot({
    filtered_df <- df
    events <- events
    if (input$label_filter != "All") {
      filtered_df <- filtered_df %>%
        filter(VADER_label == input$label_filter)
    }
    filtered_df <- filtered_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    ggplot(data = filtered_df, aes(x = date, y = count, color = VADER_label, group = VADER_label)) +
      geom_line() +
      labs(x = "Date", y = "Count of Tweets") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851"),
        labels = c(Negative = "Negative", Positive = "Positive")
      ) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) +
      scale_x_date(limits = input$date_range, expand = c(0, 0), date_labels = "%b %d, %Y")
  })
  
  # Render the topic plot
  output$topicPlot <- renderPlot({
    filtered_topic_df <- topic_df
    filtered_topic_df <- topic_df
    # if (input$topic_filter != "All") {
    #   filtered_topic_df <- filtered_topic_df %>%
    #     filter(topic == input$topic_filter)
    # }
    filtered_topic_df <- filtered_topic_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])

    filtered_topic_df %>% 
      group_by(topic) %>% 
      summarise(val = n()) %>% 
      mutate(topic = fct_reorder(topic, val)) %>%
      ggplot( aes(x=topic, y=val)) +
      geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      scale_color_manual(
        values = c(
          "Topic 1" = "#ff4444",
          "Topic 2" = "#00C851",
          "Topic 3" = "#FF8800",
          "Topic 4" = "#FFC400",
          "Topic 5" = "#4285F4"
        ),
        labels = c(
          "Topic 1" = "Topic 1",
          "Topic 2" = "Topic 2",
          "Topic 3" = "Topic 3",
          "Topic 4" = "Topic 4",
          "Topic 5" = "Topic 5"
        )
      ) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d")
      ) 
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

