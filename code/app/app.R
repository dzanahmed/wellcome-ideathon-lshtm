library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)
library(forcats)
library(bsicons)

# 
tweets <- readRDS("tweets.rds")


# Prepare the transformed data
df <- tweets %>% 
  group_by(date, VADER_label) %>%
  summarise(count = as.double(n()))%>%
  pivot_wider(names_from = VADER_label, values_from = count) %>%
  ungroup() %>% 
  mutate(Positive = rollapply(Positive,7,mean,align='right',fill=NA),
         Negative = rollapply(Negative,7,mean,align='right',fill=NA),
         Neutral = rollapply(Neutral,7,mean,align='right',fill=NA)) %>% 
  pivot_longer(cols =-date ) %>% 
  rename(avg = value,
         VADER_label = name) %>% 
  left_join(tweets %>% 
              group_by(date, VADER_label) %>%
              summarise(count = as.double(n())), 
            by = c("date", "VADER_label")
  )
  
  



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


vbs <- list(
  value_box(
    title = "Total number of flags", 
    value = "123",
    showcase = bs_icon("bar-chart"),
    theme_color = "danger",
    height = "120px"
    # p("The 1st detail")
  ),
  value_box(
    title = "Validated tweets", 
    value = "456",
    showcase = bs_icon("graph-up"),
    theme_color = "warning",
    height = "120px"
    # p("The 2nd detail"),
    # p("The 3rd detail")
  ),
  value_box(
    title = "3rd value", 
    value = "789",
    showcase = bs_icon("pie-chart"),
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

layout_column_wrap(
  width = "150px",
  height = "120px",
  !!!vbs
)





# Define UI


ui <- page_navbar(
    title = "The Outbreak Outliars",
    sidebar = sidebar(
      title = "Dashboard controls",
      # selectInput(
      #   inputId = "label_filter",
      #   label = "Select Sentiment Label",
      #   choices = c("All", "Negative", "Positive", "Neutral"),
      #   selected = "All"
      # ),
      
      # This doesnt work is the box chose, needs to be fixed
      
      selectInput(
        inputId = "topic_filter",
        label = "Select Topic",
        choices = c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5"),
        selected = "All"
      ),
      
      
      # This is the box check for the sentiment but also requires changes in the output function 
      # to work properly
      
      checkboxGroupInput(
        inputId = "label_filter",
        label = "Select Sentiment Label",
        choices = c("All",unique(df$VADER_label)),
        selected ="All"
      ),
      
      
      sliderInput(
        inputId = "date_range",
        label = "Select Time Bounds",
        min = min(ymd(tweets$date), na.rm = TRUE),
        max = max(ymd(tweets$date), na.rm = TRUE),
        value = c(min(ymd(tweets$date), na.rm = TRUE), max(ymd(tweets$date), na.rm = TRUE))
      )
    ),
   nav_panel("Sentiment",
            layout_column_wrap(
               width = "150px",
               height = "120px",
               fill = FALSE,
               vbs[[1]], vbs[[2]]
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
      card_header("Topic modelling"),
      plotOutput("topicPlot")
    )
  ),
  nav_panel("Topic", 
            card(
              width = 12,
              status = "primary",
              card_header("Topic modelling"),
              plotOutput("topic_time")
            )),
  
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
   
    # if (input$label_filter != "All") {
    #   # filtered_df <- df[df$VADER_label %in% input$label_filter,]
    #   # filtered_df
    #   if(is.null(input$label_filter)){NULL}
    #   
    #     filtered_df <- filtered_df %>%
    #     filter(VADER_label %in% input$label_filter)
    # }
    
    
    if (input$label_filter != "All"){
       
       filtered_df <- filtered_df %>%
         filter(VADER_label == input$label_filter)
       }
        
       
    
    filtered_df <- filtered_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    ggplot(data = filtered_df, aes(x = date, color = VADER_label, group = VADER_label)) +
      geom_point(aes(y =count), alpha = 0.5) +
      # geom_smooth() +
      geom_line(aes(y = avg)) +
      labs(x = "Date", y = "Count of Tweets", color = "7-day rolling average") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851", Neutral = "#FFdd00"),
        labels = c(Negative = "Negative", Positive = "Positive", Neutral = "Neutral")
      ) +
      # geom_vline(data = events, aes(xintecept = date)) + 
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
    
# This is the function that filters the output to options selected from the box menu
# Need to be fixed
    
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
      geom_bar(stat="identity", fill=c("#ff4444","#00C851","#FF8800","#FFC400","#4285F4"),
               alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
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

  # Render the topic time series
  output$topic_time <- renderPlot({
    filtered_topic_df <- topic_df
    filtered_topic_df <- topic_df
    
    # This is the function that filters the output to options selected from the box menu
    # Need to be fixed
    
    if (input$topic_filter != "All") {
      filtered_topic_df <- filtered_topic_df %>%
        filter(topic == input$topic_filter)
    }
    filtered_topic_df <- filtered_topic_df %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    ggplot(data = filtered_topic_df, aes(x = date, y = count, color = topic, group = topic)) +
      geom_point() +
      geom_smooth() +
      labs(x = "Date", y = "Count of Tweets") +
      scale_color_manual(
        values = c(Negative = "#ff4444", Positive = "#00C851"),
        labels = c(Negative = "Negative", Positive = "Positive")) +
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
        )) +
      theme(
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.background = element_rect(fill = "#2d2d2d"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#2d2d2d", color = "#2d2d2d"))
    
      }) 
  }
  

# Run the Shiny app
shinyApp(ui = ui, server = server)
