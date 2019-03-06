# Load packages ----
library(shiny)
library(quantmod)
library(nycflights13)
library(tidyverse)
library(lubridate)

# User interface ----
ui <- fluidPage(
  titlePanel("Solution to Problem 24.3.5 #7 from R For Data Science:"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a time range (hour) and distance cutoff (miles) for the plot. See the main panel for more details."),
      
      sliderInput("times", 
                  label = "Time of Day:",
                  min = 0, max = 24, value = c(0, 24, 1)),
      
      numericInput("dist_cutoff",
                   label = "Minimum Distance Considered a 'Long' Flight:",
                   value = 1500)
    ),
    
    mainPanel(
      tags$i("Solution written out by Rachel Aronow"),
      br(),
      br(),
      strong("Question:"),
      p("We hypothesised that people leaving on Sundays are more likely to be business travellers who need to be somewhere on Monday. Explore that hypothesis by seeing how it breaks down based on distance and time: if it's true, you'd expect to see more Sunday evening flights to places that are far away."),
      br(),
      strong("Answer: "),
      p("To answer this question, we will plot the proportion of flights that are over a certain distance cutoff, grouped by weekday and hour. 
        To produce a plot, choose a distance cutoff and time of day from the left sidebar:"),

      textOutput("time_range"),
      textOutput("dist_cutoff"),
      br(),
      plotOutput("plot"),
      br(),
      br(),
      # tableOutput("table"),
      p("There are a lot of insights to be gained from this plot. However, if we aim to answer 
        the proposed question, we want to focus in on the hours we'd expect business travellers 
        to be flying out. We'd expect this to be in the evening, but not so late that the business 
        person would not be able to return to work early the next day. Therefore we choose the range 
        17:00 - 21:00, and we can see that for cutoffs between 600 and 2000 miles, Sundays have a 
        slightly larger proportion of long flights than every other day of the week besides Saturdays.
        In particular, the gap between Sundays and the rest of the week (excluding Saturday) is most 
        pronounced during 19:00-20:00 in the evening. However this conclusion is highly dependent on 
        what we consider the cutoff to be for a 'long' flight. If we change the cutoff out of this range, 
        we start to lose this 19:00-20:00 bump. This makes sense - flights shorter than 600 miles should 
        not be considered long, and flights longer than 2000 miles are much more rare in the dataset, and 
        therefore likely to tell us more about outlier flights than general trends.")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$time_range <- renderText({
    paste("You have chosen a range that goes from hour", input$times[1], "to", input$times[2])
  })
  
  output$dist_cutoff <- renderText({
    paste("You have chosen to plot the proportion of flights that travel at least", input$dist_cutoff, "miles")
  })
  

  output$plot <- renderPlot({
    mutate(flights, date = make_date(year, month, day), 
           wday = wday(date, label = TRUE),
           long = (distance >= input$dist_cutoff)) %>% 
      group_by(hour, wday) %>% 
      summarise(prop_long = mean(long, na.rm=TRUE)) %>% 
      filter((hour >= input$times[1]) & (hour <= input$times[2])) %>% 
      ggplot(aes(x=hour, y=prop_long, color=wday)) +
        geom_point() +
        geom_line() +
        xlab("Time of Day") +
        ylab(paste("Proportion of Flights Longer than", input$dist_cutoff,"miles")) +
        labs(color="Day of the Week") +
        scale_colour_brewer(palette="Spectral", direction=-1)
  })
  
}

# Run the app
shinyApp(ui, server)
