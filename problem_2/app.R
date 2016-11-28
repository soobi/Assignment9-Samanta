library(shiny)
library(gapminder)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Logged GDP vs Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1952, max = 2007, value = c(1952, 2007), sep=""),
      selectInput("countryInput","Country:", choices = gapminder$country)),
      
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

server <- function(input, output) {
  reduced_df <- reactive({
    filter(
      gapminder, 
      country == input$countryInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2]
    )
  })
  
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(log10(gdpPercap), lifeExp, color =year)) + 
      geom_point() + geom_smooth() +
      theme(plot.title = element_text(size = 15,face = "bold")) +
      labs(x = "Logged per capita GDP", y = "Life Expectancy", title = input$countryInput) 
  })
}

shinyApp(ui = ui, server = server)


