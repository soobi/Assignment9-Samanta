library(shiny)
library(gapminder)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1952, max = 2007, value = c(1975, 2000), sep=""),
      selectInput("countryInput","Country:", choices = gapminder$country)),
      
    mainPanel(
      plotOutput("main_plot")
    )
  ),
  titlePanel("Logged GDP vs Life Expectancy")
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
           aes(log10(gdpPercap), lifeExp)) + geom_point() + geom_smooth()
  })
}

shinyApp(ui = ui, server = server)


