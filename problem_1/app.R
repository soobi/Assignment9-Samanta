library(shiny)
library(babynames)
library(tidyverse)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1900, 2000), sep=""),
      textInput("nameInput", "Name", value = "Leslie"),
      radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results")
    )
  ),
  titlePanel("Baby Names")
)

server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = c("F","1"),
                      `Male only` = c("M","1"),
                      Both = c("F", "M","2")
    )
    babynames$year <- as.integer(babynames$year)
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })  
  output$main_plot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(year, n, colour = sex)) + 
      geom_line() + ggtitle(input$nameInput) + scale_color_manual(values = c("M" = "blue", "F" = "red"))
  })
  output$results <- renderTable({
    if(input$sexID == "Both"){
      spread(select(reduced_df(),-prop), key = sex, value = n, fill = "0")
    } else{
    select(reduced_df(),-prop)}
  })
}

shinyApp(ui = ui, server = server)



  
  
  

