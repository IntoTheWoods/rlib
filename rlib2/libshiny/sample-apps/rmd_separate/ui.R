library(shiny)

shinyUI(fluidPage(
    selectInput("region", "Region:", 
                choices = colnames(WorldPhones)),
    plotOutput("phonePlot", height=270)
))

