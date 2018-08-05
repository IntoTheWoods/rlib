library(shiny)
library(DT)

shinyUI(fluidPage(
    titlePanel("product milieu"),

    sidebarLayout(
        sidebarPanel("Place for other criteria"
        ),
        mainPanel("My table",
                  dataTableOutput("gapminder_table")
        )
    )
))

