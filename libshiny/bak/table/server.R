library(shiny)
library(DT)

data_test = data.frame(ID = c ("1","2","3","4","5"),
                       product = c("A","B","C","A","C"),
                       milieu = c("good","medium","bad","medium","bad"),
                       online = c(1,0,1,1,0),
                       ooh = c(0,1,0,1,1),
                       event = c(1,1,0,0,0))

shinyServer(function(input, output) {

    output$gapminder_table <- renderDataTable({ 
        data_test        
    },
    filter = 'top',
    rownames = FALSE)    
})

