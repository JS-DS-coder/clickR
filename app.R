library (shiny)
library(dplyr)

shinyServer(function(input,output) {
 
  output$tabla_1 <- DT::renderDataTable({
    mtcars %>% DT::datatable(rownames = FALSE)
    
    
    
  }) 
  
})

