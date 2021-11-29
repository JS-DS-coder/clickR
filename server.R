#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(ggplot2)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

    output$grafica_base_r <- renderPlot({
  
        plot(mtcars$wt, mtcars$mpg,xlab='wt',ylab='Millas por galon')

    })
    
    allcars = reactiveValues(
      allcarsrows = rep(TRUE, nrow(mtcars))
    )
    
    carclick = reactiveValues(
      carclickrows2 = rep(TRUE, nrow(mtcars))
    )
    
    cardclick = reactiveValues(
      cardclickrows = rep(TRUE, nrow(mtcars))
    )
    
    carbrush = reactiveValues(
      carbrushrows = rep(TRUE, nrow(mtcars))
    )
    
   observeEvent(input$clk,{
      clk = nearPoints(mtcars, input$clk, allRows = TRUE, maxpoints = 1)
      clk_tb = nearPoints(mtcars, input$clk, maxpoints = 1)
      carclick$carclickrows2 = xor(carclick$carclickrows2, clk$selected_)
      df <- nearPoints(mtcars,input$clk,xvar='wt',yvar='mpg')
      output$mtcars_tbl = DT::renderDataTable({clk_tb})
      rbind(c(input$clk$x,input$clk$y),
            c(input$dclk$x,input$dclk$y),
            c(input$mouse_hover$x,input$mouse_hover$y),
            c(input$mouse_brush$xmin,input$mouse_brush$ymin),
            c(input$mouse_brush$xmax,input$mouse_brush$ymax))
    }) 
      
   
   observeEvent(input$dclk,{
     res_dclk = nearPoints(mtcars, input$dclk, allRows = TRUE, maxpoints = 1)
     cardclick$cardclickrows = xor(cardclick$cardclickrows, res_dclk$selected_)

   })
   
   observeEvent(input$mouse_brush,{
     res_brush = brushedPoints(mtcars, input$mouse_brush, allRows = TRUE)
     res_brush_tbl = brushedPoints(mtcars, input$mouse_brush)
     carbrush$carbrushrows = xor(carbrush$carbrushrows, res_brush$selected_)
     output$mtcars_tbl = DT::renderDataTable({clk_tb})
   })

    output$grafica_ggplot <- renderPlot({
      diamonds %>% 
      ggplot(aes(x=carat,y=price,color=color))+
        geom_point() +
        ylab('Precio')+
        xlab('Kilates')+
        ggtitle('Precio de diamnetes por kilate')
      
    })
    
    output$plot_click_options <- renderPlot({

      #plot(mtcars$wt, mtcars$mpg,xlab='wt',ylab='Millas por galon')
      all1 = mtcars[allcars$allcarsrows, , drop = FALSE]
      all2 = mtcars[!allcars$allcarsrows, , drop = FALSE]
      eclk = mtcars[!carclick$carclickrows2, , drop = FALSE]
      edclk = mtcars[!cardclick$cardclickrows, , drop = FALSE]
      ebrush = mtcars[!carbrush$carbrushrows, , drop = FALSE]
      
      ggplot(all1, aes(wt, mpg)) + geom_point(size = 2, color="black") + 
        geom_point(data = all2, shape = 18, color = "gray", fill="gray", size = 2)+
        geom_point(data = eclk, shape = 18, color = "green", fill="green", size = 2)+
        geom_point(data = edclk, shape = 18, color = "black", fill="black", size = 2)+
        geom_point(data = ebrush, shape = 18, color = "green", fill="green", size = 2)+
        theme_classic()
      
    })
    
 
    
    #output$click_data <- renderPrint({
   
    #  rbind(c(input$clk$x,input$clk$y),
    #  c(input$dclk$x,input$dclk$y),
    #  c(input$mouse_hover$x,input$mouse_hover$y),
    #  c(input$mouse_brush$xmin,input$mouse_brush$ymin),
    #  c(input$mouse_brush$xmax,input$mouse_brush$ymax))
      
        
    #  })
    
    output$mtcars_tbl = DT::renderDataTable({mtcars})
    #output$mtcars_tbl <- DT::renderDataTable({
    #  df <- nearPoints(mtcars,input$clk,xvar='wt',yvar='mpg')
      
 
      #df <- brushedPoints(mtcars,input$mouse_brush,xvar='wt',yvar='mpg')
      
   # })
    
})
