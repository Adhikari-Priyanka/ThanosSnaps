library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  
  yt_ser <- reactive(yt_val(c_val = input$c, b_val = input$b))
  output$yt <- renderPrint(yt_ser())
  output$time <- renderText( length(yt_ser())) #Time taken to reach carrying capacity is length of vector containing yt values
  output$graph <- renderPlot( nice_plot(yt_ser())+
                                labs(title= "Logarithmic growth curve"), 
                              res=96 )
  
  #Thanos snap
  new_yt_ser <- reactive(thanos(x1= yt_ser(), ts=input$ts))
  output$new_yt <- renderPrint(new_yt_ser())
  output$new_time <- renderText( length(new_yt_ser()))
  output$snap_graph <- renderPlot( nice_plot(new_yt_ser())+
                                     labs(title= "Post Thanos's snap Logarithmic growth curve"),
                                   res=96 )
  
  #Did it make a difference?
  output$diff <- renderText(length(new_yt_ser()) - length(yt_ser()))
  
  observeEvent(input$c, "c is working")
  observeEvent(yt_ser(),"yt series works")
  
}
