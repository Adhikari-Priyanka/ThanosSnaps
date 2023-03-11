library(shiny)
library(shinydashboard)
#source("ui.R")
#source("server.R")

ui1 <- dashboardPage(
  dashboardHeader(title = "Did Thanos have any impact on population growth?"),
  ui_sidebar, 
  ui_body
)

ui_sidebar <- dashboardSidebar(
  
  #CSS style sidebar properties
  tags$style(" {positive: fixed;
             top: 330px; 
             left:330px;
             margin-top: 40px;
             border: 3px solid red}"),
  
  "wor22",
  
  numericInput("c", 
             label = "Enter C carrying capacity value", 
             min= 95, max= 1000, value = 500, step =10),
  numericInput("b", 
           label= "input b value", 
           min=0.01, max=2, step =0.02, value= 0.66),
  actionButton("go", "Plot this!"),
  actionButton("clear", "Reset"),
  
  
  h3("What if Thanos snaps at time = "), 
  numericInput("ts", 
             label="Time when Thanos snap happens", 
             min= 3, max= 1000, value = 12),
  actionButton("new_go", "Plot this!"),
  actionButton("new_clear", "Reset"),
   
  h3("So how much change did Thanos bring?"),
  "Thanos delayed the inevitable reaching carrying capacity by time: ",
  textOutput("diff"),
  
  #sidebar properties
  collapsed = FALSE
)

ui_body <- dashboardBody(
  "This is the log growth",
  #verbatimTextOutput("yt"),
  "Reaches carrying capacity at time ",
  textOutput("time"), #when will it reach C
  plotOutput("graph"),
  
  "What happens if Thanos snaps the population in half?",
  #verbatimTextOutput("new_yt"),
  "new time taken to reach carrying capacity",
  textOutput("new_time"),
  plotOutput("snap_graph")
)

server1 <- function(input, output, session){
  gr<-1
  
  #normal log growth
  yt_ser <- eventReactive(input$go,
    yt_val(c_val = input$c, b_val = input$b)
    )
  #output$yt <- renderPrint(yt_ser())
  output$time <- renderText( length(yt_ser())) #Time taken to reach carrying capacity is length of vector containing yt values
  output$graph <- renderPlot( nice_plot(yt_ser())+
                                labs(title= "Logarithmic growth curve"), 
                              res=96 )
  
  #Thanos snap
  new_yt_ser <- eventReactive(input$new_go,
    thanos(x1= yt_ser(), ts=input$ts)
    )
  #output$new_yt <- renderPrint(new_yt_ser())
  output$new_time <- renderText( length(new_yt_ser()))
  output$snap_graph <- renderPlot( nice_plot(new_yt_ser())+
                                     labs(title= "Post Thanos's snap Logarithmic growth curve"),
                                   res=96 )
  
  #Did it make a difference?
  output$diff <- renderText(length(new_yt_ser()) - length(yt_ser()))
  
  #reset clear and new_clear
  observeEvent(input$clear, {
    updateNumericInput(session, "c", value = 500)
    updateNumericInput(session, "b", value= 0.66)
    updateNumericInput(session, "ts", value= 12)

  })
  
  observeEvent(input$new_clear, {
    updateNumericInput(session, "ts", value = 12)
  })
  
}


shinyApp(ui1, server1)

