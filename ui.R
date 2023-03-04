library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(5, 
           "higher carrying capacity = more rapid growth",
           numericInput("c", 
                        label = "Enter C carrying capacity value", 
                        min= 95, max= 1000, value = 500, step =10)), 
    column(5, 
           "high B = more quick growth",
           numericInput("b", 
                        label= "input b value", 
                        min=0.01, max=2, step =0.02, value= 0.66))
  ),
  fluidRow(
    "This is the log growth",
    verbatimTextOutput("yt"),
    "Reaches carrying capacity at time ",
    textOutput("time") #when will it reach C
  ),
  fluidRow(
    column(6, plotOutput("graph"))
  ),
  
  fluidRow(
    "What happens if Thanos snaps the population in half?",
    numericInput("ts", 
                 label="Time when Thanos snap happens", 
                 min= 3, max= 100, value = 12),
    verbatimTextOutput("new_yt"),
    "new time taken to reach carrying capacity",
    textOutput("new_time"),
    plotOutput("snap_graph")
  ),
  
  fluidRow(
    "So how much change did Thanos bring?",
    "Thanos delayed the inevitable reaching carrying capacity by time: ",
    textOutput("diff"),
    
  )
)