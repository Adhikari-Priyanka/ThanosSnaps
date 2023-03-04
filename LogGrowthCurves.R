library(shiny)
library(ggplot2)

#Function to generate vector with yt values
yt_val <- function(c_val ,b_val){
  t=1 #initialize time counter
  a=c_val-1 #calculate a value
  l <- c() #initial vector to contain yt values
  yt=1
  while ( round(yt, 0) <c_val){ 
    yt <- c_val/ (1+ a* exp(-1*b_val*t)) #Calculates yt value at time = t
    l<- c(l, yt) #adding that to vector of all yt values
    t<- t+1 #increment time counter
  }
  return(l) #return vector containing yt values
}

#Function to put a growth curve series together after Thanos snap
thanos <- function(x1, ts){
  x2 <- x1[1:ts] #growth before snap
  n <- x1[ts]/2 #population after snap
  x3 <- x1[ which(x1>n)] #growth after snap
  x4 <- c(x2, x3)
  return(x4)
}

#Function to make a nice plot
nice_plot <- function(x1){
  pl <- data.frame("Year" =1:length(x1), "Population" = x1)
  ggplot(pl, aes(x= Year, y=Population))+
    geom_line(color="green")+
    geom_point(color="purple")+
    labs(x= "Time", y="Population count")+
    theme_light()
}

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


shinyApp(ui, server)

