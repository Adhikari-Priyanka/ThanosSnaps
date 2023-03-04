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

