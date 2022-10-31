library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
pass <- read_csv('pass2.csv')

yy <- c()
for (i in seq(0,115,5)){
  yy <- c(yy,rep(i,11))
}
blank <- data.frame(X = rep(seq(0,50,5),24), Y = yy)
blank <- unite(blank,'xy',X,Y, remove = FALSE)
blank <- blank[order(blank$xy),]

plotc <- function(data,h){
  data %>%
    mutate(z=h) %>%
  ggplot() +
    geom_tile(aes(x = X,y = Y,fill = z)) +
    ggtitle('where dics go next') +
    geom_hline(yintercept = 100) +
    geom_hline(yintercept = 20) +
    scale_fill_distiller(palette = 'Spectral',
                         direction = -1,
                         name = "count")
}

plotr <- function(data,h){
  data %>%
    mutate(z=h) %>%
    ggplot() +
    geom_tile(aes(x = X,y = Y,fill = z)) +
    ggtitle('the completion rate') +
  geom_hline(yintercept = 100) +
    geom_hline(yintercept = 20) +
    scale_fill_distiller(palette = 'Spectral',
                         direction = -1,
                         name = "rate")
}

coun <- function(data,cx,cy,t){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  xid <- floor(cx/5)*5
  yid <- floor(cy/5)*5
  temp <- data %>%
    filter(TXid %in% xid) %>%
    filter(TYid %in% yid) %>%
    unite('xyid', RXid, RYid) %>%
    group_by(xyid) %>%
    summarise(count1 = n())
  temp2 <- merge(blank, temp, by.x='xy', by.y='xyid', all=TRUE)
  temp2 <- temp2[order(temp2$xy),]
  temp2$count1
}

compr <-function(data,cx,cy,t){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  xid <- floor(cx/5)*5
  yid <- floor(cy/5)*5
  temp <- data %>%
    filter(TXid %in% xid) %>%
    filter(TYid %in% yid) %>%
    unite('xyid', RXid, RYid) %>%
    group_by(xyid) %>%
    summarise(rate1 = sum(comp)/n())
  temp2 <- merge(blank, temp, by.x='xy', by.y='xyid', all=TRUE)
  temp2 <- temp2[order(temp2$xy),]
  temp2$rate1
}


ui <- fluidPage(
  titlePanel("ultimap"),
  selectInput("team", "team", c('all',unique(pass$team)), selected = 'all', multiple = FALSE),
  fluidRow(
    column(width = 4,
    plotOutput("plot1", height = "540px", width = "400px", click = 'plot1_click')),
    column(width = 4,
    plotOutput("plot2", height = "540px", width = "400px"))
  )
)

server <- function(input, output) {
  
  pcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_click,
    pcount(coun(pass,input$plot1_click$x,input$plot1_click$y,input$team))
  )
  
  rcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_click,
    rcount(compr(pass,input$plot1_click$x,input$plot1_click$y,input$team))
  )
  
  output$plot1 = renderPlot(plotc(blank, pcount()))
  output$plot2 = renderPlot(plotr(blank, rcount()))
}

shinyApp(ui, server)