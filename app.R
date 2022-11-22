library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(igraph)
library(dplyr)
library(networkD3)

#################################
# read the passing data
pass <- read_csv('pass1.csv')

# create a blank field map of mesh, step of 5 yard on both x axis and y axis
yy <- c()
for (i in seq(0,115,5)){
  yy <- c(yy,rep(i,11))
}
blank <- data.frame(X = rep(seq(0,50,5),24), Y = yy)
blank <- unite(blank,'xy',X,Y, remove = FALSE)
blank <- blank[order(blank$xy),]


#################################
# the plot function for the Expected Point Outcome
plote <- function(data,h){
  data %>%
    mutate(z=h) %>%
    ggplot() +
    geom_tile(aes(x = X,y = Y,fill = z)) +
    ggtitle('the Expected point Outcome') +
    geom_hline(yintercept = 100) +
    geom_hline(yintercept = 20) +
    scale_fill_distiller(palette = 'Spectral',
                         direction = -1,
                         name = "rate")
}

# compute the map of the Expected Point Outcome
epo <- function(data,t){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  temp <- data %>%
    unite('xyid', TXid, TYid) %>%
    group_by(xyid) %>%
    summarise(expp = sum(scoreinpo)/n())
  temp2 <- merge(blank, temp, by.x='xy', by.y='xyid', all=TRUE)
  temp2 <- temp2[order(temp2$xy),]
  temp2$expp
}

####################################
# the plot function for the change of Expected Point Outcome
plotec <- function(data,h){
  data %>%
    mutate(z=h) %>%
    ggplot() +
    geom_tile(aes(x = X,y = Y,fill = z)) +
    ggtitle('the Change of Expected point Outcome') +
    geom_hline(yintercept = 100) +
    geom_hline(yintercept = 20) +
    scale_fill_distiller(palette = 'Spectral',
                         direction = -1,
                         name = "rate")
}


###################################
# the plot function for completion count
plotc <- function(data,h){
  data %>%
    mutate(z=h) %>%
  ggplot() +
    geom_tile(aes(x = X,y = Y,fill = z)) +
    ggtitle('where disc go next') +
    geom_hline(yintercept = 100) +
    geom_hline(yintercept = 20) +
    scale_fill_distiller(palette = 'Spectral',
                         direction = -1,
                         name = "count")
}

# the plot function for completion rate
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

##########################################
# given the team chose and the interactive brush, compute the count of "where disc go next" from the chosen area
coun <- function(data,t,cxmin,cxmax,cymin,cymax){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  xminid <- floor(cxmin/5)*5
  xmaxid <- floor(cxmax/5)*5
  yminid <- floor(cymin/5)*5
  ymaxid <- floor(cymax/5)*5
  temp <- data %>%
    filter(xminid <= TXid, TXid <= xmaxid) %>%
    filter(yminid <= TYid, TYid <= ymaxid) %>%
    unite('xyid', RXid, RYid) %>%
    group_by(xyid) %>%
    summarise(count1 = n())
  temp2 <- merge(blank, temp, by.x='xy', by.y='xyid', all=TRUE)
  temp2 <- temp2[order(temp2$xy),]
  temp2$count1
}

# given the team chose and the interactive brush, compute the completion rate from the chosen area 
compr <- function(data,t,cxmin,cxmax,cymin,cymax){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  xminid <- floor(cxmin/5)*5
  xmaxid <- floor(cxmax/5)*5
  yminid <- floor(cymin/5)*5
  ymaxid <- floor(cymax/5)*5
  temp <- data %>%
    filter(xminid <= TXid, TXid <= xmaxid) %>%
    filter(yminid <= TYid, TYid <= ymaxid) %>%
    unite('xyid', RXid, RYid) %>%
    group_by(xyid) %>%
    summarise(rate1 = sum(comp)/n())
  temp2 <- merge(blank, temp, by.x='xy', by.y='xyid', all=TRUE)
  temp2 <- temp2[order(temp2$xy),]
  temp2$rate1
}

# given the team chose and the interactive brush, compute the change of Expected Point Outcome
changemap <- function(data, t){
epot <- blank %>% 
  mutate(epoteam = epo(data,t))
epot['Xi'] = 50 - epot['X']
epot['Yi'] = pmin(unlist(115 - epot['Y']), rep(95,264))
epot <- unite(epot,'xyi',Xi,Yi, remove = FALSE)
epot <- merge(epot, epot, by.x='xyi', by.y='xy', all=FALSE)
epot['epoi'] = 1-epot["epoteam.y"]
epot <- epot[c("xy","epoteam.x","epoi")]
epot
}

cepo <- function(data,t,cxmin,cxmax,cymin,cymax,epot){
  if (t != 'all'){
    data <- data %>%
      filter(team %in% t)
  }
  xminid <- floor(cxmin/5)*5
  xmaxid <- floor(cxmax/5)*5
  yminid <- floor(cymin/5)*5
  ymaxid <- floor(cymax/5)*5
  temp <- data %>%
    filter(xminid <= TXid, TXid <= xmaxid) %>%
    filter(yminid <= TYid, TYid <= ymaxid) %>%
    unite('Rxyid', RXid, RYid) %>%
    unite('Txyid', TXid, TYid)
  temp1 <- merge(temp, epot, by.x='Txyid', by.y='xy')
  temp2 <- merge(temp1, epot, by.x='Rxyid', by.y='xy')
  tempc <- temp2 %>%
    filter(comp %in% 1)
  sumc <- tempc %>%
    group_by(Rxyid) %>%
    summarise(epoc = sum(epoteam.x.y)-sum(epoteam.x.x), n=n())
  tempn <- temp2 %>%
    filter(comp %in% 0)
  sumn <- tempn %>%
    group_by(Rxyid) %>%
    summarise(epon = sum(epoi.y)-sum(epoteam.x.x), n=n())
  summ <- merge(sumc,sumn,by.x='Rxyid',by.y='Rxyid', all = TRUE)
  summ[is.na(summ)]<-0 
  summ['epochange'] <- (summ['epoc']+summ['epon'])/(summ['n.x']+summ['n.y'])
  
  temp3 <- merge(blank, summ, by.x='xy', by.y='Rxyid', all=TRUE)
  temp4 <- temp3[order(temp3$xy),]
  temp4$epochange
}



#######################################
# data preprocessing
# count the times of receiving or throwing the ball
data <- read_csv("pass1.csv") %>%
  group_by(receiver, thrower, team, year) %>%
  filter(receiver != '0') %>%
  summarise(count = n()) %>%
  arrange(year, team, desc(count)) # sort by year, and then team, and then count
data

# combine the same pair of players link, (a, b) and (b, a) are considered the same
data1 <- data
data1$pair_col <- paste(data$receiver, data$thrower)

for (i in 1:nrow(data1)){
  temp = strsplit(data1$pair_col[i], " ")
  if (temp[[1]][1] > temp[[1]][2]){
    data1$pair_col[i] <- paste(temp[[1]][2], temp[[1]][1], collapse = ' ')
  }
}

data2 <- data1 %>%
  group_by(team, year, pair_col) %>%
  summarise(sum_count = sum(count)) %>%
  arrange(team, year, desc(sum_count)) %>%
  slice_max(order_by = sum_count, n = 10) %>%
  separate(pair_col, c('from', 'to'))
data2


teams <- pull(data2, team) %>%
  unique() %>%
  na.omit()

years <- pull(data2, year) %>%
  unique() %>%
  na.omit()

igraph_generate <- function(test_data){
  simpleNetwork(test_data, height = "100px", width="100px",
                Source = 1,
                Target = 2,
                linkDistance = 10,
                charge = -900,
                fontSize = 14,
                fontFamily = "serif",
                linkColour = "#666",
                nodeColour = "#69b3a2",
                opacity = 0.9,
                zoom = T)
}


########################################
# Here is the shiny app
ui <- fluidPage(
  titlePanel("ultimap"),
  selectInput("team", "team", c('all',unique(pass$team)), selected = 'all', multiple = FALSE),
  fluidRow(
    column(width = 4,
    plotOutput("plot1", height = "540px", width = "400px", brush = 'plot1_brush')),
    column(width = 4,
    plotOutput("plot2", height = "540px", width = "400px"))
  ),
  fluidRow(
    column(width = 4,
           plotOutput("plot3", height = "540px", width = "400px")),
    column(width = 4,
           plotOutput("plot4", height = "540px", width = "400px"))
  ),
  titlePanel("Link analysis of the best players"),
  selectInput("team1", "Team", teams),
  selectInput("year1", "Year", years),
  simpleNetworkOutput("plot"),
  dataTableOutput("table")
)

server <- function(input, output) {
  
  pcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    pcount(coun(pass,input$team,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax))
  )
  
  rcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    rcount(compr(pass,input$team,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax))
  )
  
  eccount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    eccount(cepo(pass,input$team,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax, changemap(pass,input$team)))
  )
  
  output$plot1 = renderPlot(plote(blank, epo(pass,input$team)))
  output$plot2 = renderPlot(plotec(blank, eccount()))
  output$plot3 = renderPlot(plotc(blank, pcount()))
  output$plot4 = renderPlot(plotr(blank, rcount()))
  ####
  data_subset <- reactive({
    data2 %>%
      filter(team == input$team1, year == input$year1) %>%
      ungroup() %>%  
      select(from, to)
  })
  data_subset_c <- reactive({
    data2 %>%
      filter(team == input$team1, year == input$year1) %>%
      ungroup() %>%  
      select(player1 = from, player2 = to, count = sum_count)
  })
  
  output$plot <- renderSimpleNetwork({
    igraph_generate(data_subset())
  })
  
  output$table <- renderDataTable(data_subset_c())
}

shinyApp(ui, server)