library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(igraph)
library(dplyr)
library(networkD3)

# setwd('C:/Users/yanhy/Desktop/3rd/AUDL/project/AUDLshiny')
# read the passing data
pass <- read_csv('pass1.csv')
uteam <- unique(pass$team)[order(unique(pass$team))]
uyear <- unique(pass$year)[order(unique(pass$year))]
uthrower <- unique(pass$thrower)[order(unique(pass$thrower))]

#################################
#### filed map
#################################

# create a blank field map of mesh, step of 5 yard on both x axis and y axis
yy <- c()
for (i in seq(0,115,5)){
  yy <- c(yy,rep(i,11))
}
blank <- data.frame(X = rep(seq(0,50,5),24), Y = yy)
blank <- unite(blank,'xy',X,Y, remove = FALSE)
blank <- blank[order(blank$xy),]

########### EPO map ##############
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


########## count map ##########
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


########### rate map ################
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


########## epo change map ############### 
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
### passing network
#######################################
# data preprocessing
# count the times of receiving or throwing the ball
data <- pass %>%
  group_by(receiver, thrower, team, year) %>%
  filter(receiver != '0') %>%
  summarise(count = n()) %>%
  arrange(year, team, desc(count)) # sort by year, and then team, and then count

# combine the same pair of players link, (a, b) and (b, a) are considered the same
data1 <- data
data1$pair_col <- paste(data$receiver, data$thrower)

for (i in 1:nrow(data1)){
  temp = strsplit(data1$pair_col[i], " ")
  if (temp[[1]][1] > temp[[1]][2]){
    data1$pair_col[i] <- paste(temp[[1]][2], temp[[1]][1], collapse = ' ')
  }
}



igraph_generate <- function(data_subset){
  #MyClickScript <- 
  # '      d3.select(this).select("circle").transition()
  #.duration(750)
  #.attr("r", 20)'
  link_data <- data_subset
  
  name_list <- unique(link_data$source)
  name_list_2 <- unique(link_data$target)
  concat_data <- c(name_list, name_list_2)
  edge_data <- data.frame(unique(concat_data), col = '1')
  colnames(edge_data) <- c('name', 'group')
  
  mean_value <- mean(link_data$value)
  mean_sd <- sd(link_data$value)
  
  for (i in 1: nrow(link_data)){
    link_data[i, "source"] <- as.numeric(which(edge_data$name == link_data[i, "source"])) - 1
    link_data[i, "target"] <- as.numeric(which(edge_data$name == link_data[i, "target"])) - 1
    link_data[i, "value"] <- ((as.numeric(link_data[i, "value"]) - mean_value)/ mean_sd + 1) * 10
  }
  
  forceNetwork(Links = link_data, 
               Nodes = edge_data, 
               Source = "source",
               Target = "target", 
               Value = "value", 
               NodeID = "name",
               Group = "group",
               linkDistance = 200,
               opacity = 1, 
               zoom = T, 
               bounded = T,
               fontSize = 15)
  #clickAction = MyClickScript)
}

#######################################
### preferencemap
#######################################

prefer_map <- function(tex_tc){
  tex <- tex_tc[[1]]
  tc <- tex_tc[[2]]
  blank_g <- data.frame(inn_n=c('z1','z2'),mid_n=c('z3','z4','z5','z6'),outer_n=c('z7','z8'),
                        inn=c(3/4,1/4,NA,NA),mid=c(3/8,3/8,1/8,1/8),outer=c(3/4,1/4,NA,NA))
  ggplot(blank_g) + 
    coord_polar("y", start=pi/4) +
    geom_bar(aes(x=8, y=rev(outer),fill=outer_n), width=4,stat='identity', color="white", show.legend=F) + 
    geom_bar(aes(x=3.5,y=rev(mid),fill=mid_n), width=5,stat='identity', color="white", show.legend=F) +
    geom_bar(aes(x=0,y=rev(inn),fill=inn_n), width=2,stat='identity', color="white", show.legend=F) +
    scale_fill_manual(values = tc) +
    geom_text(aes(x=1, y=pi/40, label='5-yard'), color = "black", size=3) + 
    geom_text(aes(x=6, y=pi/40, label='30-yard'), color = "black", size=3) + 
    geom_text(aes(x=0.3, y=0.875, label=tex[1]), color = "black", size=4) + 
    geom_text(aes(x=0.2, y=0.375, label=tex[2]), color = "black", size=4) + 
    geom_text(aes(x=3, y=0.94, label=tex[3]), color = "black", size=4) + 
    geom_text(aes(x=3, y=0.81, label=tex[4]), color = "black", size=4) + 
    geom_text(aes(x=3, y=0.575, label=tex[5]), color = "black", size=4) + 
    geom_text(aes(x=3, y=0.175, label=tex[6]), color = "black", size=4) + 
    geom_text(aes(x=7.5, y=0.875, label=tex[7]), color = "black", size=4) + 
    geom_text(aes(x=7.5, y=0.375, label=tex[8]), color = "black", size=4) +
    theme_void()
}


##############
audl_avg <- pass %>%
  group_by(zoneid) %>%
  summarise(attempt=n(),comple=sum(comp)) %>%
  mutate(ratio=attempt/sum(attempt)) %>%
  mutate(c_rate=comple/attempt)

prefer_info <- function(year_i,team_i,player_i){
  sel_data <- pass
  if (year_i!='all'){
    sel_data <- filter(sel_data,year %in% year_i)
  }
  if (team_i!='all'){
    sel_data <- filter(sel_data,team %in% team_i)
  }
  if (player_i!=''){
    sel_data <- filter(sel_data,thrower %in% player_i)
  }
  
  info1 <- sel_data %>%
    group_by(zoneid) %>%
    summarise(attempt=n(),comple=sum(comp)) %>%
    mutate(ratio=attempt/sum(attempt)) %>%
    mutate(c_rate=comple/attempt)
  empty1 <- data.frame(zoneid=c(seq(1,8)))
  info1 <- merge(info1,empty1,by.x='zoneid',by.y='zoneid',all=TRUE)
  info1
}


textcinfo1 <- function(p1){
  tex1 <- c()
  for (i in seq(1,8)){
    tex1[i] <- as.character(p1[i,2])
  }
  tex1
  
  tc1<-c('z1'='grey','z2'='grey','z3'='grey','z4'='grey','z5'='grey','z6'='grey','z7'='grey','z8'='grey')
  for (i in seq(1,8)){
    zi <- paste('z',as.character(i),sep='')
    if (is.na(p1[i,4])){
      colori <- 'grey'
    } else{
      if (p1[i,4]<(0.7*audl_avg[i,4])){
        colori <- 'blue'
      }else if (p1[i,4]<(0.9*audl_avg[i,4])){
        colori <- 'green'
      }else if (p1[i,4]>(1.3*audl_avg[i,4])){
        colori <- 'red'
      }else if (p1[i,4]>(1.1*audl_avg[i,4])){
        colori <- 'orange'
      }else{
        colori <- 'yellow'
      }
    }
    tc1[i] <- (zi=colori)
  }
  list(tex1,tc1)
}

textcinfo2 <- function(p1){
  tex2 <- c()
  for (i in seq(1,8)){
    tex2[i] <- paste(as.character(round(p1[i,5]*100,1)),'%',sep='')
  }
  
  tc2<-c('z1'='grey','z2'='grey','z3'='grey','z4'='grey','z5'='grey','z6'='grey','z7'='grey','z8'='grey')
  for (i in seq(1,8)){
    zi <- paste('z',as.character(i),sep='')
    if (is.na(p1[i,5])){
      colori <- 'grey'
    } else{
      if (p1[i,5]<(0.96*audl_avg[i,5])){
        colori <- 'blue'
      }else if (p1[i,5]<(0.99*audl_avg[i,5])){
        colori <- 'green'
      }else if (p1[i,5]>(1.04*audl_avg[i,5])){
        colori <- 'red'
      }else if (p1[i,5]>(1.01*audl_avg[i,5])){
        colori <- 'orange'
      }else{
        colori <- 'yellow'
      }
    }
    tc2[i] <- (zi=colori)
  }
  list(tex2,tc2)
}




########################################
### shiny app
########################################
ui <-
  navbarPage("AUDL tool",
             
  #### here is the first part ####
  
  tabPanel("Field map",
    fluidPage(
      titlePanel("ultimap"),
      selectInput("mteam", "Team", c('all',uteam, selected = 'all'), multiple = FALSE),
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
      )
    )
  ),
  
  #### here is the second part ####
  
  tabPanel("Passing Network",
     fluidPage(
       titlePanel("Link analysis of the best players"),
       selectInput("team", "Team", uteam),
       selectInput("year", "Year", uyear),
       sliderInput("nedge", "n-edge", 5,30,10),
       forceNetworkOutput("plot"),
       dataTableOutput("table")
     )
  ),
  
  #### here is the third part ####
  
  tabPanel("Throwing Map",
           fluidPage(
             selectInput("pteam", "Team", c('all',uteam), selected = 'all', multiple = FALSE),
             selectInput("pyear", "Year", c('all',uyear), selected = 'all', multiple = FALSE),
             textInput("pthrower", "Thrower"),
             'player list can be found below',
             titlePanel("Attempt Count/Rompeletion rate"),
             fluidRow(
               column(width = 4,
                      plotOutput("plot5", height = "500px", width = "500px")),
               column(width = 4,
                      plotOutput("plot6", height = "500px", width = "500px"))
             ),
             'color of left graph means the <ratio of attempts this area to all attempts> compared to the league average-----',
             '-----color of right graph means the <completion rate this area> compared to the league average',
             tableOutput("table3"),
             dataTableOutput("table2")
           )
  )
)


server <- function(input, output) {
  
  #### here is the first part ####
  
  pcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    pcount(coun(pass,input$mteam,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax))
  )
  
  rcount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    rcount(compr(pass,input$mteam,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax))
  )
  
  eccount <- reactiveVal(rep(0,264))
  observeEvent(
    input$plot1_brush,
    eccount(cepo(pass,input$mteam,input$plot1_brush$xmin,input$plot1_brush$xmax,input$plot1_brush$ymin,input$plot1_brush$ymax, changemap(pass,input$team)))
  )
  
  output$plot1 = renderPlot(plote(blank, epo(pass,input$mteam)))
  output$plot2 = renderPlot(plotec(blank, eccount()))
  output$plot3 = renderPlot(plotc(blank, pcount()))
  output$plot4 = renderPlot(plotr(blank, rcount()))
  
  #### here is the second part ####
  
  data_subset <- reactive({
    data2 <- data1 %>%
      group_by(team, year, pair_col) %>%
      summarise(sum_count = sum(count)) %>%
      arrange(team, year, desc(sum_count)) %>%
      slice_max(order_by = sum_count, n = input$nedge) %>%
      separate(pair_col, c('from', 'to'))
    colnames(data2) <- c('team', 'year', 'source', 'target', 'value')
    data2 %>%
      filter(team == input$team, year == input$year) %>%
      ungroup() %>%  
      select(source, target, value) %>%
      as.data.frame()
  })
  
  output$plot <- renderForceNetwork({
    igraph_generate(data_subset())
  })
  
  output$table <- renderDataTable(data_subset())
  
  #### here is the third part #####
  
  output$plot5 = renderPlot(prefer_map(textcinfo1(prefer_info(input$pyear,input$pteam,input$pthrower))))
  output$plot6 = renderPlot(prefer_map(textcinfo2(prefer_info(input$pyear,input$pteam,input$pthrower))))
  output$table3 = renderTable(data.frame(attempt_ratio=c('> +30%','+10% ~ +30%','-10% ~ +10%','-30% ~ -10%','< -30%'),
                                         color=c('red','orange','yellow','green','blue'),
                                         completion_rate=c('> +4%','+1% ~ +4%','-1% ~ +1%','-4% ~ -1%','< -4%')))
  output$table2 = renderDataTable({
    filt_p <- pass
    if (input$pyear!='all'){
      filt_p <- filter(filt_p,year %in% input$pyear)
    }
    if (input$pteam!='all'){
      filt_p <- filter(filt_p,team %in% input$pteam)
    }
    filt_p <- data.frame(player=unique(filt_p$thrower)[order(unique(filt_p$thrower))])
    filt_p
  })
}

shinyApp(ui, server)