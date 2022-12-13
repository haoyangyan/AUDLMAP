library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(igraph)
library(dplyr)
library(networkD3)
# read the passing data
pass <- read_csv('pass1.csv')
uteam <- unique(pass$team)[order(unique(pass$team))]
uyear <- unique(pass$year)[order(unique(pass$year))]
uthrower <- unique(pass$thrower)[order(unique(pass$thrower))]

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

ui<-  navbarPage("AUDL tool",
   #### here is the first part
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

server <- function(input, output){
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
