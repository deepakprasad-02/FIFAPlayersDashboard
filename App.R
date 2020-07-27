library(shinydashboard)
library(DataExplorer) # Missing Values
library(tidyverse) # Data Manipulation & Visualization 
library(magrittr) #  For %<>% operator 
library(stringr) # String Manipulation
library(GGally) # Data Visualizaton
library(gridExtra) # Data Visualization
library(ggrepel) # Data Visalization
library(psych) # For headTail function
library(highcharter)

list.files(path = "../input")
options(scipen = 999) # Scientific Format

#Loading the data to the fifa dataframe
fifa <- read.csv("fifa19.csv")
fifa <- fifa[-1]
head(fifa)

#Defining club names to filter in the fifa dataset
leagues <- list( "La Liga","EPL", "Bundesliga")
premier_league_clubs <- c("Arsenal","Bournemouth", "Burnley", "Chelsea", "Crystal Palace", "Everton", "Hull City", "Leicester City", "Liverpool", 
                          "Manchester City", "Manchester United", "Middlesbrough", "Southampton", "Stoke City", "Sunderland", "Swansea City", 
                          "Tottenham Hotspur", "Watford", "West Bromwich Albion", "West Ham United")

la_liga_clubs <- c("FC Barcelona","SD Eibar","RCD Espanyol","Getafe CF","Girona CF","UD Las Palmas","Levante UD","Real Madrid CF","Real Sociedad","Sevilla FC","Valencia CF")

bundesliga_clubs <- c("FC Bayern Munich","Borussia Dortmund", "FC Schalke 04", "FC Augsburg", "Hertha BSC Berlin", "SV Werder Bremen", "Eintracht Frankfurt", "SC Freiburg", "Hannover 96", 
                      "TSG 1899 Hoffenheim", "RB Leipzig", "Bayer 04 Leverkusen", "1. FSV Mainz 05", "Borussia MÃÂ¶nchengladbach", "VfB Stuttgart", "VfL Wolfsburg")

#Filtering the data with respect to clubs
premier_league <- fifa  %>% filter(Club %in% premier_league_clubs)

bundesliga<-fifa%>%filter(Club %in% bundesliga_clubs)

la_liga<-fifa%>%filter(Club %in% la_liga_clubs)

#Selecting only the required attributes for the analysis after filtering the clubs
premier_league  %<>% dplyr::select(Name, Club, Value, Age, Overall, Potential, Position)

bundesliga  %<>% dplyr::select(Name, Club, Value, Age, Overall, Potential, Position)

la_liga  %<>% dplyr::select(Name, Club, Value, Age, Overall, Potential, Position)

#Creating a new variable for postion preference of a player in all the leagues
premier_league  %<>% mutate(Pref.Pos = str_trim(
  str_sub(string = premier_league$Position, 
          start = str_length(premier_league$Position)-4, 
          end = str_length(premier_league$Position))))

bundesliga  %<>% mutate(Pref.Pos = str_trim(
  str_sub(string = bundesliga$Position, 
          start = str_length(bundesliga$Position)-4, 
          end = str_length(bundesliga$Position))))

la_liga  %<>% mutate(Pref.Pos = str_trim(
  str_sub(string = la_liga$Position, 
          start = str_length(la_liga$Position)-4, 
          end = str_length(la_liga$Position))))

# Deleting the observations in positions data set
premier_league$Position <- NULL
bundesliga$Position <- NULL
la_liga$Position <- NULL

#Validating the mutated variable
table(premier_league$Pref.Pos)
table(bundesliga$Pref.Pos)
table(la_liga$Pref.Pos)

#creating 4 new columnsin each of the league and the observations contain a flag(1/0)
premier_league  %<>% mutate(Goal_Keeper = if_else(Pref.Pos %in% "GK",1,0),
                            Defence = if_else(Pref.Pos %in% c("CB", "RB", "LB", "LWB", "RWB"), 1, 0),
                            Midfielder = if_else(Pref.Pos %in% c("CM", "CDM","CAM","LM","RM"), 1, 0),
                            Forward = if_else(Pref.Pos %in% c("ST", "CF", "LW","RW"), 1, 0)) %>% dplyr::select(-Pref.Pos)

bundesliga  %<>% mutate(Goal_Keeper = if_else(Pref.Pos %in% "GK",1,0),
                        Defence = if_else(Pref.Pos %in% c("CB", "RB", "LB", "LWB", "RWB"), 1, 0),
                        Midfielder = if_else(Pref.Pos %in% c("CM", "CDM","CAM","LM","RM"), 1, 0),
                        Forward = if_else(Pref.Pos %in% c("ST", "CF", "LW","RW"), 1, 0)) %>% dplyr::select(-Pref.Pos)

la_liga  %<>% mutate(Goal_Keeper = if_else(Pref.Pos %in% "GK",1,0),
                     Defence = if_else(Pref.Pos %in% c("CB", "RB", "LB", "LWB", "RWB"), 1, 0),
                     Midfielder = if_else(Pref.Pos %in% c("CM", "CDM","CAM","LM","RM"), 1, 0),
                     Forward = if_else(Pref.Pos %in% c("ST", "CF", "LW","RW"), 1, 0)) %>% dplyr::select(-Pref.Pos)

#Total statistics form the previous step for all the leagues
paste0("Number of Goal Keeper: ",table(premier_league$Goal_Keeper)[2])
paste0("Number of Defence: ",table(premier_league$Defence)[2])
paste0("Number of Midfielder :",table(premier_league$Midfielder)[2])
paste0("Number of Forward: ",table(premier_league$Forward)[2])
paste0("Total Players: ", nrow(premier_league))

paste0("Number of Goal Keeper: ",table(la_liga$Goal_Keeper)[2])
paste0("Number of Defence: ",table(la_liga$Defence)[2])
paste0("Number of Midfielder :",table(la_liga$Midfielder)[2])
paste0("Number of Forward: ",table(la_liga$Forward)[2])
paste0("Total Players: ", nrow(la_liga))

paste0("Number of Goal Keeper: ",table(bundesliga$Goal_Keeper)[2])
paste0("Number of Defence: ",table(bundesliga$Defence)[2])
paste0("Number of Midfielder :",table(bundesliga$Midfielder)[2])
paste0("Number of Forward: ",table(bundesliga$Forward)[2])
paste0("Total Players: ", nrow(bundesliga))


#Updating value to be shown in euro for the players
pl <- premier_league
pl$Value <- str_remove_all(pl$Value,"€")
pl$Value <- str_replace_all(pl$Value,"K", "000")
pl$Value <- str_remove_all(pl$Value,"M")

bl <- bundesliga
bl$Value <- str_remove_all(bl$Value,"€")
bl$Value <- str_replace_all(bl$Value,"K", "000")
bl$Value <- str_remove_all(bl$Value,"M")

ll <- la_liga
ll$Value <- str_remove_all(ll$Value,"€")
ll$Value <- str_replace_all(ll$Value,"K", "000")
ll$Value <- str_remove_all(ll$Value,"M")


#Converting the value obtained from previous step from string to numeric
pl$Value <- as.numeric(as.character(pl$Value))
pl <- pl  %>% mutate(Value = if_else(pl$Value < 1000 , Value * 1000000, Value))

bl$Value <- as.numeric(as.character(bl$Value))
bl <- bl  %>% mutate(Value = if_else(bl$Value < 1000 , Value * 1000000, Value))

ll$Value <- as.numeric(as.character(ll$Value))
ll <- ll  %>% mutate(Value = if_else(ll$Value < 1000 , Value * 1000000, Value))

#Creating a data frame containing frequency of players in different positions
position_freq_pl <- data.frame(Position = c("Goal Keeper", "Defence", "Midfielder", "Forward", "Total"),
                               Frequency = c(table(pl$Goal_Keeper)[2],table(premier_league$Defence)[2], table(pl$Midfielder)[2], table(pl$Forward)[2], nrow(pl)))
position_freq_bl <- data.frame(Position = c("Goal Keeper", "Defence", "Midfielder", "Forward", "Total"),
                               Frequency = c(table(bl$Goal_Keeper)[2],table(bundesliga$Defence)[2], table(bl$Midfielder)[2], table(bl$Forward)[2], nrow(bl)))
position_freq_ll <- data.frame(Position = c("Goal Keeper", "Defence", "Midfielder", "Forward", "Total"),
                               Frequency = c(table(ll$Goal_Keeper)[2],table(la_liga$Defence)[2], table(ll$Midfielder)[2], table(ll$Forward)[2], nrow(ll)))

#creating count of players by country for the map
countries_count <- count(fifa, Nationality)
countries_count$Nationality<-as.character(countries_count$Nationality)
countries_count$n<-as.numeric(countries_count$n)
df <- data.frame(countries_count$Nationality,countries_count$n)

#Shiny dashboard ui, server code below

ui <- dashboardPage(
                    skin = "black",
                    dashboardHeader(title = "FIFA 19",titleWidth = 230,dropdownMenu(
                      type = "notifications", 
                      icon = icon("question-circle"),
                      badgeStatus = NULL,
                      notificationItem("Leagues - league and player statistics", icon = icon("users")),
                      notificationItem("Map - shows No. of players by country", icon = icon("map")),
                      notificationItem("Data source", icon = icon("link"), href = "https://www.kaggle.com/karangadiya/fifa19")
                      )),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Leagues", tabName = "Leagues", icon = icon("dashboard")),
                        menuItem("Map", tabName = "Maps", icon = icon("th"))
                      )
                    ),
                    dashboardBody(
                      tags$head(tags$style(
                        HTML('.reset-this {
                              min-height: 0px;}
                              .content-wrapper .element.style {
                              background-color: #ffffff;
                              min-height: 1400px !important;
                             }'))),
                                  
                                  tabItems(
                                    # First tab content
                                    tabItem(
                                      tabName = "Leagues",
                                            selectInput("league", "Choose a League:",
                                                        leagues),
                                      fluidRow(
                                            box(plotOutput("plot1", height = 600)),
                                            box(plotOutput("plot2", height = 600)),
                                            box(plotOutput("result", height = 600)),
                                            box(plotOutput("plot3", height = 600))
                                      )
                                    ),
                                    
                                    # Second tab content
                                    tabItem(tabName = "Maps",
                                            fluidRow(box(highchartOutput("map",height = 800, width = 1200),height = 850, width = 1200))
                                    )
                                    
                                    
                                    
                                  )
                    ))

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  #Below code is generating the map 
  output$map<-renderHighchart({hcmap("https://code.highcharts.com/mapdata/custom/world-highres.js", data = df,
        name = "Number of Players", value = "countries_count.n", joinBy = c("name", "countries_count.Nationality"),
        borderColor = "transparent") %>%
    hc_title(text = "<b>World Map with player distribution</b>",
             margin = 20, align = "center",
             style = list(color = "black", useHTML = TRUE)) %>%
    hc_subtitle(text = "Map with number of players from each country",
                align = "center", style = list(color = "grey", fontWeight = "bold")) %>%
    hc_colorAxis(dataClasses = color_classes(c(seq(0, 200, by = 30), 1000))) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_legend(titletext="cdnsc",layout = "vertical", align = "LEFT",
              floating = TRUE, valueDecimals = 0) })
  #Based on the dropdown input by the user in map, league statistics would be changed based on the below if and else conditions
  output$result<-renderPlot(
    
    if(input$league=="EPL"){
      output$plot1 <- renderPlot({
        p1<-pl  %>% arrange(-Value) %>% head(20) %>% 
          ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
          geom_col(fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          theme_minimal()+
          labs(title = "Most Valuable Players",
               subtitle = "Plot of Players vs their value in Euros",
               x = "Players",
               y = "Values (in Euros)")
        
        p1+theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
      })
      
      output$plot2 <- renderPlot({
        grid.arrange(
          
          pl  %>% arrange(-Value) %>% filter(Goal_Keeper == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value/ 1000000 , "M")))+
            geom_col(fill = "peachpuff2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Goal Keepers",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          pl  %>% arrange(-Value) %>% filter(Defence == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "pink1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Defence Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          pl  %>% arrange(-Value) %>% filter(Midfielder == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "wheat2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Midfielder Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          pl  %>% arrange(-Value) %>% filter(Forward == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "salmon1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Forward Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold"))
        )
      })

      output$plot3 <- renderPlot({ggplot(position_freq_pl, aes(reorder(Position, Frequency), Frequency, fill = Position, label = Frequency))+
          geom_col(show.legend = FALSE,fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1,size=6, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          scale_fill_ordinal()+
          theme_minimal()+
          labs(x = "Position on field",
               subtitle = "Plot of number of players per position on field",
               y="Count of players",
               title = "Number of Players")+
          theme(
            plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
            plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
            axis.title.x = element_text(color="BLACK", size=14, face="bold"),
            axis.title.y = element_text(color="BLACK", size=14, face="bold"),
            axis.text.x = element_text(face = "bold", color = "BLACK", 
                                       size = 11),
            axis.text.y = element_text(face = "bold", color = "BLACK", 
                                       size = 11))
        })
      pl %>% group_by(Club) %>% summarise(Total.Value = sum(Value, na.rm = TRUE)) %>% 
        ggplot(aes(reorder(Club, Total.Value), Total.Value))+
        geom_col(fill = "tomato")+
        coord_flip()+
        theme_minimal()+
        labs(x = "Clubs",
             y= "Club Value")+labs(title = "Most Valuable Clubs in England",
                                  subtitle = "Plot of Clubs versus their value",
                                  x = "Clubs",
                                  y= "Club Value in Euros")+
        theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
      
    }
    else if(input$league=="Bundesliga"){
      output$plot1 <- renderPlot({
        b1<-bl  %>% arrange(-Value) %>% head(20) %>% 
          ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
          geom_col(fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          theme_minimal()+
          labs(title = "Most Valuable Players",
               subtitle = "Plot of Players vs their value in Euros",
               x = "Players",
               y = "Values (in Euros)")
        
        b1+theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
      })
      
      output$plot2 <- renderPlot({
        grid.arrange(
          
          bl  %>% arrange(-Value) %>% filter(Goal_Keeper == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "peachpuff2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Goal Keepers",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          bl  %>% arrange(-Value) %>% filter(Defence == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "pink1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Defence Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          bl  %>% arrange(-Value) %>% filter(Midfielder == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "wheat2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Midfielder Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          bl  %>% arrange(-Value) %>% filter(Forward == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "salmon1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Forward Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold"))
        )
      })
      
      output$plot3 <- renderPlot({ggplot(position_freq_bl, aes(reorder(Position, Frequency), Frequency, fill = Position, label = Frequency))+
          geom_col(show.legend = FALSE,fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1,size=6, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          theme_minimal()+
          labs(x = "Position on field",
               subtitle = "Plot of number of players per position on field",
               y="Count of players",
               title = "Number of Players")+
          theme(
                 plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
                 plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
                 axis.title.x = element_text(color="BLACK", size=14, face="bold"),
                 axis.title.y = element_text(color="BLACK", size=14, face="bold"),
                 axis.text.x = element_text(face = "bold", color = "BLACK", 
                                            size = 11),
                 axis.text.y = element_text(face = "bold", color = "BLACK", 
                                            size = 11)
                 
               )})
      
      bl %>% group_by(Club) %>% summarise(Total.Value = sum(Value, na.rm = TRUE)) %>% 
        ggplot(aes(reorder(Club, Total.Value), Total.Value))+
        geom_col(fill = "tomato")+
        coord_flip()+
        theme_minimal()+
        labs(title = "Most Valuable Clubs in Germany",
             subtitle = "Plot of Clubs versus their value",
             x = "Clubs",
             y= "Club Value in Euros")+
        theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
    }
    else{
      output$plot1 <- renderPlot({
        l1<-ll  %>% arrange(-Value) %>% head(20) %>% 
          ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
          geom_col(fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          theme_minimal()+
          labs(title = "Most Valuable Players",
               subtitle = "Plot of Players vs their value in Euros",
               x = "Players",
               y = "Values (in Euros)")
        
        l1+theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
      })
      
      output$plot2 <- renderPlot({
        grid.arrange(
          
          ll  %>% arrange(-Value) %>% filter(Goal_Keeper == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "peachpuff2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Goal Keepers",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          ll  %>% arrange(-Value) %>% filter(Defence == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "pink1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Defence Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          ll  %>% arrange(-Value) %>% filter(Midfielder == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "wheat2")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Midfielder Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold")),
          
          ll  %>% arrange(-Value) %>% filter(Forward == 1) %>% head(5) %>% 
            ggplot(aes(reorder(Name, Value), Value, label = paste0("€", Value / 1000000, "M")))+
            geom_col(fill = "salmon1")+
            geom_text_repel(ylim = 1, segment.color = "white", color = "black",fontface = "bold")+
            coord_flip()+
            theme_minimal()+
            labs(subtitle = "Most Valuable Forward Players",
                 x = "Players",
                 y = "Values (in Euros)")+
            theme(plot.subtitle = element_text(color="BLACK", size=13, face="bold",hjust = 0.5),
                  axis.title.x = element_text(color="BLACK", size=11,face="bold"),
                  axis.title.y = element_text(color="BLACK", size=13,face="bold"),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(color="BLACK", size=11,face="bold"))
        )
      })
      
      output$plot3 <- renderPlot({ggplot(position_freq_ll, aes(reorder(Position, Frequency), Frequency, fill = Position, label = Frequency))+
          geom_col(show.legend = FALSE,fill = "lightsteelblue2")+
          geom_text_repel(ylim = 1,size=6, segment.color = "BLACK", color = "BLACK",fontface = "bold")+
          coord_flip()+
          scale_fill_ordinal()+
          theme_minimal()+
          labs(x = "Position on field",
               subtitle = "Plot of number of players per position on field",
               y="Count of players",
               title = "Number of Players")+
          theme(
            plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
            plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
            axis.title.x = element_text(color="BLACK", size=14, face="bold"),
            axis.title.y = element_text(color="BLACK", size=14, face="bold"),
            axis.text.x = element_text(face = "bold", color = "BLACK", 
                                       size = 11),
            axis.text.y = element_text(face = "bold", color = "BLACK", 
                                       size = 11))
          
          
          })
      
      ll %>% group_by(Club) %>% summarise(Total.Value = sum(Value, na.rm = TRUE)) %>% 
        ggplot(aes(reorder(Club, Total.Value), Total.Value))+
        geom_col(fill = "tomato")+
        coord_flip()+
        theme_minimal()+
        labs(title = "Most Valuable Clubs in Spain",
             subtitle = "Plot of Clubs versus their value",
             x = "Clubs",
             y= "Club Value in Euros")+
        theme(
          plot.title = element_text(color="BLACK", size=20, face="bold",hjust = 0.5),
          plot.subtitle = element_text(color = "grey",size=13,hjust = 0.5,face="bold"),
          axis.title.x = element_text(color="BLACK", size=14, face="bold"),
          axis.title.y = element_text(color="BLACK", size=14, face="bold"),
          axis.text.x = element_text(face = "bold", color = "BLACK", 
                                     size = 11),
          axis.text.y = element_text(face = "bold", color = "BLACK", 
                                     size = 11)
          
        )
    }
    
  )
  
}

shinyApp(ui, server)
