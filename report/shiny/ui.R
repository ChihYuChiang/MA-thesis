"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
library(shiny)

PLOT_WIDTH <- "600px" 








"
------------------------------------------------------------
Front end
------------------------------------------------------------
"
ui <- fluidPage(#--Header
                #CSS
                tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
                tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap-4.0.0-beta.2-dist/css/bootstrap.min.css")),
  
                #--Set up title
                #Tab title
                title='Fellow version supplement',
                
                #Displayed title
                titlePanel(HTML('<h2>Fellow version report supplement
                                  <span id="authorship">Chih-Yu Chiang • chihyuchiang@uchicago.edu</span>
                                 </h2>')),
                
                
                tabsetPanel(
                  tabPanel("Personality",
                           fluidRow(column(width=2,
                                           checkboxGroupInput("type_personality",
                                                              "",
                                                              c("In-game / Self"="InS",
                                                                "Real / Self"="OutS",
                                                                "Ideal / Self"="IdS",
                                                                "In-game / Fellow"="InF",
                                                                "Real / Fellow"="OutF",
                                                                "Stereotype / Public"="SteS")),
                                           actionButton("distButton_personality", "Draw distribution")),
                                    column(width=8,
                                           plotOutput("dist_personality_1", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_2", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_3", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_4", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_5", width=PLOT_WIDTH)
                                    )
                            )),
                  tabPanel("SDT",
                           fluidRow(column(width=2,
                                           checkboxGroupInput("type_SDT",
                                                              "",
                                                              c("In-game "="In",
                                                                "Real"="Out",
                                                                "Ideal"="Id")),
                                           actionButton("distButton_SDT", "Draw distribution")),
                                    column(width=8,
                                           plotOutput("dist_SDT_1", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_2", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_3", width=PLOT_WIDTH)
                                    )
                            ))
                )
)
