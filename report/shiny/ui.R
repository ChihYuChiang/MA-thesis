"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
library(shiny)








"
------------------------------------------------------------
Front end
------------------------------------------------------------
"
ui <- fluidPage(#--Set up title
                #Tab title
                title='Fellow version supplement',
                
                #Displayed title
                titlePanel(HTML('<h2>Fellow version report supplement
                                  <span id="authorship">Chih-Yu Chiang • chihyuchiang@uchicago.edu</span>
                                 </h2>')),
                
                
                #--Decide primary layout
                sidebarLayout(
                  sidebarPanel(
                         checkboxGroupInput("type_personality",
                                            "",
                                            c("In-game / Self"="InS",
                                              "Real / Self"="OutS",
                                              "Ideal / Self"="IdS",
                                              "In-game / Fellow"="InF",
                                              "Real / Fellow"="OutF",
                                              "Stereotype / Public"="SteS")),
                         actionButton("distButton_personality", "Draw distribution")),
                  mainPanel(
                         plotOutput("dist_personality_1"),
                         plotOutput("dist_personality_2"),
                         plotOutput("dist_personality_3"),
                         plotOutput("dist_personality_4"),
                         plotOutput("dist_personality_5"))
                )
)
