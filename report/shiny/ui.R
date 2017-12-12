"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
library(shiny)

PLOT_WIDTH <- "600px"

#--Filter var for cor table
#Numeric column, without "_", skip first 30 vars
var_cor <- names(DT)[30:ncol(DT)][!(c(30:ncol(DT)) %in% grep("_", names(DT))) & sapply(DT[, 30:ncol(DT)], is.numeric)]

#Exception +
exc_plus <- c("Duration (in seconds)",
              "GProfile-3_1",
              "GProfile-7_1", "GProfile-7_2", "GProfile-7_3", "GProfile-7_4", "GProfile-7_5", "GProfile-7_6",
              "GProfile-10_2", "GProfile-11_2")

#Exception -
exc_minus <- c("MTurkCode")

#Process exceptions
var_cor <- c(var_cor, exc_plus)[!(c(var_cor, exc_plus) %in% exc_minus)]

#Break into 3 columns
var_cor_sub <- c(1, length(var_cor) %/% 3 + 1, length(var_cor) %/% 3 * 2 + 2, length(var_cor))








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
                            )),
                  tabPanel("Cor Table",
                           fluidRow(column(width=2,
                                           checkboxGroupInput("var_cor_1",
                                                              "",
                                                              var_cor[var_cor_sub[1] : var_cor_sub[2]]),
                                           actionButton("corButton", "Draw cor table")),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_2",
                                                              "",
                                                              var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]])),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_3",
                                                              "",
                                                              var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]])),
                                    column(width=8,
                                           plotOutput("cor", width=PLOT_WIDTH)
                                    )
                           ))
                )
)
