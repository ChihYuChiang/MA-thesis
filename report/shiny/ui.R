library(shiny)








"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
PLOT_WIDTH <- "500px"




"
### Filter var for cor table
"
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

#Break into 6 columns
var_cor_sub <- c(1,
                 length(var_cor) %/% 6 * 1 + 1,
                 length(var_cor) %/% 6 * 2 + 2,
                 length(var_cor) %/% 6 * 3 + 3,
                 length(var_cor) %/% 6 * 4 + 4,
                 length(var_cor) %/% 6 * 5 + 5,
                 length(var_cor))








"
------------------------------------------------------------
Front end
------------------------------------------------------------
"
ui <- fluidPage(#--Header
                #CSS,
                tags$head(tags$link(rel="stylesheet", type="text/css", href="bootstrap-4.0.0-beta.2-dist/css/bootstrap.min.css")),
                tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
  
                #--Set up title
                #Tab title
                title='Fellow version supplement',
                
                #Displayed title
                titlePanel(HTML('<h2>MAPSS Thesis IV
                                  <span id="authorship">Chih-Yu Chiang • December 12, 2017</span>
                                 </h2>')),
                
                
                tabsetPanel(
                  tabPanel("Personality",
                           fluidRow(column(width=1,
                                           checkboxGroupInput("type_personality",
                                                              "",
                                                              c("In-game (self)"="InS",
                                                                "Real (self)"="OutS",
                                                                "Ideal (self)"="IdS",
                                                                "In-game (fellow)"="InF",
                                                                "Real (fellow)"="OutF",
                                                                "Stereotype (Self)"="SteS")),
                                           actionButton("distButton_personality", "Draw distribution")),
                                    column(width=1,
                                           checkboxGroupInput("type_personalityG",
                                                              "",
                                                              c("In-game - Real"="InSOutS",
                                                                "Ideal - In-game"="IdSInS",
                                                                "Ideal - Real"="IdSOutS"))),
                                    column(width=4,
                                           plotOutput("dist_personality_sum", width=PLOT_WIDTH),
                                           hr(),
                                           plotOutput("dist_personality_1", width=PLOT_WIDTH),
                                           verbatimTextOutput("t_personality_1"),
                                           plotOutput("dist_personality_2", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_3", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_4", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_5", width=PLOT_WIDTH)),
                                    column(width=4,
                                           plotOutput("dist_personality_absum", width=PLOT_WIDTH),
                                           hr(),
                                           plotOutput("dist_personality_ab1", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_ab2", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_ab3", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_ab4", width=PLOT_WIDTH),
                                           plotOutput("dist_personality_ab5", width=PLOT_WIDTH))
                           )),
                  tabPanel("SDT",
                           fluidRow(column(width=2,
                                           checkboxGroupInput("type_SDT",
                                                              "",
                                                              c("In-game "="In",
                                                                "Real"="Out",
                                                                "Ideal"="Id")),
                                           actionButton("distButton_SDT", "Draw distribution")),
                                    column(width=2,
                                           checkboxGroupInput("type_SDTG",
                                                              "",
                                                              c("In-game - Real"="InOut",
                                                                "Ideal - In-Game"="IdIn",
                                                                "Ideal - Real"="IdOut"))),                                           
                                    column(width=4,
                                           plotOutput("dist_SDT_sum", width=PLOT_WIDTH),
                                           hr(),
                                           plotOutput("dist_SDT_1", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_2", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_3", width=PLOT_WIDTH)),
                                    column(width=4,
                                           plotOutput("dist_SDT_absum", width=PLOT_WIDTH),
                                           hr(),
                                           plotOutput("dist_SDT_ab1", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_ab2", width=PLOT_WIDTH),
                                           plotOutput("dist_SDT_ab3", width=PLOT_WIDTH))
                            )),
                  tabPanel("Description",
                           fluidRow(column(width=2,
                                           checkboxGroupInput("var_desc_1",
                                                              "",
                                                              var_cor[var_cor_sub[1] : var_cor_sub[2]]),
                                           actionButton("descButton", "Describe variable")),
                                    column(width=2,
                                           checkboxGroupInput("var_desc_2",
                                                              "",
                                                              var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]]),
                                           actionButton("descButton_clear", "Clear selection")),
                                    column(width=2,
                                           checkboxGroupInput("var_desc_3",
                                                              "",
                                                              var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]])),
                                    column(width=2,
                                           checkboxGroupInput("var_desc_4",
                                                              "",
                                                              var_cor[(var_cor_sub[4] + 1) : var_cor_sub[5]])),
                                    column(width=2,
                                           checkboxGroupInput("var_desc_5",
                                                              "",
                                                              var_cor[(var_cor_sub[5] + 1) : var_cor_sub[6]])),
                                    column(width=2,
                                           checkboxGroupInput("var_desc_6",
                                                              "",
                                                              var_cor[(var_cor_sub[6] + 1) : var_cor_sub[7]]))
                            ),
                           fluidRow(column(width=12,
                                           verbatimTextOutput("desc"),
                                           plotOutput("dist", width="100%"))
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
                                                              var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]]),
                                           actionButton("corButton_clear", "Clear selection")),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_3",
                                                              "",
                                                              var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]])),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_4",
                                                              "",
                                                              var_cor[(var_cor_sub[4] + 1) : var_cor_sub[5]])),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_5",
                                                              "",
                                                              var_cor[(var_cor_sub[5] + 1) : var_cor_sub[6]])),
                                    column(width=2,
                                           checkboxGroupInput("var_cor_6",
                                                              "",
                                                              var_cor[(var_cor_sub[6] + 1) : var_cor_sub[7]]))
                            ),
                            fluidRow(column(width=8,
                                            plotOutput("cor", width=PLOT_WIDTH))
                           )),
                  tabPanel("Text Answer",
                           fluidRow(column(width=12,
                                           tableOutput("textAnswer"))
                           )),
                  tabPanel("Codec",
                           fluidRow(column(width=12,
                                           tableOutput("codec"))
                           ))
                )
)
