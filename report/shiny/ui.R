library(shiny)
library(shinyjs)
library(shinythemes)




"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
#Setup the texts
source("text.R")

PLOT_WIDTH <- "400px"
CHECKBOX_WIDTH <- "200px"




"
### Filter var for cor table
"
#Numeric column, without "_", skip first 30 vars
var_cor <- names(DT)[30:ncol(DT)][!(c(30:ncol(DT)) %in% grep("_", names(DT))) & sapply(DT[, 30:ncol(DT)], is.numeric)]

#Exception +
exc_plus <- c("Duration (in seconds)",
              "GProfile-3_1",
              "GProfile-7_1", "GProfile-7_2", "GProfile-7_3", "GProfile-7_4", "GProfile-7_5", "GProfile-7_6",
              "GProfile-8",
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
                #Enable shinyjs
                useShinyjs(),
                
                #CSS and shiny theme based on Bootstrap https://rstudio.github.io/shinythemes/
                theme=shinytheme("paper"),
                tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
  
                #Tab title
                title="Fellow version supplement",
                

        #--Main content    
        sidebarLayout(
          sidebarPanel(width=3,
                #Title
                h3("MAPSS Thesis IV"),
                tags$p(id="authorship",
                     "Chih-Yu Chiang",
                     br(),
                     "January 15, 2017"),
                hr(),
                
                #Filter
                h4("Observation Filter",
                   span(class="floatRight",
                        actionButton("filter_reset", "Reset filter"))),
                tags$p(tags$b(FILTER$F1_1), code(FILTER$F1_2), br(), FILTER$F1_3),
                sliderInput("filter_1", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(tags$b(FILTER$F2_1), code(FILTER$F2_2), br(), FILTER$F2_3),
                sliderInput("filter_2", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(tags$b(FILTER$F3_1), code(FILTER$F3_2), br(), FILTER$F3_3),
                sliderInput("filter_3", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(tags$b(FILTER$F4_1), code(FILTER$F4_2), br(), FILTER$F4_3),
                sliderInput("filter_4", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(tags$b(FILTER$F5_1), code(FILTER$F5_2), br(), FILTER$F5_3),
                sliderInput("filter_5", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(tags$b(FILTER$F6_1), code(FILTER$F6_2), br(), FILTER$F6_3),
                sliderInput("filter_6", NULL, min=1, max=7, value=c(1, 7)),
                tags$p(id="obs",
                       "Current number of observations: ", tags$b(textOutput("filter", inline=TRUE)))
          ),
          mainPanel(width=9,
                tabsetPanel(
                  
                  
                  #--Double Lasso selection
                  tabPanel("Double Lasso Selection",
                           fluidRow(column(width=12,
                                           h3(DLS$T1),
                                           h5(DLS$T2),
                                           tags$ol(
                                             tags$li(DLS$C1),
                                             tags$li(DLS$C2),
                                             tags$li(DLS$C3),
                                             tags$li(DLS$C4),
                                             tags$li(DLS$C5),
                                             tags$li(DLS$C6),
                                             tags$li(DLS$C7)
                                           ),
                                           span(class="note",
                                                span("Note"),
                                                tags$ul(
                                                  tags$li(DLS$N1),
                                                  tags$li(DLS$N2),
                                                  tags$li(DLS$N3)
                                                )
                                           ),
                                           fileInput("dlsFile_upload", NULL, accept=".csv", buttonLabel="Import combination", placeholder=NULL),
                                           hr())
                           ),
                           fluidRow(column(width=12,
                                           div(
                                             checkboxGroupInput("var_dls_1", "", width=CHECKBOX_WIDTH, var_cor[var_cor_sub[1] : var_cor_sub[2]]),
                                             checkboxGroupInput("var_dls_2", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]]),
                                             checkboxGroupInput("var_dls_3", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]]),
                                             checkboxGroupInput("var_dls_4", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[4] + 1) : var_cor_sub[5]]),
                                             checkboxGroupInput("var_dls_5", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[5] + 1) : var_cor_sub[6]]),
                                             checkboxGroupInput("var_dls_6", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[6] + 1) : var_cor_sub[7]])
                                           ),
                                           span(class="clear"))
                           ),
                           fluidRow(column(width=12,
                                           span(actionLink(class="space_r", "dlsButton_outcome", "Update outcomes"),
                                                actionLink(class="space_r", "dlsButton_treatment", "Update treatments"),
                                                actionLink(class="space_r", "dlsButton_covariate", "Update covariates")
                                           ),
                                           span(actionLink(class="space_r space_l4", "dlsButton_save", "Save this combination"),
                                                hidden(span(id="saved", "saved at slot")),
                                                textOutput("dlsSave", inline=TRUE)
                                           ),
                                           div(class="space_t",
                                               "Load combination",
                                               div(class="inLine space_l", uiOutput("dlsLoad"))
                                           ),
                                           div(tags$b(class="space_r", "Outcome variable:"), span(id="dlsPh_1", class="ph", DLS$PH1), textOutput("dlsVar_outcome", inline=TRUE)),
                                           div(tags$b(class="space_r", "Treatments:"), span(id="dlsPh_2", class="ph", DLS$PH2), textOutput("dlsVar_treatment", inline=TRUE)),
                                           div(tags$b(class="space_r", "Covariates:"), span(id="dlsPh_3", class="ph", DLS$PH3), textOutput("dlsVar_covariate", inline=TRUE)),
                                           br(),
                                           tableOutput("dlsCodec"),
                                           div(class="inLine space_t", selectInput("dlsMode", label=NULL, choices=c("LM", "DLS + LM"), selected="LM", width="120px")),
                                           div(class="inLine space_l", actionButton(class="space_b", "dlsButton", "Implement")),
                                           div(class="inLine space_l", downloadButton(class="space_b", "dlsFile_download", "Download saved combination")),
                                           verbatimTextOutput("dls"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           )),
                  
                  
                  #--Personality
                  tabPanel("Personality",
                           fluidRow(column(width=12,
                                           h3(PERSON$T1),
                                           tags$ol(
                                             tags$li(PERSON$C1),
                                             tags$li(PERSON$C2),
                                             tags$li(PERSON$C3),
                                             tags$li(PERSON$C4),
                                             tags$li(PERSON$C5),
                                             tags$li(PERSON$C6)
                                           ),
                                           span(class="note",
                                             span("Note"),
                                             tags$ul(
                                               tags$li(PERSON$N1),
                                               tags$li(PERSON$N2),
                                               tags$li(PERSON$N3),
                                               tags$li(PERSON$N4)
                                             )
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=2,
                                           div(
                                             checkboxGroupInput("type_personality", width=CHECKBOX_WIDTH,
                                                                PERSON$ST1,
                                                                c("In-game (self)"="InS",
                                                                  "Real (self)"="OutS",
                                                                  "Ideal (self)"="IdS",
                                                                  "In-game (fellow)"="InF",
                                                                  "Real (fellow)"="OutF",
                                                                  "Stereotype (self)"="SteS")),
                                             checkboxGroupInput("type_personalityG", width=CHECKBOX_WIDTH,
                                                                PERSON$ST2,
                                                                c("In-game - Real (self)"="InSOutS",
                                                                  "In-game - Real (fellow)"="InFOutF",
                                                                  "Ideal - In-game (self)"="IdSInS",
                                                                  "Ideal - Real (self)"="IdSOutS"))
                                           ),
                                           span(class="clear"), #Clear the float effect above
                                           actionButton("distButton_personality", "Draw distribution")),
                                    column(width=5,
                                           plotOutput("dist_personality_sum"),
                                           htmlOutput("t_personality_sum"),
                                           hidden(hr(id="hr_personality_1", class="large")),
                                           br(),
                                           plotOutput("dist_personality_1"),
                                           htmlOutput("t_personality_1"),
                                           br(),
                                           plotOutput("dist_personality_2"),
                                           htmlOutput("t_personality_2"),
                                           br(),
                                           plotOutput("dist_personality_3"),
                                           htmlOutput("t_personality_3"),
                                           br(),
                                           plotOutput("dist_personality_4"),
                                           htmlOutput("t_personality_4"),
                                           br(),
                                           plotOutput("dist_personality_5"),
                                           htmlOutput("t_personality_5")),
                                    column(width=5,
                                           plotOutput("dist_personality_absum"),
                                           htmlOutput("t_personality_absum"),
                                           hidden(hr(id="hr_personality_2", class="large")),
                                           br(),
                                           plotOutput("dist_personality_ab1"),
                                           htmlOutput("t_personality_ab1"),
                                           br(),
                                           plotOutput("dist_personality_ab2"),
                                           htmlOutput("t_personality_ab2"),
                                           br(),
                                           plotOutput("dist_personality_ab3"),
                                           htmlOutput("t_personality_ab3"),
                                           br(),
                                           plotOutput("dist_personality_ab4"),
                                           htmlOutput("t_personality_ab4"),
                                           br(),
                                           plotOutput("dist_personality_ab5"),
                                           htmlOutput("t_personality_ab5"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           )),
                  
                  
                  #--SDT
                  tabPanel("SDT",
                           fluidRow(column(width=12,
                                           h3(SDT$T1),
                                           tags$ol(
                                             tags$li(SDT$C1),
                                             tags$li(SDT$C2),
                                             tags$li(SDT$C3),
                                             tags$li(SDT$C4),
                                             tags$li(SDT$C5),
                                             tags$li(SDT$C6)
                                           ),
                                           span(class="note",
                                                span("Note"),
                                                tags$ul(
                                                  tags$li(SDT$N1),
                                                  tags$li(SDT$N2),
                                                  tags$li(SDT$N3),
                                                  tags$li(SDT$N4)
                                                )
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=2,
                                           div(
                                             checkboxGroupInput("type_SDT", width=CHECKBOX_WIDTH,
                                                                SDT$ST1,
                                                                c("In-game "="In",
                                                                  "Real"="Out",
                                                                  "Ideal"="Id")),
                                             checkboxGroupInput("type_SDTG",
                                                                SDT$ST2, width=CHECKBOX_WIDTH,
                                                                c("In-game - Real"="InOut",
                                                                  "Ideal - In-Game"="IdIn",
                                                                  "Ideal - Real"="IdOut"))
                                           ),
                                           span(class="clear"),
                                           actionButton("distButton_SDT", "Draw distribution")),
                                    column(width=5,
                                           plotOutput("dist_SDT_sum"),
                                           htmlOutput("t_SDT_sum"),
                                           hidden(hr(id="hr_SDT_1", class="large")),
                                           br(),
                                           plotOutput("dist_SDT_1"),
                                           htmlOutput("t_SDT_1"),
                                           br(),
                                           plotOutput("dist_SDT_2"),
                                           htmlOutput("t_SDT_2"),
                                           br(),
                                           plotOutput("dist_SDT_3"),
                                           htmlOutput("t_SDT_3")),
                                    column(width=5,
                                           plotOutput("dist_SDT_absum"),
                                           htmlOutput("t_SDT_absum"),
                                           hidden(hr(id="hr_SDT_2", class="large")),
                                           br(),
                                           plotOutput("dist_SDT_ab1"),
                                           htmlOutput("t_SDT_ab1"),
                                           br(),
                                           plotOutput("dist_SDT_ab2"),
                                           htmlOutput("t_SDT_ab2"),
                                           br(),
                                           plotOutput("dist_SDT_ab3"),
                                           htmlOutput("t_SDT_ab3"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           )),
                  
                  
                  #--Description
                  tabPanel("Description",
                           fluidRow(column(width=12,
                                           h3(DESC$T1),
                                           tags$ol(
                                             tags$li(DESC$C1),
                                             tags$li(DESC$C2),
                                             tags$li(DESC$C3),
                                             tags$li(DESC$C4),
                                             tags$li(DESC$C5)
                                           ),
                                           span(class="note",
                                                span("Note"),
                                                tags$ul(
                                                  tags$li(DESC$N1),
                                                  tags$li(DESC$N2),
                                                  tags$li(DESC$N3)
                                                )
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=12,
                                           div(
                                             checkboxGroupInput("var_desc_1", "", width=CHECKBOX_WIDTH, var_cor[var_cor_sub[1] : var_cor_sub[2]]),
                                             checkboxGroupInput("var_desc_2", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]]),
                                             checkboxGroupInput("var_desc_3", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]]),
                                             checkboxGroupInput("var_desc_4", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[4] + 1) : var_cor_sub[5]]),
                                             checkboxGroupInput("var_desc_5", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[5] + 1) : var_cor_sub[6]]),
                                             checkboxGroupInput("var_desc_6", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[6] + 1) : var_cor_sub[7]])
                                           ),
                                           span(class="clear"),
                                           span(actionButton("descButton", "Describe variable"), actionButton("descButton_clear", "Clear selection")))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           tableOutput("descCodec"),
                                           htmlOutput("desc"),
                                           uiOutput("dist"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           )),
                  
                  
                  #--Cor table
                  tabPanel("Cor Table",
                           fluidRow(column(width=12,
                                           h3(COR$T1),
                                           tags$ol(
                                             tags$li(COR$C1),
                                             tags$li(COR$C2),
                                             tags$li(COR$C3),
                                             tags$li(COR$C4),
                                             tags$li(COR$C5)
                                           ),
                                           span(class="note",
                                                span("Note"),
                                                tags$ul(
                                                  tags$li(COR$N1),
                                                  tags$li(COR$N2),
                                                  tags$li(COR$N3)
                                                )
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=12,
                                           div(
                                             checkboxGroupInput("var_cor_1", "", width=CHECKBOX_WIDTH, var_cor[var_cor_sub[1] : var_cor_sub[2]]),
                                             checkboxGroupInput("var_cor_2", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[2] + 1) : var_cor_sub[3]]),
                                             checkboxGroupInput("var_cor_3", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[3] + 1) : var_cor_sub[4]]),
                                             checkboxGroupInput("var_cor_4", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[4] + 1) : var_cor_sub[5]]),
                                             checkboxGroupInput("var_cor_5", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[5] + 1) : var_cor_sub[6]]),
                                             checkboxGroupInput("var_cor_6", "", width=CHECKBOX_WIDTH, var_cor[(var_cor_sub[6] + 1) : var_cor_sub[7]])
                                           ),
                                           span(class="clear"),
                                           span(actionButton("corButton", "Draw cor table"), actionButton("corButton_clear", "Clear selection")))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           tableOutput("corCodec"),
                                           uiOutput("cor"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           )),
                  
                  
                  #--Text response
                  tabPanel("Text Response",
                           fluidRow(column(width=12,
                                           h3(TEXT$T1),
                                           tags$ol(
                                             tags$li(TEXT$C1),
                                             tags$li(TEXT$C2),
                                             tags$li(TEXT$C3),
                                             tags$li(TEXT$C4)
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=12,
                                           tableOutput("textAnswer"))
                           )),
                  
                  
                  #--Codec
                  tabPanel("Codec",
                           fluidRow(column(width=12,
                                           h3(CODEC$T1),
                                           tags$ol(
                                             tags$li(CODEC$C1),
                                             tags$li(CODEC$C2),
                                             tags$li(CODEC$C3),
                                             tags$li(CODEC$C4)
                                           ),
                                           hr())
                           ),
                           fluidRow(column(width=12,
                                           tableOutput("codec"))
                           ),
                           fluidRow(column(width=12,
                                           br(),
                                           br())
                           ))
                ) #tabsetPpanel
))) #sidebarLayout, mainPanel, and fluidPage
