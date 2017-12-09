"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(data.table)
library(colorspace)

DT <- fread("../../data/survey2.csv")




"
## Reverse (1-7 Likert) target responses
"
#--Select target columns
#Personality: 1_24 2_135; SDT: 1_246 2_246
targetColIndex <- matches("(^Person.+((1_[24])|(2_[135]))$)|(^SDT.+_[246]$)", vars=names(DT))


#--Reverse 1-7 likert
reversed <- 8 - DT[, targetColIndex, with=FALSE]


#--Assign back to DT
#Use parenthesis since the synax does not allow with=FALSE here
DT[, (targetColIndex) := reversed]




"
## Combine sub-items
"
#--personalities (5 constructs; 2 items each)
#Computation
subColIndex_1 <- matches("^Person.+1_\\d$", vars=names(DT))
subColIndex_2 <- matches("^Person.+2_\\d$", vars=names(DT))
personalities <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE]) / 2

#Substitution
newColName <- gsub("1_", "", grep("^Person.+1_\\d$", names(DT), value=TRUE))
DT[, (newColName) := personalities]


#--SDT (3 constructs; 4 items each)
#Computation
subColIndex_1 <- matches("^SDT.+1_[135]$", vars=names(DT))
subColIndex_2 <- matches("^SDT.+1_[246]$", vars=names(DT))
subColIndex_3 <- matches("^SDT.+2_[135]$", vars=names(DT))
subColIndex_4 <- matches("^SDT.+2_[246]$", vars=names(DT))
SDTs <- (DT[, subColIndex_1, with=FALSE] + DT[, subColIndex_2, with=FALSE] + DT[, subColIndex_3, with=FALSE] + DT[, subColIndex_4, with=FALSE]) / 4

#Substitution
newColName <- gsub("1_", "", grep("^SDT.+1_[123]$", names(DT), value=TRUE))
DT[, (newColName) := SDTs]




"
Function for distribution
"
#Personality
dist_personality <- function(DT, personality, types){
  #A map for personality code and str pairs
  personaCodec <- c("1"="Extraversion", "2"="Agreeableness", "3"="Conscientiousness;", "4"="Emotion stability", "5"="Openness")
  typeCodec <- c("InS"="In-game / Self", "OutS"="Real / Self", "IdS"="Ideal / Self", "InF"="In-game / Fellow", "OutF"="Real / Fellow", "SteS"="Stereotype / Public")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^Person.+-%s$", personality), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("Person%s-%s", type, personality)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=personaCodec[toString(personality)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}

#SDT
dist_SDT <- function(DT, SDT, types){
  #A map for personality code and str pairs
  SDTCodec <- c("1"="Autonomy", "2"="Relatedness", "3"="Competence")
  typeCodec <- c("In"="In-game", "Out"="Real", "Id"="Ideal")
  
  #Acquire specific columns of that personality
  targetColIndex <- matches(sprintf("^SDT.+-%s$", SDT), vars=names(DT))
  
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("SDT%s-%s", type, SDT)), fill=toString(which(types == type))),
                   binwidth=0.5, alpha=0.6)
  }
  geom_hists <- lapply(types, make_hist)
  
  #Use a list to add ggplot components
  ggplot(data=DT[, targetColIndex, with=FALSE]) +
    geom_hists +
    scale_x_continuous(breaks=seq(1, 7), minor_breaks=NULL, labels=seq(1, 7), limits=c(0.5, 7.5)) +
    labs(x="score", title=SDTCodec[toString(SDT)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(typeCodec[unlist(types)])) +
    theme_minimal()
}








"
------------------------------------------------------------
Back end
------------------------------------------------------------
"
server <- function(input, output) {
  "
  Process outputs
  "
  #--Acquire dist
  dist_personality_1.out <- eventReactive(input$distButton_personality, {
    dist_personality(DT, 1, input$type_personality)
  })
  dist_personality_2.out <- eventReactive(input$distButton_personality, {
    dist_personality(DT, 2, input$type_personality)
  })
  dist_personality_3.out <- eventReactive(input$distButton_personality, {
    dist_personality(DT, 3, input$type_personality)
  })
  dist_personality_4.out <- eventReactive(input$distButton_personality, {
    dist_personality(DT, 4, input$type_personality)
  })
  dist_personality_5.out <- eventReactive(input$distButton_personality, {
    dist_personality(DT, 5, input$type_personality)
  })
  
  dist_SDT_1.out <- eventReactive(input$distButton_SDT, {
    dist_SDT(DT, 1, input$type_SDT)
  })
  dist_SDT_2.out <- eventReactive(input$distButton_SDT, {
    dist_SDT(DT, 2, input$type_SDT)
  })
  dist_SDT_3.out <- eventReactive(input$distButton_SDT, {
    dist_SDT(DT, 3, input$type_SDT)
  })
  
  
  
  
  "
  Render output
  "
  #--Render dist
  output$dist_personality_1 <- renderPlot({
    dist_personality_1.out()
  })
  output$dist_personality_2 <- renderPlot({
    dist_personality_2.out()
  })
  output$dist_personality_3 <- renderPlot({
    dist_personality_3.out()
  })
  output$dist_personality_4 <- renderPlot({
    dist_personality_4.out()
  })
  output$dist_personality_5 <- renderPlot({
    dist_personality_5.out()
  })
  
  output$dist_SDT_1 <- renderPlot({
    dist_SDT_1.out()
  })
  output$dist_SDT_2 <- renderPlot({
    dist_SDT_2.out()
  })
  output$dist_SDT_3 <- renderPlot({
    dist_SDT_3.out()
  })
}