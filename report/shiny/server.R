library(shiny)
library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)








"
----------------------------------------------------------------------
## Exploration
----------------------------------------------------------------------
"
"
### Distribution comparison
"
#--Personality
#Function for distribution
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


#--SDT
#Function for distribution
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
### Distribution
"
#Function for dist
dist_gen <- function (targetColName) {
  ggplot(data=DT[, targetColName, with=FALSE]) +
    geom_histogram(mapping=aes_(x=as.name(targetColName)),
                   bins=nrow(table(DT[, targetColName, with=FALSE])), binwidth=1, alpha=0.65) +
    labs(title=targetColName) +
    theme_minimal()
}
lapply(c("Demo-1", "Demo-2", "GProfile-1"), dist_gen)




"
### Scatter plot
"
#Use name for columns
targetColName <- c("SDTInOut-sum", "PersonInSOutS-sum")
criteria <- quote(get("PrefS-a1") > 0)

#Common mapping
p <- ggplot(mapping=aes_(x=as.name(targetColName[1]), y=as.name(targetColName[2])))

#Use filtered row number decide if add additional layers
if(DT[eval(criteria), .N,]) p <- p + geom_point(data=DT[eval(criteria), targetColName, with=FALSE], mapping=aes(color="g1"))
if(DT[!eval(criteria), .N,]) p <- p + geom_point(data=DT[!eval(criteria), targetColName, with=FALSE], mapping=aes(color="g2"))

#Plotting
p + scale_color_discrete(name="Group", labels=c("g1"="PrefS-a1 > 5", "g2"="PrefS-a1 < 5"))








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
  
  
  #--Cor table
  cor.out <- eventReactive(input$corButton, {
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3)
    corrplot(cor(DT[, targetColName, with=FALSE]),
             method="color", type="lower", addCoef.col="black", diag=FALSE, tl.srt=90, tl.cex=0.8, tl.col="black",
             cl.pos="r", col=colorRampPalette(diverge_hcl(3))(100)) #From the palette, how many color to extrapolate
  })
  
  
  
  
  "
  Render output
  "
  #--Render dist
  output$dist_personality_1 <- renderPlot({dist_personality_1.out()})
  output$dist_personality_2 <- renderPlot({dist_personality_2.out()})
  output$dist_personality_3 <- renderPlot({dist_personality_3.out()})
  output$dist_personality_4 <- renderPlot({dist_personality_4.out()})
  output$dist_personality_5 <- renderPlot({dist_personality_5.out()})
  
  output$dist_SDT_1 <- renderPlot({dist_SDT_1.out()})
  output$dist_SDT_2 <- renderPlot({dist_SDT_2.out()})
  output$dist_SDT_3 <- renderPlot({dist_SDT_3.out()})
  
  
  #--Render cor table
  output$cor <- renderPlot({cor.out()})

}