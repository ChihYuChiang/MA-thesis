library(shiny)
library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)
library(pander)

#Prevent output text wrapping
panderOptions("table.split.table", 200)








"
----------------------------------------------------------------------
## Initialization
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




"
### Multiple plot function (Cookbook for R)
"
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}








"
------------------------------------------------------------
Back end
------------------------------------------------------------
"
server <- function(session, input, output) {
  "
  DT filtering
  "
  # observeEvent()
  



  "
  Process outputs
  "
  #--Acquire dist and t test
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
  
  
  t_personality_1.out <- eventReactive(input$distButton_personality, {
    if (length(input$type_personality) == 2) {
      x1 <- sprintf("Person%s-1", input$type_personality[1])
      x2 <- sprintf("Person%s-1", input$type_personality[2])
      pander(t.test(DT[[x1]], DT[[x2]], paired=TRUE))
    }
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
  
  
  #--Dist table
  dist.out <- eventReactive(input$distButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    plots <- lapply(targetColName, dist_gen)
    multiplot(plotlist=plots, cols=3) #A self-defined function for combining a list of plots
  })
  
  
  #--Description table
  desc.out <- eventReactive(input$distButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    pander(summary(DT[, targetColName, with=FALSE]))
  })
  
  
  #--Cor table
  cor.out <- eventReactive(input$corButton, {
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6)
    corrplot(cor(DT[, targetColName, with=FALSE]),
             method="color", type="lower", addCoef.col="black", diag=FALSE, tl.srt=90, tl.cex=0.8, tl.col="black",
             cl.pos="r", col=colorRampPalette(diverge_hcl(3))(100)) #From the palette, how many color to extrapolate
  })
  
  
  #--Codec (static content)
  #Remove first couple of vars
  codec.out <- codec[33:nrow(codec)]
  
  #Remove system vars
  filter <- !(codec.out$Variable %in% grep("(Click)|(Submit)|(Count)|(MTurk)", names(DT), value=TRUE))
  codec.out <- codec.out[filter]

  


  "
  Render output
  "
  #--Render dist and t-test
  output$dist_personality_1 <- renderPlot({dist_personality_1.out()})
  output$dist_personality_2 <- renderPlot({dist_personality_2.out()})
  output$dist_personality_3 <- renderPlot({dist_personality_3.out()})
  output$dist_personality_4 <- renderPlot({dist_personality_4.out()})
  output$dist_personality_5 <- renderPlot({dist_personality_5.out()})
  
  output$dist_SDT_1 <- renderPlot({dist_SDT_1.out()})
  output$dist_SDT_2 <- renderPlot({dist_SDT_2.out()})
  output$dist_SDT_3 <- renderPlot({dist_SDT_3.out()})
  
  output$t_personality_1 <- renderPrint({t_personality_1.out()})
  
  
  #--Render dist table
  output$dist <- renderPlot({dist.out()})
  
  
  #--Render description
  output$desc <- renderPrint({desc.out()})
  
  
  #--Render cor table
  output$cor <- renderPlot({cor.out()})
  
  
  #--Render codec
  output$codec <- renderTable({codec.out})
  
  
  #--Clear selection
  observeEvent(input$distButton_clear, {
    updateCheckboxGroupInput(session, inputId="var_desc_1", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_desc_2", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_desc_3", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_desc_4", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_desc_5", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_desc_6", selected=character(0))
  })
  
  observeEvent(input$corButton_clear, {
    updateCheckboxGroupInput(session, inputId="var_cor_1", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_cor_2", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_cor_3", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_cor_4", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_cor_5", selected=character(0))
    updateCheckboxGroupInput(session, inputId="var_cor_6", selected=character(0))
  })

}
