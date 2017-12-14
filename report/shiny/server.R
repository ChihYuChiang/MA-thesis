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
#Function for distribution comparison
dist_compare <- function(DT, construct, types, item, gap=0) {
  #A map for construct and item code and str pairs
  strCodec <- list(
    "Person"=list(
      item=c("1"="Extraversion", "2"="Agreeableness", "3"="Conscientiousness", "4"="Emotion stability", "5"="Openness", "sum"="Summation",
             "ab1"="Extraversion (absolute)", "ab2"="Agreeableness (absolute)", "ab3"="Conscientiousness (absolute)", "ab4"="Emotion stability (absolute)", "ab5"="Openness (absolute)", "absum"="Summation (absolute)"),
      type=c("InS"="In-game (self)", "OutS"="Real (self)", "IdS"="Ideal (self)", "InF"="In-game (fellow)", "OutF"="Real (fellow)", "SteS"="Stereotype (self)",
             "InSOutS"="In-game - real", "IdSInS"="Ideal - in-game", "IdSOutS"="Ideal - real")
    ),
    "SDT"=list(
      item=c("1"="Autonomy", "2"="Relatedness", "3"="Competence", "sum"="Summation",
             "ab1"="Autonomy (absolute)", "ab2"="Relatedness (absolute)", "ab3"="Competence (absolute)", "absum"="Summation (absolute)"),
      type=c("In"="In-game", "Out"="Real", "Id"="Ideal",
             "InOut"="In-game - real", "IdIn"="Ideal - in-game", "IdOut"="Ideal - real")
    )
  )
  
  #Decide scales according to item and gap
  #Complicated is bad!!!!!
  itemNo <- c("Person"=5, "SDT"=3)[construct]
  scales <- list(
    binwidth=if (item == "sum" | item == "absum") 0.5 * itemNo else 0.5,
    limits=if (gap == 1) {
      if (item == "sum" | item == "absum") c(-6 * itemNo - 0.5 * itemNo, 6 * itemNo + 0.5 * itemNo) else c(-6.5, 6.5)
    } else if (gap == 0) {
      if (item == "sum") c(1 * itemNo - 0.5 * itemNo, 7 * itemNo + 0.5 * itemNo) else c(0.5, 7.5)
    },
    breaks=if (gap == 1) {
      if (item == "sum" | item == "absum") seq(-6 * itemNo, 6 * itemNo, itemNo) else seq(-6, 6)
    } else if (gap == 0) {
      if (item == "sum") seq(1 * itemNo, 7 * itemNo, itemNo) else seq(1, 7)
    }
  )
  
  #Make individual hist
  make_hist <- function(type) {
    geom_histogram(mapping=aes_(x=as.name(sprintf("%s%s-%s", construct, type, item)), fill=toString(which(types == type))),
                   binwidth=scales$binwidth, alpha=0.6)
  }
  
  #Make hist list of all items
  geom_hists <- lapply(types, make_hist)
  
  #Use the list to add ggplot components
  ggplot(data=DT) +
    geom_hists +
    scale_x_continuous(breaks=scales$breaks, minor_breaks=NULL, labels=scales$breaks, limits=scales$limits) +
    labs(x="score", title=strCodec[[construct]]$item[toString(item)]) +
    scale_fill_manual(values=diverge_hcl(length(types)), name="Item", labels=unname(strCodec[[construct]]$type[unlist(types)])) + #labels does not accept names vector
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
  dist_personality.out <- eventReactive(input$distButton_personality, {
    #If gaps are selected, use gap input, otherwise use default
    if (length(input$type_personalityG) > 0) {
      list(
        isum=dist_compare(DT, "Person", input$type_personalityG, "sum", gap=1),
        i1=dist_compare(DT, "Person", input$type_personalityG, 1, gap=1),
        i2=dist_compare(DT, "Person", input$type_personalityG, 2, gap=1),
        i3=dist_compare(DT, "Person", input$type_personalityG, 3, gap=1),
        i4=dist_compare(DT, "Person", input$type_personalityG, 4, gap=1),
        i5=dist_compare(DT, "Person", input$type_personalityG, 5, gap=1),
        iabsum=dist_compare(DT, "Person", input$type_personalityG, "absum", gap=1),
        iab1=dist_compare(DT, "Person", input$type_personalityG, "ab1", gap=1),
        iab2=dist_compare(DT, "Person", input$type_personalityG, "ab2", gap=1),
        iab3=dist_compare(DT, "Person", input$type_personalityG, "ab3", gap=1),
        iab4=dist_compare(DT, "Person", input$type_personalityG, "ab4", gap=1),
        iab5=dist_compare(DT, "Person", input$type_personalityG, "ab5", gap=1)
      ) 
    } else {
      list(
        isum=dist_compare(DT, "Person", input$type_personality, "sum"),
        i1=dist_compare(DT, "Person", input$type_personality, 1),
        i2=dist_compare(DT, "Person", input$type_personality, 2),
        i3=dist_compare(DT, "Person", input$type_personality, 3),
        i4=dist_compare(DT, "Person", input$type_personality, 4),
        i5=dist_compare(DT, "Person", input$type_personality, 5)
      ) 
    }
  })
  
  t_personality_1.out <- eventReactive(input$distButton_personality, {
    if (length(input$type_personality) == 2) {
      x1 <- sprintf("Person%s-1", input$type_personality[1])
      x2 <- sprintf("Person%s-1", input$type_personality[2])
      pander(t.test(DT[[x1]], DT[[x2]], paired=TRUE))
    }
  })
    
  dist_SDT.out <- eventReactive(input$distButton_SDT, {
    #If gaps are selected, use gap input, otherwise use default
    if (length(input$type_SDTG) > 0) {
      list(
        isum=dist_compare(DT, "SDT", input$type_SDTG, "sum", gap=1),
        i1=dist_compare(DT, "SDT", input$type_SDTG, 1, gap=1),
        i2=dist_compare(DT, "SDT", input$type_SDTG, 2, gap=1),
        i3=dist_compare(DT, "SDT", input$type_SDTG, 3, gap=1),
        iabsum=dist_compare(DT, "SDT", input$type_SDTG, "absum", gap=1),
        iab1=dist_compare(DT, "SDT", input$type_SDTG, "ab1", gap=1),
        iab2=dist_compare(DT, "SDT", input$type_SDTG, "ab2", gap=1),
        iab3=dist_compare(DT, "SDT", input$type_SDTG, "ab3", gap=1)
      )
    } else {
      list(
        isum=dist_compare(DT, "SDT", input$type_SDT, "sum"),
        i1=dist_compare(DT, "SDT", input$type_SDT, 1),
        i2=dist_compare(DT, "SDT", input$type_SDT, 2),
        i3=dist_compare(DT, "SDT", input$type_SDT, 3)
      )
    }
  })
  
  
  #--Dist table
  dist.out <- eventReactive(input$descButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    plots <- lapply(targetColName, dist_gen)
    multiplot(plotlist=plots, cols=3) #A self-defined function for combining a list of plots
  })
  
  
  #--Description table
  desc.out <- eventReactive(input$descButton, {
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
  
  
  #--Short answers (static content)
  textAnswer.out <- DT[, .(`Enough-2`, `GProfile-9`, `Demo-Feedback`)]
  
  
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
  output$dist_personality_sum <- renderPlot({dist_personality.out()$isum})
  output$dist_personality_1 <- renderPlot({dist_personality.out()$i1})
  output$dist_personality_2 <- renderPlot({dist_personality.out()$i2})
  output$dist_personality_3 <- renderPlot({dist_personality.out()$i3})
  output$dist_personality_4 <- renderPlot({dist_personality.out()$i4})
  output$dist_personality_5 <- renderPlot({dist_personality.out()$i5})
  output$dist_personality_absum <- renderPlot({dist_personality.out()$iabsum})
  output$dist_personality_ab1 <- renderPlot({dist_personality.out()$iab1})
  output$dist_personality_ab2 <- renderPlot({dist_personality.out()$iab2})
  output$dist_personality_ab3 <- renderPlot({dist_personality.out()$iab3})
  output$dist_personality_ab4 <- renderPlot({dist_personality.out()$iab4})
  output$dist_personality_ab5 <- renderPlot({dist_personality.out()$iab5})

  output$dist_SDT_sum <- renderPlot({dist_SDT.out()$isum})  
  output$dist_SDT_1 <- renderPlot({dist_SDT.out()$i1})
  output$dist_SDT_2 <- renderPlot({dist_SDT.out()$i2})
  output$dist_SDT_3 <- renderPlot({dist_SDT.out()$i3})
  output$dist_SDT_absum <- renderPlot({dist_SDT.out()$iabsum})  
  output$dist_SDT_ab1 <- renderPlot({dist_SDT.out()$iab1})
  output$dist_SDT_ab2 <- renderPlot({dist_SDT.out()$iab2})
  output$dist_SDT_ab3 <- renderPlot({dist_SDT.out()$iab3})
  
  output$t_personality_1 <- renderPrint({t_personality_1.out()})
  
  
  #--Render dist table
  output$dist <- renderPlot({dist.out()})
  
  
  #--Render description stat
  output$desc <- renderPrint({desc.out()})
  
  
  #--Render cor table
  output$cor <- renderPlot({cor.out()})
  
  
  #--Render Short answers
  output$textAnswer <- renderTable({textAnswer.out}, width="1000px")
  

  #--Render codec
  output$codec <- renderTable({codec.out})
  
  
  #--Clear selection
  #When press button
  observeEvent(input$descButton_clear, {
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
  
  #When click on other set of options
  observeEvent(input$type_personalityG, {
    updateCheckboxGroupInput(session, inputId="type_personality", selected=character(0))
  })
  observeEvent(input$type_personality, {
    updateCheckboxGroupInput(session, inputId="type_personalityG", selected=character(0))
  })
  
  observeEvent(input$type_SDTG, {
    updateCheckboxGroupInput(session, inputId="type_SDT", selected=character(0))
  })
  observeEvent(input$type_SDT, {
    updateCheckboxGroupInput(session, inputId="type_SDTG", selected=character(0))
  })

}
