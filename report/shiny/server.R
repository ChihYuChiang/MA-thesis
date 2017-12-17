library(shiny)
library(shinyjs)
library(pander)
library(markdown)
library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)

#Prevent pander wrapping
panderOptions("table.split.table", 200)








"
----------------------------------------------------------------------
## Initialization
----------------------------------------------------------------------
"
....Initialization <- function() {}


"
### Distribution comparison
"
#Function for distribution comparison
dist_compare <- function(construct, types, item, gap=0) {
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
                   bins=15, alpha=0.65) +
    labs(title=targetColName) +
    theme_minimal()
}




"
### Multiple plot function (Modify from Cookbook for R)
"
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols))
  
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




"
### T-test
"
tTest <- function(construct, types, item) {
  col1 <- sprintf("%s%s-%s", construct, types[1], item)
  col2 <- sprintf("%s%s-%s", construct, types[2], item)
  
  #DT does not accept as.name (symbol); it requires object
  testOutput <- t.test(DT[, get(col1)], DT[, get(col2)], paired=TRUE)
  
  #Rename the caption of output table
  testOutput$data.name <- paste(col1, "and", col2, sep=" ")
  
  #Use pander_return to preserve the string in rmd format
  testOutput <- pander_return(testOutput, style="rmarkdown")
  
  #Transform rmd into html and add class for proper display
  testOutput <- sub("<table>", '<table class="table" style="width: 80%">', markdownToHTML(text=testOutput, fragment.only=TRUE))
  
  return(testOutput)
}








"
------------------------------------------------------------
Back end
------------------------------------------------------------
"
....BackEnd <- function() {}


server <- function(session, input, output) {
  "
  DT filtering
  "
  ........DTFilter <- function() {}
  # observeEvent()
  
  
  

  "
  Process outputs
  "
  #--Acquire dists
  ........AcquireDists <- function() {}
  
  dist_personality.out <- eventReactive(input$distButton_personality, {
    #If gaps are selected, use gap input, otherwise use default
    if(length(input$type_personalityG) > 0) {
      #Show hr when needed
      shinyjs::show("hr_personality_1")
      shinyjs::show("hr_personality_2")
      
      #Return the processed content
      list(
        isum=dist_compare("Person", input$type_personalityG, "sum", gap=1),
        i1=dist_compare("Person", input$type_personalityG, 1, gap=1),
        i2=dist_compare("Person", input$type_personalityG, 2, gap=1),
        i3=dist_compare("Person", input$type_personalityG, 3, gap=1),
        i4=dist_compare("Person", input$type_personalityG, 4, gap=1),
        i5=dist_compare("Person", input$type_personalityG, 5, gap=1),
        iabsum=dist_compare("Person", input$type_personalityG, "absum", gap=1),
        iab1=dist_compare("Person", input$type_personalityG, "ab1", gap=1),
        iab2=dist_compare("Person", input$type_personalityG, "ab2", gap=1),
        iab3=dist_compare("Person", input$type_personalityG, "ab3", gap=1),
        iab4=dist_compare("Person", input$type_personalityG, "ab4", gap=1),
        iab5=dist_compare("Person", input$type_personalityG, "ab5", gap=1)
      ) 
    } else {
      #Show hr when needed
      shinyjs::show("hr_personality_1")
      shinyjs::hide("hr_personality_2")
      
      #Return the processed content
      list(
        isum=dist_compare("Person", input$type_personality, "sum"),
        i1=dist_compare("Person", input$type_personality, 1),
        i2=dist_compare("Person", input$type_personality, 2),
        i3=dist_compare("Person", input$type_personality, 3),
        i4=dist_compare("Person", input$type_personality, 4),
        i5=dist_compare("Person", input$type_personality, 5)
      ) 
    }
  })
  
  dist_SDT.out <- eventReactive(input$distButton_SDT, {
    #If gaps are selected, use gap input, otherwise use default
    if (length(input$type_SDTG) > 0) {
      #Show hr when needed
      shinyjs::show("hr_SDT_1")
      shinyjs::show("hr_SDT_2")
      
      #Return the processed content
      list(
        isum=dist_compare("SDT", input$type_SDTG, "sum", gap=1),
        i1=dist_compare("SDT", input$type_SDTG, 1, gap=1),
        i2=dist_compare("SDT", input$type_SDTG, 2, gap=1),
        i3=dist_compare("SDT", input$type_SDTG, 3, gap=1),
        iabsum=dist_compare("SDT", input$type_SDTG, "absum", gap=1),
        iab1=dist_compare("SDT", input$type_SDTG, "ab1", gap=1),
        iab2=dist_compare("SDT", input$type_SDTG, "ab2", gap=1),
        iab3=dist_compare("SDT", input$type_SDTG, "ab3", gap=1)
      )
    } else {
      #Show hr when needed
      shinyjs::show("hr_SDT_1")
      shinyjs::hide("hr_SDT_2")
      
      #Return the processed content
      list(
        isum=dist_compare("SDT", input$type_SDT, "sum"),
        i1=dist_compare("SDT", input$type_SDT, 1),
        i2=dist_compare("SDT", input$type_SDT, 2),
        i3=dist_compare("SDT", input$type_SDT, 3)
      )
    }
  })
  
  
  #--Acquire paired t-tests
  ........AcquireT <- function() {}
  
  t_personality.out <- eventReactive(input$distButton_personality, {
    if(length(input$type_personalityG) == 2) {
      list(
        isum=tTest("Person", input$type_personalityG, "sum"),
        i1=tTest("Person", input$type_personalityG, 1),
        i2=tTest("Person", input$type_personalityG, 2),
        i3=tTest("Person", input$type_personalityG, 3),
        i4=tTest("Person", input$type_personalityG, 4),
        i5=tTest("Person", input$type_personalityG, 5),
        iabsum=tTest("Person", input$type_personalityG, "absum"),
        iab1=tTest("Person", input$type_personalityG, "ab1"),
        iab2=tTest("Person", input$type_personalityG, "ab2"),
        iab3=tTest("Person", input$type_personalityG, "ab3"),
        iab4=tTest("Person", input$type_personalityG, "ab4"),
        iab5=tTest("Person", input$type_personalityG, "ab5")
      )
    } else if(length(input$type_personality) == 2) {
      list(
        isum=tTest("Person", input$type_personality, "sum"),
        i1=tTest("Person", input$type_personality, 1),
        i2=tTest("Person", input$type_personality, 2),
        i3=tTest("Person", input$type_personality, 3),
        i4=tTest("Person", input$type_personality, 4),
        i5=tTest("Person", input$type_personality, 5)
      )
    }
    
  })
  
  t_SDT.out <- eventReactive(input$distButton_SDT, {
    #If gaps are selected, use gap input, otherwise use default
    if(length(input$type_SDTG) == 2) {
      list(
        isum=tTest("SDT", input$type_SDTG, "sum"),
        i1=tTest("SDT", input$type_SDTG, 1),
        i2=tTest("SDT", input$type_SDTG, 2),
        i3=tTest("SDT", input$type_SDTG, 3),
        iabsum=tTest("SDT", input$type_SDTG, "absum"),
        iab1=tTest("SDT", input$type_SDTG, "ab1"),
        iab2=tTest("SDT", input$type_SDTG, "ab2"),
        iab3=tTest("SDT", input$type_SDTG, "ab3")
      )
    } else if(length(input$type_SDT) == 2){
      list(
        isum=tTest("SDT", input$type_SDT, "sum"),
        i1=tTest("SDT", input$type_SDT, 1),
        i2=tTest("SDT", input$type_SDT, 2),
        i3=tTest("SDT", input$type_SDT, 3)
      )
    }
  })
  
  
  #--Dist table
  ........AcquireDistTable <- function() {}
  
  dist.out <- eventReactive(input$descButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    plots <- lapply(targetColName, dist_gen)
    
    multiplot(plotlist=plots, cols=4) #A self-defined function for combining a list of plots
  })
  
  #Decide the height dynamically
  dist.width <- eventReactive(input$descButton, {
    targetLength <- length(c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6))
    if(targetLength == 1) 2200 else 1100
  })
  
  dist.height <- eventReactive(input$descButton, {
    targetLength <- length(c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6))
    if(targetLength == 1) 500 else ceiling(targetLength / 4) * 250
  })
  
  
  #--Description table
  ........AcquireDesc <- function() {}
  
  desc.out <- eventReactive(input$descButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    descOutput <- pander_return(summary(DT[, targetColName, with=FALSE]), style="rmarkdown")
    
    #Dynamically adjust table width
    descOutput <- gsub("<table>", sprintf('<table class="table" style="width: %spx">', 110 * if(length(targetColName) %% 10 == 0) 10 else length(targetColName) %% 10), markdownToHTML(text=descOutput, fragment.only=TRUE))
    
    #Centering table head
    descOutput <- gsub('<th align="center">', '<th class="text-center">', descOutput)
    
    #Acquire description of specific vars
    #When no match, show the synthetic ones
    filter <- codec$Variable %in% targetColName
    descCodec <- if(nrow(codec[filter]) == 0) tail(codec, 5) else codec[filter]
    
    #Output
    list(descOutput=descOutput, descCodec=descCodec)
  })

  
  #--Cor table
  ........AcquireCor <- function() {}
  
  #The plot
  cor.out <- eventReactive(input$corButton, {
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6)
    
    #Avoid error when 0 or 1 item is selected
    if(length(targetColName) <= 1) return()
    
    #Main plot
    corrplot(cor(DT[, targetColName, with=FALSE]),
             method="color", type="lower", addCoef.col="black", diag=FALSE, tl.srt=90, tl.cex=0.8, tl.col="black",
             cl.pos="r", col=colorRampPalette(diverge_hcl(3))(100)) #From the palette, how many color to extrapolate
    
    #scatter plot (Special case when exactly 2 items are selected)
    if(length(targetColName) == 2) {
      p <- ggplot(data=DT[, targetColName, with=FALSE], mapping=aes_(x=as.name(targetColName[1]), y=as.name(targetColName[2]))) +
        geom_jitter() + geom_smooth(color=diverge_hcl(5)[4], alpha=0.3, size=0.5)
      
      #Add label of correlation
      p + annotate("text", x=max(DT[, targetColName[1], with=FALSE]) * 0.8, y=max(DT[, targetColName[2], with=FALSE]) * 0.8,
                   label=sprintf("Cor = %s", round(cor(DT[, targetColName[1], with=FALSE], DT[, targetColName[2], with=FALSE]), digit=4)),
                   size=5, color=diverge_hcl(5)[1], alpha=0.7)
    }
  })
  
  #Decide the width and height dynamically
  cor.size <- eventReactive(input$corButton, {
    targetLength <- length(c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6))
    max(targetLength * 50, 500)
  })
  
  #The corresponding codec
  corCodec.out <- eventReactive(input$corButton, {
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6)
    
    #Avoid error when 0 or 1 item is selected
    if(length(targetColName) <= 1) return()
    
    #Acquire description of specific vars
    #When no match, show the synthetic ones
    filter <- codec$Variable %in% targetColName
    if(nrow(codec[filter]) == 0) tail(codec, 5) else codec[filter]
  })

  
  #--Short answers (static content)
  ........AcquireTextAnswer <- function() {}
  
  textAnswer.out <- DT[, .(`Enough-2`, `GProfile-9`, `Demo-Feedback`)]
  
  #Rename for display
  colnames(textAnswer.out) <- c("Game", "Video game benefit", "General comment")
  
  
  #--Codec (static content)
  ........AcquireCodec <- function() {}
  
  #Remove first couple of vars
  codec.out <- codec[33:nrow(codec)]
  
  #Remove system vars
  filter <- !(codec.out$Variable %in% grep("(Click)|(Submit)|(Count)|(MTurk)", names(DT), value=TRUE))
  codec.out <- codec.out[filter]
  
  #Rename for display
  colnames(codec.out) <- c("Item/Variable", "Prompt/Description")

  


  "
  Render output
  "
  #--Render dist comparison
  ........RenderDists <- function() {}
  
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
  
  
  #--Render t-tests
  ........RenderT <- function() {}
  
  #If NULL, don't return to avoid vacant box in the UI
  output$t_personality_sum <- renderText({if (!is.null(x <- t_personality.out()$isum)) x})
  output$t_personality_1 <- renderText({if (!is.null(x <- t_personality.out()$i1)) x})
  output$t_personality_2 <- renderText({if (!is.null(x <- t_personality.out()$i2)) x})
  output$t_personality_3 <- renderText({if (!is.null(x <- t_personality.out()$i3)) x})
  output$t_personality_4 <- renderText({if (!is.null(x <- t_personality.out()$i4)) x})
  output$t_personality_5 <- renderText({if (!is.null(x <- t_personality.out()$i5)) x})
  output$t_personality_absum <- renderText({if (!is.null(x <- t_personality.out()$iabsum)) x})
  output$t_personality_ab1 <- renderText({if (!is.null(x <- t_personality.out()$iab1)) x})
  output$t_personality_ab2 <- renderText({if (!is.null(x <- t_personality.out()$iab2)) x})
  output$t_personality_ab3 <- renderText({if (!is.null(x <- t_personality.out()$iab3)) x})
  output$t_personality_ab4 <- renderText({if (!is.null(x <- t_personality.out()$iab4)) x})
  output$t_personality_ab5 <- renderText({if (!is.null(x <- t_personality.out()$iab5)) x})
  
  output$t_SDT_sum <- renderText({if (!is.null(x <- t_SDT.out()$isum)) x})
  output$t_SDT_1 <- renderText({if (!is.null(x <- t_SDT.out()$i1)) x})
  output$t_SDT_2 <- renderText({if (!is.null(x <- t_SDT.out()$i2)) x})
  output$t_SDT_3 <- renderText({if (!is.null(x <- t_SDT.out()$i3)) x})
  output$t_SDT_absum <- renderText({if (!is.null(x <- t_SDT.out()$iabsum)) x})
  output$t_SDT_ab1 <- renderText({if (!is.null(x <- t_SDT.out()$iab1)) x})
  output$t_SDT_ab2 <- renderText({if (!is.null(x <- t_SDT.out()$iab2)) x})
  output$t_SDT_ab3 <- renderText({if (!is.null(x <- t_SDT.out()$iab3)) x})
  

  #--Render dist table
  ........RenderDistTable <- function() {}
  
  #Dynamic resizing
  output$dist_plot <- renderPlot({dist.out()})
  output$dist <- renderUI({plotOutput("dist_plot",
                                      width=dist.width(),
                                      height=dist.height())})
  
  #--Render description stat
  ........RenderDesc <- function() {}
  
  output$descCodec <- renderTable({desc.out()$descCodec}, width="1100px")
  output$desc <- renderText({desc.out()$descOutput})
  
  
  #--Render cor table
  ........RenderCor <- function() {}
  
  #Descriptions
  output$corCodec <- renderTable({corCodec.out()}, width="1100px")
  
  #Dynamic resizing
  output$cor_plot <- renderPlot({cor.out()})
  output$cor <- renderUI({plotOutput("cor_plot",
                                     width=cor.size(),
                                     height=cor.size())})
  
  
  #--Render text response
  ........RenderTextAnswer <- function() {}
  
  output$textAnswer <- renderTable({textAnswer.out}, width="1100px")
  

  #--Render codec
  ........RenderCodec <- function() {}
  
  output$codec <- renderTable({codec.out}, width="1100px")
  
  
  #--Clear selection
  ........ClearSelection <- function() {}
  
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
