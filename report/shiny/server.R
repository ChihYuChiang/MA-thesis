library(shiny)
library(shinyjs)
library(pander)
library(markdown)
library(tidyverse)
library(data.table)
library(colorspace)
library(corrplot)
library(glmnet)

#Prevent pander wrapping
panderOptions("table.split.table", 200)




server <- function(session, input, output) {
  "
  ----------------------------------------------------------------------
  ## Initialization
  ----------------------------------------------------------------------
  "
  ....Initialization <- function() {}
  
  #Import necessary functions for processing
  source("func.R")
  
  #Setup the texts
  source("text.R")
  
  #Create customized reactive value to store info between reactions
  rv <- reactiveValues()

  
  
  



  
  "
  ----------------------------------------------------------------------
  ## Process Output
  ----------------------------------------------------------------------
  "
  ....ProcessOutput <- function() {}
  
  
  
  
  "
  ### DT filter
  "
  ........DTFilter <- function() {}
  
  filter.out <- reactive({
    #Filter DT
    DT <<- DT_raw[
      `GProfile-3_1` >= input$filter_1[1] & `GProfile-3_1` <= input$filter_1[2]][
      `PrefS-a2` >= input$filter_2[1] & `PrefS-a2` <= input$filter_2[2]][
      `PrefS-5` >= input$filter_3[1] & `PrefS-5` <= input$filter_3[2]][
      `Relation-8` >= input$filter_4[1] & `PrefS-5` <= input$filter_4[2]][
      `GProfile-10_2` >= input$filter_5[1] & `PrefS-5` <= input$filter_5[2]][
      `GProfile-11_2` >= input$filter_6[1] & `PrefS-5` <= input$filter_6[2]]
    
    #Output new number of observations
    nrow(DT)
  })
  
  
  
  
  "
  ### Load double Lasso file
  "
  ........LoadDls <- function() {}
  
  observeEvent(input$dlsFile_upload, {fread(input$dlsFile_upload$datapath)})
  
  
  
  
  "
  ### Save double Lasso selection
  "
  ........SaveDls <- function() {}
  
  #--Create empty df
  rv$dlsSave <- data.table(matrix(data=vector(), nrow=0, ncol=3,
                           #Specify row and column names
                           dimnames=list(c(), c("outcome", "treatment", "covariate"))))
  
  
  #--Save current selection when btn clicked
  dlsSave.out <- eventReactive(input$dlsButton_save, {
    #Input check: outcome has to have selection
    if(rv$dlsVar_outcome %>% na.omit() %>% length() == 0)  return()
    
    shinyjs::show("saved")
    rv$dlsSave <- rbind(rv$dlsSave, list(paste(rv$dlsVar_outcome, collapse=" "), paste(rv$dlsVar_treatment %>% na.omit(), collapse=" "), paste(rv$dlsVar_covariate %>% na.omit(), collapse=" ")))
    
    #Return the number of saved entries
    nrow(rv$dlsSave)
  })

  
  
  
  "
  ### Acquire double Lasso selection (+ simple lm)
  "
  ........AcquireDls <- function() {}
  
  #--Dynamically show codec when var selected
  dlsCodec.out <- reactive({
    targetColName <- c(input$var_dls_1, input$var_dls_2, input$var_dls_3, input$var_dls_4, input$var_dls_5, input$var_dls_6)
    selectedColName <- c(rv$dlsVar_outcome, rv$dlsVar_treatment, rv$dlsVar_covariate)
    
    #Ref to Func.R
    produceCodec(c(targetColName, selectedColName) %>% na.omit())
  })

  
  #--Update selected var
  dlsVar_outcome.out <- reactive({
    if(rv$dlsVar_outcome %>% na.omit() %>% length() == 0) {shinyjs::show("dlsPh_1"); NULL}
    else {shinyjs::hide("dlsPh_1"); rv$dlsVar_outcome}
  })
  dlsVar_treatment.out <- reactive({
    if(rv$dlsVar_treatment %>% na.omit() %>% length() == 0) {shinyjs::show("dlsPh_2"); NULL}
    else {shinyjs::hide("dlsPh_2"); rv$dlsVar_treatment}
  })
  dlsVar_covariate.out <- reactive({
    if(rv$dlsVar_covariate %>% na.omit() %>% length() == 0) {shinyjs::show("dlsPh_3"); NULL}
    else {shinyjs::hide("dlsPh_3"); rv$dlsVar_covariate}
  })
  
  #Record the var from selection and clean the selection
  observeEvent(input$dlsButton_outcome, {rv$dlsVar_outcome <- updateDlsVar("1", rv$dlsVar_outcome)})
  observeEvent(input$dlsButton_treatment, {rv$dlsVar_treatment <- updateDlsVar("2", rv$dlsVar_treatment)})
  observeEvent(input$dlsButton_covariate, {rv$dlsVar_covariate <- updateDlsVar("3", rv$dlsVar_covariate)})
  
  #Update the var from saved combination
  observeEvent(input$dlsLoadCom, {
    rv$dlsVar_outcome <- rv$dlsSave[as.integer(input$dlsLoadCom), outcome]
    rv$dlsVar_treatment <- rv$dlsSave[as.integer(input$dlsLoadCom), treatment] %>% as.character() %>% strsplit(split=" ") %>% unlist()
    rv$dlsVar_covariate <- rv$dlsSave[as.integer(input$dlsLoadCom), covariate] %>% as.character() %>% strsplit(split=" ") %>% unlist()
  })


  #--Implement double Lasso selection on single var set
  observeEvent(input$dlsButton, {
    #--Initialization
    #Save as independent vars to avoid data.table syntax problems
    outcomeVar <- rv$dlsVar_outcome %>% na.omit()
    treatmentVar <- rv$dlsVar_treatment %>% na.omit()
    covariateVar <- rv$dlsVar_covariate %>% na.omit()
    
    #Input check (general)
    if(length(outcomeVar) == 0) {rv$dls <- "Oucome variable cannot be empty."; return()}
    if(length(outcomeVar %>% union(treatmentVar) %>% union(covariateVar)) != length(c(outcomeVar, treatmentVar, covariateVar))) {rv$dls <- "Overlapped selection between the constructs."; return()}
    
    #Expand the matrix
    DT_dls <- expandDt(outcomeVar, treatmentVar, covariateVar)
  
    
    #--Select process to proceed
    switch(input$dlsMode,
      "LM"={
        #Input check (particular condition)
        if(length(union(treatmentVar, covariateVar)) == 0) {rv$dls <- "Treatment + covariate must be 1 or more variables."; return()}
        
        #Implement simple lm
        ytreatmentVar <- union(outcomeVar, treatmentVar)
        model_lm <- lm(as.formula(sprintf("`%s` ~ .", outcomeVar)), data=DT_dls)
      },
      "DLS + LM"={
        #Input check (particular condition)
        if(length(covariateVar) == 0) {rv$dls <- "Covariate must be 2 or more variables."; return()}
        
        #Use the function to acquire the selected dfs
        DT_select <- lassoSelect(df=DT_dls, ytreatment=union(outcomeVar, treatmentVar), test=covariateVar, outcome=outcomeVar)
        
        #Implement simple lm
        model_lm <- lm(as.formula(sprintf("`%s` ~ .", outcomeVar)), data=DT_select)
      })
    
    
    #--Output summary with outcome var as title
    rv$dls <- list(outcomeVar, summary(model_lm))
  })
  
  
  #--Implement double Lasso selection on multiple var sets
  observeEvent(input$dlsButton_multi, {
    #--Initialization
    DTs_dls <- copy(rv$dlsSave)

    #The var df can't be NULL
    if(nrow(DTs_dls) == 0) {rv$dls <- "Saved variable set cannot be empty."; return()}
  
    #Processing the text of each cell
    DTs_dls[, (c("treatment", "covariate")) := lapply(.SD, function(x) {base::strsplit(x, split=" ")}), .SDcols=c("treatment", "covariate")][
    
    #Expand the matrix
      , df := expandDt_multi(outcome, treatment, covariate)]
    
    #--Select process to proceed
    switch(input$dlsMode,
           "LM"={
             #Apply simple lm to each df 
             DTs_dls[, model_lm := lm_multi(outcome=outcome, data=df)]
           },
           "DLS + LM"={
             #Apply dls to each df 
             DTs_dls[, df_select := lassoSelect_multi(df=df, treatment=treatment, test=covariate, outcome=outcome)][
               
             #Apply simple lm
               , model_lm := lm_multi(outcome=outcome, data=df_select)]
           })
    
    
    #--Output summary with outcome var as title
    rv$dls <- list()
    for(i in 1:nrow(DTs_dls)) {
      rv$dls[[i]] <- list(
        sprintf("Model %s", i),
        sprintf("outcome: %s", DTs_dls[i, outcome][[1]]),
        DTs_dls[i, model_lm][[1]] %>% summary #Each cell is selected as a list and therefore require subsetting
      )
    }
    return(rv$dls)
  })

  
  
  
  "
  ### Acquire dists
  "
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
  
  
  
  
  "
  ### Acquire paired t-tests
  "
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
  
  
  
  
  "
  ### Dist table
  "
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
  
  
  
  
  "
  ### Description table
  "
  ........AcquireDesc <- function() {}
  
  desc.out <- eventReactive(input$descButton, {
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    
    #Save for later use
    rv$descVar <- targetColName
    
    #Get description DT
    descOutput <- pander_return(summary(DT[, targetColName, with=FALSE]), style="rmarkdown")
    
    #Dynamically adjust table width
    descOutput <- gsub("<table>", sprintf('<table class="table" style="width: %spx">', 110 * if(length(targetColName) %% 10 == 0) 10 else length(targetColName) %% 10), markdownToHTML(text=descOutput, fragment.only=TRUE))
    
    #Centering table head
    gsub('<th align="center">', '<th class="text-center">', descOutput)
  })
  
  
  #--Dynamically show codec when var selected
  descCodec.out <- reactive({
    targetColName <- c(input$var_desc_1, input$var_desc_2, input$var_desc_3, input$var_desc_4, input$var_desc_5, input$var_desc_6)
    targetColName <- if(length(targetColName) == 0) rv$descVar else targetColName

    #Ref to Func.R
    produceCodec(targetColName)
  })

  
  
  
  "
  ### Cor table
  "
  ........AcquireCor <- function() {}
  
  #--The plot
  cor.out <- eventReactive(input$corButton, {
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6)
    
    #Save for later
    rv$corVar <- targetColName
    
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
  
  
  #--Dynamically show codec when var selected
  corCodec.out <- reactive({
    targetColName <- c(input$var_cor_1, input$var_cor_2, input$var_cor_3, input$var_cor_4, input$var_cor_5, input$var_cor_6)
    targetColName <- if(length(targetColName) == 0) rv$corVar else targetColName
    
    #Ref to Func.R
    produceCodec(targetColName)
  })

  
  
  
  "
  ### Short answers (static content)
  "
  ........AcquireTextAnswer <- function() {}
  
  textAnswer.out <- DT[, .(`Enough-2`, `GProfile-9`, `Demo-Feedback`)]
  
  #Rename for display
  colnames(textAnswer.out) <- c("Game", "Video game benefit", "General comment")
  
  
  
  
  "
  ### Codec (static content)
  "
  ........AcquireCodec <- function() {}
  
  #Remove first couple of vars
  codec.out <- codec[33:nrow(codec)]
  
  #Remove system vars
  filter <- !(codec.out$Variable %in% grep("(Click)|(Submit)|(Count)|(MTurk)", names(DT), value=TRUE))
  codec.out <- codec.out[filter]
  
  #Rename for display
  colnames(codec.out) <- c("Item/Variable", "Prompt/Description")

  

  
  
  
  
  
  "
  ----------------------------------------------------------------------
  ## Render Output
  ----------------------------------------------------------------------
  "
  ....RenderOutput <- function() {}

  "
  ### Download double Lasso selection saved result
  "
  ........DownloadDlsSave <- function() {}
  
  output$dlsFile_download <- downloadHandler(
    filename=paste("dls_", Sys.Date(), ".csv", sep=""),
    content=function(file) write.csv(isolate(rv$dlsSave), file, row.names = FALSE),
    contentType="text/csv"
  )

    
  
  
  "
  ### Show double Lasso selection saving result
  "
  ........ShowDlsSave <- function() {}

  output$dlsSave <- renderText({dlsSave.out()})  
  
  

  
  "
  ### Load UI for double Lasso selection saved results
  "
  ........RenderDlsLoadInput <- function() {}
  
  output$dlsLoad <- renderUI({selectInput("dlsLoadCom",
                                          label=NULL, width="120px",
                                          choice=if(nrow(rv$dlsSave) == 0) NULL else seq(1, nrow(rv$dlsSave)))
                            })

  
  
  
  "
  ### Render double Lasso selection
  "
  ........RenderDls <- function() {}
  
  output$dlsCodec <- renderTable({dlsCodec.out()})
  
  output$dlsVar_outcome <- renderText({dlsVar_outcome.out()})
  output$dlsVar_treatment <- renderText({dlsVar_treatment.out()})
  output$dlsVar_covariate <- renderText({dlsVar_covariate.out()})
  
  output$dls <- renderPrint({req(rv$dls); rv$dls})


  

  "
  ### Render dists
  "
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
  
  
  
  
  "
  ### Render t-tests
  "
  ........RenderT <- function() {}
  
  #If NULL, don't return to avoid vacant box in the UI
  output$t_personality_sum <- renderText({if(!is.null(x <- t_personality.out()$isum)) x})
  output$t_personality_1 <- renderText({if(!is.null(x <- t_personality.out()$i1)) x})
  output$t_personality_2 <- renderText({if(!is.null(x <- t_personality.out()$i2)) x})
  output$t_personality_3 <- renderText({if(!is.null(x <- t_personality.out()$i3)) x})
  output$t_personality_4 <- renderText({if(!is.null(x <- t_personality.out()$i4)) x})
  output$t_personality_5 <- renderText({if(!is.null(x <- t_personality.out()$i5)) x})
  output$t_personality_absum <- renderText({if(!is.null(x <- t_personality.out()$iabsum)) x})
  output$t_personality_ab1 <- renderText({if(!is.null(x <- t_personality.out()$iab1)) x})
  output$t_personality_ab2 <- renderText({if(!is.null(x <- t_personality.out()$iab2)) x})
  output$t_personality_ab3 <- renderText({if(!is.null(x <- t_personality.out()$iab3)) x})
  output$t_personality_ab4 <- renderText({if(!is.null(x <- t_personality.out()$iab4)) x})
  output$t_personality_ab5 <- renderText({if(!is.null(x <- t_personality.out()$iab5)) x})
  
  output$t_SDT_sum <- renderText({if(!is.null(x <- t_SDT.out()$isum)) x})
  output$t_SDT_1 <- renderText({if(!is.null(x <- t_SDT.out()$i1)) x})
  output$t_SDT_2 <- renderText({if(!is.null(x <- t_SDT.out()$i2)) x})
  output$t_SDT_3 <- renderText({if(!is.null(x <- t_SDT.out()$i3)) x})
  output$t_SDT_absum <- renderText({if(!is.null(x <- t_SDT.out()$iabsum)) x})
  output$t_SDT_ab1 <- renderText({if(!is.null(x <- t_SDT.out()$iab1)) x})
  output$t_SDT_ab2 <- renderText({if(!is.null(x <- t_SDT.out()$iab2)) x})
  output$t_SDT_ab3 <- renderText({if(!is.null(x <- t_SDT.out()$iab3)) x})
  

  
  
  "
  ### Render dist table
  "
  ........RenderDistTable <- function() {}
  
  #Dynamic resizing
  output$dist_plot <- renderPlot({dist.out()})
  output$dist <- renderUI({plotOutput("dist_plot",
                                      width=dist.width(),
                                      height=dist.height())})
  
  
  
  
  "
  ### Render description stat
  "
  ........RenderDesc <- function() {}
  
  output$descCodec <- renderTable({descCodec.out()}, width="1100px")
  output$desc <- renderText({desc.out()})
  
  
  
  
  "
  ### Render cor table
  "
  ........RenderCor <- function() {}
  
  #Descriptions
  output$corCodec <- renderTable({corCodec.out()}, width="1100px")
  
  #Dynamic resizing
  output$cor_plot <- renderPlot({cor.out()})
  output$cor <- renderUI({plotOutput("cor_plot",
                                     width=cor.size(),
                                     height=cor.size())})
  
  
  
  
  "
  ### Render text response (static content)
  "
  ........RenderTextAnswer <- function() {}
  
  output$textAnswer <- renderTable({textAnswer.out}, width="1100px")
  

  
  
  "
  ### Render codec (static content)
  "
  ........RenderCodec <- function() {}
  
  output$codec <- renderTable({codec.out}, width="1100px")
  
  
  
  
  "
  ### Render obs number
  "
  ........RenderObsNumber <- function() {}
  
  output$filter <- renderText({filter.out()})
  
  
  
  
  "
  ### Clear selection
  "
  ........ClearSelection <- function() {}
  
  #When press button
  observeEvent(input$descButton_clear, {
    map(c("var_desc_1", "var_desc_2", "var_desc_3", "var_desc_4", "var_desc_5", "var_desc_6"),
        ~ updateCheckboxGroupInput(session, inputId=., selected=character(0)))
  })
  
  observeEvent(input$corButton_clear, {
    map(c("var_cor_1", "var_cor_2", "var_cor_3", "var_cor_4", "var_cor_5", "var_cor_6"),
        ~ updateCheckboxGroupInput(session, inputId=., selected=character(0)))
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
  
  
  
  
  "
  ### Reset filter
  "
  ........ResetFilter <- function() {}
  observeEvent(input$filter_reset, {
    map(c("filter_1", "filter_2", "filter_3", "filter_4", "filter_5", "filter_6"),
        ~ updateSliderInput(session, inputId=., value=c(1, 7)))
  })
  
}
