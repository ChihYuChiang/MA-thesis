"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
#Basics
library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(data.table)
library(Matrix)
library(SparseM)
library(feather)




"
Prepare raw data
"
#--Read in predicted scores (probability) of the 7 genres
#../data/output/df_predicted.csv
df_predicted <- fread("data/df_predicted.csv", header=TRUE) %>%
  select(-Review) %>%
  filter(Source == 1) #Preserve only from GameRadar








"
------------------------------------------------------------
Back end
------------------------------------------------------------
"
server <- function(input, output) {
  "
  Process outputs
  "
  #--Acquire search result titles
  searchResultTb.out <- eventReactive(input$searchButton, {
    #Validate the input text to not be empty
    validate(
      need(input$searchText != "", "Please enter valid text to search")
    )
    
    #Acquire the result game titles
    searchResult <- fuzzyMatch(input$searchText, df_main["Game Title"])
    
    #Filter df_main with the result game titles
    searchResultTb <- filter(df_main, `Game Title` %in% searchResult[[1]])
    
    #Sort the df_main according to search result distance order
    searchResultTb[match(searchResult[[1]], searchResultTb[["Game Title"]]), ] 
  })
  
  

  



    
  "
  Render output
  "
  #--Objects
  #Search result
  output$searchResult <- DT::renderDataTable({
    DT::datatable(searchResultTb.out() %>%
                    mutate(` ` = `Game Title`) %>%
                    select(`Game Title`, `Release Date`, ` `), #Display only title, release date, and review link
                  selection="single",
                  options=list(pageLength=10, dom="tip", columnDefs=list(
                    list(targets=3, #Transform the 3rd column into a link
                         render=JS(
                           "function(data, type, row, meta) {",
                           "return '<span title=\"' + data + '\"><a href=\"https://www.google.com/search?q=' + data + ' video game\">Click to google this game</a></span>';",
                           "}")),
                    list(targets="_all", #Center text in all column
                         className="dt-center")
                  )))
  })

}