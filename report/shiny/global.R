#Access
#https://chihyuchiang.shinyapps.io/MAPSS_Thesis_IV/

#Read in Rdata environment
#After produce the data by analysis.R, move the file from bin to shiny folder
load(".RData")

#Duplicate for server filter
DT_raw <- DT