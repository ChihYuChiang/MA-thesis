"
------------------------------------------------------------
Initialization
------------------------------------------------------------
"
library(shiny)








"
------------------------------------------------------------
Front end
------------------------------------------------------------
"
ui <- fluidPage(#--Header
                #CSS
                tags$head(tags$link(rel="stylesheet", type="text/css", href="main.css")),
  
  
                #--Set up title
                #Tab title
                title='Video Game Experiential Features',
                
                #Displayed title
                titlePanel(HTML('<h2>Video Game Experiential Features
                                  <span id="authorship">Chih-Yu Chiang • chihyuchiang@uchicago.edu</span>
                                 </h2>')),
                
                
                #--Decide primary layout
                sidebarLayout(
                  #Side bar                
                  sidebarPanel(
                    h3("Instruction"),
                    HTML("<p>
                           This app displays the <b>experiential features</b> of 11,724 video games of the last several decades. Based on these features, one can explore the experiences delivered by those video games. Regarding their in-game experiences, this app helps identify similar games and portrays the experiences by keywords that distinguishing a specific title from the other games.
                          </p>"),
                    tags$ol(tags$li("Please use video game names to search a game title you intend to explore."),
                            tags$li("Select a target game title by clicking on the search result.", img(src="instruct_1.png", height="150"))),
                    HTML("<p>
                           The games' experiential features are identified through a predictive model developed in a research project of Chih-Yu Chiang, advised by James Evans and Oleg Urminsky of the University of Chicago. This model is based on multiple player surveys and expert game reviews of major video game information platforms, including GameSpot, GamesRadar, and Polygon, with a total of 15-million words of analysis. This model incorporates multiple machine learning techniques to extract experiential compositions of a video game from a video game specific Word2Vec embedding space.
                         </p>"),
                    HTML("<p>
                           This app utilizes two constructs to measure the video game experiential features. One is a game's relationship to the Experiential Keyword Groups, English word sets that present corresponding human experiences. Another is a game's relationship to the Experiential Genres. They can be deemed as clusters of the Keyword Groups. For more information about these constructs, please refer to the <a href=\"https://github.com/ChihYuChiang/MAPSS-Thesis/blob/master/writing/Thesis%20-%20final%20draft.pdf\">original paper</a>.
                         </p>"),
                    p(class="ss-text", "(Please allow 10 seconds for initialization and each operation, the free shiny.io server is agonizingly slow)"),
                    textInput(inputId="searchText", label="", value=""),
                    actionButton(inputId="searchButton", label="search"),
                    p(""), #To make a space above. Somehow the `br()` doesn't work here.
                    DT::dataTableOutput(outputId="searchResult"),
                    br()
                  ),
                  
                  #Main panel
                  mainPanel(
                    h3("Target Title"),
                    DT::dataTableOutput(outputId="targetTitle"),
                    br(),
                    HTML("<p class=\"footnote\">
                           ESRB Rating: The <a href=\"http://www.esrb.org/\">Entertainment Software Rating Board</a> (ESRB) assigns age and content ratings for video games and apps indicating the appropriate age group for the content.
                           <br>
                           GS Score: General quality score (1-10) of a video game, rated by expert video gamers who invited by <a href=\"https://www.gamespot.com/\">GameSpot</a>, a major video game information website in the U.S.
                           <br>
                           User Score: General quality score (1-10) of a video game, rated by general visitors of GameSpot.
                           <br>
                           Genre 1-7: Propensity of a video game to be assigned to a specific Genre by the algorithm.
                          </p>"),
                    br(), hr(),
                    h3("Similar Titles"),                    
                    DT::dataTableOutput(outputId="similarTitle"),
                    br(), hr(),                   
                    h3("Dominant Features"), 
                    DT::dataTableOutput(outputId="dominantFeature"),
                    br(),
                    HTML("<p class=\"footnote\">
                           Keyword Group: Internal ID of a Keyword Group. 
                           <br>
                           Similarity: -1 to 1; the higher the similiarity, the more similar the game and the target game.
                          </p>"),
                    br(), hr(),
                    h3("Distingusishing Features"),
                    h4("Strong"),
                    DT::dataTableOutput(outputId="dStrongFeature"),
                    br(),
                    HTML("<p class=\"footnote\">
                           Percent Rank: Percentage position of a video game, among all games in the database, regarding the similarity to a specific Keyword Group.
                          </p>"),
                    br(),
                    h4("Weak"),
                    DT::dataTableOutput(outputId="dWeakFeature")
                  )
                )
)