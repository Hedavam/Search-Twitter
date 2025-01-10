library(shiny)
library(shinydashboard)
library(lubridate)
library(tidytext)
library(stringr)
library(tidyverse)
# library(corpus)
library(rtweet)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rtweet)
library(twitteR)
library(shinycustomloader)
library(wordcloud2)
library(data.table)
library(shinyjs)
library(textdata)


#NEW TO DO: optimization for users on mobile
#Tweetfinder alert when tweet with that word can't be found

#NEW TODO (11/6/2022); Scroll to plot created (useful if ppl use different plots out of order)


#afinnsentiments <- get_sentiments("afinn") #!!!! THIS LINE BREAKS THE PUBLISHING PROCESS b/cuz it seems to require user input that can't be processed on server side (not rlly sure what's going on)

#write_csv(afinnsentiments, "./data/afinnsentiments.csv") 


sentimentsToUse <- read_csv("./data/afinnsentiments.csv")



header <- dashboardHeader(title = "Search Twitter")

sidebar<- dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
        menuItem("Info", tabName = "model", icon = icon("user-plus")
                 
        ),
        menuItem("Frequently Used Words", tabName = "used_words", icon = icon("clipboard-list"),
                 radioButtons(inputId = "radio1",
                              label = "Duration",
                              choices = c("Words","Hashtags", "@s")
                 ),
                 
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go1", "Search"),
                     a(id = "toggleAdvanced1", "Show/hide description")), #https://deanattali.com/2015/04/23/shinyjs-r-package/
                 
                 shinyjs::hidden(
                     div(id = "advanced1", 
                     style = "display: block;
                                                    vertical-align:top; 
                                                    width: 98.1%", #MAYBE Change to 30.8% when doing horizontal scroll fix; Change to 95.8 % for all words on screen fix (no scroll)
                                                    
                         verbatimTextOutput("advanced1Description"),
                        tags$style("#advanced1Description{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro;
                                
                                 }")
                         
                     )
                 )
                 
            
                 
               
                 
                 
                 
        ),
        menuItem("Word Cloud", tabName = "cloud", icon = icon("cloud"),
                 #sliderInput("freq",
                 #           "Minimum Frequency:",
                 #          min = 1,  max = 50, value = 15),
                 sliderInput(inputId = "max",
                             label = "Maximum Number of Words:",
                             min = 1,  max = 300,  value = 50), #what's a good starting value for wordcloud???
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go2", "Show"),
                     a(id = "toggleAdvanced2", "Show/hide description")),
                 
                 shinyjs::hidden(
                     div(id = "advanced2", 
                         style = "display: block;
                                                    vertical-align:top; 
                                                    width: 98.1%", #MAYBE Change to 30.8% when doing horizontal scroll fix; Change to 95.8 % for all words on screen fix (no scroll)
                         
                         verbatimTextOutput("advanced2Description"),
                         tags$style("#advanced2Description{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro;
                                
                                 }")
                         
                     )
                 )
                 
                 
        ),
        
        menuItem("Word Usage Over Time", tabName = "word_usage", icon = icon("hourglass-half"),
                 textInput(inputId = "wordInput",
                           label = "Enter word(s) (no space between commas): "
                 ),
                 textInput(inputId = "year",
                           label = "Year: "
                 ),
                 div(style="display: inline-block;vertical-align:top; width: 100px;",
                     actionButton("go3", "Search"),
                     a(id = "toggleAdvanced3", "Show/hide description")),
                 shinyjs::hidden(
                     div(id = "advanced3", 
                         style = "display: block;
                                                    vertical-align:top; 
                                                    width: 98.1%", #MAYBE Change to 30.8% when doing horizontal scroll fix; Change to 95.8 % for all words on screen fix (no scroll)
                         
                         verbatimTextOutput("advanced3Description"),
                         tags$style("#advanced3Description{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro;
                                
                                 }")
                         
                     )
                 )
                 
        ),
        
        menuItem("Tweet Sentiments", tabName = "tweetSentiments", icon = icon("heartbeat")),
        
        menuItem("Tweet Finder", tabName = "tweetbyname", icon = icon("twitter"), badgeLabel = "new", badgeColor = "green"),
        
        menuItem("About App", tabName = "AboutApp", icon = icon("info-circle"))
        
        
    )
)


body <- dashboardBody(
    tabItems(
        
        tabItem(tabName = "model",
                conditionalPanel("input.go==1 && input.nameInput != ''",
                                 h1(strong(textOutput("person"))), 
                                 h3(id = "refreshmessageid", strong(textOutput("refreshmessage")))),
                 #Add the refresh page thing here unless i can do it with validate message
                textInput(inputId = "nameInput",
                          label = "Input Twitter ID: ",
                          
                          #placeholder = "elonmusk" for example
                ),
                
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("go", "Search")),
                
                
                actionButton("resetButton", "Reset"),
                
                #Wrap everything under this comment in conditional panel (if-version; if username AND search button are clicked then show)
                conditionalPanel("input.go > 0 && input.nameInput != ''", #the condition for input.go has to be calculated based on the amount of times the go button is clicked
                fluidRow(
                    br(), #line break to create space between "go" button and fluidRow of widgets
                    infoBoxOutput("follower"), 
                    infoBoxOutput("tweetvsretweet"),
                    infoBoxOutput("stat"),
                    infoBoxOutput("interactions"),infoBoxOutput("sources"),infoBoxOutput("tweettime")),
                    
                    #USED CONDITIONAL PANEL TO GET LOADING SCREEN TO START AT BUTTON CLICK; SOURCE: https://stackoverflow.com/questions/50584066/shiny-apply-shinycustomerloader-after-pressing-actionbutton
                                  h2(textOutput(outputId = "text")),
                                  h3(withLoader(tableOutput(outputId = "checkingOutput"), type="html",loader="pacman")), #SHOULD USE CONDITIONAL PANEL TO MAKE IT SO THAT USER HAS TO CLICK THE BUTTON TO SEARCH USERNAME BEFORE USING ANY OF THE TABS ON THE SIDE???
              #could put all the outputs in conditional panel (given that button to search username is clicked and that something has been inputted)
              #Update: View line with conditional panel condition to see that the actual conditions work, now decide what to wrap in conditional panel   
               fluidPage(
                    
                    tags$head(tags$style("#checkingOutput{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro
                                 }")),
                    fluidRow(
                        
                                    plotOutput(outputId ="graph1"),
                                    plotOutput(outputId = "plot"), #change to wordcloud2Output if i switch to wordcloud2
                        plotOutput(outputId= "graph2")
                    )
                )),
              
              #have the else-version of conditional panel if search is clicked with no username so that a "error" message is outputted prompting user to input a username
              conditionalPanel("input.nameInput == '' && input.go > 0",
                               h4(id = "emptyUsernamego", strong(textOutput("emptyUsernameMessagego"))),
                               tags$style(HTML("#emptyUsernamego{color: red;}")),
                              ),
              
              #Chain of conditionalPanels for unclickedButton error messages given that main search button (go) isn't clicked but smth is inputted in the nameInput box
              
              conditionalPanel("input.nameInput != '' && input.go == 0 && input.go1 > 0",
                               h4(id = "unclickedButtongo1", strong(textOutput("unclickedButtonMessagego1"))),
                               tags$style(HTML("#unclickedButtongo1{color: red;}")),
              ),
              
              conditionalPanel("input.nameInput != '' && input.go == 0 && input.go2 > 0",
                               h4(id = "unclickedButtongo2", strong(textOutput("unclickedButtonMessagego2"))),
                               tags$style(HTML("#unclickedButtongo2{color: red;}")),
              ),
              
              conditionalPanel("input.nameInput != '' && input.go == 0 && input.go3 > 0",
                               h4(id = "unclickedButtongo3", strong(textOutput("unclickedButtonMessagego3"))),
                               tags$style(HTML("#unclickedButtongo3{color: red;}")),
              ),
              
              
              #Chain of conditionalPanels for emptyUsername error messages given that dashboard action buttons are clicked
              
              #for go1 (frequently used words actionButton)
              conditionalPanel("input.nameInput == '' && input.go1 > 0", # GREATER THAN 0 REPRESENTS BUTTON CLICKED MORE THAN ONCE
                               h4(id = "emptyUsernamego1", strong(textOutput("emptyUsernameMessagego1"))),
                               tags$style(HTML("#emptyUsernamego1{color: red;}")),
                              ),
              
              conditionalPanel("input.nameInput == '' && input.go2 > 0",
                               h4(id = "emptyUsernamego2", strong(textOutput("emptyUsernameMessagego2"))),
                               tags$style(HTML("#emptyUsernamego2{color: red;}")),
                              ),
              
              conditionalPanel("input.nameInput == '' && input.go3 > 0",
                               h4(id = "emptyUsernamego3", strong(textOutput("emptyUsernameMessagego3"))),
                               tags$style(HTML("#emptyUsernamego3{color: red;}")),
                              )
              
              
              ),
        
        tabItem(tabName = "tweetSentiments",
               
                div(style="display: inline-block;vertical-align:top; width: 100px",
                    actionButton("showSentiments", "Reveal Sentiments")),
                    tags$br(),
                    a(id = "toggleAdvanced4", "Show/hide description"),
                
                shinyjs::hidden(
                    div(id = "advanced4", 
                        style = "display: block;
                                                    vertical-align:top; 
                                                    width: 100%", #MAYBE Change to 30.8% when doing horizontal scroll fix; Change to 95.8 % for all words on screen fix (no scroll)
                        
                        textOutput("advanced4Description"),
                        tags$style("#advanced4Description{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro;
                                
                                 }")
                        
                    )
                ),
                
                conditionalPanel("input.go==1 && input.nameInput != ''",
                plotOutput(outputId= "graph3")),
                
            
                
                
                #Unclicked button
                conditionalPanel("input.nameInput != '' && input.go == 0 && input.showSentiments > 0",
                                 h4(id = "unclickedButtonshowSentiments", strong(textOutput("unclickedButtonMessageshowSentiments"))),
                                 tags$style(HTML("#unclickedButtonshowSentiments{color: red;}")),
                ),
                
                #Empty username
                conditionalPanel("input.nameInput == '' && input.showSentiments > 0",
                                 h4(id = "emptyUsernameshowSentiments", strong(textOutput("emptyUsernameMessageshowSentiments"))),
                                 tags$style(HTML("#emptyUsernameshowSentiments{color: red;}")),
                                 )
                
                ),
        
        #else-version with error message (if actionButton clicked + empty input.nameInput; then display error message)
        
        
        tabItem(tabName = "tweetbyname",
                textInput(inputId = "nameInputforTweets",
                          label = "Input Word: "
                ),
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("gofindtweet", "Search")),
                
                tags$br(),
                a(id = "toggleAdvanced5", "Show/hide description"),
                
                shinyjs::hidden(
                    div(id = "advanced5", 
                        style = "display: block;
                                                    vertical-align:top; 
                                                    width: 100%", #MAYBE Change to 30.8% when doing horizontal scroll fix; Change to 95.8 % for all words on screen fix (no scroll)
                        
                        textOutput("advanced5Description"),
                        tags$style("#advanced5Description{color: black;
                                 font-size: 15px;
                                 font-family: Source Sans Pro;
                                
                                 }")
                        
                    )
                ),
                
                
                conditionalPanel("input.go==1 && input.nameInput != ''",
                                 h2(textOutput(outputId = "tweetfindertext")),
                                 tableOutput(outputId = "tweetfinder"),
                                ),
                
                #Unclicked button
                conditionalPanel("input.nameInput != '' && input.go == 0 && input.gofindtweet > 0",
                                 h4(id = "unclickedButtongofindtweet", strong(textOutput("unclickedButtonMessagegofindtweet"))),
                                 tags$style(HTML("#unclickedButtongofindtweet{color: red;}")),
                ),
    
                
                #Empty username
                conditionalPanel("input.nameInput == '' && input.gofindtweet > 0",
                                 h4(id = "emptyUsernamegofindtweet", strong(textOutput("emptyUsernameMessagegofindtweet"))),
                                 tags$style(HTML("#emptyUsernamegofindtweet{color: red;}")),
                )
                
        ),
        
       #else-version with error message (if actionButton clicked + empty input.nameInput; then display error message)
        
        
        tabItem(tabName = "AboutApp",
                h1(id="app-heading", "Search Twitter"),
                tags$style(HTML("#app-heading{color: #008ABA;}")),
                tags$style(HTML("#app-heading{text-align:center}")),
                
                h3("Search Twitter is an app that is able to show trends and  various other information from an individual's tweets. The app analyzes the
             3,200 most recent tweets made by the Twitter User, and outputs information ranging from the user's most popular Tweets to the most used words in their tweets."),
                
                h4("
The data was extracted directly from twitter’s database through a developer’s account. The rtweet package was used for corpus extraction. 



The corpus consists of tweets and other Twitter analytics of various Twitter users depending on the username we input. We chose to go with the corpus obtained from twitter’s database and not a csv file, firstly because it is more convinient to find information/data on not one single person but multiple Twitter users and secondly because the corpus consists of many variables than suppose a csv file from Kaggle . 



We did not have a single specific questoin when approaching this project, but wanted to see the different types of information we could obtain through the tweets of various individuals. For example, one of the things we wanted to find out was what certain Twitter users talk about the most. Through our code and dataset, we are able to find the most used words and hashtags from various Twitter users’ , giving us information on the topics they talk about and value the most. A possible new feature in this app could be providing a theme name based on the  hashtags a user uses the most. For example: Greta Thunberg’s theme can be Climate Speaker and Joe Biden’s theme could be Politician.



If this app is used in a bigger scale, the Tweet Finder can be used to check if  a user whose, account has been reported, has tweets with explicit words. The

“N word” surfaces around tweets every day, so we see this function having a practical use in real life.

As of now, the function only supports a single word, but support for multiple words could be added in the Tweet Finder function.



The limitations in our analysis is that our dataset only allows for the most recent 3,200 tweets from Twitter users. Thus, our data is more compact and not does not completely include every text from each twitter user. Moreover, there may be some Twitter users who have less/far less tweets published, and therefore, our information that we obtain can be fairly insignificant. 

"),
                h4("
             In order to utilize the app, you will first need to enter the individual's Twitter Username (@username; without the @symbol) in the box provided. Please be patient, as it may take time for the app to output and render all of its informatoin and visuals."),
                
                h4("After inputing a Twitter username, you will see 6 widgets appear at the top of the app. These 6 widgets different information dependent on the Twitter User you choose to focus on."), 
                h4("The first box is explanatory, showing the amount of Twitter followers that the selected Twitter user has."), 
                h4("The following box, “Tweets Vs Retweets” categorizes the Twitter User’s past 3,200 tweets as either being a Tweet from themselves or a retweet from another user. 
              The blue widget, “Tweets in (month),” will show how many tweets the Twitter User has made in the month that we are in. For example, if we are in the month December, the box will read “Tweets in December.”"),
                h4("The red widget called “Most Interaction,” references a Twitter User that is referenced the most in your selected Twitter user’s timeline."), 
                h4("The green widget, “Most Twitter Usage From,” shows which platform that the selected Twitter user posts most of their tweets from."), 
                h4("Finally, the yellow widget, “Preferred Tweeting Time,” displays, in military time, between which hour they tweet from the most. For example, if our selected Twitter user’s preferred tweeting time reads 21, then we know that, on average, that person publishes the most tweets from between 9:00pm to 10:00 pm."),
                h4("When clicking on the first tab, 'Frequenty Used Words,' you will be prompted with three options which consist of 'words', 'hastags', and '@'s.' This tab will create a graph  
             that shows the 25 most used words, hashtags, or @'s, based on the option selected."), 
                
                h4("Below this tab, you will see a tab labeled 'Wordcloud.'  This tab is equipped with a slider that modifies the max number of words displayed on the wordcloud, which is created based on the Twitter User's tweets.
             The wordcloud is a visual that consists of the most used words, hashtags, and @'s. The bigger the word is within the wordcloud, the more that specific word appears in the Twitter User's tweets."), 
                
                h4("The third tab 'Word Usage Over Time:' controls a graph that displays the Word Usage Over Time Graph (shows the change in how many times a specific word is used thorughout each month). You will be able to type in any word or words,separated by commas (no spaces), to see
             the change in the number of times they appear in the Twitter User's Texts."), 
                h4("The fourth tab 'Tweet Sentiments' has a button that once clicked outputs a graph that shows whether the word used within the Twitter User's tweets has either a more positive connotation
             or a negative connotation(differentiated by whether the sentiment value is postitive or negative). Groups by month. "),
                h4("The penultimate tab “TweetFinder” prompts you to enter a word into the type box. Once a word is entered,  TweetFinder will compile a list of tweets with that specific word(remember, however, that the list covers only the most recent 3,200 tweets)."),
                
                
                h2(id="note-heading", "Important Notes: "),
                tags$style(HTML("#note-heading{color: red;}")),
                
                h4("• To save rendering time, we made the decision to make the user reload the app when they want to input a new username."),
                
                h4("• Errors occur if the inputted twitterID (@) doesn't exist, if it's mispelled, or if they have no tweets. (must refresh if error occurs). Also if the error (check logs) appears, it means rtweet can't collect the data the user wants because it doesn't exist or is too old."),
                
                h4("• You may see that some graphs, such as the Word Usage Over Time Graph and Sentiment Graph, fail to
             cover each month. However, this is only due to the fact that the app only takes the most recent 3,200 tweets from the Twitter User, and thus, the months shown ranges based on how frequently or infrequently they tweet."),
                
                
                h3("Designed by:"),
                tag("a", list(href = "http://www.linkedin.com/in/hedavam-solano/", "Hedavam Solano, ")),
                tag("a", list(href = "https://www.instagram.com/terryppark/", "Terry Park, ")),
                tag("a", list(href = "https://twitter.com/AnweshanAdhika3", "Anweshan Adhika")))
    ),
    
    
    
)


ui <- dashboardPage(header, 
                    sidebar,
                    body)



server <- function(input, output, session) {
    
    
    
    #Show/hide description within sidebar (server side); tab 1 
    observeEvent(
        shinyjs::onclick("toggleAdvanced1",
                     shinyjs::toggle(id = "advanced1", anim = TRUE)), 
        
        
        
        #SHOW/HIDE Description text; All in one line (scrollable); formatting the text here formats it on output.
        #So, instead of horizontal scroll, all text could be displayed with line breaks. Which is better option?
        output$advanced1Description <- renderText({
                        "This tab will create a graph that shows the 25 most used words, hashtags, or @'s, based on the option selected."
                     })
      
      #Whole description shows version
        #output$advanced1Description <- renderText({
            #"This tab will create a 
#graph that shows the 25 
#most used words, hashtags, 
#or @'s, based on option selected."
 #       })
        
        )    
    #Show/hide description within sidebar (server side); tab 2
    observeEvent(
        shinyjs::onclick("toggleAdvanced2",
                         shinyjs::toggle(id = "advanced2", anim = TRUE)), 
        
        
        
        #SHOW/HIDE Description text; All in one line (scrollable); formatting the text here formats it on output.
        #So, instead of horizontal scroll, all text could be displayed with line breaks. Which is better option?
        output$advanced2Description <- renderText({
            "This tab will create a visual of the user's most used words, hashtags, and @'s. The bigger the word, the higher its usage."
        })
        
        #Whole description shows version
        #output$advanced1Description <- renderText({
        #"This tab will create a 
        #graph that shows the 25 
        #most used words, hashtags, 
        #or @'s, based on option selected."
        #       })
        
    )   
    
    #Show/hide description within sidebar (server side); tab 3
    observeEvent(
        shinyjs::onclick("toggleAdvanced3",
                         shinyjs::toggle(id = "advanced3", anim = TRUE)), 
        
        
        
        #SHOW/HIDE Description text; All in one line (scrollable); formatting the text here formats it on output.
        #So, instead of horizontal scroll, all text could be displayed with line breaks. Which is better option?
        output$advanced3Description <- renderText({
            "This tab will create a graph that shows the change in how many times a specific word(s) is used thorughout each month."
        })
        
        #Whole description shows version
        #output$advanced1Description <- renderText({
        #"This tab will create a 
        #graph that shows the 25 
        #most used words, hashtags, 
        #or @'s, based on option selected."
        #       })
        
    )  
    
    #Show/hide description within sidebar (server side); tab 4
    observeEvent(
        shinyjs::onclick("toggleAdvanced4",
                         shinyjs::toggle(id = "advanced4", anim = TRUE)), 
        
        
        
        #SHOW/HIDE Description text; All in one line (scrollable); formatting the text here formats it on output.
        #So, instead of horizontal scroll, all text could be displayed with line breaks. Which is better option?
        output$advanced4Description <- renderText({
            "This tab will create a graph that shows the change in how many times a specific word(s) is used thorughout each month."
        })
        
        #Whole description shows version
        #output$advanced1Description <- renderText({
        #"This tab will create a 
        #graph that shows the 25 
        #most used words, hashtags, 
        #or @'s, based on option selected."
        #       })
        
    )  
    
    #Show/hide description within sidebar (server side); tab 5
    observeEvent(
        shinyjs::onclick("toggleAdvanced5",
                         shinyjs::toggle(id = "advanced5", anim = TRUE)), 
        
        
        
        #SHOW/HIDE Description text; All in one line (scrollable); formatting the text here formats it on output.
        #So, instead of horizontal scroll, all text could be displayed with line breaks. Which is better option?
        output$advanced5Description <- renderText({
            "Once a word(s) is entered, TweetFinder will compile a list of tweets with that specific word(s)."
        })
        
        #Whole description shows version
        #output$advanced1Description <- renderText({
        #"This tab will create a 
        #graph that shows the 25 
        #most used words, hashtags, 
        #or @'s, based on option selected."
        #       })
        
    )  
    
    
        
    
    person.data<- reactive(
        {
            
            isolate({
                withProgress({
                    setProgress(message = "Processing tweets...")
                })
            })
            my_authorization <- rtweet::create_token(app = "Search Twitter",
                                                     consumer_key = "Nn5wKFpcHwn7udi0q1DShbRWl",
                                                     consumer_secret = "kSB6e5d5sLqPmR030ysbuj5os8e4svt8xRIxdhaeqEH9cT3lxK", 
                                                     access_token="1691818249007005696-CETmmt0GWIcVOM7QjD8k9xc2xJwIS3", 
                                                     access_secret = "xo8ZfOKKEpC25gWnJRoMJL3zXprPazlutRa5Frlpfq0E7",
                                                     set_renv=FALSE)
            
            #Error message if username isn't found.NOT WORKING. ERROR MESSAGE SHOWS UP ALL THE TIME
            #shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
             #   need(input$nameInput == FALSE, "The Username entered doesn't exist or doesn't have tweets associated with it. Please refresh the page and reenter a valid Username")
            #)
            
            #Success message if username IS found.
            #shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
             #   need(input$nameInput == TRUE, "Please refreseh page to enter a new username and begin a new search.")
            #
            
            
            datatweets <- rtweet::get_timeline(c({input$nameInput}), n = 1500, parse=T, token=my_authorization) #home  paramerter can get tweets user sees on their feed (made by the accounts they follow)
            rtweet::write_as_csv(datatweets, "./data/twitterdata.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            
            
            #WORKING ERORR MESSAGE #USE For all other features on homepage
            
            ###!!!--- Hard Coding some data in here for Demo Purposes ---!!!###
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "The Username entered doesn't exist or doesn't have tweets associated with it.")
            ))
            
            #Texts <- readtext::readtext("twitterdata.csv", text_field = "text")
            
            
            return(Texts$name%>%head(1))
            
        }
    )
    
    
    #Reset button action
    observeEvent(input$resetButton, {
        session$reload();
    })
    
    #MAYBE ADD THE MESSAGE TO REFRESH BEFORE INPUTTING NEW USERNAME IN THIS SEARCH BUTTON CALL 
    #MAke BUTTON TRIGGER LOADING SCREEN
    observeEvent(input$go, {
        output$person <- renderText({
            isolate(paste("User Name: ",person.data())) #ISOLATE IS SUPER IMPORTANT TO CREATE DEPENDENCY ON THE actionButton; meaning the function we call won't be updtated UNTIL actionButton is pressed 
        })})                                            #Also meaning, that our REACTIVE EXPRESSION won't take dependency on reactive objects that may change while output is being displalyed UNTIL actionButton is pressed
    
    observeEvent(input$go, {
        output$refreshmessage <- renderText({
            isolate("Refresh the page to input a new valid username!")
        })})
    
    
    #Important to add this (note the name change to the message output) to get error messages to work for different tabs (different instances)
    observeEvent(input$go, {
        output$emptyUsernameMessagego <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
   
    
 
    
    
    
    forTableOutput <- reactive(
        {
            #I'm assuming i should validate (for error message in all these Texts calls) 
            #EH I just validated that the username existed, not that we could actually pull.
            #SO if a user exists but doesn't have tweets, the error message that username doesn't exist isn't accurate
            #change error message to invalid username (doesn't exist or no tweets to pull)
            #All that matters is that we can pull tweets 
            
           
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "The Username entered doesn't exist or doesn't have tweets associated with it.")
                ))
            
            testing<-Texts%>%
                group_by(text) %>%
                summarize(text, favorite_count) %>%
                arrange(desc(favorite_count)) %>%
                mutate(likes = favorite_count)%>%
                mutate(tweets = text)%>%
                head(5)
            
            testing$n<- NULL
            testing$favorite_count<-NULL
            testing$text<- NULL
            
            
            return(testing)
        }
    )
    
    observeEvent(input$go, {
        output$checkingOutput <- renderTable({
            isolate((forTableOutput()))
        })})
    
    observeEvent(input$go, {
        output$text <- renderText({
            isolate("Top 5 Most Liked Tweets: ")
        })})
    
    GraphOutput <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            
            frequency.table<-word_tweets %>% 
                count(word, sort = TRUE)%>%
                mutate(cond1=substr(word,1,1)=="#")%>%
                mutate(cond2=substr(word,1,1)=="@")%>%
                filter(cond1=="FALSE" & cond2=="FALSE" )
            
            #frequency.table$word<-gsub("@\\w+ *", "", frequency.table$word)
            
            
            
            data.pop.words<-frequency.table%>%
                arrange(desc(n))%>%
                head(25)
            
            
            
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Top 25 Words in ", input$nameInput, "tweets"),
                         y="Word", x="Count") +
                    theme(legend.position = "none",
                          aspect.ratio=0.8) 
            )
        }
    )
    
    
    
    GraphHashtags<- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                filter(substr(word,1,1)=="#")%>%
                count(word, sort = TRUE)
            
            
            data.pop.words<-frequency.table%>%
                head(25)
            
            
            
            
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Most Used Hashtags in", input$nameInput, "Tweets"),
                         y="hastags", x="Count") +
                    theme(legend.position = "none")
            )
        }
    )
    
    GraphRates<- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            pop.words <- Texts %>%
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                filter(substr(word,1,1)=="@")%>%
                count(word, sort = TRUE)
            
            
            data.pop.words<-frequency.table%>%
                head(25)
            data.pop.words
            
            
            
            return(
                data.pop.words%>%
                    ggplot()+
                    geom_col(aes(y=reorder(word,-n), x= n, fill="#f5bc80"))+
                    labs(title=paste("Most Used @s in", input$nameInput, "Tweets"),
                         y="@s", x="Count") +
                    theme(legend.position = "none")
            )
        }
    )
    
    observeEvent(input$go1, {output$graph1 <- renderPlot({
        if (input$radio1=="Words"){
            isolate(GraphOutput())
        }
        else if(input$radio1=="Hashtags"){ isolate(GraphHashtags())}
        else if(input$radio1=="@s"){ isolate(GraphRates())}
        
    })   
    })
    
    observeEvent(input$go1, {
        output$emptyUsernameMessagego1 <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
    observeEvent(input$go1, {
        output$unclickedButtonMessagego1 <- renderText({
            isolate("Search button on Info page hasn't been clicked yet. If necessary, navigate to Info page (first option on sidebar) and click on Search button below inputted username to continue. ")
        })})
    
    wordcloudd<- reactive(
        {
            
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            pop.words <- Texts %>%                                                  
                mutate(Time = ymd_hms(created_at))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- pop.words %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                mutate(month=month(created_at))%>%
                filter(month==12)%>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            frequency.table<-word_tweets %>% 
                count(word, sort = TRUE)
            
           # setnames(frequency.table, "n", 'freq')
            
            
            
            return(wordcloud(words = frequency.table$word, freq = frequency.table$n, min.freq = 1, scale = c(2.25,.2), #adjust scale to make all words fit in wordlcloud
                             max.words=input$max, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
            
            #figPath = system.file("examples/t.png", package = "wordcloud2")
           
            #return(wordcloud2(frequency.table,  size = 1.5,color = "skyblue"))
        })
    
    observeEvent(input$go2, {
        output$plot <- renderPlot({ #renderwordcloud2 to switch wordcloud2
            isolate(wordcloudd())
        })})
    
    observeEvent(input$go2, {
        output$emptyUsernameMessagego2 <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
    observeEvent(input$go2, {
        output$unclickedButtonMessagego2 <- renderText({
            isolate("Search button on Info page hasn't been clicked yet. If necessary, navigate to Info page (first option on sidebar) and click on Search button below inputted username to continue. ")
        })})
    
    #FIX STATISTICS (month/ tweet count accuracy); #Now working; Figure out a way to make it so it AUTOMATICALLY recalculates based on current month and year 
    #DONE
    statistics <- reactive(
       
        {
            #Error message
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            currentmonth <- month((Sys.Date()))
            currentyear <- year(Sys.Date())
            
            Text <-  Texts %>%                                                  
                mutate(Time = ymd_hms(created_at))%>%
                mutate(month= month(created_at))%>%
                mutate(year = year(created_at)) %>%
                filter(year == currentyear) %>%
                filter(month == currentmonth)%>%
                filter(is_retweet == "FALSE")
            
            
            
            # rtweet::write_as_csv(tweets, "tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            # 
            # Texts <- readtext::readtext("tweets.csv", text_field = "text")
            
            #nameInput <- (input$nameInput)
            
            
            
            return(nrow(Text))
        }
    )
    observeEvent(input$go, {
        output$stat <- renderInfoBox({
            isolate(infoBox("Tweets this Month",statistics(),icon = icon("twitter"),
                            color = "light-blue", fill = TRUE))})
    })
    
    
    interaction <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            retweet= Texts %>%
                group_by(mentions_screen_name)%>%
                count(mentions_screen_name) %>%
                arrange(desc(n)) %>%
                mutate(case=mentions_screen_name=="")%>%
                filter(case=="FALSE")%>%
                head(1)
            
            retweet$n<- NULL
            retweet$case<-NULL
            
            
            
            return(retweet)
        }
    )
    observeEvent(input$go, {
        output$interactions <- renderInfoBox({
            isolate(infoBox("Most Interactions",interaction(),icon = icon("handshake"),
                            color = "red", fill = TRUE))})
    })
    
    
    source <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            popular_source <- Texts %>%
                group_by(source) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(source,n)%>%
                arrange(desc(n))%>%
                head(1)
            
            popular_source$n<-NULL
            
        
            
            return(popular_source)
        }
    )
    observeEvent(input$go, {
        output$sources <- renderInfoBox({
            isolate(infoBox("Most Twitter Usage From", source(),icon = icon("laptop"),
                            color = "green", fill = TRUE))})
    })
    
    
    Followers <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            Texts.Followcount= Texts %>%
                summarize(followers_count) %>%
                head(1)
            
            
            
            
            
            return(Texts.Followcount)
        }
    )
    observeEvent(input$go, {
        output$follower <- renderInfoBox({
            isolate(infoBox("Number of Followers ", Followers(),icon = icon("users"),
                            color = "navy", fill = TRUE))})
    })
    
    Tweets <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            #How many Tweets Out of 3000 something
            Texts.Tweets= Texts %>%
                filter(is_retweet==FALSE) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(n)
            
            #How many Retweets
            Texts.Retweets= Texts %>%
                filter(is_retweet==TRUE) %>%
                count(text) %>%
                count(sum(n)) %>%
                summarize(n)
            
            Tweet<- paste(Texts.Tweets, "VS", Texts.Retweets)
            
            
            
            return(Tweet)
        }
    )
    observeEvent(input$go, {
        output$tweetvsretweet <- renderInfoBox({
            isolate(infoBox("Tweets VS Retweets ", Tweets(),icon = icon("retweet"),
                            color = "purple", fill = TRUE))})
    })
    
    
    Time <- reactive(
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            
            Texts.Time<- Texts%>%
                mutate(Time = ymd_hms(created_at))%>%
                mutate(hour = hour(created_at))%>%
                group_by(hour)%>%
                count(hour) %>%
                arrange(desc(n))%>%
                head(1)
            
            Texts.Time$n<-NULL
            
            
            
            
            return(Texts.Time)
        }
    )
    observeEvent(input$go, {
        output$tweettime <- renderInfoBox({
            isolate(infoBox("Preferred Tweeting Time ", paste(Time(),":00", "-", Time()+1,":00"),icon = icon("clock"),
                            color = "yellow", fill = TRUE))})
    })
    
    
    ##############3TERRY 
    
    GraphOutput3= reactive(
        {      shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
            (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
            ))
        
        Text.sentiments<- Texts %>%
            mutate(year=year(created_at)) %>%
            group_by(month = month(created_at)) %>% 
            unnest_tokens(output=word, input =  text, token = "tweets")%>%
            count(word) %>%
            arrange(-n)%>%
            anti_join(stop_words, by = "word") %>%
            left_join(sentimentsToUse) %>%
            na.omit()
        
        
        collective.sentiment<-Text.sentiments %>%
            group_by(month)%>%
            summarize(total=mean(value*n)) %>%
            mutate(month=factor(month.abb[month],levels=month.abb))
        collective.sentiment
        
        collective.sentiment%>%
            ggplot()+
            geom_col(aes(y=reorder(month,total), x=total), fill="#f5bc80") +
            labs(title="Sentiments Over Time",
                 y="Month", x="Sentiment Value") +
            theme(plot.title = element_text(hjust = 0.5))
        
        }
    )
    
    observeEvent(input$showSentiments, {
        output$graph3 <- renderPlot({
            isolate(GraphOutput3()) })
        
    })
    
    observeEvent(input$showSentiments, {
        output$emptyUsernameMessageshowSentiments <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
    observeEvent(input$showSentiments, {
        output$unclickedButtonMessageshowSentiments <- renderText({
            isolate("Search button on Info page hasn't been clicked yet. If necessary, navigate to Info page (first option on sidebar) and click on Search button below inputted username to continue. ")
        })})
    
    OverTime <- reactive(
        {     shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
            (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
            ))
        
        word.over.time<-Texts%>%
            mutate(year=year(created_at))%>%
            mutate(month=month(created_at))%>%
            group_by(month)%>% 
            filter(year==input$year)%>%
            mutate(tweets.month=length(text)) %>%
            group_by(month,tweets.month)%>%
            unnest_tokens(output=word, input =  text, token = "tweets")%>%
            filter(!word %in% stop_words$word)%>%
            count(tweets.month,word, sort = TRUE)%>%
            mutate(usage.per.tweet=(n/tweets.month)*100)
        
        a <- input$wordInput
        b <- scan(text = a, sep = ",", what = "") #sep argument is important (should it be commma with space) # or let it be both ways by deleeting whitespace #comma still needed as separator
        
        #NEW TODO 11/6/2022 ADD ERROR MESSAGE FOR WORDS NOT FOUND & INVALID YEAR IN WORD USAGE OVER TIME  
        #conditionalPanel("word.over.time$year == NULL",)
                    
        
  
        
        return(word.over.time %>%
                   filter(word %in% tolower(b)) %>%
                   ggplot(aes(x=month, y= usage.per.tweet, color=word))+
                   geom_point()+
                   geom_line(size=1)+
                   labs(title="Words Usage Over Time",
                        y="Usage per 100 tweets", x="Month") +
                   theme(plot.title = element_text(hjust = 0.5))
              )
        
        
        
        }
    )
    
    observeEvent(input$go3,{
        output$graph2 <- renderPlot({
            isolate(OverTime()) })
    })
    
    observeEvent(input$go3, {
        output$emptyUsernameMessagego3 <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
    observeEvent(input$go3, {
        output$unclickedButtonMessagego3 <- renderText({
            isolate("Search button on Info page hasn't been clicked yet. If necessary, navigate to Info page (first option on sidebar) and click on Search button below inputted username to continue. ")
        })})
    
    
    #NEW TODO: 11/6/2022; ERROR MESSAGE FOR WORDS NOT FOUND (DONE); ADD CHECK FOR EMPTY INPUT THAT THROWS ERROR ON SEARCH CLICK IF INPUT IS EMPTY
    wordForTweets <- reactive( #should we allow the user to search for phrases? if so, redo the word_tweets aspect of this function which creates a table with all the words from the tweets in question
        {
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            # rtweet::write_as_csv(tweets, "tweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            # 
            # Texts <- readtext::readtext("tweets.csv", text_field = "text")
            
            #nameInput <- (input$nameInput)
            
            t.tweets<-(Texts %>%
                mutate(Time = ymd_hms(created_at)))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- t.tweets %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)
            
            
            
            #Terry's scanner
            scanner2 <- input$nameInputforTweets
            scanned2 <- scan(text = scanner2, sep = ",", what = "") #Could search for multiple words (OR - meaning if either word shows up in the search it'll show the tweet), separated by a comma (should it be separated by comma and space)
            
            
         
            
        
            
            TxttoDisplay <-  word_tweets %>%
                filter(word %in% tolower(scanned2)) %>% #needed tolower because the words in TxttoDisplay are lowercased so the input in our scanner must be converted to lowercase
                summarize(text)  #If text not found (then display error message): TODO! (AS OF 11/6/2022 NOT FINISHED)
            
            
            View(distinct(TxttoDisplay))
            
            if(nrow(TxttoDisplay) == 0){
              return("NO TWEETS ASSOCIATED WITH INPUTTED WORD. ") #WORKS; SPICE UP
            }else{
              return(distinct(TxttoDisplay)) #distinct to avoid outputting duplicates
            }
            
            
        }
    )
    
    observeEvent(input$gofindtweet, {
        output$tweetfindertext <- renderText({
            isolate((paste("Tweets by ", ((input$nameInput)), "containing the word(s): ", tolower(input$nameInputforTweets)))) 
        })})
    
    observeEvent(input$gofindtweet, {
        output$tweetfinder <- renderTable({
            isolate(wordForTweets()) })
    })
    
    observeEvent(input$gofindtweet, {
        output$unclickedButtonMessagegofindtweet <- renderText({
            isolate("Search button on Info page hasn't been clicked yet. If necessary, navigate to Info page (first option on sidebar) and click on Search button below inputted username to continue. ")
        })})
    
    #Important to add this (note the name change to the message output) to get error messages to work for different tabs (different instances)
    observeEvent(input$gofindtweet, {
        output$emptyUsernameMessagegofindtweet <- renderText({
            isolate("Username field is empty. Please refresh page and input a valid username to proceed.")
        })})
    
    
}



shinyApp(ui, server)
