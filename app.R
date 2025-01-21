libraries <- c(
  "shiny", "shinydashboard", "lubridate", "tidytext", "stringr", "tidyverse", 
  "rtweet", "tm", "wordcloud", "RColorBrewer", "twitteR", "shinycustomloader", 
  "wordcloud2", "data.table", "shinyjs", "textdata" #,corpus
)

pacman::p_load(libraries)

sentimentsToUse <- read_csv("./data/afinnsentiments.csv")

header <- dashboardHeader(title = "Search Twitter")

sidebar<- dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(
        menuItem("Info", tabName = "model", icon = icon("user-plus")),
        
        menuItem(
          "Frequently Used Words",
          tabName = "used_words",
          icon = icon("clipboard-list"),
          
          radioButtons(
            inputId = "radio1",
            label = "Duration",
            choices = c("Words", "Hashtags", "@s")
          ),
          
          div(
            style = "display: inline-block; vertical-align: top; width: 100px;",
            actionButton("go1", "Search"),
            a(id = "toggleAdvanced1", "Show/hide description")
          ), # https://deanattali.com/2015/04/23/shinyjs-r-package/
          
          shinyjs::hidden(
            div(
              id = "advanced1",
              style = "display: block; vertical-align: top; width: 98.1%;",
              
              verbatimTextOutput("advanced1Description"),
              tags$style(
                "#advanced1Description {
                  color: black;
                  font-size: 15px;
                  font-family: Source Sans Pro;
                }"
              )
            )
          )
        ),
        
        menuItem(
          "Word Cloud",
          tabName = "cloud",
          icon = icon("cloud"),
          
          sliderInput(
            inputId = "max",
            label = "Maximum Number of Words:",
            min = 1, max = 300, value = 50
          ),
          
          div(
            style = "display: inline-block; vertical-align: top; width: 100px;",
            actionButton("go2", "Show"),
            a(id = "toggleAdvanced2", "Show/hide description")
          ),
          
          shinyjs::hidden(
            div(
              id = "advanced2",
              style = "display: block; vertical-align: top; width: 98.1%;",
              
              verbatimTextOutput("advanced2Description"),
              tags$style(
                "#advanced2Description {
                  color: black;
                  font-size: 15px;
                  font-family: Source Sans Pro;
                }"
              )
            )
          )
        ),
        
        menuItem(
          "Word Usage Over Time",
          tabName = "word_usage",
          icon = icon("hourglass-half"),
          
          textInput(
            inputId = "wordInput",
            label = "Enter word(s) (no space between commas): "
          ),
          
          textInput(
            inputId = "year",
            label = "Year: "
          ),
          
          div(
            style = "display: inline-block; vertical-align: top; width: 100px;",
            actionButton("go3", "Search"),
            a(id = "toggleAdvanced3", "Show/hide description")
          ),
          
          shinyjs::hidden(
            div(
              id = "advanced3",
              style = "display: block; vertical-align: top; width: 98.1%;",
              
              verbatimTextOutput("advanced3Description"),
              tags$style(
                "#advanced3Description {
                  color: black;
                  font-size: 15px;
                  font-family: Source Sans Pro;
                }"
              )
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
                
                conditionalPanel("input.go==1 && input.nameInput != ''", #USED CONDITIONAL PANEL TO GET LOADING SCREEN TO START AT BUTTON CLICK; SOURCE: https://stackoverflow.com/questions/50584066/shiny-apply-shinycustomerloader-after-pressing-actionbutton
                                 h1(strong(textOutput("person"))), 
                                 h3(id = "refreshmessageid", strong(textOutput("refreshmessage")))),
                textInput(inputId = "nameInput",
                          label = "Input Twitter ID: "),
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    actionButton("go", "Search")),
                actionButton("resetButton", "Reset"),
                conditionalPanel("input.go > 0 && input.nameInput != ''", 
                  fluidRow(
                      br(),
                      infoBoxOutput("follower"), 
                      infoBoxOutput("tweetvsretweet"),
                      infoBoxOutput("stat"),
                      infoBoxOutput("interactions"),infoBoxOutput("sources"),infoBoxOutput("tweettime")),
                  h2(textOutput(outputId = "text")),
                  h3(withLoader(tableOutput(outputId = "checkingOutput"), type="html",loader="pacman")),
                  fluidPage(
                      tags$head(tags$style("#checkingOutput{color: black;
                                   font-size: 15px;
                                   font-family: Source Sans Pro
                                   }")),
                      fluidRow(plotOutput(outputId ="graph1"),
                               plotOutput(outputId = "plot"), #change to wordcloud2Output if i switch to wordcloud2
                               plotOutput(outputId= "graph2"))
                  )
                ),
              
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
  
        tabItem(tabName = "AboutApp",
                h1(id = "app-heading", "Search Twitter"),
                tags$style(HTML("#app-heading { color: #008ABA; text-align: center; }")),
                
                h3(
                  "Search Twitter is an app that is able to show trends and various other information from an individual's tweets. 
          The app analyzes the 3,200 most recent tweets made by the Twitter user and outputs information ranging from 
          the user's most popular Tweets to the most used words in their tweets."
                ),
                
                h4(
                  "The data was extracted directly from Twitter’s database through a developer’s account. The rtweet package was used for corpus extraction.
          The corpus consists of tweets and other Twitter analytics of various Twitter users depending on the username we input. 
          We chose to go with the corpus obtained from Twitter’s database and not a CSV file, firstly because it is more convenient to 
          find information/data on not one single person but multiple Twitter users, and secondly because the corpus consists of many variables 
          compared to a CSV file from Kaggle.
      
          We did not have a single specific question when approaching this project but wanted to see the different types of information we could obtain 
          through the tweets of various individuals. For example, one of the things we wanted to find out was what certain Twitter users talk about the most. 
          Through our code and dataset, we are able to find the most used words and hashtags from various Twitter users, giving us information on the topics 
          they talk about and value the most. A possible new feature in this app could be providing a theme name based on the hashtags a user uses the most. 
          For example: Greta Thunberg’s theme can be Climate Speaker and Joe Biden’s theme could be Politician.
      
          If this app is used on a larger scale, the Tweet Finder can be used to check if a user whose account has been reported has tweets with explicit words. 
          The 'N word' surfaces around tweets every day, so we see this function having a practical use in real life.
      
          As of now, the function only supports a single word, but support for multiple words could be added in the Tweet Finder function.
      
          The limitations in our analysis are that our dataset only allows for the most recent 3,200 tweets from Twitter users. Thus, our data is more compact 
          and does not completely include every text from each Twitter user. Moreover, there may be some Twitter users who have fewer tweets published, and 
          therefore, the information that we obtain can be fairly insignificant."
                ),
                
                h4(
                  "In order to utilize the app, you will first need to enter the individual's Twitter Username (@username; without the @ symbol) in the box provided. 
          Please be patient, as it may take time for the app to output and render all of its information and visuals."
                ),
                
                h4(
                  "After inputting a Twitter username, you will see 6 widgets appear at the top of the app. These 6 widgets display 
          different information dependent on the Twitter User you choose to focus on."
                ),
                
                h4("The first box is explanatory, showing the number of Twitter followers that the selected Twitter user has."),
                
                h4(
                  "The following box, “Tweets Vs Retweets,” categorizes the Twitter User’s past 3,200 tweets as either being a Tweet 
          from themselves or a retweet from another user. The blue widget, “Tweets in (month),” will show how many tweets the 
          Twitter User has made in the current month."
                ),
                
                h4(
                  "The red widget, “Most Interaction,” references a Twitter User that is mentioned the most in your selected Twitter user’s timeline."
                ),
                
                h4(
                  "The green widget, “Most Twitter Usage From,” shows which platform the selected Twitter user posts most of their tweets from."
                ),
                
                h4(
                  "Finally, the yellow widget, “Preferred Tweeting Time,” displays, in military time, the hour during which they tweet the most."
                ),
                
                h4(
                  "When clicking on the first tab, 'Frequently Used Words,' you will be prompted with three options: 'words,' 'hashtags,' and '@'s.' 
          This tab will create a graph that shows the 25 most used words, hashtags, or @'s based on the option selected."
                ),
                
                h4(
                  "The 'Wordcloud' tab includes a slider to modify the max number of words displayed. The wordcloud visualizes the most 
          used words, hashtags, and @'s. The larger the word in the wordcloud, the more frequently it appears."
                ),
                
                h4(
                  "The 'Word Usage Over Time' tab displays a graph showing changes in how often a specific word is used throughout each month. 
          You can enter words separated by commas (no spaces) to analyze."
                ),
                
                h4(
                  "The 'Tweet Sentiments' tab shows a graph that categorizes the sentiment of the words used in a user's tweets as positive or negative."
                ),
                
                h4(
                  "The 'TweetFinder' tab lets you input a word and compiles a list of tweets containing that word."
                ),
                
                h2(id = "note-heading", "Important Notes:"),
                tags$style(HTML("#note-heading { color: red; }")),
                
                h4("• To save rendering time, you must reload the app when entering a new username."),
                h4("• Errors occur if the inputted Twitter username doesn't exist, is misspelled, or has no tweets."),
                h4(
                  "• Some graphs, such as the 'Word Usage Over Time' and 'Sentiment Graph,' may not cover all months due to data limitations."
                ),
                
                h3("Designed by:"),
                tag("a", list(href = "http://www.linkedin.com/in/hedavam-solano/", "Hedavam Solano, ")),
                tag("a", list(href = "https://www.instagram.com/terryppark/", "Terry Park, ")),
                tag("a", list(href = "https://twitter.com/AnweshanAdhika3", "Anweshan Adhikari"))
        )
  )
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
        output$advanced1Description <- renderText({
                        "This tab will create a graph that shows the 25 most used words, hashtags, or @'s, based on the option selected."
                     })
        
    )    
  
    #Show/hide description within sidebar (server side); tab 2
    observeEvent(
        shinyjs::onclick("toggleAdvanced2",
                         shinyjs::toggle(id = "advanced2", anim = TRUE)), 
        
        output$advanced2Description <- renderText({
            "This tab will create a visual of the user's most used words, hashtags, and @'s. The bigger the word, the higher its usage."
        })
    )   
    
    #Show/hide description within sidebar (server side); tab 3
    observeEvent(
        shinyjs::onclick("toggleAdvanced3",
                         shinyjs::toggle(id = "advanced3", anim = TRUE)), 

        output$advanced3Description <- renderText({
            "This tab will create a graph that shows the change in how many times a specific word(s) is used thorughout each month."
        })
    )  
    
    #Show/hide description within sidebar (server side); tab 4
    observeEvent(
        shinyjs::onclick("toggleAdvanced4",
                         shinyjs::toggle(id = "advanced4", anim = TRUE)), 
        
        output$advanced4Description <- renderText({
            "This tab will create a graph that shows the change in how many times a specific word(s) is used thorughout each month."
        })
    )  
    
    #Show/hide description within sidebar (server side); tab 5
    observeEvent(
        shinyjs::onclick("toggleAdvanced5",
                         shinyjs::toggle(id = "advanced5", anim = TRUE)), 

        output$advanced5Description <- renderText({
            "Once a word(s) is entered, TweetFinder will compile a list of tweets with that specific word(s)."
        })
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
  
            datatweets <- rtweet::get_timeline(c({input$nameInput}), n = 1500, parse=T, token=my_authorization) #home  paramerter can get tweets user sees on their feed (made by the accounts they follow)
            rtweet::write_as_csv(datatweets, "./data/twitterdata.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
            
            ###!!!--- Hard Coding some data in here for Demo Purposes ---!!!###
            shiny::validate( #use shiny:: before validate call so it doesn't confuse with another library
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "The Username entered doesn't exist or doesn't have tweets associated with it.")
            ))
            
            #Texts <- readtext::readtext("twitterdata.csv", text_field = "text") --- Remnant from when we had Twitter Data
            
            return(Texts$name%>%head(1))
            
        }
    )
    
    #Reset button action
    observeEvent(input$resetButton, {
        session$reload();
    })
    
    observeEvent(input$go, {
        output$person <- renderText({
            isolate(paste("User Name: ",person.data())) #isolate() needed to create dependency on actionButton
        })})                                           
    
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
            shiny::validate(
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
            shiny::validate(
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
                filter(cond1=="FALSE" & cond2=="FALSE")

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
            shiny::validate(
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
            shiny::validate(
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
    
    observeEvent(input$go1, {output$graph1 <- 
      renderPlot({
        if (input$radio1=="Words"){
            isolate(GraphOutput())
        }
        else if(input$radio1=="Hashtags"){isolate(GraphHashtags())}
        else if(input$radio1=="@s"){isolate(GraphRates())}
        
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
            shiny::validate(
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
            
            return(wordcloud(words = frequency.table$word, freq = frequency.table$n, min.freq = 1, scale = c(2.25,.2), #adjust scale to make all words fit in wordlcloud
                             max.words=input$max, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
        })
    
    observeEvent(input$go2, {
        output$plot <- renderPlot({
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
            shiny::validate(
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
            shiny::validate(
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
            shiny::validate(
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
            shiny::validate( 
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            #How many Tweets
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
            shiny::validate(
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
  
    GraphOutput3= reactive(
        {      shiny::validate(
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
        {shiny::validate( 
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
        b <- scan(text = a, sep = ",", what = "")
        
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
    
    
    wordForTweets <- reactive(
        {
            shiny::validate(
                (need(try(Texts <- readtext::readtext("./data/biden_tweets.csv", text_field = "text")), "Can't load.")
                ))
            
            t.tweets<-(Texts %>%
                mutate(Time = ymd_hms(created_at)))
            
            remove_reg <- "&amp;|&lt;|&gt;"
            
            word_tweets <- t.tweets %>%
                mutate(Tweet = str_remove_all(text, remove_reg)) %>%
                unnest_tokens(output=word, input = Tweet, token = "tweets") %>%
                filter(!word %in% stop_words$word)

            
            scanner2 <- input$nameInputforTweets
            scanned2 <- scan(text = scanner2, sep = ",", what = "") #Could search for multiple words (OR - meaning if either word shows up in the search it'll show the tweet), separated by a comma
            
            TxttoDisplay <-  word_tweets %>%
                filter(word %in% tolower(scanned2)) %>% #needed tolower because the words in TxttoDisplay are lowercased so the input in our scanner must be converted to lowercase
                summarize(text)  
            
            if(nrow(TxttoDisplay) == 0){
              return("NO TWEETS ASSOCIATED WITH INPUTTED WORD. ") #WORKS; SPICE UP
            } else{
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
