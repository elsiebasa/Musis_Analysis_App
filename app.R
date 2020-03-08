#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidytext)
library(textdata)
library(lubridate)
library(httr)
library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
get_info <- function(artist, title){
  fromJSON((
    str_glue("https://api.vagalume.com.br/search.php?apikey=114e5edf0076c875c605607fa7b82eec&art={artist}&mus={title}&extra=relart,artpic" , artist = str_replace_all(artist , c(" " = "%20")),
             title = str_replace_all(title, c(" " = "%20"))
    ))
  )
}
# sentiment analysis


ui <- fluidPage(
  #suppress warings while wating for inputs
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  # Application title
  titlePanel("Music Lyrics App"),
  
  
  sidebarLayout(
    sidebarPanel(
      textInput("artist","artist", width = NULL,
                placeholder = "e.g Justin Bieber"
      ),
      textInput("song","song", width = NULL,
                placeholder = "e.g Baby"
      ),
      checkboxGroupInput("checkboxes", "Choose:",
                         choiceNames =
                           list("Sentiment Analysis Chart"),
                         choiceValues =
                           list("sentiment")
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(column(10, verbatimTextOutput("lyric"))),
      plotOutput("pie")
      
    )
  )
  
)


server <- function(input, output) {
  
  options(warn = -1) 
  
  output$lyric <- renderText({ 
    lyric <- get_info(input$artist, input$song)$mus$text
    
  })
  
  sentiment_analysis <-  reactive({
    lines<- get_info(input$artist, input$song)$mus$text %>%
      str_split("\n") %>% 
      unlist() 
    lyric_df<- tibble(line = 1:length(lines), lines)
    
    token <-  lyric_df %>% unnest_tokens(word, lines) %>%
      anti_join(stop_words)
    
    
    sentiment <- token %>% left_join(get_sentiments("nrc")) %>% 
      filter(sentiment %in% c("positive", "negative" )) %>% 
      group_by(line) %>% 
      summarize(
        positive = sum(sentiment == "positive", na.rm = TRUE), 
        negative = sum(sentiment == "negative", na.rm = TRUE), 
        netural = n() - positive - negative) %>%
      mutate(
        line,
        sentiment = case_when(
          positive > negative ~ "positive",
          positive < negative ~ "negative",
          TRUE ~ "netural"
        )
      )
    sentiment %>% count(sentiment)
    
  })
  output$pie <- renderPlot({ 
    if ("sentiment" %in% input$checkboxes) {
      df_pie = sentiment_analysis()
      bp<- ggplot( df_pie, aes(x="", y=n, fill=sentiment)) +geom_bar(width = 1, stat = "identity")
      pie <- bp + coord_polar("y", start=0)
      pie
    }
    
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



