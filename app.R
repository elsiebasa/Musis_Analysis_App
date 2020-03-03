#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(httr)
library(tidyverse)
library(jsonlite)
library(shiny)
get_info <- function(artist, title){
  fromJSON((
    str_glue("https://api.vagalume.com.br/search.php?apikey=114e5edf0076c875c605607fa7b82eec&art={artist}&mus={title}&extra=relart,artpic" , artist = str_replace_all(artist , c(" " = "%20")),
             title = str_replace_all(title, c(" " = "%20"))
    ))
  )
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Music Lyrics App"),
   

   sidebarLayout(
      sidebarPanel(
        textInput("artist","artist", width = NULL,
                  placeholder = "e.g Justin Bieber"
        ),
        textInput("song","song", width = NULL,
                  placeholder = "e.g Baby"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(

         fluidRow(column(10, verbatimTextOutput("lyric")))
         
      )
   )
)


server <- function(input, output) {
   



   
   output$lyric <- renderText({ 
     lyrics <- get_info(input$artist, input$song)$mus$text
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

