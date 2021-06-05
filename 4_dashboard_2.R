### 4: Dashboard 2

# load packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# load data
movies_horror <- read_csv(
  "data/movies_horror.csv",
  # not specifying col_types of "genre" columns in case more genres are added in the future
  col_types = cols(tconst = col_character(), primaryTitle = col_character(),
                   releaseYear = col_integer(), averageRating = col_double(),
                   numVotes = col_integer(), runtimeMinutes = col_integer(),
                   actorID = col_character(), actorName = col_character(),
                   actorBirthYear = col_integer(), actorDeathYear = col_integer(),
                   actorCategory = col_character(), actorAlive = col_integer(),
                   actorAge = col_integer(), directorID = col_character(),
                   directorName = col_character(), directorBirthYear = col_integer(),
                   directorDeathYear = col_integer(), directorAlive = col_integer(),
                   directorAge = col_integer())) %>%
  # keep only movies with a value for releaseYear
  filter(!is.na(releaseYear))

# ui
ui <- fluidPage(
  h1("The top 10 horror films by year", align = "center"), # title
  p("Use the slider below to select a year.", align = "center"),
  # slider to select year
  fluidRow(
    # rows are made up of 12 columns
    column(width = 6,
           offset = 3, # widget begins in column 4
           align = "center",
           sliderInput(
             inputId = "selectYear", # call using input$selectYear
             label = "Release year",
             ticks = FALSE, # don't show ticks and labels
             min = min(movies_horror$releaseYear),
             max = max(movies_horror$releaseYear),
             value = max(movies_horror$releaseYear), # default value is max year in dataset
             width = "100%", # use all available width
             sep = "" # no thousands separator because these are years
  ))),
  # checkbox to limit to films with at least 1,000 votes on IMDB
  fluidRow(
    column(width = 6,
           offset = 3,
           align = "center",
           checkboxInput(
             inputId = "at_least_1000", # call using input$at_least_1000
             label = "Only include films with at least 1,000 votes on IMDB?",
             value = FALSE, # unchecked by default
             width = "100%"
  ))),
  # display how many horror films were released in the selected year
  # UI can't access objects from server function (like nrow(data_horror()),
  # so we have to use renderText / textOutput)
  h4(textOutput("n_films"), align = "center"),
  # UI can't see input either, so again need to use renderText / textOutput
  h2(textOutput("top_10_title"), align = "center"), # title
  fluidRow(
    column(width = 8, # allow table to be wider
           offset = 2,
           align = "center",
           div(style = 'overflow-x: scroll', # allow table to scroll horizontally on small screens
               tableOutput("top_10_table")) # table
    )
  )
)

# server
server <- function(input, output, session) {
  
  # data: horror movies for selected year
  data_horror <- reactive(
    if (input$at_least_1000) {
      movies_horror %>%
        filter(numVotes >= 1000) %>%
        filter(releaseYear == input$selectYear)
    } else {
      movies_horror %>%
        filter(releaseYear == input$selectYear)
    }
  )
  
  # data: top 10 horror movies for selected year
  # note that ties are broken by the film with the most votes
  data_top10films <- reactive({
    data_horror() %>%
      # arrange by rating and number of votes
      arrange(desc(averageRating, numVotes)) %>%
      # pick top 10
      slice_head(n = 10)
  })
  
  # text: number of horror films in the year
  output$n_films <- renderText(
    if (input$at_least_1000) {
      paste0(
        "There were ",
        format(nrow(data_horror()), big.mark = ","),
        " horror films released in ",
        input$selectYear,
        " with at least 1,000 ratings on IMDB.")
    } else {
      paste0(
        "There were ",
        format(nrow(data_horror()), big.mark = ","),
        " horror films released in ",
        input$selectYear,
        ".")
    }
  )
  
  # table title
  output$top_10_title <- renderText(
    paste("Top 10 horror films of", input$selectYear)
  )
  
  # table: top 10 films for the year
  output$top_10_table <- renderTable(
    width = "100%", # use 100% of available width
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE, # bordered table
    na = "", # display nothing for NA values
    data_top10films() %>%
      # select columns of interest
      select(primaryTitle, averageRating, numVotes, runtimeMinutes,
             directorName, actorName) %>%
      rename(Title = primaryTitle, `IMDB rating` = averageRating,
             `IMDB votes` = numVotes, `Runtime (minutes)` = runtimeMinutes,
             Director = directorName, Actor = actorName) %>%
    mutate(`IMDB votes` = format(`IMDB votes`, big.mark = ","))) # add thousands separator
  
}

# run Shiny app
shinyApp(ui, server)
