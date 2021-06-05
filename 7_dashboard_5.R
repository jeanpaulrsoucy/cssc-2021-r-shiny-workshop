### 7: Dashboard 5

# load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

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
ui <- dashboardPage(
  dashboardHeader(
    title = "Horror Film Data" # short title
  ),
  dashboardSidebar(collapsed = FALSE, # open by default
                   sidebarMenu(
                     id = "tabs", # input$tabs will give the name of the selected tab
                     menuItem(text = "Top 10s", # name of tab in the sidebar
                              tabName = "tab_top10", # how to refer to tab in the script
                              icon = icon("trophy"), # icon (by default from Font Awesome)
                              selected = TRUE), # this tab is open by default),
                     menuItem(text = "Trends",
                              tabName = "tab_trends",
                              icon = icon("chart-line")),
                     menuItem(text = "About",
                              tabName = "tab_about",
                              icon = icon("question-circle"))
                   )),
  dashboardBody(
    # use a theme by inserting the theme function in dashboardBody
    shinyDashboardThemes(theme = "poor_mans_flatly"),
    # tabs
    tabItems(
      tabItem(
        tabName = "tab_top10",
        fluidPage(
          p("Use the range slider below to select a range of years.", align = "center"),
          # slider to select range of years
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
                     # if you provide a vector of length 2, it turns the slider into a range slider
                     value = c(max(movies_horror$releaseYear) - 19, max(movies_horror$releaseYear)), # shows last 20 years by default
                     width = "100%", # use all available width
                     sep = "" # no thousands separator because these are years
                   ))),
          tabsetPanel(
            id = "tabset_top10", # input$tabset_top10 will return "value" of the active tab
            type = "tabs",
            tabPanel(
              title = "Top 10 films",
              value = "films",
              br(), # blank line for spacing
              # checkbox to limit to films with at least 1,000 votes on IMDB
              fluidRow(
                column(width = 6,
                       offset = 3,
                       align = "center",
                       checkboxInput(
                         inputId = "at_least_1000", # call using input$at_least_1000
                         label = "Only include films with at least 1,000 votes on IMDB?",
                         value = FALSE, # unchecked by default
                         width = "100%" # should change to max pixel width
                       ))),
              # display how many horror films were released in the selected year
              # UI can't access objects from server function (like nrow(data_horror()),
              # so we have to use renderText / textOutput)
              h4(textOutput("n_films"), align = "center"),
              # UI can't see input either, so again need to use renderText / textOutput
              h2(textOutput("top_10_title"), align = "center"), # title
              fluidRow(
                column(width = 8,
                       offset = 2,
                       align = "center",
                       div(style = 'overflow-x: scroll', # allow table to scroll horizontally on small screens
                           tableOutput("top_10_table")) # table
                )
              )
            ),
            tabPanel(
              title = "Top 10 directors",
              value = "directors",
              br(), # blank line for spacing
              uiOutput("slider_top10d"),
              fluidRow(
                column(width = 8,
                       offset = 2,
                       align = "center",
                       div(style = 'overflow-x: scroll', # allow table to scroll horizontally on small screens
                           tableOutput("top_10_d_table"))))),
            tabPanel(
              title = "Top 10 leading actors/actresses",
              value = "actors",
              br(), # blank line for spacing
              uiOutput("slider_top10a"),
              fluidRow(
                column(width = 8,
                       offset = 2,
                       align = "center",
                       div(style = 'overflow-x: scroll', # allow table to scroll horizontally on small screens
                           tableOutput("top_10_a_table")))))
          )
        )
      ),
      tabItem(
        tabName = "tab_trends",
        fluidPage(
          br(), # blank line for spacing
          fluidRow(
            column(
              width = 6,
              h3("Average rating", align = "center"),
              plotlyOutput("plot_rating"),
              h5(textOutput("text_rating"), align = "center"),
            ),
            column(
              width = 6,
              h3("Average runtime", align = "center"),
              plotlyOutput("plot_runtime"),
              h5(textOutput("text_runtime"), align = "center"),
            )
          ),
          fluidRow(
            column(
              width = 6,
              h3("Average age (director)", align = "center"),
              plotlyOutput("plot_d_age"),
              h5(textOutput("text_d_age"), align = "center"),
            ),
            column(
              width = 6,
              h3("Average age (lead)", align = "center"),
              plotlyOutput("plot_a_age"),
              h5(textOutput("text_a_age"), align = "center"),
            )
          )
        )
      ),
      # the content of the about tab is simply HTML
      tabItem(
        tabName = "tab_about",
        fluidPage(
          br(), # blank line for spacing
          # increase font size relative to normal
          HTML(
            "<p style = 'font-size: 130%'>Dashboard created by <a href='https://jeanpaulsoucy.com/' target='_blank'>Jean-Paul R. Soucy</a> for the <a href='https://ssc.ca/en/meetings/annual/2021-annual-meeting/student-conference' target='_blank'>Ninth Annual Canadian Statistics Student Conference</a> scientific workshop: <b>What makes R Shiny so shiny? A step-by-step introduction to interactive dashboards in R</b>.
          <br><br>Data courtesy of <a href='https://www.imdb.com/interfaces/' target='_blank'>IMDB</a> for personal and non-commercial use.
          <br><br>Dashboard code is available on <a href='https://github.com/jeanpaulrsoucy/cssc-2021-r-shiny-workshop' target='_blank'>GitHub</a>.
          </p>")
        )
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  # data: horror movies for selected year
  data_horror <- reactive({
    if (input$selectYear[1] == input$selectYear[2]) {
      dat <- movies_horror %>%
        filter(releaseYear == input$selectYear[1])
    } else {
      dat <- movies_horror %>%
        filter(releaseYear >= input$selectYear[1] & releaseYear <= input$selectYear[2])
    }
    if (input$at_least_1000) {
      dat <- dat %>%
        filter(numVotes >= 1000)
    }
    # return data
    dat
  })
  
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
  output$n_films <- renderText({
    if (input$selectYear[1] == input$selectYear[2]) {
      txt <- paste0(
        "There were ",
        format(nrow(data_horror()), big.mark = ","),
        " horror films released in ",
        input$selectYear[1])
    } else {
      txt <- paste0(
        "There were ",
        format(nrow(data_horror()), big.mark = ","),
        " horror films released between ",
        input$selectYear[1], # min year
        " and ",
        input$selectYear[2]) # max year
    }
    if (input$at_least_1000) {
      txt <- paste0(txt, " with at least 1,000 ratings on IMDB.")
    } else {
      txt <- paste0(txt, ".")
    }
    # output text
    txt
  })
  
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
  
  # data: top 10 directors/actors
  data_top10da <- reactive({
    
    # do not run until inputs are available
    # prevents "error" from briefly flashing on the dashboard when trying to
    # run a function before the input is available
    req(input$tabset_top10)
    
    # check which tab is active
    if (input$tabset_top10 == "directors") {
      col_da <- "directorName"
    } else if (input$tabset_top10 == "actors") {
      col_da <- "actorName"
    } else {
      # fail silently if input_top10 is defined but is not "directors" or "actors"
      # for example, if you are switching between the film and director tab
      # this silent failure will prevent an error message from flashing in the tab
      # (i.e., because col_da is not defined)
      stop("")
    }
    # get data
    dat <- data_horror()
    # remove missing director/lead actor
    dat <- dat[!is.na(dat[, col_da]), ]
    # keep relevant columns
    dat <- dat[, c(col_da, "averageRating", "numVotes")] # actor or director
    # summarize data
    dat <- dat %>%
      group_by(across(1)) %>% # group by first column (actor or director)
      # group_by(!!sym(col_da)) %>%
      # note that I've avoided using quasiquotation here
      # an equivalent statement would be:
      # group_by(!!sym(col_da)) %>%
      # read more: https://adv-r.hadley.nz/quasiquotation.html
      # summarize data
      summarize(totalFilms = n(),
                averageRating = mean(averageRating),
                totalVotes = sum(numVotes),
                .groups = "drop") %>%
      arrange(desc(averageRating))
  })
  
  # input slider: top 10 directors/actors
  # using renderUI so I can use the same slider in both the actor and director tab
  # since it server the same function in both
  slider_top10da <- reactive({
    # do not run until inputs are available
    # req(input$tabset_top10)
    # set ID for slider value
    if (input$tabset_top10 == "directors") {
      slider_id <- "minFilms_d"
    } else if (input$tabset_top10 == "actors") {
      slider_id <- "minFilms_a"
    }
    # get max value for slider
    dat <- data_top10da()
    films_max <- max(dat$totalFilms)
    # slider
    fluidRow(
      # rows are made up of 12 columns
      column(width = 6,
             offset = 3, # widget begins in column 4
             align = "center",
             sliderInput(
               inputId = slider_id,
               label = "Minimum number of films",
               ticks = FALSE, # don't show ticks and labels
               min = 1,
               max = films_max,
               step = 1, # move slider in integer increments
               value = 5, # default value is max year in dataset
               width = "100%", # use all available width
             )))
  })
  
  # table: top 10 directors/actors
  top_10_da_table <- reactive({
    # req(input$tabset_top10)
    # get min films value
    if (input$tabset_top10 == "directors") {
      slider_id <- "minFilms_d"
    } else if (input$tabset_top10 == "actors") {
      slider_id <- "minFilms_a"
    }
    # since slider_id is a variable, we can't do input$slider_id
    # instead, we use input[[slider_id]] to access the desired value
    # wait for min films value to be available
    req(input[[slider_id]])
    data_top10da() %>%
      filter(totalFilms >= input[[slider_id]]) %>%
      slice_head(n = 10) %>%
      # format numbers
      mutate(totalVotes = format(totalVotes, big.mark = ",")) %>% # add thousands separator
      # rename columns
      rename("Name" = 1, "Total films" = 2, "Average rating" = 3, "Total votes" = 4)
  })
  
  # director/actor tab elements
  output$top_10_d_table <- renderTable(
    width = "100%", # use 100% of available width
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE, # bordered table
    na = "", # display nothing for NA values
    # don't try to render when input$tabset_top10 is still "films"
    # this will briefly flash an error on the dashboard
    if (input$tabset_top10 == "directors") {
      top_10_da_table()
    })
  output$slider_top10d <- renderUI(
    # don't try to render when input$tabset_top10 is still "films"
    # this will briefly flash an error on the dashboard
    if (input$tabset_top10 == "directors") {
      slider_top10da()
    })
  output$top_10_a_table <- renderTable(
    width = "100%", # use 100% of available width
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE, # bordered table
    na = "", # display nothing for NA values
    # don't try to render when input$tabset_top10 is still "films"
    # this will briefly flash an error on the dashboard
    if (input$tabset_top10 == "actors") {
      top_10_da_table()
    })
  output$slider_top10a <- renderUI(
    # don't try to render when input$tabset_top10 is still "films"
    # this will briefly flash an error on the dashboard
    if (input$tabset_top10 == "actors") {
      slider_top10da()
    })
  
  # data: trend over time
  # no need to make reactive since it does not need to change in response to input
  # get movie data and filter out extremely long movies (> 4 hrs)
  # these movies throw off the scale of the runtime plot
  # don't exclude runtime = NA so that we can report how many are missing
  data_trend <- movies_horror %>%
    filter(runtimeMinutes <= 60 * 4 | is.na(runtimeMinutes))
  
  # plot: trend over time
  plot_trend <- function(y) {
    # y is the y variable for the plot (e.g., averageRating)
    # switch is an alternative to nested if statements
    lab_y <- switch(
      y,
      "averageRating" = "Average rating (IMDB)",
      "runtimeMinutes" = "Runtime (minutes)",
      "actorAge" = "Actor/actress age (years)",
      "directorAge" = "Director age (years)"
    )
    # variable for hover label
    text_var <- switch(
      y,
      "averageRating" = "primaryTitle",
      "runtimeMinutes" = "primaryTitle",
      "actorAge" = "actorName",
      "directorAge" = "directorName"
    )
    
    # generate trend plot
    p <- ggplot(data = data_trend, aes_string(x = "releaseYear", y = y)) +
      # text aes is used by plotly for the hover label
      # don't place text aes in ggplot() or geom_smooth() (it will break)
      geom_point(aes_string(text = text_var), colour = "grey", alpha = 0.1) + # make individual points very faint
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      labs(x = "Year", y = lab_y) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
      )
      # automatically convert to plotly
      ggplotly(p)
  }
  
  # trend plots
  output$plot_rating <- renderPlotly(plot_trend(y = "averageRating"))
  output$plot_runtime <- renderPlotly(plot_trend(y = "runtimeMinutes"))
  output$plot_d_age <- renderPlotly(plot_trend(y = "directorAge"))
  output$plot_a_age <- renderPlotly(plot_trend(y = "actorAge"))
  
  # text: how many missing values in trend plot?
  text_trend <- function(y) {
    lab_y <- switch(
      y,
      "averageRating" = "average rating",
      "runtimeMinutes" = "runtime",
      "actorAge" = "actor/actress age",
      "directorAge" = "director age"
    )
    # n total
    n_total <- nrow(data_trend)
    # n missing
    n_missing <- nrow(data_trend[is.na(data_trend[, y]), ])
    # output text
    paste0("There are ", format(n_missing, big.mark = ","),
           "/", format(n_total, big.mark = ","), " missing values for ",
           lab_y, ".")
  }
  
  # trend plot text
  output$text_rating <- renderText(text_trend(y = "averageRating"))
  output$text_runtime <- renderText(text_trend(y = "runtimeMinutes"))
  output$text_d_age <- renderText(text_trend(y = "directorAge"))
  output$text_a_age <- renderText(text_trend(y = "actorAge"))
  
}

# run Shiny app
shinyApp(ui, server)
