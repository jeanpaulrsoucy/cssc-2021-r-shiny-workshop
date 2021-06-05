### 3: Dashboard 1

# load packages
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# load data
movies_all <- read_csv("data/movies_all.csv",
                       col_types = "ccidiif") %>%
  # label isHorror factor
  mutate(isHorror = factor(isHorror,
                           levels = c(0, 1),
                           labels = c("Not horror", "Horror")))

# ui
ui <- fluidPage(
  h1("Horror films versus non-horror films", align = "center"), # main title
  h2("Average IMDB rating by year", align = "center"), # plot title
  plotOutput("plot_rating"), # first plot
  h2("Average runtime by year", align = "center"), # plot title
  plotOutput("plot_runtime") # second plot
)

# server
server <- function(input, output, session) {
  
  # ggplot: average rating by year for horror films and non-horror films
  output$plot_rating <- renderPlot({
    ggplot(data = movies_all,
           aes(x = releaseYear, y = averageRating,
               group = isHorror, colour = isHorror)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    labs(x = "Year", y = "Average rating", colour = "Genre") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 13),
    )
  })
  
  # ggplot: average runtime by year for horror films and non-horror films
  output$plot_runtime <- renderPlot({
    ggplot(data = movies_all,
           aes(x = releaseYear, y = runtimeMinutes,
               group = isHorror, colour = isHorror)) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      labs(x = "Year", y = "Runtime (minutes)", colour = "Genre") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
      )
  })
  
}

# run Shiny app
shinyApp(ui, server)
