### 2: Process datasets from IMDB ###

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# create folder for processed data
dir.create("data", showWarnings = FALSE)

# load data
movies <- read_tsv("raw/title.basics.tsv.gz", na = "\\N", quote = "",
                   col_types = cols_only(
                     tconst = col_character(),
                     titleType = col_character(),
                     primaryTitle = col_character(),
                     startYear = col_integer(),
                     runtimeMinutes = col_integer(),
                     genres = col_character()))
ratings <- read_tsv("raw/title.ratings.tsv.gz", na = "\\N", quote = "",
                    col_types = cols_only(
                      tconst = col_character(),
                      averageRating = col_double(),
                      numVotes = col_integer()))
names <- read_tsv("raw/name.basics.tsv.gz", na = "\\N", quote = "",
                   col_types = cols_only(
                     nconst = col_character(),
                     primaryName = col_character(),
                     birthYear = col_integer(),
                     deathYear = col_integer(),
                     primaryProfession = col_character()))
principals <- read_tsv("raw/title.principals.tsv.gz", na = "\\N", quote = "",
                       col_types = cols_only(
                         tconst = col_character(),
                         ordering = col_integer(),
                         nconst = col_character(),
                         category = col_character()))

# process data

## movies
movies <- movies %>%
  # we are only interested in movies
  filter(titleType == "movie") %>%
  # is it a horror movie?
  mutate(isHorror = ifelse(grepl("Horror", genres), 1, 0)) %>%
  # rename year column
  rename(releaseYear = startYear) %>%
  # keep only necessary columns
  select(tconst, primaryTitle, releaseYear,
         runtimeMinutes, genres, isHorror)

## join ratings to movies
movies <- ratings %>%
  # join with movies
  inner_join(
    movies,
    by = "tconst")

## subset just horror movies
movies_horror <- movies %>%
  filter(isHorror == 1) %>%
  select(-isHorror)

## extract actors from names
actors <- names %>%
  # we are only interested in actors/actresses
  filter(str_detect(primaryProfession, "actor|actress")) %>%
  # keep only necessary columns
  select(nconst, primaryName, birthYear, deathYear)

## extract directors from names
directors <- names %>%
  # we are only interested in directors
  filter(str_detect(primaryProfession, "director")) %>%
  # keep only necessary columns
  select(nconst, primaryName, birthYear, deathYear)

## extract actor/actress with first billing in each movie (horror movies only)
actors <- actors %>%
  # join to principals
  inner_join(
    principals %>%
      filter(str_detect(category, "actor|actress")),
    by = "nconst"
  ) %>%
  # filter to horror movies
  filter(tconst %in% movies_horror$tconst) %>%
  # get first actor/actress by order
    group_by(tconst) %>%
    filter(ordering == min(ordering)) %>%
    ungroup %>%
  # rename columns
  rename(
    actorName = primaryName, actorBirthYear = birthYear,
    actorDeathYear = deathYear, actorCategory = category,
    actorID = nconst) %>%
  # keep necessary columns
  select(actorID, tconst, actorName, actorBirthYear,
         actorDeathYear, actorCategory)

## extract director for each movie (horror movies only)
directors <- directors %>%
  # join to principals
  inner_join(
    principals %>%
      filter(str_detect(category, "director")),
    by = "nconst"
  ) %>%
  # filter to horror movies
  filter(tconst %in% movies_horror$tconst) %>%
  # get first actor/actress by order
  group_by(tconst) %>%
  filter(ordering == min(ordering)) %>%
  ungroup %>%
  # rename columns
  rename(
    directorName = primaryName, directorBirthYear = birthYear,
    directorDeathYear = deathYear, directorID = nconst) %>%
  # keep necessary columns
  select(directorID, tconst, directorName, directorBirthYear,
         directorDeathYear)

# create and write final datasets

## all movies - horror versus not horror
movies_all <- movies %>%
  # keep only necessary columns
  select(tconst, primaryTitle, releaseYear,
         averageRating, numVotes,
         runtimeMinutes, isHorror)
write.csv(movies_all, "data/movies_all.csv", row.names = FALSE)

## horror movies - lead actor/actress, director, detailed genre breakdown
movies_horror <- movies_horror %>%
  # join lead actor/actress
  left_join(
    actors,
    by = "tconst"
  ) %>%
  # calculate whether actor is alive (1) or dead (0) or no age info (NA)
  mutate(actorAlive = case_when(
    is.na(actorBirthYear) ~ NA_real_,
    is.na(actorDeathYear) ~ 1,
    TRUE ~ 0)) %>%
  # calculate actor age at time of release
  mutate(actorAge = releaseYear - actorBirthYear) %>%
  # join director
  left_join(
    directors,
    by = "tconst"
  ) %>%
  # calculate whether director is alive (1) or dead (0) or no age info (NA)
  mutate(directorAlive = case_when(
    is.na(directorBirthYear) ~ NA_real_,
    is.na(directorDeathYear) ~ 1,
    TRUE ~ 0)) %>%
  # calculate director age at time of release
  mutate(directorAge = releaseYear - directorBirthYear) %>%
  # calculate director age at time of release
  mutate(directorAge = releaseYear - directorBirthYear) %>%
  # indicator column for each genre
  separate_rows(genres, sep = ",") %>%
  mutate(yes = 1) %>%
  # one column for each genre - 1 (belongs to genre) / 0 (does not belong)
  pivot_wider(
    names_from = genres,
    values_from = yes,
    names_prefix = "genre_",
    values_fill = 0
  )
write.csv(movies_horror, "data/movies_horror.csv", row.names = FALSE)
