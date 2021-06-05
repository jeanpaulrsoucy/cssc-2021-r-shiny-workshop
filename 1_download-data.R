### 1: Download dataset from IMDB ###

# dataset documentation can be found here: https://www.imdb.com/interfaces/

# load packages
library(curl)

# create folder for raw data
dir.create("raw", showWarnings = FALSE)

# download data files from IMDB
curl_download("https://datasets.imdbws.com/name.basics.tsv.gz", "raw/name.basics.tsv.gz")
curl_download("https://datasets.imdbws.com/title.basics.tsv.gz", "raw/title.basics.tsv.gz")
curl_download("https://datasets.imdbws.com/title.principals.tsv.gz", "raw/title.principals.tsv.gz")
curl_download("https://datasets.imdbws.com/title.ratings.tsv.gz", "raw/title.ratings.tsv.gz")
