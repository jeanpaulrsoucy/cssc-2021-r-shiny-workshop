### 0: Install packages required to run the rest of the scripts ###

# update all installed packages (warning: will not ask user about updating)
update.packages(ask = FALSE)

# install packages required to run the dashboards
# only installs those not already installed
pkg_1 <- c("shiny", "shinydashboard", "dashboardthemes",
           "readr", "dplyr", "ggplot2", "plotly")
pkg_1 <- pkg_1[!(pkg_1 %in% installed.packages()[ , "Package"])]
if (length(pkg_1 > 0)) install.packages(pkg_1)

# optional: packages for downloading and processing data (scripts 1 & 2)
# only installs those not already installed
pkg_2 <- c("curl", "tidyr", "stringr")
pkg_2 <- pkg_2[!(pkg_2 %in% installed.packages()[ , "Package"])]
if (length(pkg_2 > 0)) install.packages(pkg_2)
