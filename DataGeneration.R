###script for data generation


##data for report

set.seed(123)

#N(2,2)
x_0 <- rnorm(300, mean = 2, sd = 2)

#normal mixture
x_1 <- c(rnorm(150), rnorm(150, mean = 4, sd = 2))

#mixture of three gamma's
#each gamma has E(X) = 20
x_2 <- c(rgamma(100, shape = 1, scale = 20),
         rgamma(100, shape = 4, scale = 5),
         rgamma(100, shape = 20, scale = 1))

#old faithful data
library(MASS)
x_3 <- geyser$waiting #Azzalini, Bowman (1990)


##data for presentation

#normal mixture

set.seed(123)
x_1 <- c(rnorm(150), rnorm(150, mean = 4, sd = 2))

#beer data

library(RCurl)
git_file <- 
    getURL("https://raw.githubusercontent.com/bgstieber/Top250Beer/master/Top250Beers.csv")
beer_data <- read.csv(text = git_file, stringsAsFactors = FALSE)
x_1 <- beer_data[!is.na(beer_data$ABV), ]$ABV

##misc. (not used but is still interesting)

#avg. driving distance for pga tour 1986, 1996, and 2015

library(rvest)
pga1986 <- read_html('http://www.pgatour.com/stats/stat.101.1986.html')
pga1996 <- read_html('http://www.pgatour.com/stats/stat.101.1996.html')
pga2015 <- read_html('http://www.pgatour.com/stats/stat.101.2015.html')

dd1986 <- pga1986 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd1996 <- pga1996 %>% 
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd2015 <- pga2015 %>%
    html_nodes('table') %>%
    .[2] %>%
    html_table(., header = TRUE)

dd <- c(dd1986[[1]]$AVG., dd1996[[1]]$AVG., dd2015[[1]]$AVG.)
