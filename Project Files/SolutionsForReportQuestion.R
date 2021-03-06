#  Question:
# Using the `density` function and the beer data 
# (`abv.complete`, which are the alcohol by volume measures for the top 250 beers as rated by 
# BeerAdvocate.com) provided below, estimate a 95% pointwise bootstrapped percentile interval. 
# 
# Additionally, use the approximate variance function $\tilde{V}$ given by @Jann2007[p. 2] to 
# estimate a 95% pointwise confidence interval. Create plots using `bw.SJ` and `bw.nrd0`. 
# 
#
# run in LaTeX:
# $$\tilde{V}\left(\widehat{f_K}(x;h)\right) = 
# \frac{1}{nh}R(K) \widehat{f_K}(x;h) - \frac{1}{n} \widehat{f_K}(x;h)^2$$
# 


# data comes from the ABV of beer advocate's top 250 beers for 2016
# 2 beers have missing abv data
# quick R script to read in the .csv
library(RCurl)
git_file <- 
    getURL("https://raw.githubusercontent.com/bgstieber/Top250Beer/master/Top250Beers.csv")
beer_data <- read.csv(text = git_file, stringsAsFactors = FALSE)
abv.complete <- beer_data[!is.na(beer_data$ABV), ]$ABV

#hist(abv.complete, breaks = ceiling(1.5 * sqrt(length(abv.complete))))

# Solution 

##Bootstrap percentile interval


#function that computes the 95% pointwise percentile interval
boot_dens <- function(x, from, to, B, bw){
    #calculate original density
    orig_dens <- density(x, from = from, to = to, bw = bw)
    #initialize
    boot_dens <- matrix(0, ncol = B, nrow = length(orig_dens$x))
    #bootstrap iters
    for(i in 1:B){
        boot.x <- sample(x, length(x), replace = T)
        boot.density <- density(boot.x, from = from, to = to, bw = bw)
        boot_dens[,i] <- boot.density$y
    }
    #upper and lower quantiles
    lower <- apply(boot_dens, 1, function(mat) quantile(mat, .025))
    upper <- apply(boot_dens, 1, function(mat) quantile(mat, .975))
    #store in data.frame
    data.frame(grid = orig_dens$x,
               yhat = orig_dens$y,
               lower = lower,
               upper = upper)
    
}
#better plotting
library(ggplot2)
b_theme <- theme_bw() + theme(panel.grid = element_blank())
theme_set(b_theme)

range_abv <- range(abv.complete)
range_abv <- range_abv + c(-.5, .5) #wiggle room

set.seed(1984)

boot.abv.SJ <- boot_dens(x = abv.complete, from = range_abv[1],
                         to = range_abv[2], B = 1500, bw = 'SJ')

boot.abv.nrd0 <- boot_dens(x = abv.complete, from = range_abv[1],
                           to = range_abv[2], B = 1500, bw = 'nrd0')

ggplot(boot.abv.SJ, aes(x = grid))+
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = .2)+
    geom_line(aes(y = yhat),
              colour = '#b70101')+
    geom_rug(data = data.frame(abv.complete), 
             aes(x = abv.complete, y = 0.001),
             inherit.aes = FALSE, sides = 'b')+
    xlab('ABV')+ylab('Density')+
    ggtitle('ABV Density and 95% Boostrapped Percentile Interval',
            subtitle = 'Sheather - Jones Bandwidth')

ggplot(boot.abv.nrd0, aes(x = grid))+
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = .2)+
    geom_line(aes(y = yhat),
              colour = '#b70101')+
    geom_rug(data = data.frame(abv.complete), 
             aes(x = abv.complete, y = 0.001),
             inherit.aes = FALSE, sides = 'b')+
    xlab('ABV')+ylab('Density')+
    ggtitle('ABV Density and 95% Boostrapped Percentile Interval',
            subtitle = "Silverman's ROT Bandwidth")

## Approximate Confidence Interval


app_var <- function(x, bw){
    R <- 1 / (2 * sqrt(pi)) # assume gaussian
    h <- ifelse(bw == 'SJ', bw.SJ(x), bw.nrd0(x))
    n <- length(x)
    range_x <- range(x)
    range_x <- range_x + c(-.5, .5) #wiggle room
    dens_x <- density(x, from = range_x[1], to = range_x[2],
                      kernel = 'gaussian', bw = h)
    
    var.dens <- (1 / (n * h)) * R * dens_x$y - ((1 / n) * (dens_x$y) ^ 2)
    
    lower <- dens_x$y - (2 * sqrt(var.dens))
    upper <- dens_x$y + (2 * sqrt(var.dens))
    
    data.frame(grid = dens_x$x,
               yhat = dens_x$y,
               lower = lower,
               upper = upper)
}

app.abv.SJ <- app_var(x = abv.complete, bw = 'SJ')
app.abv.nrd0 <- app_var(x = abv.complete, bw = 'nrd0')

ggplot(app.abv.SJ, aes(x = grid))+
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = .2)+
    geom_line(aes(y = yhat), colour = '#b70101')+
    geom_rug(data = data.frame(abv.complete), 
             aes(x = abv.complete, y = 0.001),
             inherit.aes = FALSE, sides = 'b')+
    xlab('ABV')+
    ylab('Density')+
    ggtitle('ABV Density and 95% Pointwise CI',
            subtitle = 'Sheather - Jones Bandwidth')

ggplot(app.abv.nrd0, aes(x = grid))+
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = .2)+
    geom_line(aes(y = yhat), colour = '#b70101')+
    geom_rug(data = data.frame(abv.complete), 
             aes(x = abv.complete, y = 0.001),
             inherit.aes = FALSE, sides = 'b')+
    xlab('ABV')+
    ylab('Density')+
    ggtitle('ABV Density and 95% Pointwise CI',
            subtitle = "Silverman's ROT Bandwidth")