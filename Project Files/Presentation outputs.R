library(ggplot2)

bw.MS <- function(x) {3 * ((2 * sqrt(pi))^(-1) / (35 * length(x)))^(1/5) * sd(x)}

#generate sample data

#example 1 (mixture of normals)

set.seed(123)
x_1 <- c(rnorm(150), rnorm(150, mean = 4, sd = 2))
df_x <- data.frame(x = x_1)
n <- length(x_1)

from_1 <- min(x_1) - .5 * sd(x_1)
to_1 <- max(x_1) + .5 * sd(x_1)

h.MS <- bw.MS(x_1)
h.UCV <- bw.ucv(x_1)

dens.MS <- density(x_1, from = from_1, to = to_1,
                   bw = h.MS, n = 1000)

dens.UCV <- density(x_1, from = from_1, to = to_1,
                    bw = h.UCV, n = 1000)

#create data.frame

df_dens <- data.frame(h = rep(
    c(paste('MS:', round(h.MS, 2)),
      paste('UCV:', round(h.UCV, 2))),
    each = length(dens.MS$x)),
    x = c(dens.MS$x,
          dens.UCV$x),
    y = c(dens.MS$y, dens.UCV$y),
    stringsAsFactors = FALSE)

#make plot

p1 <- ggplot()+
    geom_histogram(data = df_x, aes(x = x, y = ..density..),
                   bins = 1.4 * ceiling(sqrt(n)),
                   fill = NA, colour = 'black')+
    geom_line(data = df_dens, aes(x = x, y = y),
              size = 1.2, colour = 'dodgerblue4')+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 14))+
    xlab('X')+ylab('Density')+
    facet_wrap(~h)


#another data set
library(MASS)

x_1 <- geyser$waiting
df_x <- data.frame(x = x_1)
n <- length(x_1)

from_1 <- min(x_1) - .5 * sd(x_1)
to_1 <- max(x_1) + .5 * sd(x_1)

h.MS <- bw.MS(x_1)
h.UCV <- bw.ucv(x_1)

dens.MS <- density(x_1, from = from_1, to = to_1,
                   bw = h.MS, n = 1000)

dens.UCV <- density(x_1, from = from_1, to = to_1,
                    bw = h.UCV, n = 1000)

#create data.frame

df_dens <- data.frame(h = rep(
    c(paste('MS:', round(h.MS, 2)),
      paste('UCV:', round(h.UCV, 2))),
    each = length(dens.MS$x)),
    x = c(dens.MS$x,
          dens.UCV$x),
    y = c(dens.MS$y, dens.UCV$y),
    stringsAsFactors = FALSE)

#make plot

p2 <- ggplot()+
    geom_histogram(data = df_x, aes(x = x, y = ..density..),
                   bins = 1.4 * ceiling(sqrt(n)),
                   fill = NA, colour = 'black')+
    geom_line(data = df_dens, aes(x = x, y = y),
              size = 1.2, colour = 'dodgerblue4')+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 14))+
    xlab('Waiting Time')+ylab('Density')+
    facet_wrap(~h)


#another data set
library(RCurl)
git_file <- 
    getURL("https://raw.githubusercontent.com/bgstieber/Top250Beer/master/Top250Beers.csv")
beer_data <- read.csv(text = git_file, stringsAsFactors = FALSE)
x_1 <- beer_data[!is.na(beer_data$ABV), ]$ABV

df_x <- data.frame(x = x_1)
n <- length(x_1)

from_1 <- min(x_1) - .1 * sd(x_1)
to_1 <- max(x_1) + .1 * sd(x_1)

h.MS <- bw.MS(x_1)
h.UCV <- bw.ucv(x_1)

dens.MS <- density(x_1, from = from_1, to = to_1,
                   bw = h.MS, n = 1000)

dens.UCV <- density(x_1, from = from_1, to = to_1,
                    bw = h.UCV, n = 1000)

#create data.frame

df_dens <- data.frame(h = rep(
    c(paste('MS:', round(h.MS, 2)),
      paste('UCV:', round(h.UCV, 2))),
    each = length(dens.MS$x)),
    x = c(dens.MS$x,
          dens.UCV$x),
    y = c(dens.MS$y, dens.UCV$y),
    stringsAsFactors = FALSE)

#make plot

p3 <- ggplot()+
    geom_histogram(data = df_x, aes(x = x, y = ..density..),
                   bins = 1.4 * ceiling(sqrt(n)),
                   fill = NA, colour = 'black')+
    geom_line(data = df_dens, aes(x = x, y = y),
              size = 1.2, colour = 'dodgerblue4')+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 14))+
    xlab('Alcohol by Volume (%)')+ylab('Density')+
    facet_wrap(~h)


par(mar=c(2, 1, 2, 1), mfrow=c(2,1),
   oma = c(2, 1, 2, 1))
set.seed(1)
n <- 10
kCenter <- rnorm(n)
x <- seq(-5, 5, length.out = 500)
f <- fhat <- rep(0, length(x))
h <- 1.1 * (n)^(-1/5)

plot(0,0, pch = '', xlab = '', ylab = '',
     xlim = c(-3, 3), ylim = c(0, .4),
     main = 'KDE with Large Bandwidth'
     )

for(i in 1:n){
    f <- dnorm((x - kCenter[i]) / h) * (1/(n * h))
    lines(x, f, col = 'blue')
    fhat <- fhat + f
}

lines(x, fhat)
points(kCenter, rep(0, n), col = 'red', pch = 'X')


# set.seed(1)
# n <- 10
# kCenter <- rnorm(12)
# x <- seq(-5, 5, length.out = 500)
f <- fhat <- rep(0, length(x))
h <- .5 * (n)^(-1/5)

plot(0,0, pch = '', xlab = '', ylab = '',
     xlim = c(-3, 3), ylim = c(0, .6),
     main = 'KDE with Small Bandwidth'
)

for(i in 1:n){
    f <- dnorm((x - kCenter[i]) / h) * (1/(n * h))
    lines(x, f, col = 'blue')
    fhat <- fhat + f
}

lines(x, fhat)
points(kCenter, rep(0, n), col = 'red', pch = 'X')


"
<!--
# Introduction

Kernel density estimation (KDE) for a univariate vector `x` can be very useful in exploratory analyses. We can calculate KDE as:

$$ \widehat{f_h}(x) = n^{-1} \sum_{i = 1}^{n}K\left(\frac{x - x_i}{h}\right) $$

$K$ is a kernel function (in our report we use the Gaussian kernel), and $h$ is the bandwidth. $h$ controls how much smoothing we apply in $\widehat{f}$. Large $h$ $\rightarrow$ more smoothing, small $h$ $\rightarrow$ less smoothing.

# Optimal bandwidth

The optimal bandwidth is calculated by minimizing the asymptotic mean integrated squared error (error = $\widehat{f} - f$):

$$ h_{AMISE} = \left(\frac{R(K)}{n\sigma_K^4R(f^{''})}\right)^{\frac{1}{5}} $$

Where $R(g) = \int g^2$, which is a measure of the roughness of a function. When $n$ or $R(f^{''})$ is large, we opt for less smoothing (smaller $h$). 

The only unknown in the expression for $h_{AMISE}$ is $R(f^{''})$. The potential choices for $h$ typically rely on some way to get around not knowing $R(f^{''})$.

-->
"