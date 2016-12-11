Knorm <- function(u) dnorm(u)
kde <- function (x, grid = 512, give_dens = FALSE) 
{
    require(dplyr)
    require(ggplot2)
    
    n <- length(x)
    
    h.SROT = 0.9 * min(c(IQR(x) / 1.34, sd(x))) * (n)^(-1/5)
    h.LSCV = bw.ucv(x, lower = .025 * h.SROT, upper = 1.25 * h.SROT)
    h.SJ = bw.SJ(x)
    h.MS = 3 * ((2 * sqrt(pi))^(-1) / (35 * n))^(1/5) * sd(x)
    h_vec <- c(h.SROT, h.LSCV, h.SJ, h.MS)
    
    max_h <- max(h_vec)
    low_grid <- floor(min(x) - max_h * 3)
    high_grid <- ceiling(max(x) + max_h * 3)
    
    
    print(c('LS CV' = round(h.LSCV, 3),
            'T MS' = round(h.MS, 3),
            'SJ BW' = round(h.SJ, 3),
            "Silverman's ROT" = round(h.SROT, 3)))
    
    
    if(n > 1000){
        message(paste0('Note: x too large (n = ', n, 
                       ' > 1000). Using `density` instead'))
        
        all_dens <- do.call('cbind', 
                            lapply(h_vec, function(bw)
                                density(x = x, from = low_grid, to = high_grid,
                                        bw= bw)$y))
        
        
        dens_SNR <- density(x, bw = h.SNR,
                            from = low_grid,
                            to = high_grid)
        
        sum_results <- data.frame(
            X_grid = dens_SNR$x,
            kde_hSROT = all_dens[,1],
            kde_hSJ = all_dens[,3],
            kde_hLSCV = all_dens[,2],
            kde_hMS = all_dens[,4]
        )
        
    }else{
        
        grid_vals <- seq(low_grid, high_grid, length.out = grid)
        
        grid_df <- expand.grid(x = x, grid = grid_vals)
        
        
        
        newx.hsrot <- (grid_df[,2] - grid_df[,1]) / h.SROT
        newx.hsj <- (grid_df[,2] - grid_df[,1]) / h.SJ
        newx.hLSCV <- (grid_df[,2] - grid_df[,1]) / h.LSCV
        newx.hMS <- (grid_df[,2] - grid_df[,1]) / h.MS
        
        grid_df[,3] <- (1 / (n * h.SROT)) * Knorm(newx.hsrot)
        grid_df[,4] <- (1 / (n * h.SJ)) * Knorm(newx.hsj)
        grid_df[,5] <- (1 / (n * h.LSCV)) * Knorm(newx.hLSCV)
        grid_df[,6] <- (1 / (n * h.MS)) * Knorm(newx.hMS)
        
        
        names(grid_df) <- c(
            "X_act", 
            "X_grid", 
            'kde_hSROT',
            'kde_hSJ',
            'kde_hLSCV',
            'kde_hMS'
        )
        
        
        sum_results <- 
            grid_df %>%
            group_by(X_grid) %>%
            summarise(kde_hSROT = sum(kde_hSROT),
                      kde_hSJ = sum(kde_hSJ),
                      kde_hLSCV = sum(kde_hLSCV),
                      kde_hMS = sum(kde_hMS))
    }
    
    
    p1 <- ggplot()+
        geom_histogram(data = NULL, aes(x = x, y = ..density..),
                       bins = 1.4 * ceiling(sqrt(n)),
                       fill = NA, colour = 'black')+
        geom_line(data = sum_results, aes(x = X_grid, y = kde_hSROT,
                                          colour = paste('SROT:', round(h.SROT, 2))))+
        geom_line(data = sum_results, aes(x = X_grid, y = kde_hSJ,
                                          colour = paste('SJ:', round(h.SJ, 2))))+
        geom_line(data = sum_results, aes(x = X_grid, y = kde_hLSCV,
                                          colour = paste('LSCV:', round(h.LSCV, 2)))) +
        geom_line(data = sum_results, aes(x = X_grid, y = kde_hMS,
                                          colour = paste('MS:', round(h.MS, 2))))+
        xlab('X') + ylab('Density')+
        theme_bw()+
        theme(panel.grid = element_blank())+
        scale_colour_brewer(palette = 'Set1',
                            name = 'Bandwidth')
    plot(p1)
    
    if(give_dens){
        return(sum_results)
    }
}

# 
# set.seed(123)
# 
# x_0 <- rnorm(300, mean = 2, sd = 2)
# kde(x_0)
# x_1 <- c(rnorm(150), rnorm(150, mean = 4, sd = 2))
# kde(x_1)
# x_2 <- c(rgamma(100, shape = 1, scale = 20),
#        rgamma(100, shape = 4, scale = 5),
#        rgamma(100, shape = 20, scale = 1))
# kde(x_2)
# library(MASS)
# x_3 <- geyser$waiting #Azzalini, Bowman (1990)
# kde(x_3)
# 
# set.seed(123)
# x_4 <- c(rnorm(160), rnorm(95, mean = -3, sd = sqrt(3/4)),
#          rnorm(45, mean = 4, sd = sqrt(1/2)))
# 
# 
# h.MS <- function(x) {3 * ((2 * sqrt(pi))^(-1) / (35 * length(x)))^(1/5) * sd(x)}
# 
# set.seed(123)
# x_1 <- c(rnorm(150), rnorm(150, mean = 4, sd = 2))
# 
# df1 <- data.frame(x = x_1, group = rep(c(1,2), each = 150))
# 
# dens_x_ucv <- density(df1$x, bw = 'ucv')
# dens_x_ms <- density(df1$x, bw = h.MS(df1$x))
# 
# df_dens <- data.frame(
#     h = rep(c('CV','MS'), each = length(dens_x_ms$x)),
#     x = rep(dens_x_ms$x, 2),
#     y = c(dens_x_ucv$y, dens_x_ms$y),
#     stringsAsFactors = FALSE
# )
# 
# 
# 
# ggplot()+
#     geom_histogram(data = df1, 
#                    aes(x = x, y = ..density..),
#                    bins = ceiling(1.4 * sqrt(300)),
#                    alpha = .6)+
#     theme_bw()+
#     theme(legend.position = 'none',
#           panel.grid = element_blank())+
#     geom_line(data = NULL, aes(x = dens_x_ucv$x, y = dens_x_ucv$y, group = 1),
#               inherit.aes = FALSE)
