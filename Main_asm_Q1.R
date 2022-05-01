library(ggplot2)
library(jsonlite)
library(plyr)
library(dplyr)
library(RColorBrewer)
library('MCMCpack')

# loading data

df <- read.csv('analysis.csv')

head(df,5)

# Question 1.a

players_df <- filter(df, df$White.Player == 'Anand, Viswanathan'|
                       df$White.Player == 'Carlsen, Magnus')

# checking for missing values
print(is.na(players_df))

Players_perf <- players_df[c('White.Player','White.ACPL')]
head(Players_perf,3)
colnames(Players_perf) = c('Player','ACPL')

Players_perf$Player <- factor(Players_perf$Player)
Players_perf$ACPL <-as.numeric(Players_perf$ACPL)

dim(Players_perf)

# box plot of players
ggplot(Players_perf) + geom_boxplot(aes(Player, ACPL, fill = Player)) +
  geom_jitter(aes(Player, ACPL, shape = Player))+ 
  scale_fill_manual(values=c("yellow", "blue"))

t.test(ACPL ~ Player, data = Players_perf, var.equal=TRUE)

AV_ACPL <- as.numeric(unlist(select(filter(Players_perf,Player=='Anand, Viswanathan'),c('ACPL'))))
CM_ACPL <- as.numeric(unlist(select(filter(Players_perf,Player=='Carlsen, Magnus'),c('ACPL'))))

# mean
cat('Anand, viswanathan', mean(AV_ACPL))
cat(' Carlsen, Magnus', mean(CM_ACPL))

# median
cat('Anand, viswanathan', median(AV_ACPL))
cat(' Carlsen, Magnus', median(CM_ACPL))

# std. dev
cat('Anand, viswanathan', sd(AV_ACPL))
cat(' Carlsen, Magnus', sd(CM_ACPL))

# t-test

t.test(ACPL ~ Player, data = Players_perf, var.equal=TRUE)

mean_diff <- mean(AV_ACPL) - mean(CM_ACPL)
mean_diff

# Gibbs sampler
compare_gibbs <- function(data, ind, mu0 = 85, tau0 = 1/25, del0 = 0, gamma0 = 1/25, a0 = 289, b0 = 3.4, num_iter = 5000)
{
  y1 <- data[ind == 1]
  y2 <- data[ind == 2]
  n1 <- length(y1)
  n2 <- length(y2)
 
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  mat <- matrix(0, nrow = num_iter, ncol = 3)

  an <- a0 + (n1 + n2)/2
  for(s in 1 : num_iter)
  {

    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    

    tau_n <- tau0 + tau * (n1 + n2)
    mu_n <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / tau_n
    mu <- rnorm(1, mu_n, sqrt(1/tau_n))

    gamma_n <- tau0 + tau*(n1 + n2)
    del_n <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamma_n
    del <-rnorm(1, del_n, sqrt(1/gamma_n))

    mat[s, ] <- c(mu, del, tau)
  }
  colnames(mat) <- c("mu", "del", "tau")
  return(mat)
}

model_gibbs <- compare_gibbs(Players_perf$ACPL, as.factor(Players_perf$Player))
plot(as.mcmc(model_gibbs))

raftery.diag(as.mcmc(model_gibbs))


head(model_gibbs,10)

apply(model_gibbs, 2, mean)

apply(model_gibbs, 2, sd)

mean(1/sqrt(model_gibbs[, 3]))

sd(1/sqrt(model_gibbs[, 3]))

apply(model_gibbs, 2, function(x) quantile(x, c(0.05, 0.95)))

ACPL1_sim <- rnorm(5000, model_gibbs[, 1] + model_gibbs[, 2], sd = 1/sqrt(model_gibbs[, 3]))
ACPL2_sim <- rnorm(5000, model_gibbs[, 1] - model_gibbs[, 2], sd = 1/sqrt(model_gibbs[, 3]))

model_gibbs <- as.mcmc(ACPL1_sim)

model_gibbs

acf(ACPL1_sim,lag.max = 100)
acf(ACPL2_sim,lag.max = 100)

plot <- ggplot(data.frame(ACPL_sim_diff = ACPL1_sim-ACPL2_sim),aes(ACPL1_sim-ACPL2_sim, ..density..))

plot <- plot + geom_histogram(bins = 30, aes(ACPL_sim_diff,alpha = 0.5, colour = "blue"), show.legend = FALSE)
plot <- plot + geom_density(fill = "skyblue", aes(ACPL_sim_diff,alpha = 0.5), show.legend = FALSE)
plot <- plot + xlab("Difference between simulated samples") + ylab("Probability density")

plot

plot <- ggplot(data.frame(ACPL1_sim),aes(ACPL1_sim, ..density..))
plot <- ggplot(data.frame(ACPL2_sim),aes(ACPL2_sim, ..density..))

#install.packages('LaplacesDemon')

library(LaplacesDemon)

# joint density plot
joint.density.plot(ACPL1_sim, ACPL2_sim, Title='combined PDF of Players' , contour=TRUE, color=FALSE, Trace=c(1,10))

mean(ACPL1_sim > ACPL2_sim)

ggplot(data.frame(ACPL1_sim, ACPL2_sim)) + geom_point(color='green',fill="black",aes(ACPL1_sim, ACPL2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)


# Question 1.b

set_players <- df[c('White.Player','White.ACPL')]

head(set_players,5)

summary(set_players)

#plot 1
ggplot(set_players) +
  geom_boxplot(aes(x = reorder(White.Player, White.ACPL, median),
                   White.ACPL,
                   fill = reorder(White.Player, White.ACPL, median)),
               show.legend=FALSE) +
  labs(x = "", y = "ACPL") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5,angle = 90, face = "bold", vjust = 0.3, hjust = 0.98))

#plot 2
set_players %>% ggplot(aes(x = reorder(White.Player, White.Player, length)),color='red') + stat_count() +
  labs(x = "", y = "Games Count") +theme_minimal() +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"), axis.text.x = element_text(size = 5,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

#plot 3
set_players %>% ggplot(aes(White.ACPL)) + stat_bin(bins = 9) +
  labs(x = "ACPL",y = "Count") +theme_minimal()

#plot 4
ggplot(data.frame(size = tapply(set_players$White.ACPL, set_players$White.Player, length),
                  mean_score = tapply(set_players$White.ACPL, set_players$White.Player, mean)),
       aes(size, mean_score)) + geom_point() + xlab("Player sample size") + ylab("Avg Score") +
  ggtitle("Effect vs Sample size")

# Gibbs

perf_gibbs <- function(data,
                               levels,
                               mu_0 = 85,
                               tau_0 = 1/100,
                               a_0 = 1,
                               b_0 = 50,
                               alpha_0 =1,
                               beta_0 = 50,
                               num_iter = 5000)
  
{
  a_0 <- 289
  b_0 <- 3.4
  alpha_0 <-289
  beta_0 <- 3.4
  mu_0 <- 85
  tau_0 <- 1/25
  
  m <- nlevels(levels)
  ybar <- theta <- tapply(data, levels, mean)
  tau_w <- mean(1 / tapply(data, levels, var))
  
  mu <- mean(theta)
  tau_b <-var(theta)
  
  n_m <- tapply(data, levels, length)
  alpha_n <- alpha_0 + sum(n_m)/2
  
  theta_matrix <- matrix(0, nrow = num_iter, ncol = m)
  matrix_store <- matrix(0, nrow = num_iter, ncol = 3)
  
  for(iter in 1:num_iter)
  {
    for(j in 1:m)
    {
      tau_n <- n_m[j] * tau_w + tau_b
      theta_n <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / tau_n
      theta[j] <- rnorm(1, theta_n, 1/sqrt(tau_n))
    }
    
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((data[levels == j] - theta[j])^2)
    }
    
    beta_n <- beta_0 + ss/2
    tau_w <- rgamma(1, alpha_n, beta_n)
    
    tau_m <- m * tau_b + tau_0
    mu_m <- (mean(theta) * m * tau_b + mu_0 * tau_0) / tau_m
    mu <- rnorm(1, mu_m, 1/sqrt(tau_m))
    
    a_m <- a_0 + m/2
    b_m <- b_0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, a_m, b_m)
    
    theta_matrix[iter,] <- theta
    matrix_store[iter, ] <- c(mu, tau_w, tau_b)
  }
  colnames(matrix_store) <- c("mean", "precision(w)", "precision(b)")
  colnames(theta_matrix) <- levels(levels)
  return(list(params = matrix_store, theta = theta_matrix))
}

set_players$White.Player <- as.character(set_players$White.Player)
set_players <- set_players[order(set_players$White.Player), ]
set_players$White.Player <- as.factor(set_players$White.Player)

set_players$White.ACPL <- set_players$White.ACPL + rnorm(nrow(set_players), 1, 1)/1000

gibbs_fit <- perf_gibbs(set_players$White.ACPL, set_players$White.Player)

par("mar")

plot(as.mcmc(gibbs_fit$params))

apply(gibbs_fit$params, 2, mean)
apply(gibbs_fit$params, 2, sd)

mean(1/sqrt(gibbs_fit$params[, 2]))
sd(1/sqrt(gibbs_fit$params[, 2]))

mean(1/sqrt(gibbs_fit$params[, 3]))
sd(1/sqrt(gibbs_fit$params[, 3]))

theta_ht <- apply(gibbs_fit$theta, 2, mean)

theta_ht

names(theta_ht) <- colnames(gibbs_fit$theta)

head(data.frame(sort(theta_ht, decreasing = TRUE)),7)

theta_quantile_limits <- apply(gibbs_fit$theta, 2, quantile, prob = c(0.05, .95))

theta_quantile_limits

theta_df_err <- data.frame(lower = theta_quantile_limits[1, ],
                             upper = theta_quantile_limits[2, ],
                             mean = theta_ht,
                           player = colnames(gibbs_fit$theta))

theta_df_err

df_theta <- data.frame(ACPL_samples = as.numeric(gibbs_fit$theta),
                       player = rep(colnames(gibbs_fit$theta),
                                    each = nrow(gibbs_fit$theta)))

print(df_theta)

#plot 5

ggplot(df_theta) +
  geom_boxplot(aes(x = reorder(player, ACPL_samples, median),
                   ACPL_samples,
                   fill = reorder(player, ACPL_samples, median)),
               show.legend=FALSE) +
  labs(x = "",y = "Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5,angle = 90, face = "bold", vjust = 0.2, hjust = 0.98))

#### comparision

df_theta_ht <- data.frame(theta_ht)

head(df_theta_ht,10)

df_theta_ht$White.Player <- unique(sort(set_players$White.Player))

df_theta_ht

df_theta_ht <- df_theta_ht[order(df_theta_ht$theta_hat),]

df_theta_ht

head(df_theta_ht$White.Player,10)
tail(df_theta_ht$White.Player,10)



