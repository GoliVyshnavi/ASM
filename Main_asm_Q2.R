library(ggplot2)
library(dplyr)
library(ggpubr)
library(MCMCpack)
library(standardize)
library(caret)
library(tidyverse)

# Loading data
df <- read.csv('analysis.csv')
which(is.na(df))

df

# 2.a
chess_df <- subset(df[,4:15])

head(chess_df,5)

sliced_chess_df <- chess_df%>%mutate_if(is.numeric,scale)

sliced_chess_df$PreDeepBlue [sliced_chess_df$PreDeepBlue == "FALSE"] <- 0
sliced_chess_df$PreDeepBlue [sliced_chess_df$PreDeepBlue == "TRUE"] <- 1

summary(sliced_chess_df)
head(sliced_chess_df,5)

sliced_chess_df <- sliced_chess_df[, -3] 

sliced_chess_df <- sliced_chess_df[, -5]

summary(sliced_chess_df)

corr_mat <- cor(sliced_chess_df)

corr_mat

apply(sliced_chess_df, 2, mean)

apply(sliced_chess_df, 2, sd)

dim(sliced_chess_df)
summary(sliced_chess_df)


apply(df_chess, 2, mean)

apply(df_chess, 2, range)

apply(df_chess, 2, sd)

# plot histogram

hist(df_chess$Combined.ACPL)

pairs(subset(df_chess, select = c(Year, White.ACPL,Black.ACPL,Combined.ACPL,PreDeepBlue)))

cor(subset(df_chess, select = -c(Year, Combined.ACPL)))

plot(Combined.ACPL~., data = df_chess)

boxplot(Year ~ PreDeepBlue, data = df_chess) 

boxplot(Combined.ACPL ~ Year, data = df_chess) 

# LR model build

df_chess <- subset(df, select=c("Year","Game.Number","Combined.ACPL","PreDeepBlue"))
head(df_chess,5)

df_chess$PreDeepBlue [df_chess$PreDeepBlue == "FALSE"] <- 0

df_chess$PreDeepBlue [df_chess$PreDeepBlue == "TRUE"] <- 1

lm_0 <-lm(Combined.ACPL~., df_chess)

step_AIC_val <- step(lm(Combined.ACPL~1, data = df_chess), direction = "forward", scope = list(upper = lm1))
step_AIC_val <- step(lm(Combined.ACPL~., data = df_chess), direction = "forward", scope = list(upper = lm1))
step_AIC_val
step_AIC2 <- step(lm(Combined.ACPL ~ Year^2+PreDeepBlue, data = df_chess)) 
step_AIC2
step_AIC_val

summary(lm_0)
plot(lm_0, which = 2)+abline(lm_0,col='blue')

ht_y <- predict(lm_0)
ht_y

df_chess <- subset(df, select=c("Year","PreDeepBlue"))

plot(ht_y, df_chess$Combined.ACPL)

# 2.b 

df_chess <- df
df_chess$PreDeepBlue [df_chess$PreDeepBlue == "TRUE"] <- 1
df_chess$PreDeepBlue [df_chess$PreDeepBlue == "FALSE"] <- 0

head(df_chess,5)

lm_1 <-lm(Combined.ACPL~PreDeepBlue, df_chess)

summary(lm_1)

cor(df_chess$Combined.ACPL,df_chess$PreDeepBlue)
plot(df_chess$Combined.ACPL,df_chess$PreDeepBlue)

