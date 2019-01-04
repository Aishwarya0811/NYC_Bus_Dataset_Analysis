```{r}
library(caTools)
set.seed(101)
#splitting the dataset into test/train dataset 
sample <- sample.split(df$How_Long_Delayed,SplitRatio = 0.7)
df.train <- subset(df,sample == TRUE)
df.test <- subset(df,sample == FALSE)
View(df.test)
#Linear regression model application.
model.df <- lm(How_Long_Delayed ~., df.train)
library(MASS)
model.df.step <- stepAIC(model.df,direction="both")
class(model.df.step)
summary(model.df.step)
#summary(model.df)
```

```{r}
df.predict <- predict(model.df.step,df.test)
#View(df.predict)
results <- cbind(df.predict,df.test$How_Long_Delayed)
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
results
```

```{r}
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
### Generate Lift Chats
library(gains)
install.packages("gains")
df.predict <- predict(model.df.step,df.test)
View(df.predict)

gain <- gains(df.test$How_Long_Delayed, df.predict)

#Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(df.test$How_Long_Delayed))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(df.test$How_Long_Delayed))~c(0, dim(df.test)[1]), lty = 5)
```

```{r}
### Plot decile-wise chart
heights <- gain$mean.resp/mean(df.test$How_Long_Delayed)
decile_lift <- barplot(heights, names.arg = gain$depth,  ylim = c(0,3), col = "gold3",  
                       xlab = "Percentile", ylab = "Mean Response", 
                       main = "Decile-wise lift chart")
```

```{r}
#residual 
some.residuals <- results$real[1:500] - results$pred[1:500]

plot(some.residuals, type = "p", pch = 16,
     col = "blue1",
     ylab = "Sample Residuals", 
     ylim = c(-50, 50), bty = "n"
)
```
