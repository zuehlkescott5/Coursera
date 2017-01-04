

# This script corresponds with actual R code used in the RMarkdown file.  The process used to write this is as follows: 
# 1.  Write R code to answer questions, provide plots and insights.
# 2.  Piecewise move code over to R Markdown document, adding commentary to make Rmd more report-like.
#
#
# You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:
  
#  “Is an automatic or manual transmission better for MPG”
# "Quantify the MPG difference between automatic and manual transmissions"

#install.packages("ggplot2")
library(ggplot2)
#install.packages("bestglm")
library(bestglm)
data(mtcars)
str(mtcars)

mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

manualtrans <- subset(mtcars,am == 1)
autotrans <- subset(mtcars,am == 0)

round(mean(manualtrans$mpg),2)
round(mean(autotrans$mpg),2)

test <- t.test(manualtrans$mpg, autotrans$mpg)
round(test$p.value,3)
round(test$conf.int[1],2)
round(test$conf.int[2],2)

fit1 <- lm(mpg~am,data = mtcars)
summary(fit1)$r.squared

col_idx <- grep("mpg",names(mtcars))
new_cols <- c(2:length(mtcars),col_idx)

mtcars.Xmpg <- subset(mtcars, select = new_cols)

colnames(mtcars.Xmpg)[colnames(mtcars.Xmpg)=='mpg'] <- 'y'
mtcars.Xy <- mtcars.Xmpg

fit3 <- bestglm(mtcars.Xy, IC = 'AIC', method = 'exhaustive')
formula(fit3$BestModel)

bestFit <- lm(mpg ~ wt + qsec + am, data = mtcars)

summary(bestFit)$r.squared
round(bestFit$coefficients[4],2)

g <- ggplot(data = mtcars, aes(x=factor(am),y=mpg)) +
  geom_boxplot() +
  xlab('Transmission (0 is Automatic, 1 is Manual)') 
  ylab('MPG') +
  ggtitle('MPG in Automatic vs Manual Transmissions') + 
  theme(plot.title = element_text(hjust = 0.5))
g

par(mfrow=c(2,2))
plot(bestFit)






