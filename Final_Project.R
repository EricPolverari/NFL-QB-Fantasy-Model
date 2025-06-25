# import data
#install.packages("readxl")
library(readxl)
setwd("~/Clemson University/Summer 2024/Final")
ngs <- read_excel("~/Clemson University/Summer 2024/Final/NGS.xlsx")


#create new variable for response variable of interest
ngs$FP <-ngs$TD*4 + ngs$YDS*.04 - ngs$INT*2

# remove irrelavent data columns and rename variables
library(dplyr)
ngs <- ngs %>% select(-`PLAYER NAME`,- TEAM, -ATT, -YDS, -TD, -INT, - RATE)

names(ngs)[names(ngs) == "AGG%"] <- "AGG"
names(ngs)[names(ngs) == "COMP%"] <- "COMP"
names(ngs)[names(ngs) == "xCOMP%"] <- "xCOMP"

# Reshape the data to long format for easier plotting
library(tidyr)

#correlation matrix
cor(ngs)
#install.packages("corrplot")
library(corrplot)

cor.ngs <-cor(ngs)

#install.packages("RColorBrewer")
library(RColorBrewer)

#create colored correlation matrix
clemson_palette <- colorRampPalette(c("orange", "purple", "white"))(200)

png("correlation.matrix.png", width = 800, height = 600)
corrplot(cor.ngs, method = "circle", col = clemson_palette, addCoef.col = NA)
dev.off()


# Plot fantasy points vs. time to throw
library(ggplot2)
ggplot(ngs, aes(x = TT, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Time to Throw vs. Fantasy Points") +
  xlab("Time to Throw") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Average Completed Air Yards
ggplot(ngs, aes(x = CAY, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Average Completed Air Yards vs. Fantasy Points") +
  xlab("Average Completed Air Yards") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Average Intended Air Yards
ggplot(ngs, aes(x = IAY, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Average Average Intended Air Yards vs. Fantasy Points") +
  xlab("Average Intended Air Yards") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Aggressiveness 
ggplot(ngs, aes(x = AGG, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Aggressiveness  vs. Fantasy Points") +
  xlab("Aggressiveness ") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Average Air Yards Differential (AYD)
ggplot(ngs, aes(x = AYD, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Average Air Yards Differential (AYD)  vs. Fantasy Points") +
  xlab("Average Air Yards Differential (AYD) ") +
  ylab("Fantasy Points")


# Plot fantasy points vs. Longest Completed Air Distance 
ggplot(ngs, aes(x = LCAD, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Longest Completed Air Distance  vs. Fantasy Points") +
  xlab("Longest Completed Air Distance ") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Air Yards to the Sticks  
ggplot(ngs, aes(x = AYTS, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Air Yards to the Sticks   vs. Fantasy Points") +
  xlab("Air Yards to the Sticks ") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Completion Probability  
ggplot(ngs, aes(x = COMP, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Completion Probability   vs. Fantasy Points") +
  xlab("Completion Probability") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Expected Completion Percentage 
ggplot(ngs, aes(x = xCOMP, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Expected Completion Percentage vs. Fantasy Points") +
  xlab("Expected Completion Percentage") +
  ylab("Fantasy Points")

# Plot fantasy points vs. Completion Percentage Above Expectation
ggplot(ngs, aes(x = CPAE, y = FP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("Completion Percentage Above Expectation vs. Fantasy Points") +
  xlab("Completion Percentage Above Expectation") +
  ylab("Fantasy Points")


#full model
full_model <- lm(FP ~ ., data = ngs)
summary(full_model)

#best model
library(leaps)
best.mods <- regsubsets(FP ~ ., data = ngs)

#plot BIC
plot(summary(best.mods)$bic, pch= 20, xlab= "No. variables", 
     ylab= "BIC")
lines(summary(best.mods)$bic, type= "c", lwd= 2)

#stepwise regression
library(MASS)
null<- lm(FP ~ 1, data = ngs)
full<- lm(FP ~ ., data = ngs)
best <- stepAIC(full, scope= list(lower= null, upper= full), 
        direction= "both", trace= FALSE)

#loop 10 k folds for cv error
set.seed(314)
k <- 10
folds <- sample(1:k, nrow(ngs), replace= TRUE) # Sample from 
# 1,2,...,10 n times with replacement
head(folds)
cv.errors <- matrix(nrow= k, ncol= 10)

# Loop over each of k holdout sets
for (h.out in 1:k){
  
  # Get the best fitting models for each size
  best.fit <- regsubsets(FP ~ ., data= ngs[folds != h.out, ],
                         nvmax= 10)
  
  mod.mat <- model.matrix(FP~., data= ngs[folds == h.out, ])
  
  # Loop through model size
  for (i in 1:10){
      
    coefi <- coef(best.fit, id= i)
    pred <- mod.mat[, names(coefi)]%*%coefi
    
    cv.errors[h.out, i] <- mean((ngs$FP[folds == h.out] - pred)^2)
    
  }  # End loop over model size
  
  
}  # End loop over sets

(mean.cv.errors <- apply(cv.errors, 2, mean)) 

#plot cv errors
plot(mean.cv.errors, type= "b", pch= 20, xlab= "Model Size",
     ylab= "CV Error")

#choose best three predictors variables
coef(best.fit, 3)


library(ggplot2)
library(gridExtra)

#final model
final.mod <-lm(FP ~ LCAD + AYTS + COMP , data = ngs)
summary(final.mod)

library(car)

#vif
vif(final.mod)

# residual plots
residualPlots(final.mod)

## boxplot

boxplot(final.mod$residuals, horizontal=TRUE)

library(olsrr)

## Q-Q Plot
ols_plot_resid_qq(final.mod)

## outliers
ols_plot_resid_stud_fit(final.mod)$outliers

## resid-leverage plot
ols_plot_resid_lev(final.mod)

## Cooks D plot
ols_plot_cooksd_chart(final.mod)
