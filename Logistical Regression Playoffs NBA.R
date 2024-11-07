# libraries loaded 
library(mlbench)
library(MASS)
library(pROC)
library(rmarkdown)
library(readr)
library(readxl)
library(tidyr)
library(DT)
library(tibble)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(tinytex)
library(RColorBrewer)
library(rio)
library(dbplyr)
library(psych)
library(FSA)
library(ggplot2)
library(flextable)
library(summarytools)
library(officer)
library(ggcorrplot)
library(corrplot)
library(gtsummary)
library(reshape2)

# import data set
nba <- read_csv("DataSets/Team Summaries.csv")


# exploratory data analysis (EDA) ##########################################################################################################

# select variables to use
nba <- nba %>%
  select(season, abbreviation, playoffs, age, pw, srs, o_rtg, d_rtg, n_rtg, pace, f_tr,
         x3p_ar, ts_percent, e_fg_percent, tov_percent, orb_percent, ft_fga,
         opp_e_fg_percent, opp_tov_percent, opp_drb_percent, opp_ft_fga)

# define the structure and attributes of data 
str(nba)

# make playoffs an integer
nba <- nba %>%
  mutate(playoffs = as.integer(playoffs))


# first and last 6 rows of data
head(nba)
tail(nba)

# drop 2023 data as incomplete
nba <- nba %>%
  filter(season != "2023")

# find observation count
nrow(nba)

# descriptive stats 
summary(nba)

# nba data >= 1980
nba <- nba %>%
  filter(as.numeric(season) >= 1980)

nrow(nba)

# find missing data 
sum(is.na(nba))

# missing data per column
missing_data_count <- colSums(is.na(nba))

# drop na from data
nba <- na.omit(nba)
dim(nba)
sum(is.na(nba))

# descriptive statistics
summary(nba)

# boxplot for each variable
boxplot(nba$age) # outliers
boxplot(nba$pw)
boxplot(nba$srs) # outliers
boxplot(nba$o_rtg) # outliers
boxplot(nba$d_rtg) # outliers
boxplot(nba$n_rtg) # outliers
boxplot(nba$pace) # outliers
boxplot(nba$f_tr) # outliers
boxplot(nba$x3p_ar) # outliers
boxplot(nba$ts_percent) # outliers
boxplot(nba$e_fg_percent) # outliers
boxplot(nba$tov_percent) # outliers
boxplot(nba$orb_percent)
boxplot(nba$ft_fga) # outliers
boxplot(nba$opp_e_fg_percent) # outliers
boxplot(nba$opp_tov_percent) # outliers
boxplot(nba$opp_drb_percent)
boxplot(nba$opp_ft_fga)# outliers

outliervars1 <-  c("age","srs", "o_rtg", "d_rtg", "pace", "f_tr", "x3p_ar", 
                  "ts_percent", "e_fg_percent", "tov_percent", "ft_fga", 
                  "opp_e_fg_percent", "opp_tov_percent", "opp_ft_fga")

# all box plots together for variables with outliers
ggplot(data = gather(nba[, outliervars]), aes(x = key, y = value, fill = key)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Box Plots for Variables with Outliers",
       x = "Variables",
       y = "Values")


# histograms 
hist(nba$age, main = "Age", xlab = "Age")
hist(nba$pw, main = "Pythagorean Wins", xlab = "Pythagorean Wins")
hist(nba$srs, main = "Simple Rating System (SRS)", xlab = "SRS")
hist(nba$o_rtg, main = "Offensive Rating (O_rtg)", xlab = "O_rtg")
hist(nba$d_rtg, main = "Defensive Rating (D_rtg)", xlab = "D_rtg")
hist(nba$n_rtg, main = "Net Rating (N_rtg)", xlab = "N_rtg")
hist(nba$pace, main = "Pace", xlab = "Pace")
hist(nba$f_tr, main = "Free Throw Rate (F_tr)", xlab = "F_tr")
hist(nba$x3p_ar, main = "Three-Point Attempt Rate (x3p_ar)", xlab = "x3p_ar")
hist(nba$ts_percent, main = "True Shooting Percentage (ts_percent)", xlab = "ts_percent")
hist(nba$e_fg_percent, main = "Effective Field Goal Percentage (e_fg_percent)", xlab = "e_fg_percent")
hist(nba$tov_percent, main = "Turnover Percentage (tov_percent)", xlab = "tov_percent")
hist(nba$orb_percent, main = "Offensive Rebound Percentage (orb_percent)", xlab = "orb_percent")
hist(nba$ft_fga, main = "Free Throws Attempted per Field Goals Attempted (ft_fga)", xlab = "ft_fga")
hist(nba$opp_e_fg_percent, main = "Opponent Effective Field Goal Percentage (opp_e_fg_percent)", xlab = "opp_e_fg_percent")
hist(nba$opp_tov_percent, main = "Opponent Turnover Percentage (opp_tov_percent)", xlab = "opp_tov_percent")
hist(nba$opp_drb_percent, main = "Opponent Defensive Rebound Percentage (opp_drb_percent)", xlab = "opp_drb_percent")
hist(nba$opp_ft_fga, main = "Opponent Free Throws Attempted per Field Goals Attempted (opp_ft_fga)", xlab = "opp_ft_fga")

# filter out non-numeric and non-integer columns
numeric_columns <- sapply(nba, is.numeric)
integer_columns <- sapply(nba, is.integer)
valid_columns <- numeric_columns | integer_columns
nba_numeric <- nba[, valid_columns]

# correlation matrix 
cor_nba <- cor(nba_numeric)
corrplot(cor_nba, type = "upper")

# scatter plots
plot(nba$season, nba$age)
plot(nba$season, nba$x3p_ar)
plot(nba$season, nba$pace)  
plot(nba$season, nba$orb_percent)
plot(nba$season, nba$tov_percent)
plot(nba$season, nba$opp_e_fg_percent)
plot(nba$season, nba$f_tr)
plot(nba$season, nba$tov_percent)

# technique practice 3 ##########################################################################################################

# code provided by Professor Joe Reilly 

# remove abbreviation and year variables
nba_log <- nba %>%
  select( playoffs, age, srs, o_rtg, d_rtg, n_rtg, pace, f_tr,
         x3p_ar, ts_percent, e_fg_percent, tov_percent, orb_percent, ft_fga,
         opp_e_fg_percent, opp_tov_percent, opp_drb_percent, opp_ft_fga)

# boxplots for each predictor by playoffs
boxplot(age ~ playoffs, ylab = "Age", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(pw ~ playoffs, ylab = "PW", xlab = "Playoffs", col = "antiquewhite2", data = nba_log) # filtered out
boxplot(srs ~ playoffs, ylab = "SRS", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(o_rtg ~ playoffs, ylab = "O_RTG", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(d_rtg ~ playoffs, ylab = "D_RTG", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(n_rtg ~ playoffs, ylab = "N_RTG", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(pace ~ playoffs, ylab = "Pace", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(f_tr ~ playoffs, ylab = "F_TR", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(x3p_ar ~ playoffs, ylab = "X3P_AR", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(ts_percent ~ playoffs, ylab = "TS_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(e_fg_percent ~ playoffs, ylab = "E_FG_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(tov_percent ~ playoffs, ylab = "TOV_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(orb_percent ~ playoffs, ylab = "ORB_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(ft_fga ~ playoffs, ylab = "FT_FGA", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(opp_e_fg_percent ~ playoffs, ylab = "Opp_E_FG_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(opp_tov_percent ~ playoffs, ylab = "Opp_TOV_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(opp_drb_percent ~ playoffs, ylab = "Opp_DRB_percent", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)
boxplot(opp_ft_fga ~ playoffs, ylab = "Opp_FT_FGA", xlab = "Playoffs", col = "antiquewhite2", data = nba_log)


# fit logistic regression model for playoffs
model1 <- glm(playoffs ~ ., family = binomial, data = nba_log)
summary(model1)


# remove offensive and defensive rating variables
nba_log <- nba %>%
  select( playoffs, age, srs,n_rtg, pace, f_tr,
          x3p_ar, ts_percent, e_fg_percent, tov_percent, orb_percent, ft_fga,
          opp_e_fg_percent, opp_tov_percent, opp_drb_percent, opp_ft_fga)


# fit logistic regression model for playoffs
model1 <- glm(playoffs ~ ., family = binomial, data = nba_log)
summary(model1)

# step wise feature selection
model2 <- stepAIC(model1)

# comparing aic
model1$aic
model2$aic

# summary of the second model
summary(model2)

# display fitted values through histogram
summary(model2$fitted.values)
hist(model2$fitted.values, main = "Histogram of Predicted Probability of Making Playoffs",
     xlab = "Probability of Making Playoffs", col = 'navy')

# turn the probabilities into binary predictions
nba_log$Predict <- ifelse(model2$fitted.values > 0.5, "Yes", "No")

# create a confusion matrix to evaluate the model's performance
mytable <- table(nba_log$playoffs, nba_log$Predict)
rownames(mytable) <- c("Observed No", "Observed Yes")
colnames(mytable) <- c("Predicted No", "Predicted Yes")
mytable

# calculate the efficiency of the model
efficiency <- sum(diag(mytable)) / sum(mytable)
efficiency

# measure the area under (AUC) the ROC curve for sensitivity and specificity
roc_data <- roc(nba_log$playoffs ~ model2$fitted.values, data = nba_log)
plot(roc_data, main = "ROC Curve", col = "navy")
auc_value <- auc(roc_data)