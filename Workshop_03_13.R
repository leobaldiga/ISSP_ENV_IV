#loading data and libraries-----
#load libraries
library(haven)
library(lavaan)
library(psych)
library(tidyr)
library(dplyr)
library(dm)
library(writexl)
library(kableExtra)
library(webshot2)
library(ggplot2)
library(likert)
library(knitr)
library(stringr)
library(scales)
library(gridExtra)

isspenvt2010 <- read.csv('2010/issp10_34_imp1.csv')

env_concerns <- c('threxg10', 'prghrm10', 'worent10', 'morimp10', 'others10')
#making an index for environmental concerns-----
#create additive index 'enatt10' for env_concerns variables, removing '6'
isspenvt2010 <- isspenvt2010 %>%
  filter_at(vars(env_concerns), all_vars(. != 6)) %>%
  mutate(enatt10 = rowSums(.[env_concerns], na.rm = TRUE),
         enatt10_mean = rowMeans(.[env_concerns], na.rm = TRUE))

summary(isspenvt2010$enatt10)
summary(isspenvt2010$enatt10_mean)

hist(isspenvt2010$enatt10_mean, main = 'Environmental Concerns', xlab = 'Mean Environmental Concerns')

zincplot <- qqnorm(isspenvt2010$zincs10, main = 'Income z-scores')
qqline(isspenvt2010$zincs10)

#qqplot
enatt10_meanplot <- qqnorm(isspenvt2010$enatt10_mean, main = 'Environmental Concerns')
qqline(isspenvt2010$enatt10_mean)

#Models-----
#linear model enatt10_mean ~ age
lm1 <- lm(enatt10 ~ ageyrs10, data = isspenvt2010)
summary(lm1)
plot(lm1)

#plot
plot1 <- ggplot(isspenvt2010, aes(x = ageyrs10, y = enatt10_mean)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Environmental Concerns by Age',
       x = 'Age',
       y = 'Environmental Concerns') +
  theme_minimal()
print(plot1)

#linear model enatt10 ~ edyrs10
lm2 <- lm(enatt10 ~ edyrs10, data = isspenvt2010)
summary(lm2)
plot(lm2)

#plot
plot2 <- ggplot(isspenvt2010, aes(x = edyrs10, y = enatt10_mean)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Environmental Concerns by Education',
       x = 'Education',
       y = 'Environmental Concerns') +
  theme_minimal()
print(plot2)

#linear model enatt10 ~ zincs10
lm3 <- lm(enatt10 ~ zincs10, data = isspenvt2010)
summary(lm3)
plot(lm3)

#plot 
plot2 <- ggplot(isspenvt2010, aes(x = zincs10, y = enatt10_mean)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Environmental Concerns by Income',
       x = 'Income',
       y = 'Environmental Concerns') +
  theme_minimal()
print(plot2)

#linear model enatt10 ~ hitax10
lm4 <- lm(enatt10 ~ hitax10, data = isspenvt2010)
summary(lm4)
plot(lm4)

#linear model enatt10 ~ stlvng10
lm5 <- lm(enatt10 ~ stlvng10, data = isspenvt2010)
summary(lm5)
plot(lm5)
ggplot(isspenvt2010,aes(y=enatt10,x=stlvng10))+geom_point()+geom_smooth(method="lm")

#multivariate linear regression all variables
#linear model enatt10 ~ ageyrs10 + edyrs10 + zincs10 + hitax10 + stlvng10
mlm <- lm(enatt10 ~ ageyrs10 + edyrs10 + zincs10 + hitax10 + stlvng10, data = isspenvt2010)
summary(mlm)
plot(mlm)

















