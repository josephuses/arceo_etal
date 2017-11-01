## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning=FALSE, fig.height = 4, fig.width = 5, fig.align = 'center', cache = TRUE)

## ------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw())

## ------------------------------------------------------------------------
# import the data
banana <- read.table("./data/banana.txt", header=TRUE, sep="")
papaya <- read.table("./data/papaya.txt", header=TRUE, sep="")
pineapple <- read.table("./data/pineapple.txt", header=TRUE, sep="")
# create a function to conver the data from wide format to long format
to_long <- function(data){
    data %>% gather(., Cell, Voltage, -Day)
}
# convert to long format
banana <- to_long(banana)
papaya <- to_long(papaya)
pineapple <- to_long(pineapple)
# create a variable fruit
banana$fruit <- "banana"
papaya$fruit <- "papaya"
pineapple$fruit <- "pineapple"
# combine all data
data <- rbind(banana, papaya, pineapple)
# remove the "cell" from the cell number names
data <- data %>%
  mutate(
    Day = as.integer(Day),
    Cell = as.integer(str_replace_all(Cell, "cell", "")),
    Voltage = as.numeric(str_replace_all(Voltage, "V", ""))
  )
data %>% DT::datatable()

## ------------------------------------------------------------------------
means <- data %>% group_by(Day, fruit) %>%
  summarise(Voltage = mean(Voltage))
means

## ------------------------------------------------------------------------
ggplot(data, aes(x = fruit, y = Voltage)) + 
  geom_boxplot()

## ------------------------------------------------------------------------
ggplot(data, aes(Day, Voltage, group = as.factor(Cell), color = fruit)) +
  geom_line() + 
  theme(legend.title = element_blank())

## ------------------------------------------------------------------------
ggplot(means, aes(Day, Voltage, color = fruit)) + 
  geom_line() + ylab("Mean Voltage") + 
  theme(legend.title = element_blank())

## ------------------------------------------------------------------------
library(lmerTest)
mod <- lmer(Voltage ~ fruit*Day + (Day|Cell), data)
anova(mod) 

## ------------------------------------------------------------------------
library(multcomp)
summary(glht(mod, linfct = mcp(fruit = "Tukey")), test = adjusted("BH"))

## ------------------------------------------------------------------------
summary(mod)

