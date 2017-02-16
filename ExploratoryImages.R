library(tidyverse)
library(trelliscopejs)
library(broom)
library(RStata)
# Set options to indicate where the executable is
options(RStata.StataPath = "\"C:\\Program Files (x86)\\Stata14\\smStata-64\"")
#Specify the version of Stata I have
options(RStata.StataVersion = 14)

skj <- read.csv("Effort_full.csv", stringsAsFactors = F) %>% 
  mutate(CPUE = SKJ/Eff1) %>% 
  filter(GearCode == "PS") %>% 
  group_by(Year, Flag) %>% 
  summarize(Effort = sum(Eff1))

ggplot(data = skj, aes(x = Year, y = Effort)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(y = "Effort (fishing hours)")

model <- lm(Effort~Year, skj)

stata("regress Effort Year, robust", data.in = skj)



slope <- coefficients(model)[[2]]
intercept <- coefficients(model)[[1]]



#############

iccat <- read.csv("ICCAT Data_full.csv")

countries <- iccat %>% 
  group_by(YearC, Stock, Flag) %>% 
  count() %>% 
  group_by(YearC, Stock) %>% 
  count()

ggplot(countries, aes(x = YearC, y = nn, color = Stock)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year", y = "Number of countries \n in the fishery") +
  scale_color_brewer(palette = "Set1")
  

##############

iccat2 <- iccat %>% 
  group_by(YearC, Stock) %>% 
  summarize(Catch = sum(Qty_t)) %>% 
  left_join(countries, by = c("YearC", "Stock")) %>% 
  mutate(Effort = intercept + (slope * YearC),
         CPUE = Catch/Effort,
         CPUE2 = Catch / nn)

ggplot(data = iccat2, aes(x = YearC, y = Catch, color = Stock)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year", y = "Catches (tonnes)") +
  scale_color_brewer(palette = "Set1")

ggplot(data = iccat2, aes(x = YearC, y = CPUE2, color = Stock)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  labs(x = "Year", y = "Catch per unit effort\n (tonnes / fishing countries)") +
  scale_color_brewer(palette = "Set1")



############

iccat2 <- iccat %>% 
  group_by(YearC, Stock, Flag) %>% 
  summarize(Catch = sum(Qty_t)) %>% 
  left_join(countries, by = c("YearC", "Stock")) %>% 
  mutate(Effort = intercept + (slope * YearC),
         CPUE = Catch/Effort,
         CPUE2 = Catch / nn)

ggplot(data = iccat2, aes(x = YearC, y = Catch, color = Stock)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year", y = "Catches (tonnes)")

##########
variccat <- select(iccat2, YearC, Stock, CPUE2) %>% spread(Stock, CPUE2)

stata("regress ATE YearC, robust", data.in = variccat)

stata("regress ATW YearC, robust", data.in = variccat)





