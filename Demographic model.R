

library(readr)
library(tidyverse)
library(reshape)

# size <- read_csv("C:/Users/JC/Dropbox (Personal)/Escuela/Bren-LAFF-MESM/5Winter17/ESM211/Size_full.csv") %>%
#   untable(n = .$Nr) %>% 
#   select(YearC, Flag, FleetCod, GearCod, Lat, Lon, ClassFrq, Nr)

load("size.RData")


ggplot(size, aes(x = ClassFrq, y = Nr)) +
  geom_bar(stat = "identity") +
  facet_grid(YearC~., scales = "free_y")

# Define Von Bert parameters
# Garbin & Castello, 2014
# L_inf <- mean(c(80, 80, 87.12, 94, 97.9, 97.3, 112.34, 89.38, 92.5))
K <- mean(c(0.32, 0.6, 0.22, 0.38, 0.14, 0.25, 0.14, 0.38, 0.16))

# Or from Us and Stiurskjgfas, 1981
L_inf <- 102
# K <- 0.55
t_0 <- 0.02

# Just to be clear, we use L-inf  and to from Ushisomething, and K from the review



# Define fecundity at age
# From fishbase http://www.fishbase.se/Reproduction/FecundityList.php?ID=107&GenusName=Katsuwonus&SpeciesName=pelamis&fc=416&StockCode=121
fec_a <- -1.33354
fec_b <- 3.238
# Define mortality

m <- 0.63
z <- 1.7
f <- t-m

# Create a von Bert function

vonbert <- function(length, l_inf, K, t_o){
  age <- (1/-K)*(log(1-(length/L_inf))) + t_o
  return(age)
}

fecundity <- function(length, a, b){
  f <- a*length^b
  
  return(f)
}

# save(size, file = "size.RData")

# Make sure that proportions had stayed relatively constant trhough time:

size %>%
  mutate(Age = round(vonbert(ClassFrq, L_inf, K, t_0))) %>%
  group_by(YearC, Age) %>%
  summarize(N = sum(as.numeric(Nr))) %>% 
  mutate(Age = as.factor(Age)) %>% 
  ungroup() %>% 
  ggplot(aes(x = YearC, y = log(N), color = Age)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Year", y = "log N")

# Build the pop vector

# n <- size %>%
#   mutate(Age = round(vonbert(ClassFrq, L_inf, K, t_0))) %>%
#   group_by(YearC, Age) %>%
#   summarize(N = sum(as.numeric(Nr))) %>%
#   filter(YearC == 1980)

# The "size" file is too big, and takes a long time to load, so lets save this object of interest
# save(n, file = "size_dist_1980.RData")


#Load the n vector generated from above
load("size_dist_1980.RData")

# In theory, based on max length, max age is 13 years, so lets make this a larger vector

n <- n %>% {
  .$N} %>% 
  c(rep(0, times = 8))

# Build the matrix

A <- matrix(0, 13, 13) #initial empty matrix

# Populate matrix with mortality
for (i in 2:13){
  A[i,i-1] <- m
}

# Populate matrix with fecundity

ages <- seq(1:13)
A[1,] <- vonbert(ages) #This function doesnt work yet. We need a function that translates ages to length (the original von bertalanfy equation, not the one we solved for t)

project <- popbio::pop.projection(A, n, 50)

matplot(t(project$stage.vectors), type = "l")












