

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
L_inf <- mean(c(80, 80, 87.12, 94, 97.9, 97.3, 112.34, 89.38, 92.5))
K <- mean(c(0.32, 0.6, 0.22, 0.38, 0.14, 0.25, 0.14, 0.38, 0.16))

# Or from Us and Stiurskjgfas, 1981
L_inf <- 102
K <- 0.55
t_0 <- 0.02



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

# Build the pop vector

# n <- size %>% 
#   mutate(Age = round(vonbert(ClassFrq, L_inf, K, t_0))) %>% 
#   group_by(YearC, Age) %>% 
#   summarize(N = sum(as.numeric(Nr))) %>% 
#   filter(YearC == 1980)

#Load the n vector generated from above
load("size_dist_1980.RData")

save(n, file = "size_dist_1980.RData")
  
ggplot(n, aes(x = Age, y = N)) + geom_bar(stat = "identity")


# Build the matrix

A <- matrix(0, 13, 13) #initial empty matrix

for (i in 2:13){
  A[i,i-1] <- m
}






  
  
  
  
  
  
  

