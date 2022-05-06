# Exercise 2

install.packages("taxize")

sp <- readRDS("./data/species_list.rds")
set.seed(40)
sp <- sp[ sample(x = 1:length(sp), size = 20, replace = FALSE) ]

# Are all names correct?
# Which taxonomical groups are present? (plants, insects, mammals)
# What are their family names?
