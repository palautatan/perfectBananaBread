# LIBRARY
# see ?conv_unit
library(measurements)


bbdf = read.csv("/Users/EDIE/GitHub/perfectBananaBread/00 datasets/parsed_ingredients.csv", as.is=TRUE)

amts = bbdf[which(grepl(x=bbdf$ingredient, pattern="baking soda")), 3] # DEBUG

hist(amts)
summary(amts)

readAmounts = function(ingredient) {
  amounts = bbdf[which(grepl(x=bbdf$ingredient, pattern=ingredient)), 4]
  return(amounts)
}

ingredients = c("baking soda", "flour", "sugar", "salt", "eggs", "baking powder", 
                "vanilla extract", "cinnamon", "walnuts", "oil", "butter", "milk",
                "nutmeg", "margarine")
some_amounts = lapply(ingredients, readAmounts)
names(some_amounts) = ingredients

all_units = lapply(some_amounts, table)
all_units



# * WE ARE GOING TO ASSUME LARGE EGGS
fuckers = which(grepl(pattern="egg", x=bbdf$ingredient[!is.na(bbdf$ingredient)]))
bbdf$ingredient[fuckers]



# CONVERT ALL THESE LITTLE SHITS
# $`baking soda`

# tablespoon   teaspoon  teaspoons 
# 2        118         23 
conv_unit(2, "us_tbsp", "us_tsp")

