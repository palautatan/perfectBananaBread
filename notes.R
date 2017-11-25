# PROCESSING DATA
# 1. Choose which unit of measurement we will use per the "most important" ingredients
# 2. Convert all of the measurements to their respective chosen unit
# 3. Convert all of the banana measurements into "mashed banana" units
# 4. Divide all of the "most important" ingredients measurements by the amount of banana
# 5. Create a new dataframe of just the important ingredients (think about how to account for other ingredients -- maybe keep the number of ingredients as another measure for later)
# 6. Run regressions on the proportions between banana and ingredients per recipe on rating/popularity


# ESSENTIAL INGREDIENTS
# (Bananas), baking soda, flour, sugar (white and brown),
# salt, eggs, baking powder, vanilla extract, ground cinnamon,
# walnuts, oil, butter, egg, milk, nutmeg, margarine

# INGREDIENT NOTES
#    INGREDIENT             Unit of Measurement
# 1. Bananas          |     Mashed medium Banana
# 2. Baking Soda      |
# 3. Flour            |
# 4. Sugar (W/B)      |
# 5. Salt             |
# 6. Eggs             |
# 7. Baking Powder    |
# 8. Vanilla Extract  |
# 9. Ground cinnamon  |
# 10. Walnuts         |
# 11. Oil             |
# 12. Butter          |
# 13. Eggs (DUPLICATE)|     Large eggs (?)
# 14. Milk            |
# 15. Nutmeg          |
# 16. Margarine       |


# DATAFRAME FORMAT
# STUDY | AMOUNT | UNIT | INGREDIENT | RATING


# INDEXING A DATAFRAME
# dataframe[row,column]


# 1. CHOOSING THE MEASUREMENT
bbdf = read.csv("parsed_ingredients.csv", as.is=TRUE)

# 1a. START WITH BAKING SODA
readAmounts = function(ingredient) {
  amounts = bbdf[which(grepl(x=bbdf$ingredient, pattern=ingredient)), 4] # REPLACE W/ 3 IF YOU WANT THE AMOUNTS
  return(amounts)
}

ingredients = c("baking soda", "sugar", "eggs", "flour")
some_amounts = lapply(ingredients, readAmounts)
names(some_amounts) = ingredients

# INSPECT ALL THE INGREDIENTS
lapply(some_amounts, table) # LIST OF TABLES
