# LIBRARIES
library(stringr)

# LOAD IN DATA
bbdata = read.csv("/Users/EDIE/Desktop/banana_bread_df.csv", as.is=TRUE)
attach(bbdata)




# RATINGS OVERALL
hist(longRating, main="Histogram of Ratings")

# CALORIES
plot(calories,ratingValue, main="Calories v. Rating for Banana Bread Recipes")
hist(calories, main="Histogram of Banana Bread Recipe Calories")

# BAKERS/REVIEWERS
plot(made.it, ratingValue, main="Number of Bakers v. Rating for Banana Bread Recipes")
hist(made.it, main="Histogram of Bakers Baking Recipes")

plot(reviews, ratingValue, main="Number of Reviewers v. Rating for Banana Bread Recipes")
hist(reviews, main="Histogram of Reviewers Reviewing Recipes")

plot(reviews, made.it, ylim=c(0,1000), xlim=c(0,1000))
plot(made.it, reviews, ylim=c(0,500), xlim=c(0,500))

line = lm(made.it[reviews>250] ~ reviews[reviews>250])
plot(reviews[reviews>250], made.it[reviews>250], ylim=c(0,1000), xlim=c(0,1000))
abline(line)





# ONE RECIPE
bbdata$recipe[1]
ingredients = strsplit(bbdata$recipe[1], "\', ")[[1]]
ingredients2 = unlist(lapply(ingredients, function(x) gsub("'","", x)))
ingredients3 = unlist(lapply(ingredients2, function(x) gsub("\\[|\\]","", x)))



## FIRST FUNCTION
# CAN BE VECTORIZED LOL WOOPS
split_into_ingredients = function(long_ass_string) {
  ingredients = strsplit(long_ass_string, "\', ")[[1]]
  ingredients2 = unlist(lapply(ingredients, function(x) gsub("'","", x)))
  ingredients3 = unlist(lapply(ingredients2, function(x) gsub("\\[|\\]","", x)))
  ingredients3
}


# NUMBER AMOUNTS
ingredients3[1]
grepl("[0-9]+ ([0-9]/[0-9] )?",ingredients3[1])
str_match(ingredients3[1], "[0-9]+")
str_match(ingredients3[2], "[0-9]+")
str_extract(ingredients3[10], "[0-9]+ ([0-9]/[0-9])")

# THIS IS THE BETTER REGEX
str_extract(ingredients3[2], "[0-9](/[0-9]?)+( [0-9]/[0-9])?")
str_extract(ingredients3[10], "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")



## SECOND FUNCTION
check_recipe_number = function(listed_ingredient) {
  number = str_extract(listed_ingredient, "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")
  return(number)
}

numbers = unlist(lapply(ingredients, check_recipe_number))
numbers




## THIRD FUNCTION
# SMALL FUNCTION THAT TAKES A SINGLE ELEMENT
# AND OUTPUTS ITS DECIMAL EQUIVALENT

num_to_dec = function(x) 
{
  # CHECK IF THERE IS A SLASH
  if (grepl("\\/", x)) 
  {
    if (nchar(x)<5) 
    {
      # NON MIXED NUMBERS
      num_denom = strsplit(x,"\\/")[[1]]
      dec = as.integer(num_denom[1]) / as.integer(num_denom[2])      
    } 
    else 
    {
      # MIXED NUMBERS
      mixed_no = strsplit(fucker, " ")[[1]]
      whole = as.integer(mixed_no[1])
      num_denom = strsplit(mixed_no[2],"\\/")[[1]]
      frac = as.integer(num_denom[1]) / as.integer(num_denom[2]) 
      dec = whole + frac
    }
  } 
  else 
  {
    # IF NO SLASH, JUST INTEGER
    dec = as.integer(x)
  }
  dec
}



sample_converts = unlist(lapply(numbers[[1]], num_to_dec))


# GET UNIT MEASUREMENTS
# TAKE ingredients3
# MAKE A FUNCTION THAT USES REGEX TO COLLECT
# CUPS, TEASPOONS, TABLESPOONS, OUNCES, SERVING <-- COLLECT THAT INFORMATION

# IF (grepl("pattern", "ingredient_line")) { str_extract the measurement }

if (grepl("[0-9]+ ([A-z])+", ingredients3[3])) {
  str_extract(ingredients3[3], "[A-z]+")
}



## FOURTH FUNCTION
# CHECK THE MEASUREMENT
check_measurement = function (v) {
  if (grepl("[0-9]+ ([A-z])+", v)) {
    if (grepl("cup(s)?|[A-z]+spoon(s)?|ounce(s)?", v)) {
      str_extract(v, "[A-z]+")
    } else {
      NA
    }
  }
}




# SPLIT ALL INGREDIENTS
all_recipes = lapply(bbdata$recipe, split_into_ingredients)
numbers = lapply(all_recipes, check_recipe_number)
converts = lapply(numbers, function(x) unlist(lapply(x, num_to_dec)))
the_measurements = lapply(all_recipes, function(x) unlist(lapply(x,check_measurement)))

# WE HAVE THE MEASUREMENT
# WE HAVE THE NUMBER
# MAYBE: WE CAN REMOVE THESE FROM THE STRING

i = 5
j = 1
numbers[[i]][j]
the_measurements[[i]][j]
all_recipes[[i]][j]

step1 = gsub(paste0(numbers[[i]][j]," "), "", all_recipes[[i]][j])
step2 = gsub(paste0(the_measurements[[i]][j]," "), "", step1)
step2


## FIFTH FUNCTION
# GET THE INGREDIENTS
ingredients_all = list()
for (i in 1:length(all_recipes)) {
  ingredients_per_recipe = c()
  for (j in 1:length(all_recipes[[i]])) {  
    step1 = gsub(paste0(numbers[[i]][j]," "), "", all_recipes[[i]][j])
    step2 = gsub(paste0(the_measurements[[i]][j]," "), "", step1)
    # cat(paste0(step2,"\n"))
    ingredients_per_recipe = c(ingredients_per_recipe, step2)
  }
  ingredients_all[[i]] = ingredients_per_recipe
}




# WE HAVE EVERYTHING, FRANK
i = 1
a = converts[[i]]
b = the_measurements[[i]]
c = ingredients_all[[i]]
first_df = data.frame(cbind(a,b,c))
write.table(first_df, "sample_df1.csv")

j = 5
ab = converts[[j]]
bb = the_measurements[[j]]
cb = ingredients_all[[j]]
another_df = data.frame(cbind(ab,bb,cb))
write.table(another_df, "sample_df2.csv")


# UNLIST ALL THE LISTS
# COMBINE THEM INTO A SINGLE DATAFRAME
# MAKE SURE THAT WE CAN STILL REFERENCE WHICH DATASET
# FROM WHENCE THEY CAME

unique(unlist(ingredients_all))

# BAKING SODA
# BAKING POWDER
# SALT
# EGGS
# VANILLA EXTRACT
# BANANAS
# FLOUR
# WHITE SUGAR / BROWN SUGAR / MAPLE SYRUP
# OILS
# NUTS
# CHOCOLATE
# MILK
# BUTTER
