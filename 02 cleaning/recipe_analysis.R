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

check_recipe_number = function(listed_ingredient) {
  number = str_extract(listed_ingredient, "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")
  return(number)
}

numbers = unlist(lapply(ingredients, check_recipe_number))
numbers

# FUNCTION TO CONVERT FRACTIONS
fraction_converter = function(num_string) {
  if (grepl("\\/", num_string)) {
    if (nchar(num_string)<4) {
      num_denom = strsplit(num_string,"\\/")[[1]]
      dec = as.integer(num_denom[1]) / as.integer(num_denom[2])
    }
    else {
      mixed_no = strsplit(num_string, "\\/")[[1]]
      whole_and_num = strsplit(mixed_no[1], " ")[[1]]
      whole = as.integer(whole_and_num[1])
      frac = as.integer(whole_and_num[2]) / as.integer(mixed_no[2])
      dec = whole + frac
    }
  }
  else {
    dec = as.integer(num_string)
  }
  dec
}

converted_numbers = unlist(lapply(numbers, fraction_converter))



# GET UNIT MEASUREMENTS
# TAKE ingredients3
# MAKE A FUNCTION THAT USES REGEX TO COLLECT
# CUPS, TEASPOONS, TABLESPOONS, OUNCES, SERVING <-- COLLECT THAT INFORMATION

# IF (grepl("pattern", "ingredient_line")) { str_extract the measurement }

if (grepl("[0-9]+ ([A-z])+", ingredients3[3])) {
  str_extract(ingredients3[3], "[A-z]+")
}

# CHECK THE MEASUREMENT
check_measurement = function (v) {
  if (grepl("[0-9]+ ([A-z])+", v)) {
    if (grepl("cup(s)?|[A-z]+spoon(s)?", v)) {
      str_extract(v, "[A-z]+")
    } else {
      0
    }
  }
}

unlist(lapply(ingredients3, check_measurement))



# SPLIT ALL INGREDIENTS
all_recipes = lapply(bbdata$recipe, split_into_ingredients)
numbers = lapply(all_recipes, check_recipe_number)
converted_numbers = lapply(numbers, fraction_converter)
the_measurements = lapply(all_recipes, check_measurement)
