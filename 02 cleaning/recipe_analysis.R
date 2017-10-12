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


# ONE RECIPE
bbdata$recipe[1]
ingredients = strsplit(bbdata$recipe[1], "\', ")[[1]]
ingredients2 = unlist(lapply(ingredients, function(x) gsub("'","", x)))
ingredients3 = unlist(lapply(ingredients2, function(x) gsub("\\[|\\]","", x)))

# CUTTING UP RECIPES
library(stringr)
str_match(alice, ".*\\.D([0-9]+)\\.LIS.*")[, 2]

ingredients3[1]
grepl("[0-9]+ ([0-9]/[0-9] )?",ingredients3[1])
str_match(ingredients3[1], "[0-9]+")
str_match(ingredients3[2], "[0-9]+")
str_extract(ingredients3[10], "[0-9]+ ([0-9]/[0-9])")
str_extract(ingredients3[2], "[0-9](/[0-9]?)+( [0-9]/[0-9])?")
str_extract(ingredients3[10], "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")

check_recipe = function(listed_ingredient) {
  number = str_extract(listed_ingredient, "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")
  return(number)
}

numbers = unlist(lapply(ingredients, check_recipe))
numbers

# FUNCTION TO CONVERT FRACTIONS
fraction_converter = function(num_string) {
  if (grepl("\\/", num_string)) {
    num_denom = strsplit(num_string,"\\/")[[1]]
    dec = as.integer(num_denom[1]) / as.integer(num_denom[2])
  }
  else {
    dec = as.integer(num_string)
  }
  dec
}

# FUNCTION TO CHECK FOR MIXED NUMBER CHECKING
if (grepl("([0-9] )?[0-9]/[0-9]", numbers[10])) {
  mixed_no = strsplit(numbers[10], " ")[[1]]
  sum(unlist(lapply(mixed_no, fraction_converter)))
}
