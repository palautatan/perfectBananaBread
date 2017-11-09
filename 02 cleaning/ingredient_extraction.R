bbdata = read.csv("/Users/shermanpeng/Documents/R/banana_bread/all_data.csv", as.is=TRUE)
attach(bbdata)

head(bbdata, 2)

# SPLIT INGREDIENT LIST INTO SINGLE INGREDIENTS
split_into_ingredients = function(py_string) {
  ingredients = strsplit(py_string, "\\$")[[1]]
  return(ingredients)
}

library(stringr)

# USE REGEX TO TO GET THE NUMBER 
check_recipe_number = function(listed_ingredient) {
  number = str_extract(listed_ingredient, "[0-9](/[0-9]?)?+( [0-9]/[0-9])?")
  return(number)
}

# CHANGE NUMBER TO DECIMAL 
num_to_dec = function(x) {
  # CHECK IF THERE IS A SLASH
  if (grepl("\\/", x)) {
    if (nchar(x)<5) {
      # NON MIXED NUMBERS
      num_denom = strsplit(x,"\\/")[[1]]
      dec = as.integer(num_denom[1]) / as.integer(num_denom[2])      
    } else {
      # MIXED NUMBERS
      mixed_no = strsplit(x, " ")[[1]]
      whole = as.integer(mixed_no[1])
      num_denom = strsplit(mixed_no[2],"\\/")[[1]]
      frac = as.integer(num_denom[1]) / as.integer(num_denom[2]) 
      dec = whole + frac
    }
  } else {
    # IF NO SLASH, JUST INTEGER
    dec = as.integer(x)
  }
  dec
}

check_measurement = function (single_entry) {
  if (grepl("[0-9]+ ([A-z])+", single_entry)) {
    if (grepl("cup(s)?|[A-z]+spoon(s)?|ounce(s)?", single_entry)) {
      return(str_extract(single_entry, "[A-z]+"))
    } else {
      return(0)
    }
  }
  else {
    return(0)
  }
}

all_recipes = lapply(bbdata$recipe, split_into_ingredients)
the_measurements = lapply(1:length(all_recipes), function(x) unlist(lapply(all_recipes[x][[1]],check_measurement)))

# for (i in 1:length(all_recipes)) {
#   cat(unlist(lapply(all_recipes[i][[1]], check_measurement)))
# }

# FOR ONE
# unlist(lapply(all_recipes[1][[1]], check_measurement))


numbers = lapply(all_recipes, check_recipe_number)
converts = lapply(numbers, function(x) unlist(lapply(x, num_to_dec)))
# the_measurements = lapply(all_recipes, function(x) unlist(lapply(x,check_measurement)))

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

all_ingr = c()
for (i in 1:length(longRating)) {
  a = converts[[i]]
  b = the_measurements[[i]]
  c = ingredients_all[[i]]
  if (length(a)==length(b) & length(a) == length(c)) {
    all_ingr = rbind(all_ingr, (cbind(study_no = i,amount = a, unit = b, ingredient = c, rating = longRating[i])))
  } else {
    break
  }
}


length(converts[3])

parsed_recipes = data.frame(all_ingr)
write.csv(parsed_recipes ,file = "/Users/shermanpeng/Documents/R/banana_bread/parsed_ingredients.csv")
head(parsed_recipes, 20)
