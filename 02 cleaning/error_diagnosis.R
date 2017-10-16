## ERROR DIAGNOSIS
# GOAL:
# DATA CHECKING FOR ALL_RECIPES
# WE WANT TO MAKE SURE THAT THE RECIPE IS LONG ENOUGH
all_rec_lengths = unlist(lapply(all_recipes, length))
plot(all_rec_lengths, ratingValue, main="Number of Ingredients vs. Rating")
summary(all_rec_lengths)
boxplot(all_rec_lengths)

all_rec_lengths[which(all_rec_lengths<6)]
all_recipes[which(all_rec_lengths<6)]
# CONCLUSION: THESE DATA ARE CLEAN AF



# GOAL:
# DATA CHECKING FOR NUMBERS
# WHY ARE THERE NA'S?
bad_indices = which(unlist(lapply(numbers, anyNA))==TRUE)
badder_indices = which(grepl("cooking", all_recipes[bad_indices])==FALSE)
bad_numbers = bad_indices[badder_indices]
all_recipes[bad_numbers]
numbers[bad_numbers]
# CONCLUSION: ITEMS  77  88 118 HAVE WEIRD INGREDIENTS OR HEADERS
# MAY HAVE TO HAND-CURATE THOSE



# GOAL:
# DATA CHECKING FOR CONVERTED NUMBERS
conv_num = converted_numbers[-bad_numbers]
bad_indices_2 = which(unlist(lapply(conv_num, anyNA))==TRUE)

# LOOKING INTO ISSUE
all_recipes[1]
numbers[1]
converted_numbers[1]
# CONCLUSION: NEED TO REWORK THE FUNCTION
# DONE
