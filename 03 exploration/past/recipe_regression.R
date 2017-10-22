# LOAD IN THE DATA
parsed_ingr = read.csv("/Users/EDIE/organized_recipes.csv", as.is=TRUE)[-1]
attach(parsed_ingr)

# GET THE MOST POPULAR INGREDIENTS
sorted = sort(table(ingredient))
data.frame(sorted[415:435])

# PLOT FOR BANANA
ban_amt = amount[which(grepl("banana", ingredient))]
ban_rate = rating[which(grepl("banana", ingredient))]
plot(ban_amt, ban_rate, main="Amount of Banana v. Recipe Rating")

# PLOT FOR FLOUR
flr_amt = amount[which(grepl("flour", ingredient))]
flr_rate = rating[which(grepl("flour", ingredient))]
plot(flr_amt, flr_rate, main="Amount of Flour v. Recipe Rating")


# PLOT FOR BAKING SODA
bs_amt = amount[which(grepl("baking soda", ingredient))]
bs_rate = rating[which(grepl("baking soda", ingredient))]
plot(bs_amt, bs_rate, main="Amount of Baking Soda v. Recipe Rating")


# PLOT FOR SUGAR
ws_amt = amount[which(grepl("white sugar", ingredient))]
ws_rate = rating[which(grepl("white sugar", ingredient))]
ws_mod = lm(ws_rate ~ ws_amt)
plot(ws_amt, ws_rate, main="Amount of White Sugar v. Recipe Rating")
abline(ws_mod)

# PLOT FOR BROWN SUGAR
brs_amt = amount[which(grepl("brown sugar", ingredient))]
brs_rate = rating[which(grepl("brown sugar", ingredient))]
brs_mod = lm(brs_rate ~ brs_amt)
plot(brs_amt, brs_rate, main="Amount of Brown Sugar v. Recipe Rating")
abline(brs_mod)



# PLOT FOR EGGS
egg_amt = amount[which(grepl("egg", ingredient))]
egg_rate = rating[which(grepl("egg", ingredient))]
egg_mod = lm(egg_rate ~ egg_amt)

plot(egg_amt, egg_rate, main="Amount of Eggs v. Recipe Rating")
abline(egg_mod)


# CONCLUSION:
# We cannot use normal linear regression on this
