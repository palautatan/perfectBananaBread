bbdata = read.csv("/Users/EDIE/GitHub/perfectBananaBread/00 datasets/all_data.csv")
attach(bbdata)
ratings_only = bbdata[ratingValue != 0 & made.it != 0,]
detach(bbdata)

clean_temp = c()
i = 1
for (number in ratings_only$fahrenheit) {
  if (grepl("[A-z]", number)) {
    clean_temp[i] = NA
  } else {
    clean_temp[i] = as.numeric(number)
  }
  i = i+1
}

clean_temp

hist(clean_temp)
summary(clean_temp) # WE ARE BAKING OUR BREAD AT 350

plot(clean_temp, ratings_only$longRating, main="Squat")
