parsed_ingr = read.csv("/Users/shermanpeng/Documents/R/banana_bread/parsed_ingredients.csv", as.is=TRUE)[-1]
attach(parsed_ingr)
colnames(parsed_ingr)



# RETRIEVE BANANAS ONLY
# DATAFRAME "just_banana" HAS ONLY BANANAS
all_banana = parsed_ingr[which(grepl("banana", ingredient)),]
no_ex_banana = all_banana[which(grepl("extract",all_banana$ingredient)==FALSE),]
just_banana = no_ex_banana[which(grepl("yogurt",no_ex_banana$ingredient)==FALSE),]

length(all_banana[,1])
length(just_banana[,1])


# FIRST, WANT "MASHED"
mashed_banana = just_banana[which(grepl("mashed", just_banana$ingredient)),]
# write.csv(mashed_banana, "mashed_banana.csv")
length(mashed_banana[,1])

other_banana = just_banana[which(grepl("mashed", just_banana$ingredient)==FALSE),]
# write.csv(other_banana, "other_banana.csv")
length(other_banana[,1])



# MASHED BANANAS ONLY
mashed_units = mashed_banana[which(is.na(mashed_banana$unit)==FALSE),] # THESE COO

mashed_no_units = mashed_banana[which(is.na(mashed_banana$unit)),] # THESE NEED CONVERSIONS
length(mashed_no_units$ingredient)


bananaFixer = function(entry_of_nanas) {
  if (grepl("small", entry_of_nanas[,4])) {
    # 4 SMALL = 1.167 CUP
    no_cups = 0.29175*entry_of_nanas[,2]
  } else if (grepl("larger", entry_of_nanas[,4])) {
    # 2.25 LARGE = 1.167 CUP
    no_cups = 0.4668*entry_of_nanas[,2]
  } else {
    # 3 MEDIUM = 1.167 CUP
    no_cups = 0.389*entry_of_nanas[,2]
  }
  entry_of_nanas[,2] = no_cups
  entry_of_nanas[,3] = "cups"
  entry_of_nanas[,4] = "bananas, mashed"
  entry_of_nanas
}


new_entries = c()
for (x in 1:length(mashed_no_units$ingredient)) {
  print(x)
  new_entries = rbind(new_entries, bananaFixer(mashed_no_units[x,]))
}

fix_mashed_no_units = data.frame(new_entries)




# OTHER BANANAS
other_banana = other_banana[which(grepl("garnish", other_banana$ingredient)==FALSE),]

new_entries = c()
for (x in 1:length(other_banana$ingredient)) {
  print(x)
  new_entries = rbind(new_entries, bananaFixer(other_banana[x,]))
}

fixed_other_bananas = data.frame(new_entries)




# PUT THEM TOGETHER
clean_bananas = rbind(mashed_units, fix_mashed_no_units, fixed_other_bananas)

# FIX THEM IN THE ORIGINAL DATAFRAME
original_indices = as.numeric(rownames(clean_bananas))

for (x in 1:length(clean_bananas$ingredient)) {
  print(x)
  parsed_ingr[original_indices[x],] = clean_bananas[x,]
}
parsed_ingr
#write.csv(parsed_ingr, "clean_banana_ingr.csv")

# CHECK
check_df = parsed_ingr[which(grepl("banana", ingredient)),]