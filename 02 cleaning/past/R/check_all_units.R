fix_ban = read.csv("/Users/EDIE/clean_banana_ingr.csv", as.is=TRUE)[-1]


# CUP | SERVING | SPOON
units_wrong = fix_ban[grepl("cup|serving|spoon", fix_ban$ingredient),]
write.csv(units_wrong, "units_wrong.csv")

# COOKING SPRAY - BINARY
cookspray = fix_ban[grepl("cooking spray", fix_ban$ingredient),]
fix_ban[rownames(cookspray),3] = c("binary", "binary", "binary", "binary", "binary","binary", "binary", "binary", "binary", "binary","binary", "binary", "binary")
fix_ban[rownames(cookspray),2] = c(1,1,1,1,1,1,1,1,1,1,1,1,1)

