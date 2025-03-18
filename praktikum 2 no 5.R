#Memanggil
data=read.csv("C:\\Users\\ASUS\\Downloads\\bears.csv", sep = ";", header = TRUE)
datanya<-cbind.data.frame(data)
print(datanya)


#summary data
summary(data[, c("HEADLEN", "HEADWTH", "NECK", "CHEST", "WEIGHT")])

#matriks plot
# library(pairs)

pairs(~ HEADLEN + HEADWTH + NECK + CHEST + WEIGHT, data = data, 
            main = "Matrix Plot HEADLEN, HEADWTH, NECK, CHEST, WEIGHT")