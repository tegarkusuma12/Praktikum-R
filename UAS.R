library(readxl)

data <- read_excel("C:\\Users\\ASUS\\OneDrive\\Semester-1\\Statistika\\IPM-Banten.xlsx")
data

#Buat model regresi 
model <- lm(IPM ~ `UHH (tahun)` + `HLS (tahun)` + `RLS (tahun)` + `Pengeluaran (ribu rupiah)`, data = data)

# Ringkasan model regresi
summary(model)

# Uji Simultan (Uji F)
anova_result <- anova(model)
cat("Hasil Uji Simultan (Uji F):\n")
print(anova_result)

# Uji Parsial (Uji t)
cat("Hasil Uji Parsial (Uji t):\n")
summary(model)$coefficients

