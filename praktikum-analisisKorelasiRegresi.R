library(ggplot2)
library(dplyr)

#---Data Lahan dan Keuntungan Petani---
data <- data.frame(
  No_Petani = 1:12,
  Luas_Lahan = c(0.21, 0.50, 0.14, 1.00, 0.21, 0.07, 0.50, 1.00, 0.70, 0.14, 0.35, 0.28),
  Keuntungan = c(0.50, 1.10, 0.25, 1.80, 0.40, 0.20, 0.90, 2.00, 1.20, 0.30, 0.70, 0.65)
)

# Scatter plot
ggplot(data, aes(x = Luas_Lahan, y = Keuntungan)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot Luas Lahan vs Keuntungan",
       x = "Luas Lahan (X)",
       y = "Keuntungan (Y)") +
  theme_minimal()

# 1 Korelasi Pearson
hasil_pearson <- cor.test(data$Luas_Lahan, data$Keuntungan, method = "pearson")
hasil_pearson

# 4 Koefisien Determinasi
koef_determinasi <- hasil_pearson$estimate^2
cat("Koefisien Determinasi (R-squared):", koef_determinasi, "\n")

# 2 Korelasi Rank's Spearman
hasil_spearman <- cor.test(data$Luas_Lahan, data$Keuntungan, method = "spearman")
hasil_spearman

# Mengubah data numerik menjadi kategori 
median_luas_lahan <- median(data$Luas_Lahan)
median_keuntungan <- median(data$Keuntungan)

data$Luas_Lahan_Kategori <- ifelse(data$Luas_Lahan > median_luas_lahan, "Tinggi", "Rendah")
data$Keuntungan_Kategori <- ifelse(data$Keuntungan > median_keuntungan, "Tinggi", "Rendah")

# Membuat tabel kontingensi
tabel_kontingensi <- table(data$Luas_Lahan_Kategori, data$Keuntungan_Kategori)
print(tabel_kontingensi)


#-----Data Bank------
Bank <- rep(c("Swasta", "Pemerintah"), each = 3)
Kepuasan <- rep(c("Tidak Puas", "Netral", "Puas"), 2)
Frekuensi <- c(16, 9, 15, 10, 5, 25)

data <- data.frame(Bank, Kepuasan, Frekuensi)

# 3 Tabel Kontingensi
tabel_kontingensi <- xtabs(Frekuensi ~ Bank + Kepuasan, data = data)
print(tabel_kontingensi)

# Uji Chi-Square
uji_chi <- chisq.test(tabel_kontingensi)
print(uji_chi)



