# 1 soal materi penduga selang untuk rata-rata
mu = 3.6
sd = 0.3
n = 36
a = 1 - 0.95 #0.05

se = sd / sqrt(n) #standar error 

#rumus
z = qnorm(1 - a/2)
batas_bawah = mu - (z*se)
batas_atas = mu + (z*se)

# Tampilkan hasil
cat("P(",round(batas_bawah, 3),"<= mu <=",round(batas_bawah, 3),")" )
cat("Selang kepercayaan 95% untuk rata-rata nilai IPK adalah (",
    round(batas_bawah, 3), ",", round(batas_atas, 3), ")")

# 2 soal materi penduga selang untuk beda dua rata-rata
mu_xa = 36
mu_xb = 42
n_a = 50
n_b = 75
sd_a = 6
sd_b = 8

alpha = 1 - 0.96 #0.04

se = sqrt((sd_a^2/n_a) + (sd_b^2/n_b)) #standar error

#rumus
z=qnorm(1 - alpha/2)
bb = (mu_xb-mu_xa) - z*se
ba = (mu_xb-mu_xa) + z*se

# Tampilkan hasil
cat("P(",round(bb, 2),"< μB - μA <",round(ba, 2),")\n")
cat("Selang kepercayaan 96% untuk selisih rata-rata konsumsi BBM μB - μA adalah", 
    round(bb, 2), ",", round(ba, 2))
#------END--------

# 3 penduga selang untuk proporsi
n = 450
x = 120
p = x/n # 0.267

a = 1 - 0.95 # 0.05

#rumus
z = qnorm(1 - a/2)
ba = p + z * (p*(1-p)/n)^0.5
bb = p - z * (p*(1-p)/n)^0.5

#tampilkan hasil 
cat(round(bb, 2),"< p <", round(ba, 2), "\n")
cat("Selang kepercayaan 95% untuk proporsi perokok adalah", 
    round(bb, 2), ",", round(ba, 2))
#------END--------

# 4 penduga selang ragam tidak diketahui, penduga paramter rata-rata
data = c(9.8, 10.2, 10.4, 9.8, 10, 10.2, 9.6)
n = length(data)

mu = ceiling(mean(data)) # 10
simpangan_baku = sd(data)
tingkat_kepercayaan =  0.95

a = 1 - tingkat_kepercayaan 
derajat_bebas = n - 1 # 6
se = simpangan_baku / sqrt(n) #standar error

# Hitung nilai t
t_nilai <- qt(p = 1 - a / 2, df = derajat_bebas) # 2.446912

#rumus
batas_bawah <- mu - t_nilai * se
batas_atas <- mu + t_nilai * se

#tampilkan
cat(round(batas_bawah, 2),"< μ <", round(batas_atas, 2), "\n")
cat("Selang kepercayaan 95% untuk rata-rata isi botol adalah", 
    round(batas_bawah, 2), ",", round(batas_atas, 2))
#------END--------

# 5 Pendugaan 2 parameter beda dua rata-rata(μ1 - μ2), jika kedua sampel tidak bebas 
# data
d <- c(-5, 8, -2, -12, 5, -2, -8, 1, -6, 5)
n <- length(d)

# Hitung rata-rata dan standar deviasi selisih
mean_d <- mean(d)
sd_d <- sd(d)

# Hitung standar error
se <- sd_d / sqrt(n)

# Tentukan tingkat kepercayaan dan derajat bebas
tingkat_kepercayaan <- 0.98
df <- n - 1 # 9

# Hitung nilai t
t_nilai <- qt(p = 1 - (1-tingkat_kepercayaan)/2, df = df)

# Hitung batas bawah dan atas selang kepercayaan
batas_bawah <- mean_d - t_nilai * se
batas_atas <- mean_d + t_nilai * se

# Tampilkan hasil
cat(round(batas_bawah, 2),"< μD <", round(batas_atas, 2), "\n")
cat("Selang kepercayaan 98% untuk selisih rata-rata nilai ujian adalah (", 
    round(batas_bawah, 2), ",", round(batas_atas, 2), ")")
#----END----

# SOAL KUIS
# 1
mu <- 8
sd <- 0.9
x1 <- 7.3
x2 <- 9.1

# hitung z 
z_kiri <- (x1 - mu) / sd
z_kanan <- (x2 - mu) / sd

# hitung p
p <- pnorm(round(z_kanan, 2)) - pnorm(round(z_kiri, 2))

#hasil
cat("Proporsi kucing Ragdoll dengan berat antara 7.3 dan 9.1 kg adalah:", round(p, 4))
#----END----

# 2
rata_rata <- 174.5
sd <- 6.9
n <- 50
cl <- 0.98

# hitung alpha (tingkat signifikasi)
a <- 1 - cl # 0.02

# hitung standar error
se <- sd/sqrt(n)

# hitung nilai t untuk tingkat signifikansi alpha/2 dan derajat bebas n-1
t_nilai <- qnorm(p = 1 - a/2)

# hitung batas bawah dan atas
batas_bawah <- rata_rata - t_nilai * se
batas_atas <- rata_rata + t_nilai * se

# hasil
cat(round(batas_bawah, 1),"< μ <", round(batas_atas, 1), "\n")
cat("Selang kepercayaan 98% untuk rata-rata tinggi adalah (", 
    round(batas_bawah, 1), ",", round(batas_atas, 1), ")")
#----END----

# 3
rata_rata <- 11.3
sd_sample <- 2.45
n <- 20
cl <- 0.95

# hitung alpha (tingkat signifikansi)
a <- 1 - cl # 0.05

# hitung standar error
se <- sd_sample / sqrt(n)

# hitung nilai z untuk tingkat signifikansi alpha/2 
z_nilai <- qnorm(1 - a/2)

# hitung batas bawah dan atas
batas_bawah <- rata_rata - z_nilai * se
batas_atas <- rata_rata + z_nilai * se

# hasil
cat(round(batas_bawah, 2),"< μ <", round(batas_atas, 2), "\n")
cat("Selang kepercayaan 95% untuk kadar lemak adalah (", 
    round(batas_bawah, 2), ",", round(batas_atas, 2), ")")











