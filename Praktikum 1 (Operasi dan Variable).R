# Tentukan nilai untuk a dan b
a <- 1:10  # Vektor a berisi angka 1 sampai 10
b <- 20:29  # Vektor b berisi angka 20 sampai 29

# Operasi Matematika
penjumlahan <- a + b
pengurangan <- a - b
perkalian <- a * b
pembagian <- a / b
sisa_hasil_bagi <- a %% b
pangkat <- a^b

# Tampilkan hasil
print(penjumlahan)
print(pengurangan)
print(perkalian)
print(pembagian)
print(sisa_hasil_bagi)
print(pangkat)

# Membuat deret
x <- seq(from = -5, to = 3, by = 1)
y <- seq(from = 5, to = 1, by = -1)
z <- seq(from = 1000000, to = 2000000, length = 7)
p <- rep(c(9, 5), each = 3)
q <- rep(c(1, 2), times = 6)

# Menampilkan semua deret
print(x)
print(y)
print(z)
print(p)
print(q)

# Luas persegi panjang dengan panjang 5 dan lebar 3
panjang_persegi <- 5
lebar_persegi <- 3
luas_persegi_panjang <- panjang_persegi * lebar_persegi

# Luas segitiga dengan alas 4 dan tinggi 6
alas_segitiga <- 8
tinggi_segitiga <- 10
luas_segitiga <- 0.5 * alas_segitiga * tinggi_segitiga

# Volume balok dengan panjang 2, lebar 3, dan tinggi 4
panjang_balok <- 2
lebar_balok <- 3
tinggi_balok <- 4
volume_balok <- panjang_balok * lebar_balok * tinggi_balok  

# Volume bola dengan jari-jari 5
jari_jari_bola <- 5
volume_bola <- (4/3) * pi * jari_jari_bola^3

# Cetak hasil perhitungan
print(luas_persegi_panjang)
print(luas_segitiga)
print(volume_balok)
print(volume_bola)


# Membuat data frame siswa
siswa <- data.frame(
  Nomor = 1:10,
  Nama = c("Gunadi", "Bintang", "Sinta", "Widya", "Harun", "Nurhaliza", "Didit", "Marjan", "Zualiha", "Efendy"),
  Prodi = c("D4 Teknik Elektronika", "D4 Teknik Informatika", "D4 Teknik Komputer", "D4 Teknik Mekatronika",
            "D3 Multimedia Broadcasting", "D4 Sains Data Terapan", "D3 Teknik Informatika", "D4 Teknik Elektronika",
            "D4 Teknik Telekomunikasi", "D4 Sains Data Terapan"),  
  
  Asal_KotKab = c("Bojonegoro", "Surabaya", "Banyuwangi", "Gresik", "Ngawi", "Metro", "Tulang Bawang", "Pasuruan", "Jombang", "Surabaya"),
  Usia = c(19, 19, 18, 19, 17, 18, 18, 19,  
           20, 19)
)

# Melihat data frame
print(siswa)


# Simulasi data
x <- rnorm(30, mean = 5, sd = 2)
y <- rnorm(30, mean = 3, sd = 1.5)

# Hitung rata-rata
mean_x <- mean(x)
mean_y <- mean(y)

# Hitung variansi
var_x <- var(x)
var_y <- var(y)

# Hitung kovariansi
cov_xy <- cov(x, y)

# Hitung korelasi
cor_xy <- cor(x, y)

# Tampilkan hasil
cat("Rata-rata X:", mean_x, "\n")
cat("Rata-rata Y:", mean_y, "\n")
cat("Variansi X:", var_x, "\n")
cat("Variansi Y:", var_y, "\n")
cat("Kovariansi X dan Y:", cov_xy, "\n")
cat("Korelasi X dan Y:", cor_xy, "\n")

# Inisiasi
bilangan <- 7

# Proses
if (bilangan %% 2 == 0) {
  hasil <- "Bilangan genap"
} else {
  hasil <- "Bilangan ganjil"
}

# Hasil
print(paste(bilangan, "adalah", hasil))

# Fungsi untuk mencari akar-akar persamaan kuadrat
akar_persamaan_kuadrat <- function(a, b, c) {
  # Hitung diskriminan
  diskriminan <- b^2-4*a*c
  
  if (diskriminan > 0) {
    # Akar riil berbeda
    akar1 <- (-b + sqrt(diskriminan)) / (2*a)
    akar2 <- (-b - sqrt(diskriminan)) / (2*a)
    return(paste("Akar-akar persamaan adalah", akar1, "dan", akar2))
  } else if (diskriminan == 0) {
    # Akar riil kembar
    akar <- -b/(2*a)
    return(paste("Akar persamaan adalah", akar))
  } else {
    # Akar imajiner
    return("Akar-akar persamaan adalah imajiner")
  }
}

# Contoh penggunaan
a <- 1
b <- -5
c <- 6
hasil <- akar_persamaan_kuadrat(a, b, c)
print(hasil)

# Membuat vektor nilai_angka dengan panjang 20 dan rentang 0-100
nilai_angka <- sample(0:100, 20, replace = TRUE)

# Fungsi untuk konversi nilai angka ke nilai huruf
konversi_nilai <- function(nilai) {
  if (nilai <= 40) {
    return("E")
  } else if (nilai <= 55) {
    return("D")
  } else if (nilai <= 60) {
    return("C")
  } else if (nilai <= 65) {
    return("BC")
  } else if (nilai <= 70) {
    return("B")
  } else if (nilai <= 80) {
    return("AB")
  } else {
    return("A")
  }
}

# Menerapkan fungsi konversi pada setiap nilai dalam vektor nilai_angka
nilai_huruf <- sapply(nilai_angka, konversi_nilai)

# Membuat data frame
hasil <- data.frame(nilai_angka = nilai_angka, nilai_huruf = nilai_huruf)

# Menampilkan data frame
print(hasil)

# Fungsi untuk menghitung rata-rata
hitung_rata_rata <- function(data) {
  jumlah_data <- length(data)
  jumlah_semua_data <- sum(data)
  rata_rata <- jumlah_semua_data / jumlah_data
  return(rata_rata)
}

# Fungsi untuk menghitung variansi
hitung_variansi <- function(data) {
  rata_rata_data <- hitung_rata_rata(data)
  n <- length(data)
  jumlah_kuadrat_selisih <- sum((data - rata_rata_data)^2)
  variansi <- jumlah_kuadrat_selisih / (n - 1)
  return(variansi)
}

# Fungsi untuk menghitung simpangan baku
hitung_simpangan_baku <- function(data) {
  variansi_data <- hitung_variansi(data)
  simpangan_baku <- sqrt(variansi_data)
  return(simpangan_baku)
}

# Contoh penggunaan
data_contoh <- c(2, 4, 5, 4, 5, 3, 6)

rata_rata_hasil <- hitung_rata_rata(data_contoh)
variansi_hasil <- hitung_variansi(data_contoh)
simpangan_baku_hasil <- hitung_simpangan_baku(data_contoh)
print(paste("Rata-rata:", rata_rata_hasil))
print(paste("Variansi:", variansi_hasil))
print(paste("Simpangan Baku:", simpangan_baku_hasil))

# Fungsi untuk mengurutkan data 
bubble_sort <- function(data) {
  
  n <- length(data) # Panjang dari data yang akan diurutkan
  
  # Loop luar untuk mengulang setiap pass
  for (i in 1:(n-1)) {
    # Loop dalam untuk membandingkan dan menukar elemen
    for (j in 1:(n-i)) {
      if (data[j] > data[j+1]) {
        # Jika elemen saat ini lebih besar dari elemen berikutnya, tukar
        temp <- data[j] # Simpan nilai elemen saat ini sementara
        data[j] <- data[j+1]  # Ganti elemen saat ini dengan elemen berikutnya
        data[j+1] <- temp  # Ganti elemen berikutnya dengan nilai yang disimpan sebelumnya
      }
    }
  }
  
  return(data) # Mengembalikan data yang sudah terurut
}

# Contoh penggunaan
data_acak <- c(1,2,3,4)  # Data acak yang akan diurutkan
data_terurut <- bubble_sort(data_acak)  # Memanggil fungsi
print(data_terurut)  # Menampilkan data yang sudah terurut
