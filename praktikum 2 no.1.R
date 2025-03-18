#Memanggil
data=read.csv("D:\\Pulse.csv",header=TRUE)
datanya<-cbind.data.frame(data)
print(datanya)

#1a
#data gender 
height = by(data$Hgt, data$Gender, summary)
weight = by(data$Wgt, data$Gender, summary)

#data active
active_smoke = by(data$Active, data$Smoke, summary)
active_gender = by(data$Active, data$Gender, summary)
active_exercise = by(data$Active, data$Exercise, summary)

#data rest
rest_smoke    = by(data$Rest, data$Smoke, summary)
rest_gender   = by(data$Rest, data$Gender, summary)
rest_exercise = by(data$Rest, data$Exercise, summary)

print(height)
print(weight)
print(active_smoke)
print(active_gender)
print(active_exercise)
print(rest_smoke)
print(rest_gender)
print(rest_exercise)

#1b
boxplot(Hgt ~ Gender, data = data, main = "Distribusi Tinggi Badan Berdasarkan Gender")
boxplot(Wgt ~ Gender, data = data, main = "Distribusi Berat Badan Berdasarkan Gender")
boxplot(Active ~ Smoke, data = data, main = "Boxplot berdasarkan Active dan Smoke")
boxplot(Active ~ Gender, data = data, main = "Boxplot berdasarkan Active dan Gender")
boxplot(Active ~ Exercise, data = data, main = "Boxplot berdasarkan Active dan Exercise")
boxplot(Rest ~ Smoke, data = data, main = "Boxplot berdasarkan Rest dan Smoke")
boxplot(Rest ~ Gender, data = data, main = "Boxplot berdasarkan Rest dan Gender")
boxplot(Rest ~ Exercise, data = data, main = "Boxplot berdasarkan Rest dan Exercise")

#1c
library(dplyr)

# Menghitung statistik untuk Height berdasarkan Gender
result_height <- data %>%
  group_by(Gender) %>%
  summarize(
    mean_height = mean(Hgt),
    median_height = median(Hgt),
    variance_height = var(Hgt),
    Q1_height = quantile(Hgt, 0.25),
    Q3_height = quantile(Hgt, 0.25),
    cv_height = sd(Hgt) / mean(Hgt)
  )
print(result_height)

# Menghitung statistik untuk Weight berdasarkan Gender
result_weight <- data %>%
  group_by(Gender) %>%
  summarize(
    mean_weight = mean(Wgt),
    median_weight = median(Wgt),
    variance_weight = var(Wgt),
    Q1_weight = quantile(Wgt, 0.25),
    Q3_weight = quantile(Wgt, 0.25),
    cv_weight = sd(Wgt) / mean(Wgt)
  )
print(result_weight)

# Menghitung statistik untuk Active 
result_active <- data %>%
  group_by(Smoke, Gender, Exercise) %>%
  summarize(
    mean_active = mean(Active),
    median_active = median(Active),
    variance_active = var(Active),
    Q1_active = quantile(Active, 0.25),
    Q3_active = quantile(Active, 0.75),
    cv_active = sd(Active) / mean(Active)
  )
print(result_active)

# Menghitung statistik untuk Rest 
result_rest <- data %>%
  group_by(Smoke, Gender, Exercise) %>%
  summarize(
    mean_rest = mean(Rest),
    median_rest = median(Rest),
    variance_rest = var(Rest),
    Q1_rest = quantile(Rest, 0.25),
    Q3_rest = quantile(Rest, 0.75),
    cv_rest = sd(Rest) / mean(Rest)
  )
print(result_rest)

# Menampilkan hasil

#1d

library(ggplot2)

# Histogram untuk Active dan Smoke
ggplot(data, aes(x = Active, fill = as.factor(Smoke))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Active berdasarkan Smoke",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), labels = c("0", "1"))

# Histogram untuk Active dan Gender
ggplot(data, aes(x = Active, fill = as.factor(Gender))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Active berdasarkan Gender",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), 
                    labels = c("0", "1"))

# Histogram untuk Active dan Exercise
ggplot(data, aes(x = Rest, fill = as.factor(Exercise))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Active berdasarkan Exercise",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("red","yellow", "green"), 
                    labels = c("1", "2","3"))

# Histogram untuk Rest dan Exercise
ggplot(data, aes(x = Rest, fill = as.factor(Exercise))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Rest berdasarkan Exercise",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("red","yellow", "green"), 
                    labels = c("1", "2","3"))

# Histogram untuk Rest dan Gender
ggplot(data, aes(x = Rest, fill = as.factor(Gender))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Rest berdasarkan Gender",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), labels = c("0", "1"))

# Histogram untuk Rest dan Smoke
ggplot(data, aes(x = Rest, fill = as.factor(Smoke))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Rest berdasarkan Smoke",
       x = "Rest",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), labels = c("0", "1"))

# Histogram untuk Height dan Gender
ggplot(data, aes(x = Hgt, fill = as.factor(Gender))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Tinggi Badan Berdasarkan Gender",
       x = "Tinggi Badan (cm)",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), labels = c("0", "1"))

# Histogram untuk Weight dan Gender
ggplot(data, aes(x = Wgt, fill = as.factor(Gender))) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribusi Berat Badan Berdasarkan Gender",
       x = "Berat Badan",
       y = "Frekuensi") +
  scale_fill_manual(values = c("yellow", "green"), 
                    labels = c("0", "1"))

