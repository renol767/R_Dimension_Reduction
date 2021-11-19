

#Panggil library openxlsx untuk membaca file data Excel
library(openxlsx)

#Baca data pada sheet "3varb" dalam file https://storage.googleapis.com/dqlab-dataset/dqlab_pcadata.xlsx
#dan simpan data dengan nama df_raw
df_raw <- read.xlsx("https://storage.googleapis.com/dqlab-dataset/dqlab_pcadata.xlsx", sheet = "3varb")

#Tampilkan struktur data
str(df_raw)

#Tampilkan beberapa baris observasi dengan fungsi head()
head(df_raw)

#Lakukan analisa PCA dengan fungsi prcomp()
#simpan output dengan nama pr.out
pr.out <- prcomp(df_raw, center = TRUE, scale = TRUE, retx = TRUE)

#Tampilkan komponen output fungsi prcomp()
names(pr.out)

#Tampilkan output PCA
pr.out

#Tampilkan summary dari output PCA
summary(pr.out)

#Gambarkan scree plot
#Tambahkan garis horizontal sebagai panduan untuk menggunakan kriteria Kaiser
screeplot(pr.out, type = "line")
abline(h = 1, col = "red", lty = 3)

#Gambarkan biplot dengan menggunakan fungsi biplot()
biplot(pr.out, scale = 0)