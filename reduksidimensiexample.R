library(openxlsx)
df <- read.xlsx("dqlab_pcadata.xlsx", sheet="3varb")

#standarisasi variabel (centering dan scaling)
df <- scale(df, center = TRUE, scale = TRUE)
head(df, 3)

# Menghitung matrix korelasi data
cormat <- cor(df)
cormat

# Menghitung nilai eigen
eig <- eigen(cormat)
eig

# Memilih banyaknya principal component
# Kontribusi PC1 sampai PC3 terhadap variabilitas data
round(eig$values/ncol(df),3)
# Kontribusi Kumulatif PC1 sampai PC3
round(cumsum(eig$values/ncol(df)),3)

pr.out <- prcomp(df, scale. = TRUE, center = TRUE)
pr.out
summary(pr.out)

library(factoextra)

fviz_eig(pr.out, addlabels = TRUE)

screeplot(pr.out, type = "line")
abline(h = 1, lty = 3, col = "red")

# Visualisasikan dengan Biplot
pr.out$rotation
biplot(pr.out, scale = 0)

# Menghitung Skor Baru
head(df)
df_new <- df %*% pr.out$rotation
df_new[1:6,1:2]