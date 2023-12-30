# Import Data
data <- read.csv("Crime Economics - data.csv")
View(data)

# Mengubah variabel target menjadi kategorik
data$Crime.Rate <- ifelse(data$Crime.Rate < 50, 0, 1)

# Memeriksa hasil
data2 <- data[,-1] # Membuang variabel Country
data2 <- data.frame(data2) # Menjadikan data2 dalam bentuk dataframe

# Mengubah tipe data dari variabel Population Density (per sq. km) dan Per Capita Income menjadi numerik
data2$Population.Density..per.sq..km. <- as.numeric(gsub(",", "", data2$Population.Density..per.sq..km.))
data2$Per.Capita.Income <- as.numeric(gsub(",", "", data2$Per.Capita.Income))

# Melihat data yang sudah disesuaikan untuk kebutuhan analisis
View(data2)
summary(data2) # Melihat statistik deskriptif dari data

# Membuat diagram batang untuk variabel dependen (Crime Rate)
library(ggplot2) 
ggplot(data2, aes(x = Crime.Rate, fill = factor(Crime.Rate))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Crime Rate", x = "Crime Rate", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

# Membuat histogram untuk variabel-variabel independen
hist(data2$Unemployment...., main = "Histogram Unemployment", 
     xlab = "Unemployment", col = "lightblue", border = "black")

hist(data2$HDI, main = "Histogram HDI", 
     xlab = "HDI", col = "lightblue", border = "black")

hist(data2$Population.Density..per.sq..km., main = "HIstogram Population Density (per sq, km)", 
     xlab = "Population Density (per sq, km)", col = "lightblue", border = "black")

hist(data2$Weapons.per.100.persons, main = "Histogram Weapons per 100 persons", 
     xlab = "Weapons per 100 persons", col = "lightblue", border = "black")

hist(data2$Per.Capita.Income, main = "Histogram Per Capita Income", 
     xlab = "Per Capita Income", col = "lightblue", border = "black")

hist(data2$Gini.Coefficient, main = "Histogram Gini Coefficient", 
     xlab = "Gini Coefficient", col = "lightblue", border = "black")

hist(data2$Literacy.Rate, main = "Histogram Literacy Rate", 
     xlab = "Literacy Rate", col = "lightblue", border = "black")

hist(data2$Happiness.Index, main = "Histogram Happiness Index", 
     xlab = "Happiness Index", col = "lightblue", border = "black")

# Melihat korelasi antar-variabel
library(corrplot)
# Menghitung matriks korelasi
cor_matrix <- cor(data2)
# Membuat plot korelasi dengan corrplot
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 35, addCoef.col = "black")

# Membangun Model Regresi Logistik
library(glm2)
model <- glm(Crime.Rate ~ ., data = data2, family = binomial(link = "logit"))
summary(model)

# Eliminasi Backward
step(model, test='Chisq')

# Model Setelah Eliminasi Backward
model2 <- glm(Crime.Rate ~ HDI + Per.Capita.Income + Gini.Coefficient + Happiness.Index, 
             family = binomial(link = "logit"), data = data2)
summary(model2)

# Mendapatkan koefisien dari model
coef_model <- coef(model2)

# Menghitung odds ratio
odds_ratio <- exp(coef_model)
odds_ratio

# Model dengan interaksi antara HDI dan Per.Capita.Income
model_with_interaction <- glm(Crime.Rate ~ HDI * Per.Capita.Income + Gini.Coefficient + Happiness.Index, 
                              family = binomial(link = "logit"), data = data2)
summary(model_with_interaction)

# Model dengan interaksi antara HDI dan Happiness.Index
model_with_interaction2 <- glm(Crime.Rate ~ HDI * Happiness.Index + Per.Capita.Income + Gini.Coefficient, 
                              family = binomial(link = "logit"), data = data2)
summary(model_with_interaction2)

# Model dengan interaksi antara HDI dan Gini.Coefficient
model_with_interaction3 <- glm(Crime.Rate ~ HDI * Gini.Coefficient + Per.Capita.Income + Happiness.Index, 
                               family = binomial(link = "logit"), data = data2)
summary(model_with_interaction3)

# Membandingkan model dengan interaksi dan tanpa interaksi
anova(model2, model_with_interaction, test = "Chisq")
anova(model2, model_with_interaction2, test = "Chisq")
anova(model2, model_with_interaction3, test = "Chisq")

# Uji Hosmer-Lemeshow
library(ResourceSelection)
uji_Hosmer_Lemeshow <- hoslem.test(data2$Crime.Rate, fitted(model2), g = 10)
uji_Hosmer_Lemeshow

# Klasifikasi Tabel (Confusion Matrix)
library(gmodels)
yhat <- fitted(model2) # prediksi peluang
pred_50 <- ifelse(yhat >= 0.50, 1, 0) # prediksi 0/1
classify <- as.data.frame(cbind(pred_50, case=data2$Crime.Rate))
CrossTable(classify$pred_50,classify$case,
           dnn=c("Predicted (cut=.50)", "Observed"),
           format="SAS",prop.r=FALSE,prop.c=FALSE,
           prop.t=FALSE,prop.chisq=FALSE,chisq=FALSE,fisher=FALSE)