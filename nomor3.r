## =======NO.3==================
## 
## =============================

# SVM
data <- dataObservasi
dataTrain <- rbind(dataObservasi[1:52,], dataObservasi[76:128,])
dataTest <- rbind(dataObservasi[53:75,], dataObservasi[129:150,])

# Membuat dataset yang siap di SVM kan | Training
dat_in <- data.frame(x=dataTrain[,1:2500], y = as.factor(dataTrain[,2501]))

# Plot dataset setelah pembelajaran menggunakan SVM kernel RBF
library("e1071")
model_svm <- svm(y~., data = dat_in, kernel ="radial", cost=10, scale = F)

#untuk melihat support vector dan parameter kernel
model_svm$index
summary(model_svm)

# melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
tesdata <- data.frame(x=dataTest[,1:2500], y=as.factor(dataTest[,2501]))
hasil_prediksi <- as.integer(predict(model_svm, tesdata))
table(prediksi=hasil_prediksi,aktual=tesdata$y)



# Plot dataset setelah pembelajaran menggunakan SVM kernel Polynom
library("e1071")
model_svm1 <- svm(y~., data = dat_in, kernel ="polynomial", cost=10, scale = F)

#untuk melihat support vector dan parameter kernel
model_svm1$index
summary(model_svm1)

# melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
tesdata <- data.frame(x=dataTest[,1:2500], y=as.factor(dataTest[,2501]))
hasil_prediksi1 <- as.integer(predict(model_svm1, tesdata))
table(prediksi=hasil_prediksi1,aktual=tesdata$y)
