## =======NO.5==================
## 5
## =============================

# SVM
data <- dataPCA
dataTrain <- rbind(dataPCA[1:52,], dataPCA[76:128,])
dataTrainLabel <- rbind(dataObservasi[1:52,2501], dataObservasi[76:128,2501])
dataTest <- rbind(dataPCA[53:75,], dataPCA[129:150,])
dataTestLabel <- rbind(dataObservasi[53:75,2501], dataObservasi[129:150,2501])


# Membuat dataset yang siap di SVM kan | Training
dat_in <- data.frame(x=dataTrain, y = as.factor(dataTrainLabel))

# Plot dataset setelah pembelajaran menggunakan SVM kernel RBF
library("e1071")
model_svm <- svm(y~., data = dat_in, kernel ="radial", cost=10, scale = F)

#untuk melihat support vector dan parameter kernel
model_svm$index
summary(model_svm)

# melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
tesdata <- data.frame(x=dataTest, y=as.factor(dataTestLabel))
hasil_prediksi <- as.integer(predict(model_svm, tesdata))
table(prediksi=hasil_prediksi,aktual=tesdata$y)