#Bandingkan kinerja SVM, PCA, PCA+SVM menggunakan data ini:
#http://archive.ics.uci.edu/ml/datasets/Breast+Cancer

library(xml2)
library(stringr)

##=========================================================
##1. Proses Mengambil Data dan Melakukan Normalisasi Data
##=========================================================
pg <- read_xml("datasets-uci-breast-cancer.xml")

# get all the <hdf5:Group>
recs <- xml_find_all(pg, "//hdf5:Dataset")
colx <- c(1:length(recs))
for(i in 1:length(recs)){
  colx[i] = trimws(xml_text(recs[i]))
  colx[i] = gsub("[^[:alnum:][:blank:]+?&/\\-]", "", colx[i])
  #assign(paste("kolom", i, sep = ""), colx[i])
}
# membuat nama kolom
namakolom <- trimws(xml_attr(recs, "Name"))

# membuat data frame terpisah
df1 <- data.frame(do.call('cbind', strsplit(as.character(colx[1]),'            ',fixed=TRUE)))
colnames(df1) <- namakolom[1]
df2 <- data.frame(do.call('cbind', strsplit(as.character(colx[2]),'            ',fixed=TRUE)))
colnames(df2) <- namakolom[2]
df3 <- data.frame(do.call('cbind', strsplit(as.character(colx[3]),'            ',fixed=TRUE)))
colnames(df3) <- namakolom[3]
df4 <- data.frame(do.call('cbind', strsplit(as.character(colx[4]),'            ',fixed=TRUE)))
colnames(df4) <- namakolom[4]
df5 <- data.frame(do.call('cbind', strsplit(as.character(colx[5]),'            ',fixed=TRUE)))
colnames(df5) <- namakolom[5]
df6 <- data.frame(do.call('cbind', strsplit(as.character(colx[6]),'            ',fixed=TRUE)))
colnames(df6) <- namakolom[6]
df7 <- data.frame(do.call('cbind', strsplit(as.character(colx[7]),'            ',fixed=TRUE)))
colnames(df7) <- namakolom[7]
df8 <- data.frame(do.call('cbind', strsplit(as.character(colx[8]),'            ',fixed=TRUE)))
colnames(df8) <- namakolom[8]
df9 <- data.frame(do.call('cbind', strsplit(as.character(colx[9]),'            ',fixed=TRUE)))
colnames(df9) <- namakolom[9]
df10 <- data.frame(do.call('cbind', strsplit(as.character(colx[10]),'            ',fixed=TRUE)))
colnames(df10) <- namakolom[10]

#menggabungkan semua dataframe menjadi satu
BreastCancer = cbind(df2, df3, df4, df5, df6, df7, df8, df9, df10, df1)
#Menghapus variabel yang tidak perlu
rm(df2, df3, df4, df5, df6, df7, df8, df9, df10, df1, colx, i, namakolom, pg, recs)

##=========================================================
##2. Proses Klasifikasi Menggunakan SVM
##=========================================================
# SVM + Kernel RBF(radial) & Polinom
#==========================================================

# Membuat data training 75%  dan testing 25%
dataTrain <- BreastCancer[1:213,]
dataTest <- BreastCancer[214:286,]

# Membuat dataset yang siap di SVM kan | Training
dat_in <- data.frame(x=dataTrain[,1:9], y = as.factor(dataTrain[,10]))

# Plot dataset setelah pembelajaran menggunakan SVM kernel RBF
library("e1071")
model_svm <- svm(y~., data = dat_in, kernel ="radial", cost=10, scale = F)

#untuk melihat support vector dan parameter kernel
model_svm$index
summary(model_svm)

# melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
tesdata <- data.frame(x=dataTest[,1:9], y=as.factor(dataTest[,10]))
hasil_prediksi <- as.integer(predict(model_svm, tesdata))
table(prediksi=hasil_prediksi,aktual=tesdata$y)

# Plot dataset setelah pembelajaran menggunakan SVM kernel Polynom
library("e1071")
model_svm1 <- svm(y~., data = dat_in, kernel ="polynomial", cost=10, scale = F)

#untuk melihat support vector dan parameter kernel
model_svm1$index
summary(model_svm1)

# melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
tesdata <- data.frame(x=dataTest[,1:9], y=as.factor(dataTest[,10]))
hasil_prediksi1 <- as.integer(predict(model_svm1, tesdata))
table(prediksi=hasil_prediksi1,aktual=tesdata$y)
