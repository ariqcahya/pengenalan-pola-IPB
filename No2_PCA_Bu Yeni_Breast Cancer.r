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

##===========No. 2==========
##2. Lakukan Reduksi dimensi dari kesuluruhan data dengan menggunakan PCA untuk
##Komponen utama dengan nilai persentase ciri lebih besar dari 89%. 
##==========================

#step 1 Persiapan
bc<-BreastCancer

#merubah dataframe ke bentuk binary (converting-factors-to-binary)
#https://stackoverflow.com/questions/33990760/converting-factors-to-binary-in-r
dfBC<-cbind(
  sapply(levels(bc$age), function(x) as.integer(x == bc$age)), 
  sapply(levels(bc$breast), function(x) as.integer(x == bc$breast)), 
  sapply(levels(bc$'breast-quad'), function(x) as.integer(x == bc$'breast-quad')),
  bc[4],
  sapply(levels(bc$'inv-nodes'), function(x) as.integer(x == bc$'inv-nodes')),
  sapply(levels(bc$irradiat), function(x) as.integer(x == bc$irradiat)),
  sapply(levels(bc$menopause), function(x) as.integer(x == bc$menopause)),
  sapply(levels(bc$'node-caps'), function(x) as.integer(x == bc$'node-caps')),
  sapply(levels(bc$irradiat), function(x) as.integer(x == bc$irradiat)),
  sapply(levels(bc$'tumor-size'), function(x) as.integer(x == bc$'tumor-size')),
  bc[10])
#merubah factor ke bentuk numeric
dfBC <- data.frame(lapply(dfBC, as.numeric))
#memasukan data ke variabel kecuali class prediksinya
x1<-dfBC[,1:43]
x2 <-x1

#Step 2: Hitung nilai mean masing-masing fitur dan lakukan pengurangan untuk setiap nilai elemen terhadap mean
x_mean <- c(1:43)

for(i in 1:length(x_mean)){
  x_mean[i] = mean(x1[,i])
}

for(i in 1:length(x_mean)){
  for(j in 1:nrow(x1)){
    x2[j,i] = x1[j,i] - x_mean[i]
  }
}

#Step 3: Hitung matriks kovarians (matcov)
matkov <- (t(x2)%*%as.matrix(x2))/(nrow(x2)-1)

#Step 4: Hitung nilai eigen dan vektor eigen dari matriks kovarian
eigennya<-eigen(matkov)
eigennya$vectors
eigennya$values

#Step 6: Konversi nilai eigen ke dalam bentuk persentase komponen utama
konversi=eigennya$values/sum(eigennya$values) * 100
persentase_ciri=cumsum(konversi/sum(konversi) * 100)

#Step 7: Penentuan batas persentase ciri dari komponen utama
pc<-89 ##ambil presentasi ciri sebesar 89%
idx<-as.integer()
for(i in 1:length(persentase_ciri)){
  if(persentase_ciri[i] >= pc){
    idx<-i
    break
  }
}
#Step 8: Komponen utama dengan nilai persentase ciri lebih besar dari 89%
pc_89=eigennya$vectors[, 1:idx]
dataPCA=as.matrix(x1)%*%pc_89

dataPCA_ok = cbind(dataPCA, dfBC$Class)
#mengapus variabel yang tidak perlu
rm(x_mean, persentase_ciri, pc, konversi, j, idx, i, eigennya, x2, x1, pc_89,matkov, bc)
