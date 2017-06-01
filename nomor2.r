##===========No. 2==========
##2. Lakukan Reduksi dimensi dari kesuluruhan data dengan menggunakan PCA untuk
##Komponen utama dengan nilai persentase ciri lebih besar dari 89%. 
##==========================

#step 1 Persiapan
x1<-dataObservasi[,-1]
x2 <-x1
#Step 2: Hitung nilai mean masing-masing fitur dan lakukan pengurangan untuk setiap nilai elemen terhadap mean
#rerata2 = apply(x1, 2, mean)
x_mean <- c(1:2500)

for(i in 1:length(x_mean)){
  x_mean[i] = mean(x1[,i])
}

for(i in 1:length(x_mean)){
  for(j in 1:nrow(x1)){
    x2[j,i] = x1[j,i] - x_mean[i]
  }
}

#matrixrata<-matrix(rep(rerata, 150), ncol=2500, byrow=T)
#datacov <- (x-matrixrata)

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