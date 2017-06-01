##===========No. 2==========
##2. Lakukan Reduksi dimensi dari kesuluruhan data dengan menggunakan PCA untuk
##Komponen utama dengan nilai persentase ciri lebih besar dari 89%. 
##==========================

#step 1 Ambil data dengan sejumlah fitur
x<-data.frame(matrix(rnorm(60), ncol=3))
x[1:nrow(x),1]<-x[1:nrow(x),1]-3
x[1:nrow(x),3]<-x[1:nrow(x),3]+3

#Step 2: Hitung nilai mean masing-masing fitur dan lakukan pengurangan untuk
#setiap nilai elemen terhadap mean
rerata=apply(x,2,mean)
matrixrata<-matrix(rep(rerata, 20), ncol=3, byrow=T)
datacov <- (x-matrixrata)

#Step 3: Hitung matriks kovarians (matcov)
matkov <- (t(datacov)%*%as.matrix(datacov))/(nrow(datacov)-1)

#Step 4: Hitung nilai eigen dan vektor eigen dari matriks kovarian
eigennya<-eigen(matkov)
eigennya$vectors
eigennya$values

#Step 6: Konversi nilai eigen ke dalam bentuk persentase komponen utama
konversi=eigennya$values/sum(eigennya$values) * 100
(persentase_ciri=cumsum(konversi/sum(konversi) * 100))

#Step 7: Penentuan batas persentase ciri dari komponen utama
pc<-80 ##ambil presentasi ciri sebesar 80%
idx<-as.integer()
for(i in 1:length(persentase_ciri)){
  if(persentase_ciri[i] >= pc){
    idx<-i
    break
  }
}
#Step 8: Komponen utama dengan nilai persentase ciri lebih besar dari 89%
pc_80=eigennya$vectors[, 1:idx]
dataPCA=as.matrix(x)%*%pc_80