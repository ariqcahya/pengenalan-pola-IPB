library(EBImage)

## =======NO.1================== 
## 1. Siapkan Dataset berupa :
## =============================


##a. Mengkompresi dan Menyiapkan Dataset Data Wajah
location <- 'file:///Users/ariqc/Pictures/Pola_Wajah/'

panjang <- 0

for(i in 1:75){
  y <- readImage(files = paste(location,i,".jpg",sep = ""))
  y1 <- channel(y, 'luminance')
  y2 <-  resize(y1, 50) 
  #display(y2)
  y3 <- as.vector(y2)
  if(panjang < length(y3)){
    panjang = length(y3)
  }
  if(i==1){
    m1 <- matrix(ncol = panjang, data = y3)
  }else{
    m1 <- rbind(m1, y3)
  }
  cat("simpan panjang gambar ke ", i, "\n")
  
}
##rownames(m1) <- c(1:75)
dataWajah<-cbind(m1, rep("1", 75))

##b. Mengkompresi dan Menyiapkan Dataset Data Non Wajah
location <- 'file:///Users/ariqc/Pictures/Pola_NonWajah/'

panjang2 <- 0

for(i in 1:75){
  z <- readImage(files = paste(location,i,".jpg",sep = ""))
  z1 <- channel(z, 'luminance')
  z2 <-  resize(z1, 50) 
  #display(y2)
  z3 <- as.vector(z2)
  if(panjang2 < length(z3)){
    panjang2 = length(z3)
  }
  if(i==1){
    m2 <- matrix(ncol = panjang2, data = z3)
  }else{
    m2 <- rbind(m2, z3)
  }
  cat("simpan panjang gambar ke ", i, "\n")
  
}
##rownames(m2) <- c(1:75)
dataNonWajah<-cbind(m2, rep("2", 75))

##c. membuat data observasi sejumlah 150 data
dataObservasi <- rbind(dataWajah,dataNonWajah)
##normalize(dataObservasi[,1:2500])
rownames(dataObservasi) <- c(1:150)