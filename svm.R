#1. membuat ilustrasi
data1<-matrix(c(2,3,2,10,4,6,7,7,9,5,9,9,12,4,13,6,15,2,10,16),
              ncol = 2, byrow = T)
data1
y1<-rep(-1,nrow(data1))
data2<- matrix(c(6,13,7,19,11,14,12,18,13,12,13,15,16,15,17,9,17,12,19,16), 
               ncol = 2, byrow = T)
data2
y2<-rep(1,nrow(data2))
x<-rbind(data1,data2)
y<- c(y1,y2)

#2. plot data set
plot(x,col=(3-y))

#3. Membuat dataset yang siap di SVM kan
dat_in <- data.frame(x=x, y=as.factor(y))

#4. Plot dataset setelah pembelajaran menggunakan SVM
library("e1071")
model_svm<-svm(y~., data=dat_in, kernel="linear",cost=10, scale = F)
plot(model_svm,dat_in)
#untuk melihat support vector dan parameter keenel
model_svm$index
summary(model_svm)

#5. cobakan untuk mengganti cost = 0.05
#apa yang terjadi?
model2_svm<-svm(y~., data=dat_in, kernel="linear",cost=0.05, scale = F)
plot(model2_svm,dat_in)
summary(model2_svm)

#6. parameter cost yang optimum dengan k-fold
#Gunakan k-fold --> Fungsi tune
set.seed(1)
tune.out<-tune(svm, y~., data=dat_in, kernel="polynomial",
               ranges = list(cost=c(0.05, 0.1, 0.5, 1, 1.5, 2, 2.5, 5, 10)))
summary(tune.out)
bestparam<-tune.out$best.model

#7. melakukan prediksi Lakukan prediksi dengan terutama memilih data uji yang sudah diketahui target classnya
xtes<-matrix(c(1,1,2,3,16,16,4,4,18,14), ncol=2, byrow = T)
ytes<-c(-1,-1,1,-1,1)
tesdata<-data.frame(x=xtes, y=as.factor(ytes))
(ypred<-predict(bestparam, tesdata))
table(prediksi=ypred,aktual=tesdata$y)

#8. Menggunakan fungsi kernel dan seting parameter kernel
set.seed(1)
# menggunakan data yang dibangkitkan
x<-matrix(rnorm(400), ncol=2)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,75),rep(2,100),rep(1,25))
plot(x,col=y)
dat<-data.frame(x=x,y=as.factor(y))
#Ambil data sampel 150 dari 200
train<-sample(200,150)
model_train_svm<-svm(y~.,data=dat[train,], kernel="radial",gamma=1, cost=1)
plot(model_train_svm, dat[train,])
### Bisa ujikan untuk parameter yang berbeda menggunakan "tune"

#9. Lakukan Prediksi Menggunakan SVM
tes<- -train
yakt<-as.integer(dat[tes,"y"])
ypred<-as.integer(predict(model_train_svm, dat[tes,]))
matcon<-table(aktual=yakt, prediksi=ypred)
akurasi<-paste("akurasinya
adalah",(matcon[1,1]+matcon[2,2])/sum(matcon)*100, "%", sep=" ")