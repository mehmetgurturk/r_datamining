veriseti <- read.csv("Fin500.csv")
dataset <- read.csv("https://www.bahadirfyildirim.com/media/documents/Fin500.csv", na.strings = c(""))

veriseti <- read.csv("https://www.bahadirfyildirim.com/media/documents/Fin500.csv", na.strings = c("")) #veriseti olarak çağırdık, içerisindeki na.strings ile verisetimizin içerisindeki boş alanları NA olarak yazdırdık.

head(veriseti,15) #verisetinin ilk 15 satırını göster
tail(veriseti,15) #verisetinin son 15 satırını göster
str(veriseti)
summary(veriseti)

veriseti$Industry <- as.factor(veriseti$Industry) #ilk olarak as.factor ile industry'i levele ayırıdlık sonra <- ile tekrar atadık sonra str'yi çalıştır

veriseti$Inception <- as.factor(veriseti$Inception)

veriseti$State <- as.factor(veriseti$State)

veriseti$Expenses <- gsub(" Dollars", "", veriseti$Expenses) #sadece sub ilk sıradakini değiştir gsub global olduğu argümanın içindeki her şeye bakacak # dollars sildik

veriseti$Expenses <- gsub(",", "", veriseti$Expenses) #, sildik ve tekrar atadık

veriseti$Expenses <- as.numeric(veriseti$Expenses) #numeric olarak atadık

veriseti$Growth <- gsub("%", "", veriseti$Growth)

veriseti$Growth <- as.numeric(veriseti$Growth) #fonksiyonları iç içe çalıştırabilirsin veriseti$Growth <- as.numeric(gsub("%", "", veriseti$Growth))

veriseti$Revenue <- gsub("\\$", "", veriseti$Revenue)

veriseti$Revenue <- as.numeric(gsub(",", "", veriseti$Revenue))

veriseti$City <- as.factor(veriseti$City)

str(veriseti)

library(Amelia) #missing value ile uğraşacağımız için bu paketi çağırdık.
missmap(veriseti) #missingness Map çizdirmek için
dim(veriseti) # 500 satır 11 sütun var

veriseti[3,2] #3.satır 2.sütunu gözlemliyoruz. 3.satırın tamamını görmek istiyorsak veriseti[3,], sadece sutün için veriseti[,2]

head(veriseti, 25)
complete.cases(veriseti) #veriseti tam ise true eksik yani NA var ise false olarak bize gösteren bir kod.

veriseti[!complete.cases(veriseti), ] #! ile tersi olduğunu ifade ettikk.incomlete olduğundan, sonra tüm sütunu göster.

veriseti$Revenue == 9746272 #geliri 9 milyon olanları getir
veriseti[veriseti$Revenue == 9746272, ] #trueların yanında NA'ları dönüyor bundan kaçınmak için which komutunu kullandık

which(veriseti$Revenue == 9746272) #hangi gelir 9 milyondur bunu getiriyor.
veriseti[which(veriseti$Revenue == 9746272), ] #sadece9 milyon olan satırı getiriyor

veriseti[is.na(veriseti$Expenses), ] #expensede na olan getiriyor. !koyarsak na hariç olanları yani dolu olanları getiriyor

veriseti[which(veriseti$Employees == 45), ]

#missind daya siliyruz

backup <- veriseti

veriseti <- backup

veriseti[!complete.cases(veriseti), ]
veriseti[is.na(veriseti$Industry), ]
veriseti[!is.na(veriseti$Industry), ]

#veriseti <- veriseti[complete.cases(veriseti), ] #verisetinde complete olanlar ile tekrar veriseti oluştur 

veriseti <- veriseti[!is.na(veriseti$Industry), ] #industry veri setinde dolu olanları tekrar veriseti oluştur

row.names(veriseti) #verisetindeki satır numaralrını gördük
row.names(veriseti) <- 1:nrow(veriseti) #1den başlayarak tekrar satır numara ataması gerçekleştir.

#kayıp verilerin değiştirilmesi

veriseti[!complete.cases(veriseti), ]
veriseti[is.na(veriseti$State), ]

veriseti[is.na(veriseti$State) & veriseti$City == "New York", ]
veriseti[is.na(veriseti$State) & veriseti$City == "New York", "State"] <- "NY" #State sütununda NA olan yerlere NY ekle

veriseti[is.na(veriseti$State) & veriseti$City == "San Francisco", ]
veriseti[is.na(veriseti$State) & veriseti$City == "San Francisco", "State"] <- "CA"
veriseti[veriseti$ID == 84, ]

#Median Mean imputation

summary(veriseti$Employees)
veriseti[is.na(veriseti$Employees), ]

median(veriseti[, "Employees"]) # NA baskın olduğu için sonuç NA çıktı bundan kurultmak için aşağıdaki komutu girdik 
median(veriseti[, "Employees"], na.rm = T) #NA ya median değerini ata NA'yı dikkate alma
med_emp_ret <- median(veriseti[veriseti$Industry == "Retail", "Employees"], na.rm = T)

veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Retail", ]
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Retail", "Employees"] <- med_emp_ret
veriseti[3,]

med_emp_finservices <- median(veriseti[veriseti$Industry == "Financial Services", "Employees"], na.rm = T) 
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Financial Services", "Employees"] <- med_emp_finservices

#receneue için sektröe ortlaması al yazdır expense de median veya aritmetik ortalama bunları summaryden gözlemle için yaz. 
#profit için de aynısı yap. growth sektör ortalamasını al
#veriseti <- backup

summary(veriseti)
head(veriseti,25)

veriseti[is.na(veriseti$Expenses), ]
veriseti[is.na(veriseti$Expenses) & !is.na(veriseti$Revenue), ] #Revenue boş olmaması lazım
veriseti[is.na(veriseti$Expenses) & !is.na(veriseti$Revenue), "Expenses" ] #expenses kolonun verecek

veriseti[is.na(veriseti$Expenses) & !is.na(veriseti$Revenue), "Expenses" ] <- veriseti[is.na(veriseti$Expenses) & !is.na(veriseti$Revenue), "Revenue" ] - veriseti[is.na(veriseti$Expenses) & !is.na(veriseti$Revenue), "Profit" ] 

veriseti[17,]
######################### Revenue NA olanları düzenledik

summary(veriseti)
head(veriseti,25)

veriseti[is.na(veriseti$Revenue), ]
veriseti[is.na(veriseti$Revenue) & veriseti$Industry == "Construction", ] #Revenue NA olan Industry'ler Construction
mean(veriseti[, "Revenue"])
mean(veriseti[, "Revenue"], na.rm = T)
mea_rev <- mean(veriseti[, "Revenue"], na.rm = T)
veriseti[is.na(veriseti$Revenue) & veriseti$Industry == "Construction", "Revenue"] <- mea_rev
head(veriseti, 25)

#getwd() # dosyamızın konumunu belirtiyor

library(caret)
data(iris)
str(iris)
summary(iris[,1:4]) #tüm satırların ve 1 ve 4 arasındakileri aldık C(1,3,5) 1. 3. ve 5. sütunları aldık
#calculate fotoğraf çektim bak
preProcessParams <- preProcess(iris[,1:4], method=c("scale")) #preProcess altındaki scale'i çağırdık, herbir gözlemi oraki 
#standart sapmaya oranlıyor. $$ $$ arasına matematik yani latex kodları yazılır. 
print(preProcessParams)

#herbir değeri standart sapmaya oranlayarak buluyor
scaled <- predict(preProcessParams, iris[,1:4]) #predict fonskiyonunu iris 1:4 e yaz dedik
summary(scaled)

preProcessParams <- preProcess(iris[,1:4], method=c("center")) #xi-xmü yapıyor
print(preProcessParams)

centered <-  predict(preProcessParams, iris[,1:4])
summary(centered)

preProcessParams <- preProcess(iris[,1:4], method=c("center", "scale")) #standardizasyon yaptık
print(preProcessParams)

standardized <- predict(preProcessParams, iris[,1:4])
summary(standardized)

preProcessParams <- preProcess(iris[,1:4], method=c("range")) #normalizasyon yaptık formuülleri yaz
print(preProcessParams)

normalized <- predict(preProcessParams, iris[,1:4])
summary(normalized)

preProcessParams <- preProcess(iris[,1:4], method=c("BoxCox"))
print(preProcessParams)

boxcox <- predict(preProcessParams, iris[,1:4])
summary(boxcox)

preProcessParams <- preProcess(iris[,1:4], method=c("YeoJohnson"))
print(preProcessParams)

yeojohnson <- predict(preProcessParams, iris[,1:4])
summary(yeojohnson)

#Resampling Methods

data(iris)
summary(dataset)
dataset <- iris
#Bootstrap ile 100 örneklem oluşturur.
train_control <- trainControl(method="boot", number=100)

#define an 80/20 train-test split of the dataset.

split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex, ]
data_test <- iris[ -trainIndex, ]

#Leave One Out Cross Validation (LOOCV)
Library(caret)
#Load the iris dataset
data(iris)
#define training control
train_control <-trainControl(method="LOOCV")

#k-fold Cross Validation

#load the library
#Load the iris dataset
data(iris)
#define training control
# k fold => number
train_control <- trainControl(method="cv", number=10)

#Repeated Cross Validations
#Load the iris dataset
data(iris)
# define training control
train_control <- trainControl (method="repeatedcv", number=10, repeats=3)


head(cars)
str(cars)
summary(cars)
names(cars) # değişken isimleri ile çağıracağımız için kullandık. Verisetindeki headrleri gösteriyor

scatter.smooth(x=cars$speed, y=cars$dist, main="Saçılma Diyagramı") #elimizdeki veri regresyon modele uygun mu diye. versitemizin eğrilsel mi doğrusal mı onun için çizdik
cor(cars$speed, cars$dist) #korelasyon üretmek için
GenelModel <- lm(dist ~ speed, data=cars) # lm(bağımlı ve bağımsız değişken) (lm, bağımlı değişken, bağımsız değişken , veristesi)
print(GenelModel) #tahmini distance aldık. dist^ = -17,579+3,932speed   speed x olduk.
summary(GenelModel)

AIC(GenelModel) #bilgi kriterleri
BIC(GenelModel) #bilgi kriiterleri bunları küçük olmasını bekleriz.


set.seed(100) #set.seed ile rassal sayıları yani rassal değerleri bulucağız.
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars)) #satır index numaralarını, değerlerini oluşturudk. sample fonk. çalıştırdık. 1den nrow kadar veriden yüzde 80'inin rassal olarak seç
trainingData <- cars[trainingRowIndex, ] #satırlarını al ve tüm sütunlarını al
testData <- cars[-trainingRowIndex, ]

lmMod <- lm(dist ~ speed, data=trainingData)
distPred <- predict(lmMod, testData) #test datasında tahmini değerler
print(distPred)
summary(lmMod)
AIC(lmMod)

actuals_preds <- data.frame(cbind(gercek=testData$dist, tahmin=distPred)) #observe ettiğim - tahmin ettiğim data bind = iki vektörü bind ediyor. birleştiryor yani bind ediyor. iki verktörden yeni vektör oluşturma yani birleştirme
correlation_accuracy <- cor(actuals_preds) #90% korelasyon yani ilişki var mı diye bakıyorz
head(actuals_preds)

#min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs(actuals_preds$tahmin - actuals_preds$gercek)/actuals_preds$gercek) #mapenin açılımı min personch error. gerçek değer ile tahmini değerin farkın mutlak değerini alarak gerçek değere oranlıyoruz. hata payını alarak gerçek değere oranlıyor yani. 
print(mape)

library(DAAG)
cvResults <- suppressWarnings(CVlm(cars, form.lm = dist ~ speed, m=5, dots = FALSE, seed = 29, legend.pos = "topleft", printit = FALSE, main = "CV Dogrusal Regresyon"));
#cvelm crossvalidantianlm çağırdı. kullanılacak veristesi carsi, formülü tanımla, 5 grup oluştur, noktaları göstermedik, 5 grubu aynı değerleri bulmak için seed, legenin gösterileceği yer, 
#5 tane ayrı regresyon modeli oluşturdu  en son olarak makine öğrenmesi yaptı. 
attr(cvResults, 'ms') # MSE mean squaared error


library(tidyverse)
library(caret)
library(glmnet)
data(Boston, package = "MASS")

set.seed(1212)
#test ve eğitim verisetelerini oluştur.

sample_size <- floor(0.75 * nrow(Boston)) #floor yuvarla fonskiyonu.
training_index <- sample(seq_len(nrow(Boston)), size = sample_size)
train <- Boston[training_index, ]
test <- Boston[-training_index, ] #training_index değerleri dışındaki değerleri demek

#Predictor
x <- model.matrix(medv~., train)[,-1] #med bağımlı diğerleri bağımsız şekilde . -1 ile mev value kolunun çıkartıyor. gözlem değerini kullanmayacağız.
#Response
y <- train$medv

#Ridge Regresyon

cv.r <- cv.glmnet(x, y, alpha = 0) #bu bana ridge fonk dedik. bu bana ideali üretti
cv.r$lambda.min
model.ridge <- glmnet(x, y, alpha=0, lambda =  cv.r$lambda.min)
coef(model.ridge) #coeffecient katsayılarıan baktık

x.test.ridge <- model.matrix(medv~., test)[,-1]
predictions.ridge <- model.ridge %>% predict(x.test.ridge) %>% as.vector() #model ridge çektik, sonra predict değerini aldık, en son vektör olarak yazdı predictions ridge tahmni değerlerini buldu
data.frame( RMSE.r = RMSE(predictions.ridge, test$medv), Rsquare.r = R2(predictions.ridge, test$medv))

#Lasso Regresyon

cv.l <- cv.glmnet(x, y, alpha=1)
cv.l$lambda.min
model.lasso <- glmnet(x, y, alpha=1, lambda =  cv.l$lambda.min)
coef(model.lasso) #coeffecient katsayılarıan baktık

x.test.lasso <- model.matrix(medv~., test)[,-1]
predictions.lasso <- model.lasso %>% predict(x.test.lasso) %>% as.vector() #model ridge çektik, sonra predict değerini aldık, en son vektör olarak yazdı predictions ridge tahmni değerlerini buldu
data.frame( RMSE.l = RMSE(predictions.lasso, test$medv), Rsquare.l = R2(predictions.lasso, test$medv))

#Elastic Net Regresyon

model.net <- train(medv~., data = train, method ="glmnet", trControl = trainControl("cv", number=10), tuneLength=10)
model.net$bestTune

coef(model.net$finalModel, model.net$bestTune$lambda)
x.test.net <- model.matrix(medv~., test)[,-1]
predictions.net <- model.net %>% predict(x.test.net)
data.frame(RMSE.net=RMSE(predictions.net, test$medv), Rsquare.net=R2(predictions.net, test$medv))
