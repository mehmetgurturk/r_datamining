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

veriseti <- veriseti[complete.cases(veriseti), ] #verisetinde complete olanlar ile tekrar veriseti oluştur 

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

median(veriseti[, "Employees"])
median(veriseti[, "Employees"], na.rm = T) #NA ya median değerini ata
med_emp_ret <- median(veriseti[veriseti$Industry == "Retail", "Employees"], na.rm = T)

veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Retail", ]
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Retail", "Employees"] <- med_emp_ret
data[3,]

med_emp_finservices <- median(veriseti[veriseti$Industry == "Financial Services", "Employees"], na.rm = T) 
veriseti[is.na(veriseti$Employees) & veriseti$Industry == "Financial Services", "Employees"] <- med_emp_finservices



#receneue için sektröe ortlaması al yazdır expense de median veya aritmetik ortalama bunları summaryden gözlemle için yaz. profit için de aynısı yap. growth sektör ortalamasını al