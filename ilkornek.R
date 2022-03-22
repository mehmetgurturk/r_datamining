veriseti <- read.csv("Fin500.csv")
dataset <- read.csv("https://www.bahadirfyildirim.com/media/documents/Fin500.csv")

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
