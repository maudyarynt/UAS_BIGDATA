library(readxl)
UAS_SVM <- read_excel("mod/UAS_SVM.xlsx")
View(UAS_SVM)
str(UAS_SVM)
library(e1071)
plot(UAS_SVM$Income...3, UAS_SVM$Income...6, col=UAS_SVM$Pelunasan_Kredit)
plot(UAS_SVM$Income...3, UAS_SVM$Average_Balance, col=UAS_SVM$Pelunasan_Kredit)
s<-sample(999,270)
s
col<- c("Income...3", "Income...6", "Pelunasan_Kredit")
UAS_SVM_train <- UAS_SVM[s,col]
UAS_SVM_test <- UAS_SVM[-s, col]
svmfit <- svm(Pelunasan_Kredit ~., data = UAS_SVM_train, kernel = "linear", cost = .1, scale = FALSE)
print(svmfit)
plot(svmfit, UAS_SVM_train[,col])
p <- predict(svmfit, UAS_SVM_test[,col], type="class")
plot(p)
table(p)
mean(p== UAS_SVM_test[,3])