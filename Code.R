library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(plyr)
library(dplyr)
library(ggplot)
library(ggplot2)
library(RColorBrewer)
library(ROSE)
library(hrbrthemes)
library(caret)
library(olsrr)
library(cvms)
library(tibble) 
library(MASS)
library(pROC)
library(DescTools)
library(randomForest)
library(factoextra)
hrbrthemes::import_roboto_condensed()

#Data Cleaning

data0 <- read.csv('healthcare-dataset-stroke-data.csv')
summary(data0)
str(data0)
data1 <- data0


#BMI
class(data1$bmi)
data1$bmi <- as.numeric(data1$bmi)
class(data1$bmi)
summary(data1$bmi)
data1$bmi[is.na(data1$bmi)] <- mean(data1$bmi,na.rm=TRUE)
summary(data1$bmi)



#Gender
table(data1$gender)
data1$gender <- ifelse(data1$gender == "Other", "Female", data1$gender)
table(data1$gender)

#Smoking

table(data1$smoking_status)
prob.FS <- 885 / (885 + 1892 + 789)
prob.NS <- 1892 / (885 + 1892 + 789)
prob.S <- 789 / (885 + 1892 + 789)

data2 <- data1

data2$rand <- runif(nrow(data2))
data2 <- data2%>%mutate(Probability = ifelse(rand <= prob.FS, "formerly smoked", ifelse(rand <= (prob.FS+prob.NS), "never smoked", ifelse(rand <= 1, "smokes", "Check"))))
data2 <- data2%>%mutate(smoking.status = ifelse(smoking_status == "Unknown", Probability, smoking_status))
table(data2$smoking.status)
health <- subset(data2, select = -c(rand,Probability,smoking_status))
colnames(health)[12] <- "smoking_status"
head (health,10)

ggplot(health, aes(color=smoking_status, x=age, y=bmi)) + geom_point()

#ID
class(health$id)
health$id <- NULL
class(health$id)
head(health$id, 3)


# Exploratory Data Analysis

Yes <- subset(health, stroke == '1')
No <- subset(health, stroke == '0')


##Bar Charts

strokecounts <- as.data.frame(table(health$stroke))
strokecounts$Var1 <- ifelse(strokecounts$Var1 == 0, "No", 'Yes') 

ggplot(strokecounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Stroke Status of Patients",x ="Stroke", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))




hypercounts <- as.data.frame(table(health$hypertension, health$stroke))
hypercounts$Var1 <- ifelse(hypercounts$Var1 == 0, "No", 'Yes')
hypercounts$Var2 <- ifelse(hypercounts$Var2 == 0, "No", 'Yes')
colnames(hypercounts)[1] <- 'Hypertension'
colnames(hypercounts)[2] <- 'Stroke'

ggplot(hypercounts, aes(x = Hypertension, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Hypertension Status of Patients",x ="Hypertension", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))



heartcounts <- as.data.frame(table(health$heart_disease, health$stroke))
heartcounts$Var1 <- ifelse(heartcounts$Var1 == 0, "No", 'Yes')
heartcounts$Var2 <- ifelse(heartcounts$Var2 == 0, "No", 'Yes')
colnames(heartcounts)[1] <- 'Heart_Disease'
colnames(heartcounts)[2] <- 'Stroke'

ggplot(heartcounts, aes(x = Heart_Disease, y = Freq, fill = Stroke)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Heart Disease Status of Patients",x ="Heart Disease", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))


gendercounts <- as.data.frame(table(health$gender))
ggplot(gendercounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Gender of Patients",x ="Gender", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))



workcounts <- as.data.frame(table(health$work_type))
ggplot(workcounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Patient Work Type",x ="Work Type", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))


marriedcounts <- as.data.frame(table(health$ever_married))
ggplot(marriedcounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Bar Chart of Patients Who Have Been Married",x ="Have the Patient been Married", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))



rescounts <- as.data.frame(table(health$Residence_type))
ggplot(rescounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Residence Type of the Patients",x ="Residence Type", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))


smokecounts <- as.data.frame(table(health$smoking_status))
ggplot(smokecounts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") + theme(legend.position="none") +
  geom_text(aes(label = Freq), vjust = 0) +
  labs(title="Smoking Status of Patients",x ="Smoking Status", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))




## Part 2 - Histograms


histage <- hist(health$age,xlim=c(0,100),
                main="Histogram of Age with Normal Distribution Overlay",
                xlab="Age",las=1)
xfit <- seq(min(health$age),max(health$age))
yfit <- dnorm(xfit,mean=mean(health$age),sd=sd(health$age))
yfit <- yfit*diff(histage$mids[1:2])*length(health$age)
lines(xfit,yfit,col="red",lwd=2)




histglucose <- hist(health$avg_glucose_level,xlim=c(0,300),
                    main="Histogram of Avg. Glucose with Normal Distribution Overlay",
                    xlab="Avg. Glucose",las=1)
xfit <- seq(min(health$avg_glucose_level),max(health$avg_glucose_level))
yfit <- dnorm(xfit,mean=mean(health$avg_glucose_level),sd=sd(health$avg_glucose_level))
yfit <- yfit*diff(histglucose$mids[1:2])*length(health$avg_glucose_level)
lines(xfit,yfit,col="red",lwd=2)




histbmi <- hist(health$bmi,xlim=c(0,100),
                main="Histogram of BMI with Normal Distribution Overlay",
                xlab="Body Mass Index",las=1)
xfit <- seq(min(health$bmi),max(health$bmi))
yfit <- dnorm(xfit,mean=mean(health$bmi),sd=sd(health$bmi))
yfit <- yfit*diff(histbmi$mids[1:2])*length(health$bmi)
lines(xfit,yfit,col="red",lwd=2)



## Part 3 - Boxplot


boxplot(Yes$avg_glucose_level,No$avg_glucose_level,
        main="Boxplot of Average Glucose Level by Stroke Status",
        ylab="Average Glucose Level",las=1,names=c("Stroke","No Stroke"))




boxplot(Yes$bmi,No$bmi,main="Boxplot of Body Mass Index by Stroke Status",
        ylab="BMI",las=1,names=c("Stroke","No Stroke"))



# Modeling
## Data Spilt using Original Data


health$gender <- as.factor(health$gender)
health$stroke <- as.factor(health$stroke)
table(health$gender)
class(health$stroke)


set.seed(100)
health$AgeGroup <-NULL
sample = sample.split(health$stroke, SplitRatio = 0.7)
train = subset(health, sample==TRUE)
test = subset(health, sample==FALSE)
health$gender <- as.factor(health$gender)
table(health$gender)



library(fastDummies)
healthdummy <- health
healthdummy <- dummy_cols(healthdummy,select_columns=c("gender","ever_married",
                                                       "work_type","Residence_type","smoking_status"),
                          remove_selected_columns = TRUE)

summary(healthdummy)
str(healthdummy)

healthdummy$stroke <- ifelse(healthdummy$stroke == '1', 1, 0)
table(healthdummy$stroke)

str(healthdummy)

set.seed(123)
trainIndex <- sample(x=nrow(healthdummy),size=nrow(healthdummy)*0.7)
healthdummytrain <- healthdummy[trainIndex,]
healthdummytest <- healthdummy[-trainIndex,]



## Model 1: Generalized Linear Model - Logistic Regression -DummyVar


glm_fit <- glm(stroke~., data=healthdummytrain, family = binomial)
summary(glm_fit)



stepdummy <- stepAIC(glm_fit)
summary(stepdummy)



probabilities <- stepdummy %>% predict(healthdummytest, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
print(paste("Model Accuracy : ", mean(predicted_classes == healthdummytest$stroke)))
# Confusion Matrix
table(healthdummytest$stroke, probabilities >= 0.5)
# Anova Table
anova(stepdummy)


## Model 2A: Principal Components Analysis -DummyVar


healthdummy <- healthdummy[, c(6, 1, 2, 3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
PCA <- prcomp(healthdummy[c(2:20)], center = TRUE, scale=TRUE)

summary(PCA)

str(PCA)
healthdummy2 <- cbind(healthdummy,PCA$x[,1:9])
head(healthdummy2,3)

x <- cor(healthdummy, PCA$x[,1:9])
summary(x)
x



screeplot(PCA, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)



cumpro <- cumsum(PCA$sdev^2 / sum(PCA$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 9, col="blue", lty=5)
abline(h = 0.82437, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC9"),
       col=c("blue"), lty=5, cex=0.6)




plot(PCA$x[,1],PCA$x[,2], xlab="PC1 (19.16%)", ylab = "PC2 (10.99%)", main = "PC1 / PC2 - plot")


healthdummy$stroke <- ifelse(healthdummy$stroke == 0, "No", "Yes")
fviz_pca_ind(PCA, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = healthdummy$stroke, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 19 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


## Model 2B: Generalized Linear Model - Logistic Regression -DummyVar_PCA


healthdummy$stroke <- ifelse(healthdummy$stroke == "No", 0, 1)


data_pca <- data.frame(stroke = healthdummy$stroke, PCA$x)
data_pca <- data_pca[,1:10]

set.seed(123)
trainIndex <- sample(x=nrow(data_pca),size=nrow(data_pca)*0.7)
traindata_pca <- data_pca[trainIndex,]
testdata_pca <- data_pca[-trainIndex,]



glm_fit_pca <- glm(stroke~., data=traindata_pca, family = binomial)
summary(glm_fit_pca)



stepdummy_pca <- stepAIC(glm_fit_pca)
summary(stepdummy_pca)



probabilities <- stepdummy_pca %>% predict(testdata_pca, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
print(paste("Model Accuracy : ", mean(predicted_classes == testdata_pca$stroke)))
# Confusion Matrix
table(testdata_pca$stroke, probabilities >= 0.5)
# Anova Table
anova(stepdummy_pca)


## Data Split using Oversampling Data

health$AgeGroup <- NULL
healthoversample <- ovun.sample(stroke~.,data = health, method = 'over',p = 0.3)$data

oversampletable <- as.data.frame(table(healthoversample$stroke))
regularsampletable <- as.data.frame(table(health$stroke))

#Before adjustment
ggplot(regularsampletable, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat = 'identity') +
  theme_tinyhand() + xlab("\nDid the patient experience a stroke?") + ylab("Patients\n") +
  ggtitle("Number of Patients by Stroke Occurence") + 
  scale_fill_manual(breaks = c("0", "1"), 
                    values=c("darkgreen","red3"), name = "Stroke Occured", labels = c("No","Yes")) +
  theme(axis.text.x = element_text(size=18),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=18, hjust=0.5),
        axis.title.x = element_text(size=18, hjust=0.5)) +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))

#After adjustment
ggplot(oversampletable, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat = 'identity') +
  theme_tinyhand() + xlab("\nDid the patient experience a stroke?") + ylab("Patients\n") +
  ggtitle("Number of Patients by Stroke Occurence (Oversampled)") + 
  scale_fill_manual(breaks = c("0", "1"), 
                    values=c("darkgreen","red3"), name = "Stroke Occured", labels = c("No","Yes")) +
  theme(axis.text.x = element_text(size=18),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size=18, hjust=0.5),
        axis.title.x = element_text(size=18, hjust=0.5)) +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))



healthoversample$gender <- as.factor(healthoversample$gender)
healthoversample$stroke <- as.factor(healthoversample$stroke)
table(healthoversample$gender)
class(healthoversample$stroke)

set.seed(100)
healthoversample$AgeGroup <-NULL
sample_os = sample.split(healthoversample$stroke, SplitRatio = 0.7)
train_os = subset(healthoversample, sample==TRUE)
test_os = subset(healthoversample, sample==FALSE)
healthoversample$gender <- as.factor(healthoversample$gender)
table(healthoversample$gender)



#Random Forest Model
Random_Forest_Model_os <- randomForest(stroke~., data=train_os, ntree=500)
Random_Forest_Model_os

probabilities.train_os <- predict(Random_Forest_Model_os, train_os, type='class')
confusionMatrix(probabilities.train_os, train_os$stroke, positive = '1')

probabilities.test_os <- predict(Random_Forest_Model_os, test_os, type='class')
confusionMatrix(probabilities.test_os, test_os$stroke, positive = '1')

#Random Forest Evaluation
oob.error.data_os <- data.frame(
  Trees=rep(1:nrow(Random_Forest_Model_os$err.rate), times=3),
  Type=rep(c("OOB","0","1"), each=nrow(Random_Forest_Model_os$err.rate)),
  Error=c(Random_Forest_Model_os$err.rate[,"OOB"],
          Random_Forest_Model_os$err.rate[,"0"],
          Random_Forest_Model_os$err.rate[,"1"])
)

ggplot(data = oob.error.data_os, aes(x=Trees, y=Error)) + geom_line(aes(color=Type)) + theme_ipsum() +
  ggtitle("Error Rate by Number of Trees") + xlab("\nNumber of Trees") +
  ylab("Error Rate\n") +
  theme(axis.title.x = element_text(face="bold", size=14, hjust = 0.5),
        axis.title.y = element_text(face="bold", size=14, hjust=0.5),
        axis.text.x = element_text(face="bold", size=16),
        axis.text.y = element_text(face="bold", size=16),
        legend.text = element_text(face = "bold", size=12),
        legend.title = element_text(face = "bold", size=15))

varImpPlot(Random_Forest_Model_os)


## Model 6: Random Forest Model - Original Data

Random_Forest_Model <- randomForest(stroke~., data=train, ntree=500)
Random_Forest_Model

probabilities.train <- predict(Random_Forest_Model, train, type='class')
confusionMatrix(probabilities.train, train$stroke, positive = '1')

probabilities.test <- predict(Random_Forest_Model, test, type='class')
confusionMatrix(probabilities.test, test$stroke, positive = '1')

conf_mat <- confusion_matrix(targets = test$stroke,
                             predictions = probabilities.test)
conf_mat
plot_confusion_matrix(conf_mat)

#Random Forest Evaluation
oob.error.data <- data.frame(
  Trees=rep(1:nrow(Random_Forest_Model$err.rate), times=3),
  Type=rep(c("OOB","0","1"), each=nrow(Random_Forest_Model$err.rate)),
  Error=c(Random_Forest_Model$err.rate[,"OOB"],
          Random_Forest_Model$err.rate[,"0"],
          Random_Forest_Model$err.rate[,"1"])
)

ggplot(data = oob.error.data, aes(x=Trees, y=Error)) + geom_line(aes(color=Type)) + theme_ipsum() +
  ggtitle("Error Rate by Number of Trees") + xlab("\nNumber of Trees") +
  ylab("Error Rate\n") +
  theme(axis.title.x = element_text(face="bold", size=14, hjust = 0.5),
        axis.title.y = element_text(face="bold", size=14, hjust=0.5),
        axis.text.x = element_text(face="bold", size=16),
        axis.text.y = element_text(face="bold", size=16),
        legend.text = element_text(face = "bold", size=12),
        legend.title = element_text(face = "bold", size=15))

varImpPlot(Random_Forest_Model)
