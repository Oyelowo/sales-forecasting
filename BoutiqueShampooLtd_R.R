rm(list=ls())

#load packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(tidyr)
library(tidyverse)
library(gbm)
library(dismo)
library(caTools)
library(mgcv)
library(MASS)
library(FactoMineR)

data= read.csv("C:/Users/oyeda/Desktop/Task/BoutiqueShampooLtd.csv", sep=";")

# Homework for a traineeship
# Attached you'll find Boutique Shampoo Ltd's data about their sales 
# and investments in various marketing efforts / channels. The CEO of the 
# company would like to better understand how their marketing efforts have 
# contributed to sales in the past. 
# 
# Your task is to help the CEO. To help you out with what types of questions 
# the CEO is interested in, he has compiled a list of questions he's hoping 
# to get answers to:
  

# 1.What general findings can you draw from the data?
glimpse(data)
sd(data$Total.Volume.Sales)
range(data$Total.Volume.Sales)

summary(data)
hist(data$Total.Volume.Sales)

boxplot(data$Total.Volume.Sales)
hist(data$Total.Volume.Sales, main = "Total Volume of Sales", xlab="Sales Volume",col = "grey")
abline(v=mean(data$Total.Volume.Sales), col = "red", lwd=2)


tapply(data$Total.Volume.Sales, data$Weighted.Average.Price, mean)

# shapiro.test(data$Total.Volume.Sales)
data$Dates=as.Date(data$Dates, "%d.%m.%Y")



par(mfrow=c(1,1))
plot(data$Dates, data$Weighted.Average.Price, type='l', col='red')
# par(new=T)
plot(data$Dates,data$Total.Volume.Sales, type='l', col='blue')


plot(data$Dates, data$Total.Volume.Sales, type='l')


gather(data) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() 
pairs(data[0:4])
jk= data
jk$Rebrand=as.factor(jk$Rebrand)
plot_hyp <- ggpairs(jk[2:10],mapping = aes(col=Rebrand, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
plot_hyp
corrplot(data[2:10])
#rebranding has quite a strong correlation of 0.742with the average price

colnames(data)

# initialize a plot of 'age'
sex_age <- ggplot(data = data, aes(x=Total.Volume.Sales))

# draw a bar plot of age by sex
plot(data$Total.Volume.Sales, data$Weighted.Average.Price)


sales_glm<-glm(Total.Volume.Sales~ .,data=data[2:length(data)],family ="gaussian")
summary(sales_glm)

cor(data$Total.Volume.Sales, data$Total.Value.Sales)
cor(data$Total.Volume.Sales, data$Weighted.Average.Price)


sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price+
                 Distribution + Price.Promotion.1+ Price.Promotion.2+
                 On.pack.Promo.Offer+Rebrand+ TV + Radio+ Press+
                 Outdoor+ Online,data=data,family ="gaussian")
summary(sales_glm)

#First, I used the stepwise regression to eliminate the redundant variables.
stepAIC(sales_glm, direction = "both")

sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price + Price.Promotion.1 + 
                 Rebrand + TV + Outdoor,data=data,family ="gaussian")


summary(sales_glm)
#Anova test
anova(sales_glm, test="Chisq")

# Explained deviance (D-squared) = (Null deviance - Residual deviance) / Null deviance

d_Squared<-function(null_dev, res_dev){
  d2=(null_dev-res_dev)/null_dev
  return(d2)
}

#Coeffient of determination in the GLM
d_Squared(null_dev = sales_glm$null.deviance, res_dev = sales_glm$deviance)


par(mfrow=c(2,2))
plot(sales_glm, which = c(1,2,5))

data_copy=data

data_copy[6:length(data_copy)]=0
data_copy  
sales_pred<- predict.glm(object = sales_glm, newdata = data_copy , type="response")

plot(data_copy$Dates, data_copy$Total.Volume.Sales, type='l', col='blue', ylim=c(800,2500),
      ylab='Total Volume of Sales')
par(new=T)
plot(data_copy$Dates,sales_pred, type='l', col='red', ylim=c(800,2500), ylab='')

# cor(sales_pred, data_copy$Total.Volume.Sales)

# function to calculate the mean absolute and RMSE
#function to calculate mean error
mean_error<- function(obs, pred){
  me<-mean(abs(obs-pred))
  return(me)
}

# Function that returns Root Mean Squared Error
rmse <- function(obs, pred){
  rmse<-sqrt(mean((obs-pred)^2))
  return(rmse)
}


#LOOCV

#set.seed(0)


{pred_sales_test <- c()
for (i in 1:nrow(data)){
  print(i)
  cal <- data[-i,]
  eva <-data[i,]
  sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price + Price.Promotion.1 + 
                   Rebrand + TV + Outdoor,data=data,family ="gaussian")

  sales_glm_pred_test<- predict.glm(object = sales_glm, newdata = eva, type="response")
  pred_sales_test[i]<- sales_glm_pred_test

  
}
 pred_obs <- cbind.data.frame(pred_sales_test, data$Total.Volume.Sales  )
 colnames(pred_obs)<-c("pred_sales", "obs_sales")
 #cor(pred_sales_test, data$Total.Volume.Sales  ,method = "spearman")
}

cor(pred_obs$pred_sales, pred_obs$obs_sales, method = "spearman")
plot(pred_obs$pred_sales, pred_obs$obs_sales)
# Loess method
ggplot(pred_obs, aes(x=pred_sales, y=obs_sales)) + 
  geom_point()+
  geom_smooth()

# One ellipse arround all points
ggplot(pred_obs, aes(x=pred_sales, y=obs_sales))+
  geom_point()+
  stat_ellipse()  



rmse(pred_obs$pred_sales, pred_obs$obs_sales)
mean_error(pred_obs$pred_sales, pred_obs$obs_sales)



data_reg=data
{rep<-10
for (i in 1:rep){
    #print the index to see the iteration
    print(i)
    
    #Creare a 70 sample(with replacement) from the original data
    rand<- sample(1:nrow(data_reg), size = 0.7*nrow(data_reg))
    
    #70% for the train/calibration data
    cal<- data_reg[rand,]
    
    #remaining 30 for the test/evaluation data
    eva<-data_reg[-rand,]
    
    ####GLM
    #perform a Genelralised Linear Model(GLM)
    sales_glm <- glm(Total.Volume.Sales~Weighted.Average.Price + Price.Promotion.1 + 
                       Rebrand + TV + Outdoor, data=cal, family = "poisson") 
    
    #predict into the test/evaluation data
    sales_glm_pred<- predict.glm(object = sales_glm, newdata = eva, type="response")
    
    #find the correlation between the train and test data.
    cor_glm_sales<-cor(sales_glm_pred, eva$Total.Volume.Sales, method = "spearman")
    
    
    #########
    #mean error and root mean square error
    #calculate the mean error
    error_sales_glm<- cbind.data.frame(sales_glm_pred, eva$Total.Volume.Sales)
    colnames(error_sales_glm) <- c("pred_glm_sales", "obs_sales")
    
    #Use the function created earlier to calulcate the mean error and RMSE.
    #Mean error
    sales_glm_me <- mean_error(error_sales_glm$obs_sales, error_sales_glm$pred_glm_sales)
    
    #RMSE
    sales_glm_rmse <- rmse(error_sales_glm$obs_sales, error_sales_glm$pred_glm_sales)
    
    #combine the dataframe of the mean error and RMSE
    me_rmse_sales_glm <- rbind.data.frame(sales_glm_me, sales_glm_rmse)
    
    #Change the column name to something more descriptive.
    colnames(me_rmse_sales_glm)<- c("sales_glm")
}}




################################################
##GBM
#Using all the models to see the prediction
colnames(data)
data2=data
data2$Total.Value.Sales=NULL
data3=data2[2:length(data2)]
grade_gbm1<-gbm(formula = Total.Volume.Sales~., data=data3,
                distribution = "gaussian",n.trees = 1000, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75)
summary(grade_gbm1)

best.iter1<-gbm.perf(grade_gbm1, plot.it = F, method = "OOB")
par(mfrow=c(2,2))
plot.gbm(grade_gbm1, "Weighted.Average.Price", best.iter1)
plot.gbm(grade_gbm1, "Distribution" , best.iter1)
plot.gbm(grade_gbm1, "Price.Promotion.1", best.iter1)
plot.gbm(grade_gbm1, "Price.Promotion.2", best.iter1)

par(mfrow=c(2,2))
plot.gbm(grade_gbm1, "On.pack.Promo.Offer" , best.iter1)
plot.gbm(grade_gbm1, "Rebrand"  , best.iter1)
plot.gbm(grade_gbm1, "TV", best.iter1)
plot.gbm(grade_gbm1, "Radio" , best.iter1)

par(mfrow=c(2,2))
plot.gbm(grade_gbm1, "Press" , best.iter1)
plot.gbm(grade_gbm1, "Outdoor"  , best.iter1)
plot.gbm(grade_gbm1, "Online", best.iter1)


plot(predict.gbm(grade_gbm1, data3, best.iter1), data3$Total.Volume.Sales, 
     main="Observed vs Predicted grade")
lines(lowess(predict.gbm(grade_gbm1, data3, best.iter1), data3$Total.Volume.Sales), col="red", lwd=3)
r_grade <-cor.test(predict.gbm(grade_gbm1, data3, best.iter1), data3$Total.Volume.Sales)
r2grade <- r_grade$estimate^2
r2grade
legend("topleft", paste("r^2=", round(r2grade,3)))


# Loess method
ggplot(data3, aes(x=predict.gbm(grade_gbm1, data3, best.iter1), y=data3$Total.Volume.Sales)) + 
  geom_point()+geom_smooth()+
  labs(title ="Observed vs Predicted Total Sales Volume", x = "Predicted Sales Volume", y = "Observed Sales Volume")+ 
  annotate(geom="text", x=1450, y=2400,label=paste("r^2=", round(r2grade,2)*100, "%"),
           color="red")
plot(grade_gbm1, 'TV', return.grid=TRUE)