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
# install.packages('ggthemes')
library(ggthemes)

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
  
# boxplot(data$Total.Volume.Sales)
# 1.What general findings can you draw from the data?
# shapiro.test(data$Total.Volume.Sales)
data$Dates=as.Date(data$Dates, "%d.%m.%Y")
glimpse(data)
dim(data)
sd(data$Total.Volume.Sales)
range(data$Total.Volume.Sales)

data.frame(summary(data))

plot(data$Weighted.Average.Price, data$Total.Volume.Sales)
# cor(data$TV, data$Total.Volume.Sales)

# abline(data$Weighted.Average.Price, data$Total.Volume.Sales)



data_2014<-data[data$Dates>= as.Date("2014-01-01") &
                    data$Dates<= as.Date("2014-12-31") ,]


data_2015<-data[data$Dates>= as.Date("2015-01-01") &
                  data$Dates<= as.Date("2015-12-31") ,]


data_2016<-data[data$Dates>= as.Date("2016-01-01") &
                  data$Dates<= as.Date("2016-12-31") ,]


format(data$Dates,'%Y')
year<- as.factor(format(data$Dates,'%Y'))
# install.packages('ggthemes')
library(ggthemes)
ggplot(data, aes(Dates)) + geom_line(aes(y=Total.Volume.Sales,col=year))+
  ggtitle('Total Sales Volume from 2014 to 2016')+
  theme(
    plot.title = element_text(color="red", size=13, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  ) + theme_solarized_2()+scale_colour_manual("Year", 
                                   values = c("Blue","black", "Red"))

ggplot(data, aes(Dates)) + geom_line(aes(y=Weighted.Average.Price,col=year))+
  ggtitle('Weighted Average Price from 2014 to 2016')+
  theme(
    plot.title = element_text(color="red", size=13, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  ) + theme_grey()


# datac<- data
# datac[2:14]<- scale(datac[2:14])
# ggplot(datac, aes(Dates)) + geom_line(aes(y=Weighted.Average.Price))+
#   geom_line(aes(y=Total.Volume.Sales))
# + geom_line(aes(y=TV))
# geom_line(aes(y=Total.Value.Sales))
#   ggtitle('Weighted Average Price from 2014 to 2016')
 




plot(data$Dates, data$Total.Volume.Sales, type='l', col=dat)

plot(data_2014$Dates, data_2014$Total.Volume.Sales, type='l')


hist(data$Total.Volume.Sales)

boxplot(data$Total.Volume.Sales)
hist(data$Total.Volume.Sales, main = "Total Volume of Sales", xlab="Sales Volume",col = "grey")
abline(v=mean(data$Total.Volume.Sales), col = "red", lwd=2)








par(mfrow=c(1,1))
plot(data$Dates, data$Weighted.Average.Price, type='l', col='red')
# par(new=T)
plot(data$Dates,data$Total.Volume.Sales, type='l', col='blue')


plot(data$Dates, data$Total.Volume.Sales, type='l')


####################
#Box plots

# initialise a plot of high_use and absences
box_sales_vol<- ggplot(data_plot, aes(x=Rebrand, y=Total.Volume.Sales,col=Rebrand))

# define the plot as a boxplot and draw it
box_sales_vol+ geom_boxplot() + ggtitle("Total Volume Sales vs Rebranding")


colnames(data_plot)


gather(data) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() 
pairs(data[0:4])
data_plot= data
data_plot$Rebrand=as.factor(data_plot$Rebrand)
plot_cor <- ggpairs(data_plot[2:10],mapping = aes(col=Rebrand, alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
plot_cor
corrplot(data[2:10])
#rebranding has quite a strong correlation of 0.742with the average price

colnames(data)

# initialize a plot
sales_volume <- ggplot(data = data, aes(x=Total.Volume.Sales))

# plot
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
plot(sales_glm)
#Anova test
anova(sales_glm, test="Chisq")

# Explained deviance (D-squared) = (Null deviance - Residual deviance) / Null deviance

d_Squared<-function(null_dev, res_dev){
  d2=(null_dev-res_dev)/null_dev
  return(d2)
}

#Coeffient of determination in the GLM
d_Squared(null_dev = sales_glm$null.deviance, res_dev = sales_glm$deviance)

sales_pred<- predict.glm(object = sales_glm, newdata = data , type="response")
r_glm_sales=cor(sales_pred, data$Total.Volume.Sales)
r2_glm_sales <- r_glm_sales^2

# Loess method
ggplot(data, aes(x=sales_pred,y=Total.Volume.Sales))+ 
  geom_point()+geom_smooth()+
  labs(title ="Observed vs Predicted Total Sales Volume", x = "Predicted Sales Volume", y = "Observed Sales Volume")+ 
  annotate(geom="text", x=1450, y=2400,label=paste("r^2=", round(r2_glm_sales,2)*100, "%"),
           color="red")
###to be more realistic, it is important to use a crossvalidation
#or test/train split
#I will be using Leave-One-Out-Cross-Validation
#to validate my model



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
####GLM#################
#set.seed(0)
{pred_sales_test <-observed_sales<- c()
for (i in 1:nrow(data)){
  print(i)
  cal <- data[-i,]
  eva <-data[i,]
  
  #with redundant predictors
  # sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price+
  #                  Distribution + Price.Promotion.1+ Price.Promotion.2+
  #                  On.pack.Promo.Offer+Rebrand+ TV + Radio+ Press+
  #                  Outdoor+ Online,data=data,family ="gaussian")
  #Without the redundant predictors.
  sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price + Price.Promotion.1 +
                   Rebrand + TV + Outdoor,data=cal,family ="gaussian")

  sales_glm_pred_test<- predict.glm(object = sales_glm, newdata = eva, type="response")
  pred_sales_test[i]<- sales_glm_pred_test
  observed_sales[i]<-eva$Total.Volume.Sales

  
}
 pred_obs <- cbind.data.frame(pred_sales_test, observed_sales)
 colnames(pred_obs)<-c("pred_sales", "obs_sales")
 #cor(pred_sales_test, eva$Total.Volume.Sales  ,method = "spearman")
}

r=cor(pred_obs$pred_sales, pred_obs$obs_sales, method = "spearman")
r2=r**2
r2
plot(pred_obs$pred_sales, pred_obs$obs_sales)

# Loess method
ggplot(data, aes(x=sales_pred,y=Total.Volume.Sales, color="red"))+ 
  geom_point(color="black")+geom_smooth()+
  labs(title ="Observed vs Predicted Total Sales Volume", x = "Predicted Sales Volume", y = "Observed Sales Volume")+ 
  annotate(geom="text", x=1450, y=2400,label=paste("r^2=", round(r2,2)*100, "%"),
           color="red")


# One ellipse arround all points
ggplot(pred_obs, aes(x=pred_sales, y=obs_sales))+
  geom_point()+
  stat_ellipse()  



rmse(pred_obs$pred_sales, pred_obs$obs_sales)
mean_error(pred_obs$pred_sales, pred_obs$obs_sales)




#LOOCV GBM
data$Total.Value.Sales<-NULL
{pred_sales_test_cv <-observed_sales_cv<- c()
  for (i in 1:nrow(data)){
    print(i)
    cal <- (data[2:13])[-i,]
    eva <-(data[2:13])[i,]
    sales_gbm1_cv<-gbm(formula = Total.Volume.Sales~., data=cal,
                    distribution = "gaussian",n.trees = 2300, shrinkage = 0.001, interaction.depth = 6,
                    bag.fraction = 0.75, verbose = F)
    
    # cor(sales_gbm1_pred, data3$Total.Volume.Sales)
    best.iter_cv<-gbm.perf(sales_gbm1_cv, plot.it = F, method = "OOB")
    # sales_gbm1_pred <- predict.gbm(sales_gbm1, newdata = eva, n.trees=sales_gbm1$n.trees, type = "response")
    sales_gbm1_pred_cv<- predict.gbm(object = sales_gbm1_cv, newdata = eva, best.iter_cv, type="response")
    
    pred_sales_test_cv[i]<- sales_gbm1_pred_cv
    observed_sales_cv[i]<-eva$Total.Volume.Sales
    
    
  }
  pred_obs_cv <- cbind.data.frame(pred_sales_test_cv, observed_sales_cv)
  colnames(pred_obs_cv)<-c("pred_sales", "obs_sales")
  #cor(pred_sales_test, eva$Total.Volume.Sales  ,method = "spearman")
}

r=cor(pred_obs_cv$pred_sales, pred_obs_cv$obs_sales, method = "spearman")
r2_gbm1=r**2
r2_gbm1
plot(pred_obs_cv$pred_sales, pred_obs_cv$obs_sales)




#copy the data and remove the variables of no interest in the model.
data2=data
data2$Total.Value.Sales=NULL
data_reg=data2[2:length(data2)]
{r2_glm_all<-r2_gam_all<-r2_gbm1_all<-c()
  rep<-2000
for (i in 1:rep){
    #print the index to see the iteration
    print(i)
    
    #Creare a 70 sample(with replacement) from the original data
    rand<- sample(1:nrow(data_reg), size = 0.8*nrow(data_reg))
    
    #70% for the train/calibration data
    cal<- data_reg[rand,]
    
    #remaining 30 for the test/evaluation data
    eva<-data_reg[-rand,]
    
    ####GLM
    #perform a Genelralised Linear Model(GLM)(with redundant predictors)
    # sales_glm <- glm(Total.Volume.Sales~Weighted.Average.Price+
    #                    Distribution + Price.Promotion.1+ Price.Promotion.2+
    #                    On.pack.Promo.Offer+Rebrand+ TV + Radio+ Press+
    #                    Outdoor+ Online, data=cal, family = "gaussian") 
    #GLM withiout redundant predictors
    sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price + Price.Promotion.1 +
                     Rebrand + TV + Outdoor,data=cal,family ="gaussian")
    
    
    #predict into the test/evaluation data
    sales_glm_pred<- predict.glm(object = sales_glm, newdata = eva, type="response")
    
    #find the correlation between the train and test data.
    cor_glm_sales<-cor(sales_glm_pred, eva$Total.Volume.Sales, method = "pearson")
    r2_glm_all[i]<-cor_glm_sales^2
    
    
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
    
    
      
    
    
    #GAM
    sales_gam <- gam(Total.Volume.Sales~ s(Weighted.Average.Price, k=3) +
                       s(Distribution , k=3) + Price.Promotion.1 +  
                       Price.Promotion.2 + On.pack.Promo.Offer + 
                       Rebrand+ TV + Radio+ Press+
                       Outdoor+ s(Online, k=3), data = cal, family = "gaussian")
    sales_gam_pred <- predict.gam(sales_gam, newdata = eva, type = "response")
    
    obs_pred_sales_gam<- cbind.data.frame(sales_gam_pred, eva$Total.Volume.Sales)
    colnames(obs_pred_sales_gam) <- c("pred_gam_sales", "obs_gam_sales")
    #you can just calclate the correlation straight away
    cor_gam_sales <- cor(sales_gam_pred, eva$Total.Volume.Sales, method = "pearson")
    r2_gam_all[i]<-cor_gam_sales^2
    
    #########
    #mean error and root mean square error
    error_sales_gam<- cbind.data.frame(sales_gam_pred, eva$Total.Volume.Sales)
    colnames(error_sales_gam) <- c("pred_gam_sales", "obs_sales")
    
    sales_gam_me <- mean_error(error_sales_gam$obs_sales, error_sales_gam$pred_gam_sales)
    sales_gam_rmse <- rmse(error_sales_gam$obs_sales, error_sales_gam$pred_gam_sales)
    
    me_rmse_sales_gam <- rbind.data.frame(sales_gam_me, sales_gam_rmse)
    colnames(me_rmse_sales_gam)<- c("sales_gam")
    
    
    
    
    
    ###################################################################
    #using the normal gbm, package.
    #GBM
    # sales_gbm1 <- gbm.step(data=cal, gbm.x =c('Weighted.Average.Price', 'Distribution', 'Price.Promotion.1',
    #                                           'Price.Promotion.2', 'On.pack.Promo.Offer', 'Rebrand', 'TV', 'Radio',
    #                                           'Press', 'Outdoor', 'Online'), gbm.y = "Total.Volume.Sales",
    #                        bag.fraction=0.75, learning.rate = 0.001,
    #                        family="gaussian",n.trees=50, n.folds=10,
    #                        max.trees = 1000, tree.complexity = 6)
    

    sales_gbm1<-gbm(formula = Total.Volume.Sales~., data=cal,
                    distribution = "gaussian",n.trees = 2300, shrinkage = 0.001, interaction.depth = 6,
                    bag.fraction = 0.75, verbose = F)
    
    # cor(sales_gbm1_pred, data3$Total.Volume.Sales)
    best.iter<-gbm.perf(sales_gbm1, plot.it = F, method = "OOB")
    # sales_gbm1_pred <- predict.gbm(sales_gbm1, newdata = eva, n.trees=sales_gbm1$n.trees, type = "response")
    sales_gbm1_pred<- predict.gbm(object = sales_gbm1, newdata = eva, best.iter, type="response")
    
    cor_gbm1_sales <- cor(sales_gbm1_pred, eva$Total.Volume.Sales, method = "pearson")
    r2_gbm1_all[i]<-cor_gbm1_sales^2
    
    
    #########
    #mean error and root mean square error
    error_sales_gbm1<- cbind.data.frame(sales_gbm1_pred, eva$Total.Volume.Sales)
    colnames(error_sales_gbm1) <- c("pred_gbm1_sales", "obs_sales")
    
    sales_gbm1_me <- mean_error(error_sales_gbm1$obs_sales, error_sales_gbm1$pred_gbm1_sales)
    sales_gbm1_rmse <- rmse(error_sales_gbm1$obs_sales, error_sales_gbm1$pred_gbm1_sales)
    
    me_rmse_sales_gbm1 <- rbind.data.frame(sales_gbm1_me, sales_gbm1_rmse)
    colnames(me_rmse_sales_gbm1)<- c("sales_gbm1")
    
    
}}

##########
######MODELS validation
r2_glm_all
mean(r2_glm_all)
range(r2_glm_all)

r2_gam_all
mean(r2_gam_all)
range(r2_gam_all)

r2_gbm1_all
mean(r2_gbm1_all)
range(r2_gbm1_all)

summary(sales_gbm1)
summary.gbm(sales_gbm1)



sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price+
                 Distribution + Price.Promotion.1+ Price.Promotion.2+
                 On.pack.Promo.Offer+Rebrand+ TV + Radio+ Press+
                 Outdoor+ Online,data=data,family ="gaussian")
summary(sales_glm)




sales_pred<- predict.glm(object = sales_glm, newdata = data , type="response")

###############
#Predict baseline sales
#if average price and distribution unit remain thesame
base_data<- data
base_data[nrow(data),5:length(data)]<-0
baseline_data<-base_data[nrow(data),]
summary(baseline_data)
base_sales_pred <- predict.glm(object = sales_glm, newdata = baseline_data , type="response")
base_sales_pred

summary(data)






#######plot the graph of the predicted and observed sales volume.
# Loess method
plot(sales_pred, data$Total.Volume.Sales)
ggplot(data, aes(x = sales_pred, y = Total.Volume.Sales)) +
  geom_point() + geom_smooth(col='red') +
  labs(title = "Observed vs Predicted Total Sales Volume",
       x = "Predicted Sales Volume", y = "Observed Sales Volume") +
  annotate(
    geom = "text",
    x = 1530,
    y = 2550,
    label = paste("Degree of Certainty=",
                  round(mean(r2_glm_all), 2) * 100, "%"),
    color = "red"
  ) + theme_economist()













#GAM
sales_gam <- gam(Total.Volume.Sales~ s(Weighted.Average.Price, k=3) +
                   s(Distribution , k=3) + Price.Promotion.1 +  
                   Price.Promotion.2 + On.pack.Promo.Offer + 
                   Rebrand+ TV + Radio+ Press+
                   Outdoor+ s(Online, k=3), data = data, family = "gaussian")


summary(sales_gam)


plot(sales_gam, pages=1)


################################################
##GBM
#Using all the models to see the prediction
colnames(data)
data2=data
data2$Total.Value.Sales<-NULL
data2$Total.Value.Sales<-data2$Weighted.Average.Price<- data2$Distribution<-NULL

data3=data2[2:length(data2)]
sales_gbm1<-gbm(formula = Total.Volume.Sales~., data=data3,
                distribution = "gaussian",n.trees = 2300, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75, verbose = T)


#check the optimal number of trees
best.iter<-gbm.perf(sales_gbm1, plot.it = F, method = "OOB")

# plot the performance # plot variable influence
# summary(sales_gbm1,n.trees=1) # based on the first tree
rel_imp<-summary(sales_gbm1,n.trees=best.iter) # based on the estimated best number of trees
rel_imp<- data.frame(rel_imp$var, rel_imp$rel.inf)










sales_gbm1_pred<- predict.gbm(object = sales_gbm1, newdata = data, best.iter, type="response")
cor_gbm1_sales <- cor(sales_gbm1_pred, data$Total.Volume.Sales, method = "pearson")
r2_gbm1_all[i]<-cor_gbm1_sales^2






# ############
# ##check the optimal OOB for the gbm model
# best.iter <- gbm.perf(sales_gbm1,method="OOB")
# print(best.iter)
# 
# 
# # check performance using a 50% heldout test set
# best.iter <- gbm.perf(sales_gbm1,method="test")
# print(best.iter)
# 
# # check performance using 5-fold cross-validation
# best.iter <- gbm.perf(sales_gbm1,method="cv")
# print(best.iter)
# 
# # compactly print the first and last trees for curiosity
# print(pretty.gbm.tree(sales_gbm1,1))
# print(pretty.gbm.tree(sales_gbm1,sales_gbm1$n.trees))
# 
# # contour plot of variables 1 and 2 after "best" iterations
# plot(sales_gbm1,1:2,best.iter)
# 
# # lattice plot of variables 2 and 3
# plot(sales_gbm1,2:3,best.iter)
# # lattice plot of variables 3 and 4
# plot(sales_gbm1,3:4,best.iter)
# # 3-way plots
# plot(sales_gbm1,c(1,2,6),best.iter,cont=20)
# # plot(sales_gbm1,1:3,best.iter)
# # plot(sales_gbm1,2:4,best.iter)
# # plot(sales_gbm1,3:5,best.iter)











best.iter1<-gbm.perf(sales_gbm1, plot.it = F, method = "OOB")
par(mfrow=c(2,2))
plot.gbm(sales_gbm1, "Weighted.Average.Price", best.iter1)
plot.gbm(sales_gbm1, "Distribution" , best.iter1)
plot.gbm(sales_gbm1, "Price.Promotion.1", best.iter1)
plot.gbm(sales_gbm1, "Price.Promotion.2", best.iter1)

par(mfrow=c(2,2))
plot.gbm(sales_gbm1, "On.pack.Promo.Offer" , best.iter1)
plot.gbm(sales_gbm1, "Rebrand"  , best.iter1)
plot.gbm(sales_gbm1, "TV", best.iter1)
plot.gbm(sales_gbm1, "Radio" , best.iter1)

par(mfrow=c(2,2))
plot.gbm(sales_gbm1, "Press" , best.iter1)
plot.gbm(sales_gbm1, "Outdoor"  , best.iter1)
plot.gbm(sales_gbm1, "Online", best.iter1)


################
#Finding optimal marketing efforts
##price should be 2.4-2.7, distribution, 83
summary(sales_gbm1)
summary(sales_gbm1,n.trees=best.iter1)





par(mfrow=c(1,1))
plot(predict.gbm(sales_gbm1, data3, best.iter1), data3$Total.Volume.Sales, 
     main="Observed vs Predicted sales")
lines(lowess(predict.gbm(sales_gbm1, data3, best.iter1), data3$Total.Volume.Sales), col="red", lwd=3)
r_sales <-cor.test(predict.gbm(sales_gbm1, data3, best.iter1), data3$Total.Volume.Sales)
r2sales <- r_sales$estimate^2
r2sales
legend("topleft", paste("r^2=", round(r2sales,3)))



# Loess method
ggplot(data3, aes(x=predict.gbm(sales_gbm1, data3, best.iter1), y=data3$Total.Volume.Sales)) + 
  geom_point()+geom_smooth()+
  labs(title ="Observed vs Predicted Total Sales Volume", x = "Predicted Sales Volume", y = "Observed Sales Volume")+ 
  annotate(geom="text", x=1450, y=2400,label=paste("r^2=", round(r2sales,2)*100, "%"),
           color="red")
plot(sales_gbm1, 'TV', return.grid=TRUE)






###############################################
##############################################
#Predict the sales when marketing is zero

par(mfrow=c(2,2))
plot(sales_glm, which = c(1,2,5))

###############################################################
###Time line without marketing

data_copy=data

#if marketing effort is 0
data_copy[6:length(data_copy)]=0
data_copy 
summary(data_copy)



sales_glm<-glm(Total.Volume.Sales~ Weighted.Average.Price+
                 Distribution + Price.Promotion.1+ Price.Promotion.2+
                 On.pack.Promo.Offer+Rebrand+ TV + Radio+ Press+
                 Outdoor+ Online,data=data,family ="gaussian")

sales_pred<- predict.glm(object = sales_glm, newdata = data_copy , type="response")
# sales_pred<- predict.gbm(object = sales_gbm1, newdata = data_copy, best.iter, type="response")


plot(data_copy$Dates, data_copy$Total.Volume.Sales, type='l', col='blue', ylim=c(800,2500),
     ylab='Total Volume of Sales')
par(new=T)
plot(data_copy$Dates,sales_pred, type='l', col='red', ylim=c(800,2500), ylab='')

# ggplot for better visualisation
# ggplot(data_copy, aes(x=Dates, y=Total.Volume.Sales)) + 
#   geom_line(col='blue')+ ylim(800,2500)

ggplot(data_copy, aes(Dates)) + geom_line(aes(y=Total.Volume.Sales,col='With Marketing'))+
  geom_line(aes(y=sales_pred,col='Without Marketing'))+ xlab('Date')+
  ggtitle('Predicted Effect of Marketing Cut on Total Sales Volume.')+
  theme(
    plot.title = element_text(color="red", size=9, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )+scale_colour_manual("Legend", breaks = c("With Marketing", "Without Marketing"),
                        values = c("Blue", "Red"))+ theme_economist()










############################

# install.packages(c("FactoMineR", "factoextra"))
# library("FactoMineR")
# library("factoextra")
# X=data[4:length(data)]
# PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)
# 
# res.pca <- PCA(X, scale.unit = TRUE, ncp = 5,graph = T)
# print(res.pca)
# 
# eig.val <- get_eigenvalue(res.pca)
# eig.val
# 
# fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
# 
# var <- get_pca_var(res.pca)
# var
# 
# fviz_pca_var(res.pca, col.var = "black")
# head(var$cos2, 4)
# 
# library("corrplot")
# corrplot(var$cos2, is.corr=FALSE)
# 
# fviz_pca_var(res.pca, col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = TRUE # Avoid text overlapping
# )
