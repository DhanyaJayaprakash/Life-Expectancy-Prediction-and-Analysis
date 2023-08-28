Dataset1<-read.csv("Life_Expectancy_Data1.csv",header=T)
str(Dataset1)
library(readr)
library('ISLR')
library('tidyr')
library('dplyr')
library('ggplot2')
library(corrplot)
library('olsrr')
library('leaps')

#Renaming the variables
names(Dataset1)
Z_Last<-Dataset1
names(Z_Last)<-c<- c("Country", "Country Code", "Continent","Life_expectancy", "Acc_electr", "Ad_income",
                     "Ad_income_p_ca", "Child-with_HIV",
                     "Child_noschool", "Edu_att_primary",
                     "Edu_att_Bachelors", "Mort_infant", "Pr_compl_rate",
                     "Literacy_rate", "Real_int_rate", "Population_growth", "Population_density",
                     "Population_total", "Cur_health_expn_per_capita", "Cur_health_expen",
                     "Unemployment_total", "GDP_Growth", "GDP_per_capita", "Birth_rate",
                     "Renew-en_consum", "Adult_new_HIV",
                     "People_drinking_water"," Poverty_headcount_ratio",
                     "Com_education_durat")



str(Z_Last)# displays the structure value and the entire column and row information at a glance

dim(Z_Last)#This display the dimension of the data frame which is in this case 217 rows and 29 columns

head(Z_Last)# display the first 6 rows and columns value by default of the data set

head(Z_Last,10) # display the first 10 rows and columns value of the data set depending on how many rows we wanted displayed.





# Since the dataset of the life expectancy has been displayed below are there are alot of missing value which needed to be resolved.
# There are three methods/approach of dealing with the predictor variable (missing value).The reason for the missing Value had to be know before any of the approach can be used.
# Each of the approach would be tried and the perfect/best model will be selected in dealing with the missing values.

tail(Z_Last) # this is checked to ensure our data set does not contain any irrelevant information at the end

summary(Z_Last)# display the number of NA values in each of the columns displayed
ggplot(Z_Last, aes(x = Continent, y = Life_expectancy)) + 
  geom_bar(stat = "identity")


#NOTE
#It is important to handle the missing values appropriately.

#Many machine learning algorithms fail if the dataset contains missing values. However, algorithms like K-nearest and Naive Bayes support data with missing values.
#You may end up building a biased machine learning model which will lead to incorrect results if the missing values are not handled properly.
#Missing data can lead to a lack of precision in the statistical analysis.

colSums(is.na(Z_Last))# display on the data set if there is NA values or Not

#md.pattern(Data1)

library(mice)

Z_Last<-Z_Last[,c(-1,-2,-3,-8,-11,-14,-25,-28)]
Z_Last <- Z_Last[-12] # the column population total was removed as it keeps populating erroron the table.

#View(Data1)

Z_Last_imp<-mice(Z_Last, seed =23109);

summary(Z_Last) #Display the data set after the multiple imputation have been performed on the data set

colSums(is.na(Z_Last))# Display the set of Columns left in the dataset 

md.pattern(Z_Last)
vif(Z_Last_imp)

##print(Z_Last)

complete(Z_Last_imp)# these helps to check the imputed values and see if there might have an error at any of the steps while computing the steps

DataC <- complete(Z_Last_imp)# showing computed values of the data set after the imputation function was used.

summary(DataC)


Z_Last_imp$imp # these function helps to analyse and organised the dataset in a logical manner

complete(Z_Last_imp,2) # display the first 2 columns of the data set

stripplot(Z_Last_imp, pch = 20, cex = 1.2) #Draws the Strip plot(One dimentional scatter plots ) of the data set

xyplot(Data1_imp, Child_noschool ~  Population_growth | .imp, pch = 20, cex = 1.4)

model.fit <- with (Z_Last_imp, lm(Life_Expectancy_at_birth ~.))

                                    
summary(model.fit)
pooled.model.fit<-pool(model.fit)
summary(pooled.model.fit)  

#Checking for collinearity using R

#library("faraway")

Full_model <- lm(Life_expectancy ~ ., data=DataC)
summary(Full_model)
ols_vif_tol(Full_model)
vif(Full_model)

install.packages('corrplot')
library(corrplot)
m <- cor(DataC[-1])
m <- cor(DataC[-1])
corrplot(m, tl.pos = 'lt',tl.cex = 0.5,tl.srt=35, method ='circle')

#X<-Data1_imp[,-9]
#Data1_imp.corr<-cor(X)
#Data1_imp.corr
#multicollinearity
ols_vif_tol(Full_model)
#vif(Full_model)
#Removing attributes with high VIF
Relation2<-DataC[,c(-2,-3,-4,-7,-10,-12,-16,-17)]

FnRelation<-cor(Relation2)

corrplot(FnRelation,tl.pos ='lt', tl.cex=0.55,method='circle')
summary(Relation2)
Full_modelCor <- lm(Life_expectancy ~ ., data=Relation2)
summary(Full_modelCor)
Relation3<-Relation2[,c(-5,-9,-10,-12)]
Full_modelRemove <- lm(Life_expectancy ~ ., data=Relation3)
summary(Full_modelRemove)

#Finding the best model
library('faraway')
#Forward feature selection
bestmodel1<-lm(Life_expectancy~1,data=Relation3)
step1<-step(bestmodel1,scope=~ Child_noschool  + Edu_att_primary+ Pr_compl_rate + Population_density  + Cur_health_expen + Unemployment_total    + People_drinking_water,
            method='forward')
summary(step1)
AIC(bestmodel1)
BIC(bestmodel1)
#Backward feature selection
bestmodel2<-lm(Life_expectancy~.,data=Relation3)
step2<-step(bestmodel2,method="backward")
summary(step2)
AIC(bestmodel2)
BIC(bestmodel2)
modelfit2 <- lm(Life_expectancy ~ Edu_att_primary + 
                  Pr_compl_rate + Population_density + Cur_health_expen + Unemployment_total + 
                  People_drinking_water, data = Relation3)
summary(modelfit2)
AIC(modelfit2)

modelfit1 <- lm(Life_expectancy ~ People_drinking_water + Pr_compl_rate + Unemployment_total + 
                  Cur_health_expen + Edu_att_primary + Population_density , data = Relation3)
summary(modelfit1)
  AIC(modelfit1)
qqnorm(modelfit1, ylab="Standardized Residuals",
         xlab="Normal Scores", main="QQ Plot for Reduced model")
qqline(modelfit1)
anova(Full_model)
anova(modelfit1,Full_model)
