######################### Econometrics of Qualitative Data : Marketing project ###################
### Data from the ERIM Database, collectbed by A. C. Nielsen
#The data span the years 1986 to 1988, and the particular subset we use concerns purchases of detergent by households in Sioux Falls (South Dakota, USA).

##################### 1. Data Exploration #####################
library(data.table)
library(ggplot2)

#Load data
hhchar = read.table("C:/Users/roman/Desktop/ENSAE 3A/Sujet47/fp-data/hhchar.txt", header = TRUE)
prices = read.table("C:/Users/roman/Desktop/ENSAE 3A/Sujet47/fp-data/prices.txt", header = TRUE, dec = '.')
purch =  read.table("C:/Users/roman/Desktop/ENSAE 3A/Sujet47/fp-data/purch.txt", header = TRUE)
weights = read.table("C:/Users/roman/Desktop/ENSAE 3A/Sujet47/fp-data/weights.txt", header = TRUE, dec = '.')

setDT(hhchar)
setDT(prices)
setDT(purch)
setDT(weights)

#Brand coding
# 1=Cheer
# 2=Oxidol
# 3=Surf
# 4=Tide
# 5=Wisk
# 6=Rest
#Verif :
#Count the number of distinct brands
unique(prices[,brand])

brandcoding = data.table(brand = c(1,2,3,4, 5, 6), brandname = c('Cheer', 'Oxidol', 'Surf', 'Tide', 'Wisk', 'Rest'  ))
purch = purch[brandcoding, nomatch=0, on = 'brand']


#We select households making at least 4 purchases 
#and buying only brands that are available throughout the complete sample. COMMENT ON FAIT CA ? ON SUPPRIME LES MENAGES QUI FONT LEURS ACHATS DANS CE TYPE DE MAGASINS ?
#Count the number of purchases per household
purchase_per_hh = purch[,.N,by='id']
setkey(purchase_per_hh,id)
setkey(hhchar,hh.id)
hhchar = hhchar[purchase_per_hh, nomatch=0] #Add a column to hhchar that gives the number of purchases per hh
#Delete hh with less that 4 purchases within the period
hhchar = hhchar[N > 3]
purch = purch[ id %in% hhchar[,hh.id]]
weights = weights[ id %in% hhchar[,hh.id]]
#Stats number of purchases per hh :
summary(hhchar[,N])

#Identify the week number of each purchase date
purch[,week := floor(15/14 + (date-9490)/7)]



### Brands
#Number of purchases per brand
purch[,.N, by = 'brand']
qplot(x=brandname, data=purch, geom="bar", fill = as.factor(brand), xlab = "Brand name", ylab = "Number of purchases") + labs(fill = "Brand code")
#Compute a reference price per store (defined as the mode price of the brand per store)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
prices[,ref_price := getmode(price), by = list(sid, brand)]
summary(prices[,getmode(price), by = list(sid, brand)][, V1])
#Create a binary variable indicating that there is a promotion (compared to ref price)
prices[,is_promotion:= ifelse(price - ref_price < 0, 1, 0)]
#Create a binary variable indicating that there is a price increase (compared to ref price)
prices[,is_price_increase:= ifelse(price - ref_price > 0, 1, 0)]

#Create a binary variable indicating that there is a display action (display variable != 0)
prices[,is_displayed:= ifelse(display > 0, 1, 0)]
#Compare av number of sales per week when there is a promotion, when there is a price increase, and in a normal week
#each hh is first linked to one specific store (with the highest weight)
prefered_store = weights[, .SD[which.max(weight)], by = id]
sales = purch[prefered_store, nomatch=0, on = 'id' ]
sales = sales[,.(vol_sales_per_week = sum(vol), nb_sales_per_week = .N), by = list(brand, week, sid)]
qplot(x = vol_sales_per_week, data = sales, geom = 'histogram', bins = 40, xlab = 'Sales volume per week per store (in ounces)')
sales = sales[prices, on = list(brand,week, sid)]
sales[is.na(sales)] = 0
#Compute average sales per week per store when there is a promotion and when not
av_sales_per_store = sales[,.(av_sales_per_week = mean(vol_sales_per_week), av_nb_sales_per_week = mean(nb_sales_per_week)),by = list(brand, sid, is_promotion)]
av_sales_per_store_normal = av_sales_per_store[is_promotion == 0]
setnames(av_sales_per_store_normal,"av_sales_per_week","av_sales_per_week_normal")
setnames(av_sales_per_store_normal,"av_nb_sales_per_week","av_nb_sales_per_week_normal")
av_sales_per_store_promo = av_sales_per_store[is_promotion == 1]
setnames(av_sales_per_store_promo,"av_sales_per_week","av_sales_per_week_promo")
setnames(av_sales_per_store_promo,"av_nb_sales_per_week","av_nb_sales_per_week_promo")
setkey(av_sales_per_store_promo, brand, sid)
setkey(av_sales_per_store_normal, brand, sid)
av_sales_per_store_comparison = av_sales_per_store_normal[av_sales_per_store_promo, nomatch = 0]
av_sales_per_store_comparison[, percent_diff_sales_per_week := ((av_sales_per_week_promo - av_sales_per_week_normal)/av_sales_per_week_normal)*100]
av_sales_per_store_comparison[, percent_diff_nb_sales_per_week := ((av_nb_sales_per_week_promo - av_nb_sales_per_week_normal)/av_nb_sales_per_week_normal)*100]
#comparison in volume :
av_sales_per_store_comparison[,mean(percent_diff_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_sales_per_week =mean(percent_diff_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity") + labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")
#Comparison in number :
av_sales_per_store_comparison[,mean(percent_diff_nb_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_nb_sales_per_week =mean(percent_diff_nb_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_nb_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity")+ labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")

#Compute average sales per week per store when there is a price increase and when not
av_sales_per_store = sales[,.(av_sales_per_week = mean(vol_sales_per_week), av_nb_sales_per_week = mean(nb_sales_per_week)),by = list(brand, sid, is_price_increase)]
av_sales_per_store_normal = av_sales_per_store[is_price_increase == 0]
setnames(av_sales_per_store_normal,"av_sales_per_week","av_sales_per_week_normal")
setnames(av_sales_per_store_normal,"av_nb_sales_per_week","av_nb_sales_per_week_normal")
av_sales_per_store_increase = av_sales_per_store[is_price_increase == 1]
setnames(av_sales_per_store_increase,"av_sales_per_week","av_sales_per_week_increase")
setnames(av_sales_per_store_increase,"av_nb_sales_per_week","av_nb_sales_per_week_increase")
setkey(av_sales_per_store_increase, brand, sid)
setkey(av_sales_per_store_normal, brand, sid)
av_sales_per_store_comparison = av_sales_per_store_normal[av_sales_per_store_increase, nomatch = 0]
av_sales_per_store_comparison[, percent_diff_sales_per_week := ((av_sales_per_week_increase - av_sales_per_week_normal)/av_sales_per_week_normal)*100]
av_sales_per_store_comparison[, percent_diff_nb_sales_per_week := ((av_nb_sales_per_week_increase - av_nb_sales_per_week_normal)/av_nb_sales_per_week_normal)*100]
#comparison in volume
av_sales_per_store_comparison[,mean(percent_diff_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_sales_per_week =mean(percent_diff_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity")+ labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")
#comparison in number
av_sales_per_store_comparison[,mean(percent_diff_nb_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_nb_sales_per_week =mean(percent_diff_nb_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_nb_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity")+ labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")

# ggplot(sales[,mean(vol_sales_per_week),by = list(brand, is_promotion)], aes(as.factor(is_promotion), V1, fill=as.factor(brand))) + 
#   geom_bar(position="dodge", stat="identity")
# sales[,mean(vol_sales_per_week),by = list(brand, is_promotion)]
# ggplot(sales[,mean(vol_sales_per_week),by = list(brand, is_price_increase)], aes(as.factor(is_price_increase), V1, fill=as.factor(brand))) + 
#   geom_bar(position="dodge", stat="identity")
# sales[,mean(vol_sales_per_week),by = list(brand, is_price_increase)]


#Compare av number of sales per week when there is a display action and when not
av_sales_per_store = sales[,.(av_sales_per_week = mean(vol_sales_per_week), av_nb_sales_per_week = mean(nb_sales_per_week)),by = list(brand, sid, is_displayed)]
av_sales_per_store_normal = av_sales_per_store[is_displayed == 0]
setnames(av_sales_per_store_normal,"av_sales_per_week","av_sales_per_week_normal")
setnames(av_sales_per_store_normal,"av_nb_sales_per_week","av_nb_sales_per_week_normal")
av_sales_per_store_displayed = av_sales_per_store[is_displayed == 1]
setnames(av_sales_per_store_displayed,"av_sales_per_week","av_sales_per_week_displayed")
setnames(av_sales_per_store_displayed,"av_nb_sales_per_week","av_nb_sales_per_week_displayed")
setkey(av_sales_per_store_displayed, brand, sid)
setkey(av_sales_per_store_normal, brand, sid)
av_sales_per_store_comparison = av_sales_per_store_normal[av_sales_per_store_displayed, nomatch = 0]
av_sales_per_store_comparison[, percent_diff_sales_per_week := ((av_sales_per_week_displayed - av_sales_per_week_normal)/av_sales_per_week_normal)*100]
av_sales_per_store_comparison[, percent_diff_nb_sales_per_week := ((av_nb_sales_per_week_displayed - av_nb_sales_per_week_normal)/av_nb_sales_per_week_normal)*100]
#Comparison in volume
av_sales_per_store_comparison[,mean(percent_diff_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_sales_per_week =mean(percent_diff_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity") + labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")
#Comparison in number
#comparison in number
av_sales_per_store_comparison[,mean(percent_diff_nb_sales_per_week, na.rm = TRUE),by = list(brand)]
ggplot(av_sales_per_store_comparison[,.(av_percent_diff_nb_sales_per_week =mean(percent_diff_nb_sales_per_week, na.rm = TRUE)),by = list(brand)], aes(as.factor(brand), av_percent_diff_nb_sales_per_week, fill=as.factor(brand))) + geom_bar(position="dodge", stat="identity")+ labs(fill="Brand code", x = "Brand code", y="Average sales difference (in %)")

# ggplot(sales[,mean(vol_sales_per_week),by = list(brand, is_displayed)], aes(as.factor(is_displayed), V1, fill=as.factor(brand))) + 
#   geom_bar(position="dodge", stat="identity")
# sales[,mean(vol_sales_per_week),by = list(brand, is_displayed)]


#Brand loyalty : compute % of brand change per hh
setkey(purch, id, date) # important for ordering
purch[,last_purch_brand:=shift(brand, 1), by=id]
purch[,changed_brand:=ifelse(last_purch_brand == brand, 0, 1), by=id]
loyalty = purch[, .(percentage_of_brand_changes = mean(changed_brand, na.rm = TRUE)), by = id]
summary(loyalty[,percentage_of_brand_changes])
#The median is 25% : 50% of the hh change brands less than 25% of the times they purchase something => strong inertia
#Check which kind of hh (size, income) are the most loyal to their brand (if there is a relationship)
setkey(hhchar, hh.id)
setkey(loyalty, id)
loyalty = hhchar[loyalty, nomatch = 0]
cor.test(loyalty$hhsize, loyalty$percentage_of_brand_changes) #No significant corr with size
cor.test(loyalty$hh.income, loyalty$percentage_of_brand_changes) #No significant corr with income
qplot(x = hhsize, y =percentage_of_brand_changes, data = loyalty, geom = 'jitter' )
qplot(x = hh.income, y =percentage_of_brand_changes, data = loyalty, geom = 'jitter' )
#Relationship between Brand choice and hh size / income ?
setkey(purch, id)
setkey(hhchar,hh.id)
hhchar_purch = purch[hhchar, nomatch = 0]
boxplot(hh.income~brandname,data=hhchar_purch, main="Brand choice and Household Income", 
        xlab="Brand", ylab="Household income") #Tide and Wisk seem to attract wealthier households
boxplot(hhsize~brandname,data=hhchar_purch, main="Brand choice and Household Size", 
        xlab="Brand", ylab="Household size") #No obvious relationship

### Interpurchase timing
#Create date of last purchase column
setkey(purch, id, date) # important for ordering
purch[,last_purch_date:=shift(date, 1), by=id]
purch[,interpurchase_time:=date-last_purch_date]
qplot(x=interpurchase_time, data=purch, geom="histogram", bins = 50)
summary(purch[,interpurchase_time])#Median = 36
#Compute average interpurchase timing per hh
av_timing_per_hh = purch[,.(Mean.interpurchase_time = mean(interpurchase_time, na.rm = TRUE)),by='id']
setkey(hhchar,hh.id)
setkey(av_timing_per_hh,id)
hhchar = hhchar[av_timing_per_hh, nomatch=0]
#Relationship with the size of the hh (/!\ Jittered scatter plot)
qplot(x=hhsize, y =Mean.interpurchase_time , data=hhchar, geom="jitter", color = hhsize)
cor.test(hhchar$hhsize, hhchar$Mean.interpurchase_time)
#Relationship with the income of the hh (/!\ Jittered scatter plot)
qplot(x=hh.income, y =Mean.interpurchase_time , data=hhchar, geom="jitter", color = hh.income)
cor.test(hhchar$hh.income, hhchar$Mean.interpurchase_time)
#Strange result : wealthier households buy detergent more often... actually there is a quite strong correlation between income and hh size !
cor.test(hhchar$hhsize, hhchar$hh.income)
qplot(x=hh.income, y =hhchar$hhsize , data=hhchar, geom="jitter")
#Relationship with purchased volume
setkey(purch, id, date) # important for ordering
purch[,last_purch_vol:=shift(vol, 1), by=id]
qplot(x=last_purch_vol, y =interpurchase_time , data=purch, geom="point")
cor.test(purch$last_purch_vol, purch$interpurchase_time)



##################### 1. Cross-sectional multinomial logit #####################
library(mlogit)

#State dependence variables
setkey(purch, id, date) # important for ordering
purch[,last_purch_brand:=shift(brand, 1), by=id]

#Create a data.table with the whole purchase history merged with the weighted price and the weighted display
setkey(purch, id)
setkey(weights, id)
purch_weights = purch[weights, all=TRUE]
#Check : the sum of the weights should be equal to 1 for each purchase of each hh
summary(purch_weights[,.(sum.weights = sum(weight)) ,by =list(id, date)][,sum.weights])
#Pb : it isn't even the case in the weights data.table...!!!! Due to approximation
summary(weights[,.(sum.weights = sum(weight)),by = id][,sum.weights])

data_logit <-  mlogit.data(purch_weights, shape = "wide", choice = "brand",id.var = 'id')
head(data_logit)
setDT(data_logit)
#Verif
data_logit[,.N, by = list(id,date, sid)][N!=6]
data_logit[,.(sum.weights = sum(weight)),by = list(id,date)][sum.weights < 6*0.99]

setkey(data_logit, alt, sid, week)
prices <- prices[, brand:=as.character(brand)]
setkey(prices, brand, sid, week)

seuil = 0.7
#Compute the weighted average of price/display, if we do not have enought weight put a -1
getweighted <- function(price,weight,seuil) {
  n=length(weight)
  sum = 0
  for(i in 1: n) sum = sum + weight[i]
  if(sum < seuil) return(-1)
  avg = 0
  for(i in 1: n) avg = avg + weight[i]*price[i]
  return(avg/sum)
}
#Compute the number of valid price/display
check = function(price){
  n=0
  for(i in 1:length(price)) if(price[i] != -1) n=n+1
  return(n)
}
#Merge Data_logit and prices
data_logit_price = data_logit[prices, nomatch = 0 ]#Removed the rows corresponding to stores where the purchased brand was not available
#Verif
#data_logit_price[,.N, by = list(id,date, sid)][N!=6]
data_logit_price = data_logit_price[order(id,date)]
#Compute the weighted_price and weighted_display
data_logit_price[,weighted_price :=getweighted(price,weight,seuil), by =list(id,date, vol, alt, brand, week, last_purch_brand)]
data_logit_price[,weighted_display :=getweighted(display,weight,seuil), by =list(id,date, vol, alt, brand, week, last_purch_brand)]
#Remove the household if they do not have access all the time to all product
data_logit_mkt_mix = data_logit_price[,.(weighted_price = mean(weighted_price),weighted_display = mean(weighted_display)),by =list(id,date, vol, alt, brand, week, last_purch_brand)]
hh_to_remove = data_logit_mkt_mix[,N:=check(weighted_price), by = list(id,date)][N!=6]
data_logit_mkt_mix = data_logit_mkt_mix [!(id %in% hh_to_remove$id)]
data_logit_mkt_mix[,N:=NULL]




# data_logit_price = data_logit[prices, nomatch = 0 ]#Removed the rows corresponding to stores where the purchased brand was not available
# data_logit_price = data_logit_price[order(id,date)] 
# 
# #We need to remove the households concerned with this issue 
# hh_to_remove = data_logit_price[,.N, by = list(id,date, sid)][N!=6]
# data_logit_price = data_logit_price[!(id %in% hh_to_remove$id)]
# #Compute weighted price and weighted display
# data_logit_price[,weighted_price := weight*price]
# data_logit_price[,weighted_display := weight*display]
# data_logit_mkt_mix = data_logit_price[,.(weighted_price = sum(weighted_price), weighted_display = sum(weighted_display)), by = list(id,date, vol, alt, brand, week)]
# data_logit_mkt_mix

#Add the household characteristics
setkey(hhchar, hh.id)
setkey(data_logit_mkt_mix, id)
data_logit_mkt_mix_and_hhchar = data_logit_mkt_mix[hhchar, nomatch = 0]

train_data = data_logit_mkt_mix_and_hhchar[insample == 1]
test_data = data_logit_mkt_mix_and_hhchar[insample == 0]

# A - Marketing Mix variables and household characteristics + No unobserved heterogeneity
library(mlogit)
#NB: By default, an intercept is added to the model,  it can be removed by using +0 or -1 in the second part.
dataA_train <- mlogit.data(train_data, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
dataA_test <- mlogit.data(test_data, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
head(dataA_train)
head(dataA_test)
modelA <- mlogit(brand ~ weighted_price + weighted_display  | hh.income + hhsize | 0, dataA_train)
summary(modelA)
modelA$freq
AIC(modelA)

#Average marginal effects
dim(effects(modelA, covariate = 'hhsize', data = dataA_train))
#summary(effects(modelA, covariate = 'hhsize', data = dataA_train)) #same as type = 'aa' : absolute absolute marginal effects
summary(effects(modelA, covariate = 'hhsize', type= 'aa', data = dataA_train))

summary(effects(modelA, covariate = 'hh.income', type= 'aa', data = dataA_train))


#Predictions
predictionsA <- predict(modelA,newdata=dataA_test)
predictionsA <- as.data.frame(predictionsA)
setDT(predictionsA)
predictionsA[, chid := unique(index(dataA_test)$chid),]
#Find the predicted brand
predictionsA[, y_predict := which.max(.SD), by = chid]
summary(as.factor(predictionsA$y_predict))

chid_val = index(dataA_test)$chid
setDT(dataA_test)
dataA_test[, chid := chid_val,]
head(dataA_test)
y_actual =dataA_test[brand==TRUE]$alt
summary(y_actual)

#https://spark.apache.org/docs/latest/mllib-evaluation-metrics.html#multiclass-classification
#https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#Confusion matrix
require(caret)
confusionMatrix( data = predictionsA$y_predict, reference = y_actual)


# B - Only Marketing Mix variables + No unobserved heterogeneity (but with intercept alpha j as in the article)
modelB <- mlogit(brand ~ weighted_price + weighted_display  | 1 | 0, dataA_train)
summary(modelB)#On a une likelihood et un R2 de McFadden un peu plus faibles
AIC(modelB)#AIC plus élevé


#Predictions
dataB_test <- mlogit.data(test_data, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
predictionsB <- predict(modelB,newdata=dataB_test)
predictionsB <- as.data.frame(predictionsB)
setDT(predictionsB)
predictionsB[, chid := unique(index(dataB_test)$chid),]
#Find the predicted brand
predictionsB[, y_predict := which.max(.SD), by = chid]
summary(as.factor(predictionsB$y_predict))

chid_val = index(dataB_test)$chid
setDT(dataB_test)
dataB_test[, chid := chid_val,]
head(dataB_test)
y_actual =dataB_test[brand==TRUE]$alt
summary(y_actual)

#https://spark.apache.org/docs/latest/mllib-evaluation-metrics.html#multiclass-classification
#https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#Confusion matrix
require(caret)
confusionMatrix( data = predictionsB$y_predict, reference = y_actual) #On a une accuracy un peu plus faible

#C- Marketing Mix variables and household characteristics + No unobserved heterogeneity + state-dependence
#We add a lagged brand choice dummy capturing state-dependence as in the article
#NB : on peut aussi ajouter comme dans le cours sur le panel dynamique plusieurs dummmies pour capturer la transition d'un état vers un autre
#Car en efet il est peut-être facile de passer d'une certaine marqueà une autre si elles se ressemblent
head(data_logit_mkt_mix_and_hhchar)
#Add dummy indicating if the h chose this alternative last time or not
data_logit_mkt_mix_and_hhchar[,chose_this_brand_last_time := ifelse(last_purch_brand == alt, 1, 0),]
head(data_logit_mkt_mix_and_hhchar)
#NB: NA when it is the first purchase of the hh => these observations will be automatically removed when performing the regression
train_data_lagged = data_logit_mkt_mix_and_hhchar[insample == 1]
test_data_lagged = data_logit_mkt_mix_and_hhchar[insample == 0]

dataC_train <- mlogit.data(train_data_lagged, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
dataC_test <- mlogit.data(test_data_lagged, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
head(dataC_train)
head(dataC_test)
modelC <- mlogit(brand ~ weighted_price + weighted_display + chose_this_brand_last_time  | hh.income + hhsize | 0, dataC_train)
summary(modelC)#Le R2 augmente énormément tout comme la likelihood
modelC$freq
AIC(modelC)#l'AIC baisse énormément (3300 contre 5600 dans le modèle A)

summary(effects(modelC, covariate = 'hh.income', type= 'aa', data = dataC_train))
summary(effects(modelC, covariate = 'hhsize', type= 'aa', data = dataC_train))


#Predictions
predictionsC <- predict(modelC,newdata=dataC_test)
predictionsC <- as.data.frame(predictionsC)
setDT(predictionsC)
predictionsC[, chid := unique(index(dataC_test)[complete.cases(dataC_test$chose_this_brand_last_time),]$chid),]
#Find the predicted brand
predictionsC[, y_predict := which.max(.SD), by = chid]
summary(as.factor(predictionsC$y_predict)) #Predictions are more balanced than in the previous model : we predict brands 1 and 2 and more brand 6
chid_val = index(dataC_test)$chid
setDT(dataC_test)
dataC_test[, chid := chid_val,]
head(dataC_test)
y_actual =dataC_test[complete.cases(dataC_test$chose_this_brand_last_time),][brand==TRUE]$alt
summary(y_actual)
#https://spark.apache.org/docs/latest/mllib-evaluation-metrics.html#multiclass-classification
#https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#Confusion matrix
require(caret)
confusionMatrix( data = predictionsC$y_predict, reference = y_actual) #Accuracy 68% (/!\ only on the data where we had access to a purchase history)

#Question : est-ce qu'on ajoute la prédiction sur les premiers achats ? (en fixant 0 partout par exemple sur les dummy)
dataC_test2 <- mlogit.data(test_data_lagged, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
head(dataC_train)
dataC_test2[is.na(dataC_test2)] <- 0
head(dataC_test2)
predictionsC2 <- predict(modelC,newdata=dataC_test2)
predictionsC2 <- as.data.frame(predictionsC2)
setDT(predictionsC2)
predictionsC2[, chid := unique(index(dataC_test2)$chid),]
#Find the predicted brand
predictionsC2[, y_predict := which.max(.SD), by = chid]
summary(as.factor(predictionsC2$y_predict)) #Predictions are more balanced than in the previous model : we predict brands 1 and 2 and more brand 6
chid_val = index(dataC_test2)$chid
setDT(dataC_test2)
dataC_test2[, chid := chid_val,]
head(dataC_test2)
y_actual =dataC_test2[brand==TRUE]$alt
summary(y_actual)
#Confusion matrix
confusionMatrix( data = predictionsC2$y_predict, reference = y_actual) #Accuracy 65% : remains far better than model A and B

#Model D - Latent class polytomous regression with lagged dummy
library(gmnl)
# The formula is divided in ???ve parts, each of them separated by the symbol |. The ???rst part is reserved for alternative-speci???c variables with a generic coef???cient. The second part corresponds to individual-speci???c variables with an alternative speci???c coef???cients. The third part corresponds to alternative-speci???c variables with an alternativespeci???ccoef???cident. Thefourthpartisreservedfortime-invariantvariablesthat modifythemeanoftherandomparameters. Finally,the???fthpartisreservedfor time-invariant variables that enter in the scale coef???cient or in the probability assignment in models with latent classes. 

dataD_train <- mlogit.data(train_data_lagged, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
dataD_test <- mlogit.data(test_data_lagged, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')

#3 segments
modelD_3 <- gmnl(brand ~ weighted_price + weighted_display + chose_this_brand_last_time | 1 | 0 | 0 | hh.income + hhsize, data = dataD_train, model = 'lc', panel = TRUE, Q = 3) 
summary(modelD_3)
AIC(modelD_3)

#Predictions
#No predict method in the gmnl package ! We need to code it using the estimated coefficients...

#Si on ajoute la pred sur les premiers achats
dataD_test2 =dataD_test
dataD_test2[is.na(dataD_test2)] <- 0

#Unconditional class probabilities
setDT(dataD_test)
test_data_D = dataD_test[brand == TRUE]
test_data_D_class_pred = test_data_D[,c("id","hh.income","hhsize"),]
intercept_class_coeffs = as.matrix(c(0, coefficients(modelD_3)[25], coefficients(modelD_3)[26]))
hh.income_class_coeffs = as.matrix(c(0, coefficients(modelD_3)[27], coefficients(modelD_3)[28]))
hhsize_class_coeffs = as.matrix(c(0, coefficients(modelD_3)[29], coefficients(modelD_3)[30]))

unconditional_class_prob_numerator = intercept_class_coeffs %*% rep(1, dim(test_data_D_class_pred)[1]) + hh.income_class_coeffs %*% test_data_D_class_pred$hh.income + hhsize_class_coeffs %*%  test_data_D_class_pred$hhsize
unconditional_class_prob_numerator = exp(unconditional_class_prob_numerator)

unconditional_class_prob = sweep(unconditional_class_prob_numerator,2,colSums(unconditional_class_prob_numerator),`/`)


#Conditional choice probabilities
coeffs_class_1_intercepts = as.matrix(c(0,coefficients(modelD_3)[1:5]))
coeffs_class_1_var = as.matrix(coefficients(modelD_3)[6:8])

coeffs_class_2_intercepts = as.matrix(c(0,coefficients(modelD_3)[9:13]))
coeffs_class_2_var = as.matrix(coefficients(modelD_3)[14:16])

coeffs_class_3_intercepts = as.matrix(c(0,coefficients(modelD_3)[17:21]))
coeffs_class_3_var = as.matrix(coefficients(modelD_3)[22:24])

conditional_class_probabilities <- function(dataD_test2, coeffs_class_1_intercepts, coeffs_class_1_var){
  alts <- unique(dataD_test2$alt)
  UTILITY <- vector("numeric", length=nrow(dataD_test2))
  conditional_choice_prob_numerator <- vector("numeric", length=nrow(dataD_test2))
  conditional_choice_prob <- vector("numeric", length=nrow(dataD_test2))
  for(j in alts){
    UTILITY[dataD_test2$alt== j] <-coeffs_class_1_intercepts[as.numeric(j)] + t(coeffs_class_1_var) %*% t(as.matrix(dataD_test2[dataD_test2$alt== j, c("weighted_price", "weighted_display", "chose_this_brand_last_time")]))
    conditional_choice_prob_numerator[dataD_test2$alt== j] <- exp(UTILITY[dataD_test2$alt== j])
  }
  conditional_choice_prob_matrix_numerator = conditional_choice_prob_numerator[dataD_test2$alt== alts[1]]
  for (j in alts[2:length(alts)]){
    conditional_choice_prob_matrix_numerator = cbind(conditional_choice_prob_matrix_numerator, conditional_choice_prob_numerator[dataD_test2$alt== j])
  }
  conditional_choice_prob_matrix = sweep(t(conditional_choice_prob_matrix_numerator) ,2,colSums(t(conditional_choice_prob_matrix_numerator) ),`/`)
  return(t(conditional_choice_prob_matrix))
}

dim(conditional_class_probabilities(dataD_test2, coeffs_class_1_intercepts, coeffs_class_1_var))
dim(unconditional_class_prob)

unconditional_choice_prob_class1 = unconditional_class_prob[1,] * conditional_class_probabilities(dataD_test2, coeffs_class_1_intercepts, coeffs_class_1_var)
unconditional_choice_prob_class2 = unconditional_class_prob[2,] * conditional_class_probabilities(dataD_test2, coeffs_class_2_intercepts, coeffs_class_2_var)
unconditional_choice_prob_class3 = unconditional_class_prob[3,] * conditional_class_probabilities(dataD_test2, coeffs_class_3_intercepts, coeffs_class_3_var)

unconditional_choice_prob = unconditional_choice_prob_class1 + unconditional_choice_prob_class2 + unconditional_choice_prob_class3
#Verif
# dim(unconditional_choice_prob)
# rowSums(unconditional_choice_prob)


predictionsD_3 <- as.data.frame(unconditional_choice_prob)
setDT(predictionsD_3 )
predictionsD_3[, chid := unique(index(dataD_test2)$chid),]
#Find the predicted brand
predictionsD_3[, y_predict := which.max(.SD), by = chid]
summary(as.factor(predictionsD_3$y_predict)) 
chid_val = index(dataD_test2)$chid
setDT(dataD_test2)
dataD_test2[, chid := chid_val,]
head(dataD_test2)
y_actual =dataD_test2[brand==TRUE]$alt
summary(y_actual)
#Confusion matrix
confusionMatrix( data = predictionsD_3$y_predict, reference = y_actual) #Accuracy 65% : remains far better than model A and B



#2 segments
modelD_2<- gmnl(brand ~ weighted_price + weighted_display + chose_this_brand_last_time | 1 | 0 | 0 | hh.income + hhsize, data = dataD_train, model = 'lc', panel = TRUE, Q = 2) 
summary(modelD_2)
AIC(modelD_2)

#4 segments : singularity issue
modelD_4<- gmnl(brand ~ weighted_price + weighted_display + chose_this_brand_last_time | 1 | 0 | 0 | hh.income + hhsize, data = dataD_train, model = 'lc', panel = TRUE, Q = 4) 
summary(modelD_4)
AIC(modelD_4)

#Model E - Latent class polytomous regression without lagged dummy
dataE_train <- mlogit.data(train_data, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')
dataE_test <- mlogit.data(test_data, shape = "long", choice = "brand", id.var = 'id', alt.var= 'alt')

modelE_3 <- gmnl(brand ~ weighted_price + weighted_display | 1 | 0 | 0 | hh.income + hhsize, data = dataE_train, model = 'lc', panel = TRUE, Q = 3) 
summary(modelE_3)#ISSUE
AIC(modelE_3)

modelE_2 <- gmnl(brand ~ weighted_price + weighted_display | 1 | 0 | 0 | hh.income + hhsize, data = dataE_train, model = 'lc', panel = TRUE, Q = 2) 
summary(modelE_2)#better than the 2 models without the lagged dummy which did not take unobserved heterogeneity into account (models A and B)
AIC(modelE_2)#same comment :4845.6 instead of 5676.027 in model A
