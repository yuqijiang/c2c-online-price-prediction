#################################
##                             ##
##        Final Project        ##
##                             ##
#################################
options(java.parameters = "-Xmx64048m") #64048 is 64 GB
memory.limit(size=10000000000024)
library(ggplot2)
library(dplyr)
library(caTools)
library(stringr)
library(xgboost)
library(quanteda)
library(SnowballC)
library(tm)
library(gridExtra)
library(corrplot)
library(caret)
library(cluster)
library(data.table)
library(quanteda)
library(readr)
getwd()
setwd("C:/Users/mckenney/OneDrive/Fall 2017/Data Mining/Final Project/train")
tr <- read_tsv(file="train.tsv")
setwd("C:/Users/mckenney/OneDrive/Fall 2017/Data Mining/Final Project/test")
names(tr)[1] <- paste("train_id")
tr$train_id <- NULL

tr$name <- as.character(tr$name)
tr$category_name <- as.character(tr$category_name)
tr$brand_name <- as.character(tr$brand_name)
tr$item_description <- as.character(tr$item_description)

tr$name <- tolower(tr$name)
tr$brand_name <- tolower(tr$brand_name)
tr$brand_name[str_detect(tr$name,'nike')] = 'nike'
tr$brand_name[str_detect(tr$name,'pink')] = 'pink'
tr$brand_name[str_detect(tr$name,"victoria's secret")] = "victoria's secret"
tr$brand_name[str_detect(tr$name,'lularo')] = 'lularoe'
tr$brand_name[str_detect(tr$name,'apple')] = 'apple'
tr$brand_name[str_detect(tr$name,'forever21')] = 'forever21'
tr$brand_name[str_detect(tr$name,'nintendo')] = 'nintendo'
tr$brand_name[str_detect(tr$name,'lululemon')] = 'lululemon'
tr$brand_name[str_detect(tr$name,'michael kors')] = 'michael kors'
tr$brand_name[str_detect(tr$name,'american eagle')] = 'american eagle'



#transforming price
par(mfrow=c(1,2))
ggplot(data=tr, aes(tr$price)) + 
  geom_histogram(aes(y=..density..),fill="dark blue",alpha=.5)+ 
  labs(title="Price") +
  labs(x="Price", y="Count")+geom_density(adjust=3,col=2)

tr$price <- log(tr$price+1)

theme_set(theme_gray(base_size = 17))
ggplot(data=tr, aes(tr$price)) + 
  geom_histogram(aes(y=..density..),fill="dark green",alpha=.5)+ 
  xlab('Term')+xlim(1,6)+
  labs(x="Log Price", y="Count")+geom_density(adjust=3,col=2)

summary(tr$price)
tr <- tr[complete.cases(tr), ]


####################################
##                                ##
##           Category             ##
##                                ##
####################################
#category seems to have 3 parts seperated by /
#i am seperating them here
splitVar <- str_split(tr$category_name, "/")
cat1 <- sapply(splitVar,'[',1)
cat1te <- sapply(splitVarte,'[',1)
cat2 <- sapply(splitVar,'[',2)
cat2te <- sapply(splitVarte,'[',2)
cat3 <- sapply(splitVar,'[',3)
cat3te <- sapply(splitVarte,'[',3)

tr$cat1 <- cat1
tr$cat2 <- cat2
tr$cat3 <- cat3
tr$cat1[is.na(tr$cat1)] <- -1
tr$cat2[is.na(tr$cat2)] <- -1
tr$cat3[is.na(tr$cat3)] <- -1


sort(summary(tr$cat1), decreasing=T)
cat2_top20 <- head(sort(summary(tr$cat2), decreasing=T), 20)
cat2_top20
cat3_top20 <- head(sort(summary(tr$cat3), decreasing=T), 20)
cat3_top20

tr$item_description <- tolower(tr$item_description)
#####################################
##                                 ##
##   Dummy features for desc/name  ##
##                                 ##
#####################################
tr$box <- (str_detect(tr$item_description, 'box'))*1


#item_description only using 2 gram for right now
#will add 3 gram in future
tr$comes_with <- (str_detect(tr$item_description, 'comes with'))*1
tr$in_box <- (str_detect(tr$item_description, 'in box'))*1
tr$like_new <- (str_detect(tr$item_description, 'like new'))*1
tr$for_rm <- (str_detect(tr$item_description, 'for rm'))*1
tr$to_save <- (str_detect(tr$item_description, 'to save'))*1
tr$free_shipping <- (str_detect(tr$item_description, 'free shipping'))*1
#3gram
tr$in_excellent_condition <- (str_detect(tr$item_description, 'in excellent condition'))*1
tr$new_with_tags <- (str_detect(tr$item_description, 'new with tags'))*1
tr$in_perfect_condition <- (str_detect(tr$item_description, 'in perfect condition'))*1
tr$new_never_worn <- (str_detect(tr$item_description, 'new never worn'))*1
tr$bundle_and_save <- (str_detect(tr$item_description, 'bundle and save'))*1
tr$save_on_shipping <- (str_detect(tr$item_description, 'save on shipping'))*1


#name only using 1 gram for right now
#will add 2 gram in future
tr$leggings <- (str_detect(tr$name, 'leggings'))*1
tr$boots <- (str_detect(tr$name, 'boots'))*1
tr$jacket <- (str_detect(tr$name, 'jacket'))*1
tr$case <- (str_detect(tr$name, 'case'))*1
tr$shirt <- (str_detect(tr$name, 'shirt'))*1
tr$baby <- (str_detect(tr$name, 'baby'))*1
tr$shorts <- (str_detect(tr$name, 'shorts'))*1
tr$brand_name[str_detect(tr$name,'lularo')] = 'Lularoe'
#2 gram
tr$tank_top <- (str_detect(tr$name, 'tank top'))*1
tr$free_shipping <- (str_detect(tr$name, 'free shipping'))*1
tr$free_ship <- (str_detect(tr$name, 'free ship'))*1
tr$body_works <- (str_detect(tr$name, 'body works'))*1
tr$iphone_plus <- (str_detect(tr$name, 'iphone plus'))*1

#
tr$oakley_watches <- ifelse(tr$brand_name=="oakley"&tr$cat3=="Watches",1,0)
tr$saint_laurent_shoulder_bag <- ifelse(tr$brand_name=="saint laurent"&tr$cat3=="Shoulder Bag",1,0)
tr$mcm_totes_shoppers <- ifelse(tr$brand_name=="mcm"&tr$cat3=="Totes & Shoppers",1,0)
tr$valentino_boots <- ifelse(tr$brand_name=="valentino"&tr$cat3=="Boots",1,0)
tr$valentino_totes_shoppers <- ifelse(tr$brand_name=="valentino"&tr$cat3=="Totes & Shoppers",1,0)
tr$rolex_watch <- ifelse(tr$brand_name=="rolex"&tr$cat3=="Watch",1,0)
tr$celine_handbag <- ifelse(tr$brand_name=="celine"&tr$cat3=="Handbag",1,0)
tr$celine_messengers_crossbody <- ifelse(tr$brand_name=="celine"&tr$cat3=="Messenger & Crossbody
                                         ",1,0)
tr$mcmworldwide_totes_shoppers <- ifelse(tr$brand_name=="mcm worldwide"&tr$cat3=="Totes & Shoppers",1,0)
tr$chloe_wallets <- ifelse(tr$brand_name=="chloe"&tr$cat3=="Wallets",1,0)
tr$amd_desktops_allinone <- ifelse(tr$brand_name=="amd"&tr$cat3=="Desktops & All-In-Ones
                                   ",1,0)
tr$alexanderwang_totes_shoppers <- ifelse(tr$brand_name=="alexander wang"&tr$cat3=="Totes & Shoppers",1,0)



########################################
##                                    ##
##             XGBoost                ##
##                                    ##
########################################
set.seed(1234)
str(tr)
tr$item_description <- NULL
tr$name <- NULL

funTime <- tr

features <- names(funTime)
for(f in features){
  if(class(funTime[[f]])=="character"){
    levels=sort(unique(funTime[[f]]))
    funTime[[f]]=as.integer(factor(funTime[[f]],levels = levels))}
}

description_dtm <- as.data.frame(description_dtm)

trainIndex <- createDataPartition(funTime$price # target variable vector
                                  , p = 0.80    # % of data for training
                                  , times = 1   # Num of partitions to create
                                  , list = F    # should result be a list (T/F)
)
train <- funTime[trainIndex,]
test <- funTime[-trainIndex,]
yTrainXG <- train$price
yTestXG <- test$price
trainXG <- train %>% select(-price)
testXG <- test %>% select(-price)


trainXG[] <- lapply(trainXG,as.numeric)
testXG[] <- lapply(testXG, as.numeric)
xgTrain <- xgb.DMatrix(as.matrix(trainXG),label=yTrainXG)
xgTest <- xgb.DMatrix(as.matrix(testXG), label=yTestXG)

xgPrm_tree <- list(boost='gbtree',objective='reg:linear',colsample_bytree=1,
                   eta=0.12,max_depth=9,min_child_weight=1,alpha=0.3,
                   lambda=0.4,gamma=0.2,subsample=0.8,seed=5,silent=TRUE)
xgPrm_linear <- list(boost='gblinear',objective='reg:linear',seed=5,silent=TRUE)
xgPrm_dart <- list(boost='dart.gbtree',objective='reg:linear',normalized_type='forest',sample_type='weighted',
                   seed=5,silent=TRUE)

xgbModel_tree <- xgb.train(xgPrm_tree,xgTrain,nrounds=275)
xgbModel_linear <- xgb.train(xgPrm_linear,xgTrain,nrounds=275)
xgbModel_dart <- xgb.train(xgPrm_dart,xgTrain,nrounds=275)


yhat_xg_tree <- predict(xgbModel_tree, newdata=xgTrain)
yhat_xg_linear <- predict(xgbModel_linear, newdata=xgTrain)
yhat_xg_dart <- predict(xgbModel_dart, newdata=xgTrain)

yhat_xgt_tree <- predict(xgbModel_tree, newdata=xgTest)
yhat_xgt_linear <- predict(xgbModel_linear, newdata=xgTest)
yhat_xgt_dart <- predict(xgbModel_dart, newdata=xgTest)



R2(pred=yhat_xg_tree, obs=yTrainXG)
R2(pred=yhat_xgt_tree, obs=yTestXG)
R2(pred=yhat_xg_linear, obs=yTrainXG)
R2(pred=yhat_xgt_linear, obs=yTestXG)
R2(pred=yhat_xg_dart, obs=yTrainXG)
R2(pred=yhat_xgt_dart, obs=yTestXG)


RMSE(pred=yhat_xg_tree, obs=yTrainXG)
RMSE(pred=yhat_xgt_tree, obs=yTestXG)
RMSE(pred=yhat_xg_linear, obs=yTrainXG)
RMSE(pred=yhat_xgt_linear, obs=yTestXG)
RMSE(pred=yhat_xg_dart, obs=yTrainXG)
RMSE(pred=yhat_xgt_dart, obs=yTestXG)

library(DiagrammeR)
names=names(trainXG)
xgb.plot.tree(feature_name=names, model=xgbModel_tree)

names = names(trainXG)
(importance_matrix <- xgb.importance(names, model = xgbModel_tree))
top8 <- importance_matrix[1:8,]
options(repr.plot.width=8, repr.plot.height=8)
ggplot(top8,aes(x=reorder(Feature,Gain),y=Gain))+
  geom_bar(stat='identity', color="black", fill="dark blue")+
  #scale_fill_manual(values=c('grey'),guide=FALSE) +
  coord_flip()+
  xlab('Features')+
  ylab('Importance')+
  ggtitle('Feature Importance')+
  theme(text = element_text(size=20))


names(prices)
prices <- exp(as.data.frame(yTrainXG))-1
yhat_xg_tree_normal <- exp(yhat_xg_tree)-1
bestmodel <- data.frame(Y=prices$yTrainXG, Yhat=yhat_xg_tree_normal)
bestmodel$error = bestmodel$Yhat - bestmodel$Y
summary(bestmodel$error)
bestmodel$error[bestmodel$error>=400] <- 400
bestmodel$error[bestmodel$error<=-400] <- -400

theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(bestmodel, aes(Yhat, Y)) + geom_count(colour="black", size=1)
#g <- g + geom_smooth(method="lm", se=T)
g <- g + theme(legend.position="none")
g <- g + labs(y="Actual Sales Price ($)", 
              x="Predicted Sales Price ($)", 
              title="Best Model Prediction")
g <- g + theme(plot.title = element_text(color="black", face="bold", size=14, hjust=0.5))
g <- g + theme(plot.subtitle = element_text(color="black", face="bold", size=11, hjust=0.5))
g <- g + theme(axis.text.x=element_text(colour="black"),
               axis.text.y=element_text(colour="black"))
g <- g + scale_x_continuous(labels = scales::dollar)
g <- g + scale_y_continuous(labels = scales::dollar)
g <- g + coord_cartesian(xlim=c(0, 500), ylim=c(0, 500)) 
g <- g + guides(fill=FALSE)
g <- g + geom_point(aes(colour = error)) + scale_colour_gradient2()
g <- g + geom_abline(intercept = 0, slope = 1)
g

summary(yhat_xg_tree_normal)




######################################
##                                  ##
##       Linear Regression          ##             
##                                  ##
######################################
setwd("C:/Users/mckenney/OneDrive/Fall 2017/Data Mining/Final Project/train")
tr <- read.delim(file="train.txt", header=T, sep="\t")
names(tr)[1] <- paste("train_id")
tr$train_id <- NULL
tr$item_condition_id <- as.factor(tr$item_condition_id)
tr$shipping <- as.factor(tr$shipping)
tr$name <- as.character(tr$name)
tr$category_name <- as.character(tr$category_name)
tr$brand_name <- as.character(tr$brand_name)
tr$item_description <- as.character(tr$item_description)
str(tr)
summary(tr)


#transforming price
hist(tr$price, breaks=1000)
tr$price <- log(tr$price+1)
hist(tr$price, breaks=1000)
summary(tr$price)
tr <- tr[complete.cases(tr), ]


####################################
##                                ##
##           Category             ##
##                                ##
####################################
#category seems to have 3 parts seperated by /
#i am seperating them here
splitVar <- str_split(tr$category_name, "/")
cat1 <- sapply(splitVar,'[',1)
cat2 <- sapply(splitVar,'[',2)
cat3 <- sapply(splitVar,'[',3)
tr$cat1 <- as.factor(cat1)
tr$cat2 <- as.factor(cat2)
tr$cat3 <- as.factor(cat3)
tr$cat1[is.na(tr$cat1)] <- -1
tr$cat2[is.na(tr$cat2)] <- -1
tr$cat3[is.na(tr$cat3)] <- -1
tr$category_name = NULL



str(tr)
sort(summary(tr$cat1), decreasing=T)
cat2_top20 <- head(sort(summary(tr$cat2), decreasing=T), 20)
cat2_top20
cat3_top20 <- head(sort(summary(tr$cat3), decreasing=T), 20)
cat3_top20

str(tr_dum)

#####################################
##                                 ##
##    Creating Dummy Variables     ##
##                                 ##
#####################################
tr_dum <- tr
dummies <- dummyVars(price ~ item_condition_id+shipping+cat1+(brand_name=="Tiffany & Co.")
                     +(brand_name=="Gucci")+(brand_name=="Tory Burch")+(brand_name=="Air Jordan")+
                       (brand_name=="Apple")+(brand_name=="Kendra Scott")+(brand_name=="Chanel")+
                       (brand_name=="Jordan")+(brand_name=="Beats")+(brand_name=="Beats by Dr. Dre")+
                       (brand_name=="Lululemon")+(brand_name=="Michael Kors")+(brand_name=="Kate Spade")+
                       (brand_name=="UGG Australia")+(brand_name=="Samsung")+(brand_name=="Fitbit")+
                       (brand_name=="Rock Revival")+(brand_name=="Ray-Ban")+(brand_name=="patagonia")+
                       (brand_name=="Dooney & Bourke")+(brand_name=="PUMA")+(brand_name=="Stamped")+
                       (brand_name=="Supreme")+(brand_name=="Louis Vuitton")+(brand_name=="David Yurman")+
                       (brand_name=="Saint Laurent")+(brand_name=="Celine")+(brand_name=="MCM Worldwide")+
                       (brand_name=="Stuart Weitzman")+(brand_name=="MICHELE")+(brand_name=="Alexander Wang")+
                       (brand_name=="Auto Meter")+(brand_name=="Demdaco")+(brand_name=="Christian Louboutin")+
                       (brand_name=="Christian Louboutin")+(brand_name=="Sherri Hill")+(brand_name=="Yeezy")+
                       (brand_name=="MCM")+(brand_name=="Valentino")+(brand_name=="Go Gear")+(brand_name=="Rolex")+
                       (brand_name=="Jovani")+(brand_name=="Balenciaga")+(brand_name=="Tieks")+(brand_name=="FOREVER 21")+
                       (brand_name=="American Eagle")+(brand_name=="Disney")+(brand_name=="Bath & Body Works")+
                       (brand_name=="Under Armour")+(brand_name=="Old Navy")+(brand_name=="Hollister")+(brand_name=="Cater's")+
                       (brand_name=="Brandy Melville")+(brand_name=="Gap")+(brand_name=="Charlotte Russe")+
                       (brand_name=="Ralph Lauren")+(brand_name=="H&M")+(brand_name=="Express")+
                       (brand_name=="Abrecrombie & Fitch")+(brand_name=="NYX")+(brand_name=="Hot Topic")+(brand_name=="Pokemon")+
                       (cat2=="Athletic Apparel")+(cat2=="Makeup")+(cat2=="Tops & Blouses")+(cat2=="Shoes")+
                       (cat2=="Jewelry")+(cat2=="Toys")+(cat2=="Cell Phones & Accessories")+(cat2=="Women's Handbags")+
                       (cat2=="Dresses")+(cat2=="Women's Accessories")+(cat2=="Jeans")+(cat2=="Video Games & Consoles")+
                       (cat2=="Sweaters")+(cat2=="Underwear")+(cat2=="Skin Care")+(cat2=="Home Décor")+(cat2=="Fragrance")+
                       (cat2=="Kitchen & Dining")+(cat2=="Kitchen & Dining")+(cat2=="Tops")+
                       (cat3=="Pants, Tights, Leggings")+(cat3=="Face")+(cat3=="T-Shirts")+(cat3=="Shoes")+(cat3=="Games")+
                       (cat3=="Lips")+(cat3=="Athletic")+(cat3=="Eyes")+(cat3=="Cases, Covers & Skins")+(cat3=="Shorts")+
                       (cat3=="Bras")+(cat3=="Tank, Cami")+(cat3=="Blouse")+(cat3=="Boots")+(cat3=="Necklaces")+(cat3=="Above Knee, Mini")+
                       (cat3=="Makeup Palettes")+(cat3=="Women")+
                       (item_description %like% "with tags")+(item_description %like% "new with")+(item_description %like% "comes with")+
                       (item_description %like% "in box")+(item_description %like% "like new")+(item_description %like% "for rm")+
                       (item_description %like% "to save")+(item_description %like% "free shipping")+
                       (name %like% "lularoe")+(name %like% "leggings")+(name %like% "boots")+(name %like% "nwt")+(name %like% "jacket")+
                       (name %like% "case")+(name %like% "top")+(name %like% "shirt")+(name %like% "iphone")+(name %like% "tank")+
                       (name %like% "baby")+(name %like% "shorts")
                     , data = tr_dum)

ex <- data.frame(predict(dummies, newdata = tr_dum))
names(ex) <- gsub("\\.", "", names(ex))
tr_dum <- cbind(tr_dum$price, ex)
names(tr_dum)[1] <- "price"
rm(dummies, ex)

names(tr_dum)

####################################
##                                ##
##        Cleaning data           ##
##                                ##
####################################
#remove false features
cols <- subset(names(tr_dum), names(tr_dum) %like% "FALSE")
tr_dum[cols] <- NULL
tr_dum$shipping0 <- NULL
tr_dum$item_condition_id1 <- NULL


#correlation analysis
#descrCor <-  cor(tr_dum[,2:ncol(tr_dum)])  
#highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .90) 
#summary(descrCor[upper.tri(descrCor)])
#highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.90)
#filteredDescr <- tr_dum[,2:ncol(tr_dum)][,-highlyCorDescr] 
#descrCor2 <- cor(filteredDescr) 
#summary(descrCor2[upper.tri(descrCor2)])
#tr_dum <- cbind(tr_dum$price, filteredDescr)
#names(tr_dum)[1] <- "price"
#rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr, nzv)



#Find if any linear combinations exist and which column combos they are
#comboInfo <- findLinearCombos(tr_dum)
#comboInfo
#tr_dum <- tr_dum[, -comboInfo$remove]
#rm(comboInfo)



#coerce features to factors
tr_dum$item_descriptionlikeforrmTRUE <- NULL
tr_dum$brand_nameAbrecrombieFitchTRUE <- NULL
tr_dum$brand_nameCatersTRUE <- NULL
tr_dum$brand_nameAutoMeterTRUE <- NULL
tr_dum$cat1 <- NULL
tr_dum$cat1Other <- NULL

tr_dum_nn <- tr_dum
tr_dum[,2:ncol(tr_dum)] <- lapply(tr_dum[,2:ncol(tr_dum)], factor)
str(tr_dum)


#search for features with only 1 factor level and remove
## remove incomplete cases
dat <- na.omit(tr_dum)
## extract factor columns and drop redundant levels
fctr <- lapply(dat[sapply(dat, is.factor)], droplevels)
## count levels
sapply(fctr, nlevels)



######################################
##                                  ##
##            Modeling              ##
##                                  ##
######################################
summary(tr_dum$price)
set.seed(1234)
trainIndex <- createDataPartition(tr_dum$price # target variable vector
                                  , p = 0.80    # % of data for training
                                  , times = 1   # Num of partitions to create
                                  , list = F    # should result be a list (T/F)
)
train1 <- na.omit(tr_dum[trainIndex,])
test1 <- na.omit(tr_dum[-trainIndex,])

#linear regression
str(train)
lm1 <- lm(price~., data=train1)
summary(lm1)

yhat_lm1_tr <- predict(lm1, newdata=train1); plot(train1$price, yhat_lm1_tr)
yhat_lm1_te <- predict(lm1, newdata=test1); plot(test1$price, yhat_lm1_te)

RMSE(pred=yhat_lm1_tr, obs=train$price)
RMSE(pred=yhat_lm1_te, obs=test$price)