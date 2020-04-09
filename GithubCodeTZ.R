#clear environment
rm(list = ls(all.names = TRUE))

###Libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidystringdist)
library(plotly)
library(maps)
library(choroplethr)
library(choroplethrMaps)
library(usmap)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(pROC)
library(lattice)
library(plyr)
library(e1071)
library(glmnet)
library(tidyr)
library(broom)
library(jtools)
library(ggstance)
library(glmnet)
#library(readr)
#library(repr)

########################INITIAL DATA CLEANING########################
##read in data:
auc_sales <- read.csv("C:/Users/McKenna/Desktop/STAT 190/TZ_Source of Truth Data Dump_12.31.19 Extract.csv")
auc_interest <- read.csv("C:/Users/McKenna/Desktop/STAT 190/TZ_EventData.csv")
auc_search <- read.csv("C:/Users/McKenna/Desktop/STAT 190/TZ_User Searches By Date_All 2019.csv")

##fix date variables:
auc_sales$Auction.Complete.Date <-as.Date(auc_sales$Auction.Complete.Date, format = "%m/%d/%Y")
auc_sales$Auction.Create.Date <-as.Date(auc_sales$Auction.Create.Date, format = "%m/%d/%Y")
auc_sales$Auction.Date <-as.Date(auc_sales$Auction.Date, format = "%m/%d/%Y")
auc_interest$date <- as.Date(auc_interest$date, format = "%m/%d/%Y")

##aggregate to month level
auc_sales$month <- round_date(auc_sales$Auction.Create.Date, unit = "month")

##add variable for days on Tractor Zoom
auc_sales$days.on.TZ <- auc_sales$Auction.Date- auc_sales$Auction.Create.Date


##Adding geographic region groupings
auc_sales$geo_region <- NA
auc_sales$geo_region[auc_sales$Auction.State %in% c("WA", "OR", "CA", "NV", "AZ", "ID", "MT", "WY", "CO", "NM", "UT")]<- "west"
auc_sales$geo_region[auc_sales$Auction.State %in% c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY", "GA", "FL", "SC", "NC", "VA", "WV")]<- "south"
auc_sales$geo_region[auc_sales$Auction.State %in% c("KS", "NE", "SD", "ND", "MN", "MO", "IA", "IL", "IN", "MI", "WI", "OH")]<- "midwest"
auc_sales$geo_region[auc_sales$Auction.State %in% c("aME", "NH", "NY", "MA", "RI", "VT", "PA", "NJ", "CT", "DE", "MD", "DC")]<- "northeast"

#Removing test auctioneers
auc_sales <- subset(auc_sales, auc_sales$Auctioner.is.test.If.1 ==0)

#Removing negative days on tractor zoom 
auc_sales <- subset(auc_sales, auc_sales$days.on.TZ >= 0)

#Removing missing price values
auc_sales <- subset(auc_sales, auc_sales$Lot.Sale.Price != "NULL")

auc_sales <- subset(auc_sales, auc_sales$Lot.Sale.Price > 1)

#############################################################

########################EXPLORATORY ANALYSIS########################
searches <- subset(auc_search)
head(searches)

#format the search dates like we did earlier for sales and interest
searches$Date <-as.Date( searches$Date, format = "%b %d '%y")
summary(searches)



#get rid of all the numbers to better standardize the data

gsub("[0-9]", "", (auc_interest))     


gsub("[0-9]", "", (auc_interest$name))

gsub("[0-9]", "", (searches$Search.Term))

#Below all search values based off of a brand name no matter what is attached will be automatically registered as that brand.
#change all alternatives to "John Deere"
searches$Search.Term[searches$Search.Term %in%
                       c("John Deere", "john deere", "John Deere a", "John Deere A", "John deer E",
                         "John Deer", "John deere", "j", "JOhn deere", "john Deere", "JOhn deer Tractor",
                         "john", "deere", "Deere", "John Deere G", "john deere r", "model a John Deere",
                         "John", "John deer", "Tractor Deere", "John Deer model a", "John Deere track",
                         "John Deere hicrop", "John Deere Gator", "john deere tractor", "John Deere b",
                         "John Deere Gators", "john deere combine", "John Deere combine", 
                         "JD Tractors WD", "John Deere B", "john deere planter", "john deere turf", 
                         "John deere Planters", "Johnn", "jd", "Jd", "JD", "john deer", "JOHN DEERE", "john deere m",
                         "John deer tractors", "John Deere tractors", "john deere R", "Tractor Deere",
                         "John Deere r", "John Deere mfwd", "John Deere planter", "John Beere",
                         "John Deere Harvester", "rollover", " ", "John Deere gator", "JOHN DEERE",
                         "r", "rt", "John Deere Chopper", "John Deere track tractors", "John deer tractors",
                         "John Deere Planter", "john deere L", "G John Deere tractor", "John Deere B",
                         "john deere h", "john deere mulching", "deere utility", "john deere planter",
                         "John Deere Wagons", "JOHNN", "John Deere sub soiler", "John Deere Tractor",
                         "John Deere skid steer", "loader deere", "johnn", "John Deere tractor",
                         "John Deere dozer, Jd tractors", "John Deere drill",
                         "jOHN dEERE mODEL", "John dear", "John Deere lawn tractors")] <- "John Deere"

#change all alternatives to "Tractor"
searches$Search.Term[searches$Search.Term %in%
                       c("Tractor", "tractor", "LS Tractor", "Ford Tractor",
                         "Tractors", "tractors", "track tractor", "antique tractors",
                         "Lawn tractor", "Mfwd tractor", "Antique Tractors", "antique Tractors",
                         "Farm tractor", "Loader tractor", "Antique tractors", "Antique Tractor",
                         "Tractors with loaders", "lawn tractor", "Antique tractor", "antique tractor",
                         "Tractor with loader", "Collector tractor", "antique Tractor",
                         "Utility tractor with loader")] <- "Tractor"

#change all alternatives to "Case IH"
searches$Search.Term[searches$Search.Term %in%
                       c("Caseih", "case ih","case IH", "IJ case", "case ih combines",
                         "Case IH", "Case ih", "Case ih combine", "Case IH combine",
                         "Case ih combine", "International Harvester", "case header",
                         "international harvester", "Case", "case", "CASE IH",
                         "ih", "IH", "Case IH combine", "Case IH magnum",
                         "case ih 4wd", "CaseIH", "caseih", "caseih disc",
                         "header case ih", "International", "international",
                         "j i case", "Case Ih", "International harvester",
                         "CaseIH magnum", "Case row", "case combine", "casee sprayer",
                         "case tractor", "Caseih Tractor", "CaseIH",
                         "International tractor", "Caseih scout", "Case IH scout",
                         "case ih scout", "CaseIH scout", "case ih puma",
                         "Case ih header", "case ih header", "case in", "casein",
                         "Case vac", "IH tractor", "ih tractor", "IH Tractor",
                         "case ih sprayer", "case ih magnum", "Case ok", "CaseC",
                         "Case wd", "header case IH", "header Case IH", "Header Case IH",
                         "case ih tractors", "case mowers", "Caseih tractor", "Case I.H.",
                         "case I.H.", "case i.h.", "caseih corn head", "internation",
                         "Ih tractor", "internacional", "Case Ih planters", "Case I",
                         "CASE INTERNATIONAL TRACTORS", "magnum", "headers case ih",
                         "j.I. case", "ji case", "ji Case", "J.I. case", "J.I. Case")] <- "Case IH"

#change all alternatives to "Kubota"
searches$Search.Term[searches$Search.Term %in%
                       c("Kubota", "kubota", "Kubota mgx", "KUBOTA", "Kubota tractors",
                         "kubota tractors", "Kubota Tractors", "kubota l", "Kubota L",
                         "kubota L", "kubota b", "kubota BX", "kuboto", "Kuboto",
                         "Kubota gx", "kubota compact tractor", "kabota", "Kabota",
                         "kubota excavators", "kubota m", "Kubota tractor", "Kubota Tractor",
                         "kubota tractor", "kubota b cab")] <- "Kubota"

#change all alternatives to "Oliver"
searches$Search.Term[searches$Search.Term %in%
                       c("Oliver", "oliver", "Oliver super", "oliver super",
                         "oliver White", "oh", "O", "o")] <- "Oliver"

#change all alternatives to "New Holland"
searches$Search.Term[searches$Search.Term %in%
                       c("New Holland", "new holland", "New holland", "new Holland")] <- "New Holland"


#John Deere graph
ggplot(data = subset(searches,Search.Term == "John Deere")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("John Deere Search Frequency Over Time")

#Tractor graph
ggplot(data = subset(searches,Search.Term == "Tractor")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("Tractor Search Frequency")

#Case IH graph
ggplot(data = subset(searches,Search.Term == "Case IH")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("Case IH Search Frequency")

#Oliver Graph
ggplot(data = subset(searches,Search.Term == "Oliver")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("Oliver Search Frequency")

#Kubota Graph
ggplot(data = subset(searches,Search.Term == "Kubota")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("Kubota Search Frequency")

#New Holland Graph
ggplot(data = subset(searches,Search.Term == "New Holland")) +
  geom_line(aes(x = Date, y = Count))+
  geom_smooth(aes(x = Date, y = Count)) +
  ggtitle("New Holland Search Frequency")
#############################################################

########################MODEL EXPLORATION########################
###Random Forests
#Create Test and Train Datasets
RNGkind(sample.kind = "Rounding")

#so we all get the same random sample (reproducibility)
set.seed(4927625) 

#make new dataset so the auc_sales aren't altered and make sure N/As are gone from price
cdata <- subset(auc_sales, auc_sales$Lot.Sale.Price != "N/A")

#80% of obs to train 20% to test
train.idx <- sample(x=1:nrow(cdata), size = floor(.8*nrow(cdata)))
head(train.idx)
train.df <- cdata[train.idx,]
test.df<-cdata[-train.idx,]


##Decision Tree

ctree <- rpart(Lot.Sale.Price ~ Auction.Type+Lot.hours+Auction.State+Lot.Drive+days.on.TZ
               +Lot.separator.hours+Lot.Year,
               data = train.df,
               method = 'anova')
ctree
#take a look at the decision tree
rpart.plot(ctree)

##forest

#create random forest for sale price with same variables from tree
forest1 <- randomForest(Lot.Sale.Price ~ Auction.Type+Lot.hours+Auction.State+Lot.Drive+days.on.TZ
                        +Lot.separator.hours+Lot.Year,
                        data = train.df,
                        mtry = 2,
                        importance=TRUE,ntree = 1000,
                        na.action = na.exclude)

#look at output for the forest
forest1

#use the tree to get predictions for the test dataset
test.df$price_pred <- predict(forest1, test.df)

#plot the predictions vs actual to get a feel for accuracy
plot(test.df$price_pred,test.df$Lot.Sale.Price,main="Forest Performance",
     xlab="Prediction",
     ylab="Actual")

#look at variable importance of the factors from the forest
varImp(forest1,type = 1)
varImpPlot(forest1,type = 1)

###OLR

auc_sales_lm <- subset(auc_sales)
auc_sales_lm$LogLotPrice <- log(auc_sales$Lot.Sale.Price)
#linear model predicting log lot price with separator hours, lot hours, lot make, lot year, lot #category, auction type

regression2 = lm(LogLotPrice ~ Lot.separator.hours + Lot.hours + Lot.Make + Lot.Year + Lot.Category + Auction.Type, data = auc_sales_lm)
summary(regression2)

#linear model predicting log lot price with separator hours, lot hours, lot make, lot year, lot #category, auction type, and geographical region

regression3 = lm(LogLotPrice ~ Lot.separator.hours + Lot.hours + Lot.Make + Lot.Year + Lot.Category + Auction.Type + auc_sales_lm$geo_region, data = auc_sales_lm)
summary(regression3)

#comparing the AICs of the two models to see which one is the better one
AIC(regression2)
AIC(regression3)


###LASSO
#creating a subset with further cleaned data for lasso regression
#necessary to do this becuase cannot have missing values--> results in a very small dataset
lasso <- auc_sales[c("Lot.Drive", "Auction.Type", "Lot.hours" ,"Auction.State", "days.on.TZ", "Lot.separator.hours", "Lot.Sale.Price")]

lasso <- subset(lasso, lasso$Auction.Type != "NULL")
lasso <- subset(lasso, lasso$Lot.Drive != "NULL")
lasso <- subset(lasso, lasso$Lot.Drive != "")
lasso <- subset(lasso, lasso$Auction.State != "NULL")
lasso <- subset(lasso, lasso$Lot.hours != "NULL")
lasso <- subset(lasso, lasso$days.on.TZ != "NULL")
lasso <- subset(lasso, lasso$Lot.separator.hours != "NULL")

##data partitioning
set.seed(100) 
index = sample(1:nrow(lasso), 0.7*nrow(lasso)) 

#creating training and testing data
train = lasso[index,] 
test = lasso[-index,] 

dim(train)
dim(test)


cols = c("Lot.Drive", "Auction.Type", "Lot.hours" ,"Auction.State", "days.on.TZ", "Lot.separator.hours", "Lot.Sale.Price")

dummies <- dummyVars(Lot.Sale.Price ~ ., data = lasso[,cols])

train_dummies = predict(dummies, newdata = train[,cols])

test_dummies = predict(dummies, newdata = test[,cols])

print(dim(train_dummies)); print(dim(test_dummies))


x = as.matrix(train_dummies)
y_train = train$Lot.Sale.Price

x_test = as.matrix(test_dummies)
y_test = test$Lot.Sale.Price

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  ) 
  
}

#lambdas <- 10^seq(2, -3, by = -.1)
lambdas = seq(0.001, 0.1, by = 0.0005)

# Setting alpha = 1 to implement lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# extracting best lambda value 
lambda_best <- lasso_reg$lambda.min 
lambda_best

#running model for results
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)

###GLMS
###Checking skewness of Lot.Sale.Price
skewness(auc_sales$Lot.Sale.Price)
qqnorm(auc_sales$Lot.Sale.Price)
qqline(auc_sales$Lot.Sale.Price)

#create subset with the log of Lot.Sale.Price for further exploration
auc_sales_log <- subset(auc_sales)
auc_sales_log$log.sale.price <- log(auc_sales$Lot.Sale.Price)

skewness(auc_sales_log$log.sale.price)
qqnorm(auc_sales_log$log.sale.price)
qqline(auc_sales_log$log.sale.price)
#based on this, Lot.Sale.Price is rightskewed always positive---> so want to either log transform or use gamma

#create a subsets with cleaned year variable 
auc_sales_year <- subset(auc_sales, auc_sales$Lot.Year >= 1000)
auc_sales_year_log <- subset(auc_sales_log, auc_sales_log$Lot.Year >= 1000)

###GLM with log transformation
glm_log_separator <- glm(log.sale.price ~ Auction.State+ geo_region+ Lot.Make + Lot.Category + Lot.hours + Lot.separator.hours + days.on.TZ + 
                           Auction.Bid.Type +Auction.Type + Lot.Drive+Lot.Sub.Category+Lot.Year +Lot.horse.power+Lot.Basic.Featured +Lot.model + month,
                         data= auc_sales_year_log)

summary(glm_log_separator)
#-65 AIC
#glm_log_separator finds a good model if only want to model separators
#we want  general model so keep looking

#new model with best AIC without significant loss of data
glm_log <- glm(log.sale.price ~ Auction.State+ geo_region+ Lot.Make + Lot.Category + Lot.hours + days.on.TZ + 
                 Auction.Bid.Type + Auction.Type + Lot.Drive+Lot.Sub.Category+Lot.Year +Lot.horse.power+Lot.Basic.Featured +Lot.model + month,
               data= auc_sales_year_log)
summary(glm_log)


###GLM with gamma and log
glm_gamma <- glm(Lot.Sale.Price ~ geo_region + Lot.Make+ Lot.Category+ Lot.hours + days.on.TZ + month +
                   Auction.Type + Lot.Drive+Lot.Year +Lot.horse.power+Lot.Basic.Featured,
                 data= auc_sales_year, 
                 family = Gamma(link = "log"))
summary(glm_gamma)







