setwd("E:/Kaggle/Big Mart Sales")

require(dplyr)
require(ggplot2)
require(scales)
library(foreign)
require(lmtest)
require(sandwich)
require(plm)

BM <- read.csv('Train_Big_Mart.csv')
BM_test <- read.csv('Train_Big_Mart.csv')
test <- read.csv('Test_Big_Mart.csv')

summary(BM)

str(BM)

ggplot(data = BM , mapping = aes(x = Outlet_Location_Type , y = Item_Outlet_Sales  , fill = Outlet_Type)) 
                              + geom_bar(stat='identity', position = 'dodge')
 
ggplot(data = BM , mapping = aes(x = Item_Type , y = Item_Outlet_Sales  )) + geom_bar(stat = 'identity') + coord_flip() + facet_grid(~Outlet_Type)

BM$Item_Weight[is.na(BM$Item_Weight)] <- mean(BM$Item_Weight,na.rm = T)
summary(BM)

write.csv(BM,"Train_Big_Mart1.csv", row.names = F)

BM_1 <- BM[1:6000,]
BM_2 <- BM[6001:8523,]
 
model1 <- lm(data = BM_1, Item_Outlet_Sales~ Outlet_Type + Outlet_Location_Type + Outlet_Size + Item_MRP) 

model2 <- lm(data = BM, Item_Outlet_Sales~ Outlet_Type + Outlet_Location_Type + Outlet_Size + Item_MRP) 


#+ Item_Type +Item_Visibility +Item_Fat_Content +Item_Weight

plot(model2)
par(mfrow=c(2,2))
coeftest(model2,vcovHC(model2, type="HC1"))

predicted <- predict(model2 , newdata = test)
test$Item_Outlet_Sales <- predicted
write.csv(test,"Test.csv", row.names = FALSE)
#cor(predicted,test$Item_Outlet_Sales)
#BM_2$predicted <- predicted


summary(BM$Item_Outlet_Sales)





library(mice)
md.pattern(BM)


#Replacing the values which are 0 in item visibilityBMVis <- data.frame(BM$Item_Visibility)


BMVis1 <- BM[BM$Item_Visibility != 0,]
BMVis0 <- BM[BM$Item_Visibility == 0,] #dataframe with only item visibility as zeroes

str(BMVis1)
vismo1 <- lm(Item_Visibility~ (Outlet_Type)+Item_Type+Item_Fat_Content + Item_Type:Item_Fat_Content  , data = BMVis1)

summary(vismo1)

coeftest(vismo1,vcovHC(vismo1, type="HC1"))

predVisi <- predict(vismo1 , newdata = BMVis0)
BMVis0$Item_Visibility <- predVisi

BM_test <- rbind(BMVis0,BMVis1)
summary(BM_test)

install.packages("rpart")
require(rpart)

#splitting outlet size dataframe

BMsize1 <- BM_test[BM_test$Outlet_Size != "",]
BMsize0 <- BM_test[BM_test$Outlet_Size == "",]
str(BMsize1)
sizepredict <- rpart(Outlet_Size ~ Item_Weight + Item_Fat_Content + Item_Visibility + Item_Type + Item_MRP + Outlet_Establishment_Year + 
                     Outlet_Location_Type + Outlet_Type   , method = "class" , data = BMsize1 )

summary(sizepredict)

predsize <- predict(sizepredict , newdata = BMsize0 , type = "class")
BMsize0$Outlet_Size <- predsize


BM_test <- rbind(BMsize0,BMsize1)
summary(BM_test)
