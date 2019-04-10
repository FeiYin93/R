# Research Project - Fei Yin
# 17137446
# School of computing
# National College of Ireland

#######################################################

# 1 Sentiment Score calculation

# 1.1 Install and load the SentimentR package
install.packages("sentimentr")
install.packages("ndjson")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("fUnitRoots")
install.packages("tseries")
install.packages("stargazer")

# 1.2 Load the data
library(sentimentr)
library(ndjson)
#df <- stream_in("D:\\Research\\Data\\Sports_and_Outdoors_5.json")
#df <- stream_in("D:\\Research\\Data\\Cell_Phones_and_Accessories_5.json")
#df <- stream_in("D:\\Research\\Data\\Beauty_5.json")
#df <- stream_in("D:\\Research\\Data\\Toys_and_Games_5.json")
#df <- stream_in("D:\\Research\\Data\\Baby_5.json")
head(df)

# 1.3 Calculate the sentiment score for each review and save it as a new column in the data frame
sentiment <- sentiment_by(df$reviewText)
df$sentiment <- sentiment$ave_sentiment
df$magnitude <- sentiment$word_count

# 1.4 a quick histogram to look at the sentiment of the reviews
library(ggplot2)
qplot(sentiment$ave_sentiment,geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")
summary(sentiment$ave_sentiment)

#######################################################

# 2 Regression data construction

# 2.1 extract product id and count no. of reviews for each product
Data <- data.frame(ProductID = names(table(df$asin)), Num_Rev = as.numeric(table(df$asin)))
Data$Log_Num_Rev <- log(Data$Num_Rev)
hist(Data$Num_Rev)
hist(Data$Log_Num_Rev)

# 2.2 Calculate average sentiment score for each product
Data$Ave_Senti <- tapply(df$sentiment,df$asin,mean)
qplot(Data$Ave_Senti,geom="histogram",binwidth=0.1,main="Sentiment Histogram")

# 2.3 Calculate sentiment score variance for each product
Data$Var_Senti <- tapply(df$sentiment,df$asin,var)
Data$Var_Senti_Squ <- Data$Var_Senti^2

# 2.4 Calculate average review magnitude for each product
Data$Ave_Magni <- tapply(df$magnitude,df$asin,mean)

# 2.5 Calculate average rating for each product
Data$Ave_Rating <- tapply(df$overall,df$asin,mean)

# 2.6 Calculate the differncce between average semtiment score and average rating
summary(Data$Ave_Senti)
hist(Data$Ave_Senti)
summary(Data$Ave_Rating)
hist(Data$Ave_Rating)
#Data$Diff <- Data$Ave_Rating - ((Data$Ave_Senti-min(Data$Ave_Senti))/(max(Data$Ave_Senti)-min(Data$Ave_Senti))*(5-1)+1)
Data$Ave_Senti1 <- scale(Data$Ave_Senti)
hist(Data$Ave_Senti1)
Data$Ave_Rating1 <- scale(Data$Ave_Rating)
hist(Data$Ave_Rating1)
Data$Diff1 <- Data$Ave_Rating1 - Data$Ave_Senti1
Data$Diff2 <- Data$Ave_Senti1 - Data$Ave_Rating1
summary(Data)

#######################################################

# 3 Correlation Matrix
Data_New <- Data[,-c(1,2)]
head(Data_New)
cor(Data_New)

library(Hmisc)
res <- rcorr(as.matrix(Data_New))
res
res$r
res$P

library(corrplot)
corrplot(res$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Insignificant correlations are leaved blank
corrplot(res$r, type = "upper", order = "hclust", p.mat = res$P, sig.level = 0.01, insig = "blank", tl.col = "black", tl.srt = 45)

library(PerformanceAnalytics)
#chart.Correlation(Data_New, histogram=TRUE, pch=19)

#######################################################

# 4 Regression

# 4.1 Unit Root test
library(fUnitRoots)

adfTest(Data_New[,1])
adfTest(Data_New[,2])
adfTest(Data_New[,3])
adfTest(Data_New[,4])
adfTest(Data_New[,5])
adfTest(Data_New[,6])
adfTest(Data_New[,7])

library(tseries)
pp.test(Data_New[,1])
pp.test(Data_New[,2])
pp.test(Data_New[,3])
pp.test(Data_New[,4])
pp.test(Data_New[,5])
pp.test(Data_New[,6])
pp.test(Data_New[,7])

# 4.2 Regression
lm <- lm(Log_Num_Rev ~ Ave_Senti + Var_Senti + Var_Senti_Squ + 
           Ave_Magni + Diff,data = Data_NeW)
summary(lm)

lm1 <- lm(Log_Num_Rev ~ Ave_Senti + Var_Senti + Var_Senti_Squ + 
            Ave_Magni + Diff1,data = Data)
summary(lm1)

lm2 <- lm(Log_Num_Rev ~ Ave_Senti + Var_Senti + Var_Senti_Squ + 
            Ave_Magni + Diff2,data = Data)
summary(lm2)

library(stargazer)
stargazer(lm, title = "results", align = F, type = "text", no.space = TRUE, out = "lm.html")



