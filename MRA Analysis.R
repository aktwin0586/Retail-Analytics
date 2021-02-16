rm(list=ls())
library(xlsx)
setwd('C:/Users/ashwi/Downloads')
par(mfrow=c(1,1))
?read.xlsx
mydata2 = read.csv("Coffe_file.csv",header = TRUE)
mydata2 = mydata2[,c(1:10)]
head(mydata2)
tail(mydata2)
attach(mydata2)

library(dplyr)
glimpse(mydata2)
str(mydata2)
structure(mydata2)
describe(mydata2)
df_status(mydata2)

anyNA(mydata2)
mydata2$Bill.Number = as.factor(Bill.Number)
df_status(mydata2)
mydata_market_basket = mydata2[,c(2,3)]
mktBasket.Agg=split(mydata_market_basket$Item.Desc,mydata_market_basket$Bill.Number)
head(mktBasket.Agg)


mktBasket.Agg2=list()
for(i in 1:length(mktBasket.Agg)){
  mktBasket.Agg2[[i]]=unique(mktBasket.Agg[[i]])
}
head(mktBasket.Agg2)

install.packages('arules')
library(arules)

install.packages('arulesViz')
library(arulesViz)

Txns=as(mktBasket.Agg2,"transactions")
summary(Txns)
inspect(Txns[10])

arules2=apriori(data=Txns,
                parameter=list(support=0.0009,confidence = .09,maxlen=2)
)

summary(quality(arules2))

plot(arules2)

inspect(sort(arules2,by='lift',))
inspect(sort(arules2,by='confidence',))

arules2 = sort(arules2,by='confidence')

rules_df=as(arules2,"data.frame")

rules_df$LHSSupport=rules_df$support/rules_df$confidence
rules_df$RHSSupport=rules_df$confidence/rules_df$lift
print(rules_df)

write.table(rules_df,file="MBA_output.csv",sep=",",append=FALSE,row.names = FALSE)

topRules <- arules2[1:10]
plot(topRules, method="graph")
