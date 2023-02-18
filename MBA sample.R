install.packages("arules")
library("arules")
install.packages("arulesViz")
library("arulesViz")
install.packages("tidyverse")
library("tidyverse")
install.packages("readxl")
library("readxl")
install.packages("plyr")
library("plyr")
install.packages("ggplot2")
library("ggplot2")
install.packages("knitr")
library("knitr")
install.packages("lubridate")
library("lubridate")
install.packages("magrittr")
library("magrittr")

summary(Online_Retail)
retail = Online_Retail

retail= retail[complete.cases(retail),]
retail
?complete.cases

summary(retail)
str(retail)
??mutate
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))

retail$Date = as.Date(retail$InvoiceDate)
TransTime = format(retail$InvoiceDate, "%H:%M:%S")
InvoiceNo = as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)
??glimpse
install.packages("pillar")
library(pillar)
library(dplyr)
glimpse(retail)
str(retail)

library(plyr)
transactionData = ddply(retail,c("InvoiceNo", "Date"), 
                        function(df1)paste(df1$Description,
                                            collapse = ","))


?ddply
head(transactionData)
transactionData$InvoiceNo = NULL
transactionData$Date = NULL
colnames(transactionData) = c("items")
transactionData

write.csv(transactionData,"C:/Users/kmmaj/OneDrive/Desktop/PGBA/MBA/market_basket_transactions.csv", quote = FALSE, row.names = FALSE )

tr = read.transactions('C:/Users/kmmaj/OneDrive/Desktop/PGBA/MBA/market_basket_transactions.csv', format = 'basket', sep = ',')

summary(tr)

if(!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)}

itemFrequencyPlot(tr,topN=20,type= "absolute",col= brewer.pal(8,'Pastel2'),main = "Absolute Item Freq Plot")

itemFrequencyPlot(tr,topN=20,type= "relative",col= brewer.pal(8,'Pastel2'),main = "Absolute Item Freq Plot")

association.rules = apriori(tr,parameter = list(supp = 0.001 , conf = 0.8,maxlen=10))

inspect(association.rules[1:10])

shorter.association.rules = apriori(tr,parameter = list(supp=0.001, conf = 0.8, maxlen= 3))

subet.rules = which(colSums(is.subset(association.rules, association.rules)) > 1)
length(subet.rules)

subset.association.rules = association.rules[-subet.rules]

metal.association.rules = apriori(tr,parameter = list(supp = 0.001 , conf = 0.8), appearance = list(default="lhs", rhs = "METAL"))


inspect(head(metal.association.rules))

metal.association.rules = apriori(tr,parameter = list(supp = 0.001 , conf = 0.8), appearance = list(lhs = "METAL", default = "rhs"))

inspect(head(metal.association.rules))
  
subrules= association.rules[quality(association.rules)$confidence>0.4]
plot(subrules)

plot(subrules, method = "two-key plot")

plotly_arules(subrules)

top10subrules = head(subrules, n=10, by = "confidence")
plot(top10subrules, method = "graph" , engine = "htmlwidget")


subrules2 = head(subrules, n=20 , by= "lift")
plot(subrules2, method = "paracoord")
