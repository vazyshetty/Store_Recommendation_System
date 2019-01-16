rsdata <- fread("Total Data.csv")
summary(rsdata)

library(dplyr)
library(tidyverse)

# Converting Character date format to R date format
rsdata$tdate = as.Date(rsdata$Order.Create.Date,format = "%Y-%m-%d")
str(rsdata$tdate)

#data has been transformed into one order (or) in one row (basket format) for moving further for analysis
temp <- rec_data_unique %>% group_by(Customer.Number,tdate) %>% summarise(items= paste(Item.Desc.Part.Desc,collapse =";"))
names(temp)
trans_data <- cSplit(temp, c("items"), sep=";")
str(trans_data)

#Apriori algorithm uses input data in transaction format. So ,convert the data into transactional format using following code. 
convert_transdata <- as(transdata, "transactions")

#Apply APRIRI algorithms
gr_rules = apriori(convert_transdata, parameter = list(supp = .001, conf = .1, minlen=2))
str(gr_rules)

#Sort rules with decreasing liFt values
gr_rules = sort(gr_rules, by ="lift", decreasing = TRUE)

#Read top 5 rules
inspect(gr_rules[1:10])

