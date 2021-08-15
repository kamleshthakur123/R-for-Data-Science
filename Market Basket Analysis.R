#### Market Basket Analysis
install.packages("arules")
library(arules)

read.csv(file.choose(),header = T)
mydata <- read.csv(file.choose(),header = T,
                   colClasses = "factor")
View(mydata)
str(mydata)
### to create all ruled with default setting
??apriori
rules <- apriori(mydata)

### Rules with specified Parameter values
rules <- apriori(mydata,parameter = list(minlen=2,
                                         maxlen=10,
                                         supp=.7,
                                         conf=.8))
### here we only want rule for minimum pruchase 2 and maximum
## purchase 10
## inspect rules
inspect(rules)

### finding some interesting rules
summary(mydata)

rules <- apriori(mydata,parameter = list(minlen=2,
                                         maxlen=10,
                                         conf=.7,
                                         support=.3),
                 appearance = list(rhs=c("Foundation=Yes"),
                                   default="lhs"))

inspect(rules)


rules <- apriori(mydata,parameter = list(minlen=2,
                                         maxlen=10,
                                         conf=.7,
                                         support=.3),
                 appearance = list(rhs=c("Foundation=Yes"),
                                   lhs=c("Blush=Yes","Bag=Yes"),
                                   default="lhs"))
inspect(rules)                 

### reducing the digits to 3
round(quality(rules),digits = 3)
rules.sorted <- sort(rules,by="lift")
inspect(rules.sorted)
### redudnacy of rules
### some time rules appear again and again , we don't want these rules

redudant <- is.redundant(rules,measure="confidence")
which(redudant)
rules.prone <- rules[!redudant]
rules.prone <- sort(rules.prone,by="lift")
inspect(rules.prone)

### Charts and Graphs
library(arulesViz)
plot(rules)
plot(rules.prone)
plot(rules,method = "grouped")
plot(rules,method= 'graph')
plot(rules,method= "paracoord")






















