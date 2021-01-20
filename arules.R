setwd('C:/Users/julia/OneDrive/Escritorio/DS/Rpubs/AssociationMining')

# The Online Retail II data set contains all the transactions occurring for 
# a UK-based and registered, non-store online retail between 01/12/2009 
# and 09/12/2011.The company mainly sells unique all-occasion gift-ware. 
# Many customers of the company are wholesalers.

# The data set used was downloaded from [link](https://archive.ics.uci.edu/ml/machine-learning-databases/00502/online_retail_II.xlsx)

# Setting up packages
library(arules)
library(readxl)
library(arulesViz)
library(dplyr)
library(stringr)

# Just get observations from 2010 and 2011 since there are too many
dat <- read_excel("online_retail_II.xlsx",
                  sheet='Year 2010-2011',
                  guess_max = 100,
                  range = cell_cols("A:C"),
				  col_types = c("text", "text", "text")	
)

# Only select the Invoice and Description columns, this is the basket info
# needed
# Remove Invoice that starts with the letter 'c', which indicates a cancellation.
# We're only intersted in purchases transactions
# Remove leading and trailing spaces and also reduce repeated whitespace inside a string.
# Invoice represents one basket
dat <- select(dat, Invoice, Description) %>% 
        filter(Description != "", 
        Description != "Discount",
        !grepl("^C", Invoice, ignore.case = TRUE)
				 ) %>%
        mutate(Description = tolower(str_squish(Description)))

# Remove some puntuaction
dat <- mutate(dat, Description = str_remove_all(Description, "'|\\.|,"))

# Write csv so it's easier to read with read.transactions
write.csv(dat, file="dat.csv", row.names=FALSE)

# Convert data frame to transactions format
tr <- read.transactions("dat.csv", format = "single", sep=",",
                        rm.duplicates=TRUE, header=TRUE, cols=1:2)

# Since many customers are wholesalers is expected to see plenty of baskets
# that contain at least 100 items
summary(tr)

# Plot of the most frequently bought items
itemFrequencyPlot(tr, topN=10)

# There are just 6 items that have a support (relative frequency) of at least 7%
# Perhaps this is because the company offers a pretty wide range of products,
# as these are unique all-occasion gifts
itemFrequencyPlot(tr, support=0.07)

## Running apriori algorithm
# Support = It's the probabaility of an specific event, in this case,
# it's the proportion of times a specific item appears compared to the total
# number of transactions. In this online retail data, the basket rules support
# is somewhat low, with a maximum support a little greater than 4%

# Confidence = Confidence level of two events that occur simultaneously.
# It is defined as conf(X?Y)=supp(X?Y)/supp(X). For example, in this data set, the
# rule {childrens cutlery dolly girl} => {childrens cutlery spaceboy} has 
# a confidence close to 76%. This means that for 76% of the "childrens cutlery dolly girl" transactions
# this rule is correct. Confidence can be interpreted as an estimate of the 
# conditional probability P(Y |X), the probability of finding the RHS of 
# the rule in transactions under the condition that these transactions also 
# contain the LHS.

# Lift = It's defined as lift(X?Y)=supp(X?Y)/(supp(X)supp(Y)).
# It can be interpreted as the deviation of the support of the whole rule from 
# the support expected under independence given the supports of the LHS 
# and the RHS. It helps to filter or rank found rules. 
# Greater lift values indicate stronger associations (greater dependence among items in the rule).

# Setting a support that isn't two high (there are only two items with a support
# above 10%) but also not too low because we want items that often bought. In this case
# a support of 1% and a confidence of 30% were chosen
# Association rules must satisfy both a minimum support and a minimum confidence constraint at the same time.
# The algorithm found 1381 rules that passed both support and confidence minimum requirements
rules <- apriori(tr, parameter=list(support=0.01, confidence=0.3, target="rules"))
summary(rules)

# Basket rules of size equal to 2
inspectDT(subset(rules, size(rules) == 2))

# Some basket rules of size greater than 3
inspect(head(subset(rules, size(rules) > 3)))

# Taking a look at the top 11 rules by lift
inspect(sort(rules, by='lift', decreasing=TRUE)[1:11])

# Checking the rules that recommend the product with highest support (white hanging heart t-light holder in the right hand side)
heart.rhs <- subset(rules, subset = rhs %in% 'white hanging heart t-light holder')
inspect(heart.rhs)

## Ploting rules
plot(rules)

## Graph visualization for small subsets of rules, in this case, the
# top 11 with the highset lift
plot(sort(rules, by='lift', decreasing=TRUE)[1:11], method='graph')

## Running eclat algorithm
# As opposed to apriori, eclat just measures a set support not an item support.
# It only requires the support level. There is no confidence or lift involved.
# Here the algorithm outputs subsets, not rules.
# minlen is the minimum subset length
eclat_sets <- eclat(tr, parameter=list(support=0.01, minlen = 2))

# There were 971 subsets (or itemsets) that satisfied a minimum support of 10%
summary(eclat_sets)
inspectDT(eclat_sets)

# Sets with the highest support
# The most frequent combination among all the transactions is jumbo bag pink polkadot
# and jumbo bag red retrospot, with a support of `825 / 20610`
inspect(sort(eclat_sets, by='support', descending=TRUE)[1:9])

# Graph plot
plot(sort(eclat_sets, by='support', decreasing=TRUE)[1:10], method='graph')

