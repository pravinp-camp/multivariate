install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("rattle")
library(rattle)
data<-read.csv("groceries1.csv")
head(data)


#data1 <- as.data.frame(data) %>% mutate(Item 1 = as.numeric(Item 1), title = as.character(title), genres = as.character(genres))

library(arules)
library(dplyr)
data %>% glimpse()

n_distinct(data$Item.s.)

grocery_transactions <- as(data, "transactions")

inspect(head(grocery_transactions))




#plot
itemFrequencyPlot(grocery_transactions,
                  type = "absolute",
                  topN = 10,
                  horiz = TRUE,
                  main = 'Absolute item frequency')





#chatgpt code

# Assuming your dataset is in a CSV file named "groceries.csv"
#transactions <- read.transactions("groceries.csv", sep = ",", format = "basket")


data <- read.transactions("groceries1.csv", sep = ",", format = "basket")


# Summary of rules
summary(rules)


# Inspect top 10 rules
inspect(head(rules, n = 10))


#plot
itemFrequencyPlot(data,
                  type = "absolute",
                  topN = 20,
                  horiz = TRUE,
                  main = 'Absolute item frequency')


# Setting the plot configuration option
par(mfrow=c(2,1))

# Plot the relative and absolute item frequency plot
itemFrequencyPlot(data,
                  type = "relative",
                  topN = 30,
                  horiz = TRUE,
                  main = 'Relative item frequency')

itemFrequencyPlot(data,
                  type = "absolute",
                  topN = 30,
                  horiz = TRUE,
                  main = 'Absolute item frequency')



# Setting the plot configuration option
par(mar=c(2,30,2,2), mfrow=c(1,1))

# Plot the 10 least popular items
barplot(sort(table(unlist(LIST(data))))[1:10],
        horiz = TRUE,
        las = 1,
        main = 'Least popular items')



# Extract the set of most frequent itemsets
itemsets = apriori(data,
                   parameter = list(support = 0.4,
                                    target = 'frequent'
                   ))
rules <- apriori(data, parameter = list(support = 0.001, confidence = 0.5))

summary(rules)
inspect(head(rules))

# Sort rules by confidence in descending order
sorted_rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Extract the top 10 rules
top_10_rules <- sorted_rules[1:10]

# Inspect the top 10 rules
inspect(top_10_rules)

library(arulesViz)

# Plot the rules using a scatterplot
plot(rules, method = "scatterplot")

# Extract the top 10 rules
top_10_rules <- head(sorted_rules, n = 10)

# Plot the top 10 rules using a scatterplot
plot(top_10_rules, method = "graph")


# Extract the top 3 rules
top_3_rules <- head(sorted_rules, n = 3)

# Plot the top 3 rules using a scatterplot
plot(top_3_rules, method = "graph")

