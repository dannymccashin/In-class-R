#user data set
library(readr)
OA_11_6_yelp_academic_dataset_user_json <- read_csv("C:/Users/danny/OneDrive/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(OA_11_6_yelp_academic_dataset_user_json)
#business data set
library(readr)
OA_11_7_yelp_academic_dataset_business_json <- read_csv("C:/Users/danny/OneDrive/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(OA_11_7_yelp_academic_dataset_business_json)

business=OA_11_7_yelp_academic_dataset_business_json
View(business)
library(ggplot2)
ggplot(business)+geom_bar(aes(x=state))+theme(axis.text.x=element_text(angle=90))

cont_table=table(business$stars)
pie(cont_table, main="Pie Chart of Star Ratings", col=rainbow(9))

mean_val <- mean(business$review_count)
sd_val <- sd(business$review_count)

# Calculate Z-scores
business$z_scores <- (business$review_count - mean_val) / sd_val

# Remove outliers
business_no_outliers <- subset(business, review_count<70)
print(business_no_outliers)



ggplot(business_no_outliers, aes(x=stars, y=review_count, fill=stars))+
  geom_boxplot()

acedemic=OA_11_6_yelp_academic_dataset_user_json

corr_cool_funny=cor(acedemic$cool_votes, acedemic$funny_votes)
print(corr_cool_funny)

corr_cool_useful=cor(acedemic$cool_votes, acedemic$useful_votes)
print(corr_cool_useful)

corr_useful_funny=cor(acedemic$useful_votes, acedemic$funny_votes)
print(corr_useful_funny)
print("All the groups have a very very strong correlation which each other")

library(caret)#Train Test Split
library(class)#K nearest neighbors
library(corrplot)#Visualize Correlation Matrix
library(ggplot2)#Graphing

model= lm(acedemic$cool_votes ~ acedemic$funny_votes)
print(model)

ggplot(acedemic) + geom_point(aes(x=cool_votes, y=funny_votes)) + 
  geom_smooth(aes(x=cool_votes, y=funny_votes),method="lm", se=F)

#Last question
library(caret)#Train Test Split
library(class)#K nearest neighbors
library(corrplot)#Visualize Correlation Matrix
library(ggplot2)#Graphing


acedemic_cluster= kmeans(acedemic[,c("review_count", "fans")], 3)
print(table(acedemic_cluster$cluster, acedemic$fans))


acedemic_x= function(k){
  kmeans(acedemic_x, centers=k)$tot.withinss
}

ggplot(acedemic)+ 
  geom_point(aes(x=review_count, y=fans, color=review_count))

#another example
acedemic_cluster= kmeans(acedemic[,c("funny_votes", "cool_votes")], 3)
print(table(acedemic_cluster$cluster, acedemic$fans))


votes_x= function(k){
  kmeans(votes_x, centers=k)$tot.withinss
}

ggplot(acedemic)+ 
  geom_point(aes(x=funny_votes, y=cool_votes, color=funny_votes))


print(acedemic)
