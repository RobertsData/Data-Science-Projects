library(tidyverse)
library(randomForest)
library(corrplot)
library(caret)

# Load and Inspect Data
whiskies = read_csv('whisky.csv')
head(whiskies)
attributes <- whiskies[3:14] # Matrix of Only Flavor Attributes 

# Distribution of Total Flavor Points
points<- whiskies %>% group_by(Distillery) %>% 
  summarize(Total_Flavor_Points = sum(Body, Sweetness, Medicinal, Winey, Smoky, Tobacco, Spicy, Nutty, Malty, Fruity, Floral))
points  %>% ggplot(aes(Total_Flavor_Points),fill="#F5C76D", color="brown") + geom_histogram(binwidth=1,fill="#F5C76D", color="black")

# Correlation Matrix
corrplot(cor(attributes, y=NULL), method="circle", type="upper", col = c("mediumseagreen", "steelblue"))

# Sum of Points In Each Category
cols <- as.list(colSums(attributes)/nrow(whiskies))
atts <-names(attributes)
ggplot() + geom_bar(aes(x= atts, y = cols),fill="#F5C76D", color="black",stat="identity")+ 
  xlab("Flavor") +
  ylab("Average Value")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Percent of Distilleries With Non-Zeros for Each Flavor 
non_zero_atts <- (colSums(attributes != 0)/86)*100
labs <- names(non_zero_atts)
as_data_frame(non_zero_atts) %>% ggplot() + 
  geom_bar(aes(labs, non_zero_atts),fill="#F5C76D", color="brown", stat="identity")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Percent of Distillerie With Flavor >0")+
  xlab("")

# K Means Classifier
set.seed(1)
fit <- kmeans(attributes, 4)
whiskies_clusters <- data.frame(whiskies, fit$cluster)
whiskies_clusters$fit.cluster <- as.factor(whiskies_clusters$fit.cluster)
whiskies_clusters %>% group_by(fit.cluster) %>% summarize(population = n())

ssPlot <- function(data) {
  SSw <- as.vector((nrow(data) - 1) * sum(apply(data, 2, var)))
  for (i in 2:9) {SSw[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(2:10, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}
ssPlot(attributes)
# Note: Plot generated using modified code from reference #3

# Examining Clusters For Flavor Profile Commonality
cluster_one <- whiskies_clusters %>% filter(fit.cluster == 1)
cluster_two <- whiskies_clusters %>% filter(fit.cluster == 2)
cluster_three <- whiskies_clusters %>% filter(fit.cluster == 3)
cluster_four <- whiskies_clusters %>% filter(fit.cluster == 4)

c1flavors <- t(colSums(cluster_one[3:14]))
c2flavors <- t(colSums(cluster_two[3:14]))
c3flavors <- t(colSums(cluster_three[3:14]))
c4flavors <- t(colSums(cluster_four[3:14]))

p1 <- ggplot() + geom_bar(aes(colnames(c1flavors), c1flavors[1,]/nrow(cluster_one)),fill="#F5C76D", color="brown",stat="identity") +  ylim(0,4) + ylab("Flavor") + xlab("Cluster 1 (16 Distilleries)")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))   
p2 <- ggplot() + geom_bar(aes(colnames(c2flavors), c2flavors[1,]/nrow(cluster_two)),fill="#F5C76D", color="brown",stat="identity") +  ylim(0,4) + ylab("")+ xlab("Cluster 2 (6 Distilleries)") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
p3 <- ggplot() + geom_bar(aes(colnames(c3flavors), c3flavors[1,]/nrow(cluster_three)),fill="#F5C76D", color="brown",stat="identity") +  ylim(0,4) + ylab("Flavor")+ xlab("Cluster 3 (38 Distilleries)")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p4 <- ggplot() + geom_bar(aes(colnames(c4flavors), c4flavors[1,]/nrow(cluster_four)),fill="#F5C76D", color="brown",stat="identity") +  ylim(0,4) + ylab("")+ xlab("Cluster 4 (26 Distilleries)")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Using Random Forest to Assess Variable Importance 
train_rf <- randomForest(y = whiskies_clusters$fit.cluster, x=attributes)
labels <- rownames(varImp(train_rf))
importances <- varImp(train_rf)
importances  %>% ggplot() +
  geom_bar(stat="identity",aes(labels, Overall),fill="#F5C76D", color="brown") + ylab("Variable Importance")+
  xlab(" ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



  