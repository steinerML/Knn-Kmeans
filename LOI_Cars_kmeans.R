# Set working directory 
setwd('C:\\LOI CSV')
getwd() #Directory set OK!

#Load dataset
cars <- read.csv('auto.csv', stringsAsFactors = FALSE)
str(cars) #Check that data is read correctly!

#Make 'herkomst' a dummy variable!
cars$herkomst <- factor(cars$herkomst, levels = c("usa", "anders"), labels = c(1, 0))

#Check that labels have been refactored correctly.
prop.table(table(cars$herkomst))
round(prop.table(table(cars$herkomst))*100, digits=1)
summary(cars$herkomst) #Check that numbers make sense.

#Normalize the file Min =0, Max = 1
#Define normalize function
normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

#Apply Normalization to dataset all variables except 'herkomst'
cars_nn <- as.data.frame(lapply(cars[2:10], normalize))
summary(cars_nn) #We check that everything makes sense!
#Normalization done!

# Determine number of clusters with Elbow method. I assume a K.max = 15.
set.seed(01234)
wss <- (nrow(cars_nn)-1)*sum(apply(cars_nn,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cars_nn,
   centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")
dev.copy(png,'elbow_method.png') #We save the plot to working directory!
dev.off()

#Apply kmeans algorithm from 'stats' package
library(stats)
set.seed(0123456) #Reproducibility of results
cars_clusters <- kmeans(cars_nn, 4) #K=4

#Add cluster to dataset in a specific column named 'cluster'
cars_final <- cbind(cars_nn, cluster= cars_clusters$cluster)
head(cars_final)

#Cluster classification: # of cars falling inside each cluster (Clusters size)
cars_clusters$size

#Clusters centers
cars_clusters$centers

#Clusters cluster info
cars_clusters$cluster

#Plot number of cars inside each cluster
library(ggplot2)
ggplot(cars_final, mapping = aes(x=cluster, fill=cluster, group=cluster)) + geom_bar(position = 'stack') + geom_text(stat='count', aes(label = ..count..), vjust = -.5, size = 5)
dev.copy(png,'cluster_contentk4.png') #We save the plot to working directory!
dev.off()

install.packages('gridExtra')
library(gridExtra)
# Plotting correlations

b1 <- ggplot(cars_final, aes(x = gewicht, y = lengte, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Gewicht VS Lengte", x = "Gewicht", y = "Lengte") + geom_smooth(method = "lm",aes(group = 1), se=FALSE, size=1) + theme_minimal()

b2 <- ggplot(cars_final, aes(x = gewicht, y = draaicirkel, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Gewicht VS Draaicirkel", x = "Gewicht", y = "Draaicirkel") + geom_smooth(method = "lm",aes(group = 1), se=FALSE, size=1) + theme_minimal()

b3 <- ggplot(cars_final, aes(x = lengte, y = hoofdruimte, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Lengte VS Hoofdruimte", x = "Lengte", y = "Hofdruimte") + geom_smooth(method = "lm",aes(group = 1), se=FALSE) + theme_minimal()

b4 <- ggplot(cars_final, aes(x = lengte, y = kofferruimte, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Lengte VS Kofferruimte", x = "Lengte", y = "Kofferruimte") + geom_smooth(method = "lm",aes(group = 1), se=FALSE) + theme_minimal()

b5 <- ggplot(cars_final, aes(x = gewicht, y = kml, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Gewicht VS Fuel Efficiency", x = "Gewicht", y = "Fuel Efficiency") + geom_smooth(method = "lm",aes(group = 1), se=FALSE) + theme_minimal()

b6 <- ggplot(cars_final, aes(x = lengte, y = prijs, color = factor(cluster))) +
  geom_point(size = 5) + labs(title = "Lengte VS Prijs", x = "Lengte", y = "Prijs") + geom_smooth(method = "lm",aes(group = 1), se=FALSE) + theme_minimal()

grid.arrange(b1,b2,b3,b4,b5,b6, nrow=3, ncol=2)
dev.copy(png,'variable_correlations.png') #We save the plot to working directory!
dev.off()

#Some more correlations

#Extra correlation plots included in the Report.
install.packages('corrplot')
library('corrplot')

#Create correlation matrix
cor_matrix <- cor(cars_nn)

#Corr matrix number
corrplot(cor_matrix, method = 'number')
dev.copy(png,'cor_matrix_number.png') #We save the plot to working directory!
dev.off()

#Corr matrix mixed
corrplot.mixed(cor_matrix, lower = 'shade', upper = 'pie', order = 'hclust')
dev.copy(png,'cor_matrix_mixed.png') #We save the plot to working directory!
dev.off()

#Corr matrix hclust
corrplot(cor_matrix, order = 'hclust', addrect = 2)
dev.copy(png,'cor_matrix_hclust.png') #We save the plot to working directory!
dev.off()

#Corr matrix hclust square
corrplot(cor_matrix, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')
dev.copy(png,'cor_matrix_hclust_square.png') #We save the plot to working directory!
dev.off()

#Aggregate() to get average value of variable per cluster.
#This is basically the $centers object returned by the kmeans() function!

#Mean price
aggregate(data=cars_final, prijs ~ cluster, mean)
#Mean fuel consumption
aggregate(data=cars_final, kml ~ cluster, mean)
#Mean reparation costs consumption
aggregate(data=cars_final, reparatie ~ cluster, mean)
#Mean hoofdruimte
aggregate(data=cars_final, hoofdruimte ~ cluster, mean)
#Mean kofferruimte
aggregate(data=cars_final, kofferruimte ~ cluster, mean)
#Mean gewicht
aggregate(data=cars_final, gewicht ~ cluster, mean)
#Mean lengte 
aggregate(data=cars_final, lengte  ~ cluster, mean)
#Mean draaicirkel 
aggregate(data=cars_final, draaicirkel  ~ cluster, mean)
#Mean gear_ratio 
aggregate(data=cars_final, gear_ratio   ~ cluster, mean)


# Plotting density plots for each cluster
c1 <- ggplot(cars_final, aes(x = prijs, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Prijs", x = "Prijs", y = "Density") +
  theme_minimal()

c2 <- ggplot(cars_final, aes(x = kml, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Kml", x = "Kml", y = "Density") +
  theme_minimal()

c3 <- ggplot(cars_final, aes(x = reparatie, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Reparatie", x = "Reparatie", y = "Density") +
  theme_minimal()

c4 <- ggplot(cars_final, aes(x = hoofdruimte, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Hoofdruimte", x = "Hoofdruimte", y = "Density") +
  theme_minimal()

c5 <- ggplot(cars_final, aes(x = kofferruimte, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Kofferruimte", x = "Kofferruimte", y = "Density") +
  theme_minimal()

c6 <- ggplot(cars_final, aes(x = gewicht, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Gewicht", x = "Gewicht", y = "Density") +
  theme_minimal()

c7 <- ggplot(cars_final, aes(x = lengte, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Lengte", x = "Lengte", y = "Density") +
  theme_minimal()

c8 <- ggplot(cars_final, aes(x = draaicirkel, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Draaicirkel", x = "Draaicirkel", y = "Density") +
  theme_minimal()

c9 <- ggplot(cars_final, aes(x = gear_ratio, fill = factor(cars_clusters$cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot for Gear Ratio", x = "Gear Ratio", y = "Density") +
  theme_minimal()

grid.arrange(c1,c2,c3,c4,c5,c6,c7,c8,c9, nrow=5, ncol= 2)
dev.copy(png,'densityplots_variable.png') #We save the plot to working directory!
dev.off()

#New function to run kmeans
#install.packages('factoextra')
library(factoextra)
library(ggplot2)
library(cluster)
set.seed(1234) #Reproducibility of results

#Cluster plot w/ k = 4
fviz_cluster(cars_clusters, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = "convex",addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk4_conv.png') #We save the plot to working directory!
dev.off()

#Improving our model with k=3

set.seed(987654) #Reproducibility of results(invented number)
#Adjust the kmeans() function with iter.max, nstart
cars_clusters3 <- kmeans(cars_nn, 3, nstart = 50, iter.max = 10)

#Add cluster to dataset in a specific column named 'cluster'
cars_final3 <-cbind(cars_nn, cluster= cars_clusters3$cluster)
head(cars_final3)

#Cluster classification: # of cars falling inside each cluster (Clusters size)
cars_clusters3$size

#Plot k number w/ fviz_nbclust
install.packages("factoextra")
library("factoextra")
fviz_nbclust(cars_nn, kmeans, method = "wss", k.max = 15,linecolor = "steelblue") + geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Elbow method")
dev.copy(png,'elbow_plotk3.png') #We save the plot to working directory!
dev.off()

#Plot number of cars inside each cluster
library(ggplot2)
ggplot(cars_final3, mapping = aes(x=cluster, fill=cluster, group=cluster)) + geom_bar(position = 'stack') + geom_text(stat='count', aes(label = ..count..), vjust = -.5, size = 5)
dev.copy(png,'cluster_contentk3.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ normalized ellipses
fviz_cluster(cars_clusters3, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk3_norm.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ convex ellipses
fviz_cluster(cars_clusters3, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk3_conv.png') #We save the plot to working directory!
dev.off()

#Improving our model with k=5

set.seed(456789) #Reproducibility of results(invented number)
#Adjust the kmeans() function with iter.max, nstart
cars_clusters5 <- kmeans(cars_nn, 5, nstart = 50, iter.max = 10)

#Add cluster to dataset in a specific column named 'cluster'
cars_final5 <-cbind(cars_nn, cluster= cars_clusters5$cluster)
head(cars_final5)

#Cluster classification: # of cars falling inside each cluster (Clusters size)
cars_clusters5$size

#Plot k number w/ fviz_nbclust
fviz_nbclust(cars_nn, kmeans, method = "wss", k.max = 15,linecolor = "steelblue") + geom_vline(xintercept = 5, linetype = 2) + labs(subtitle = "Elbow method")
dev.copy(png,'elbow_plotk5.png') #We save the plot to working directory!
dev.off()

#Plot number of cars inside each cluster
library(ggplot2)
ggplot(cars_final5, mapping = aes(x=cluster, fill=cluster, group=cluster)) + geom_bar(position = 'stack') + geom_text(stat='count', aes(label = ..count..), vjust = -.5, size = 5)
dev.copy(png,'cluster_contentk5.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ normalized ellipses
fviz_cluster(cars_clusters5, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk5_norm.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ convex ellipses
fviz_cluster(cars_clusters5, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk5_conv.png') #We save the plot to working directory!
dev.off()

#Improving our model with k=6

set.seed(789456) #Reproducibility of results(invented number)
#Adjust the kmeans() function with iter.max, nstart
cars_clusters6 <- kmeans(cars_nn, 6, nstart = 50, iter.max = 10)

#Add cluster to dataset in a specific column named 'cluster'
cars_final6 <-cbind(cars_nn, cluster= cars_clusters6$cluster)
head(cars_final6)

#Cluster classification: # of cars falling inside each cluster (Clusters size)
cars_clusters6$size

#Plot k number w/ fviz_nbclust
fviz_nbclust(cars_nn, kmeans, method = "wss", k.max = 15,linecolor = "steelblue") + geom_vline(xintercept = 6, linetype = 2) + labs(subtitle = "Elbow method")
dev.copy(png,'elbow_plotk6.png') #We save the plot to working directory!
dev.off()

#Plot number of cars inside each cluster
library(ggplot2)
ggplot(cars_final6, mapping = aes(x=cluster, fill=cluster, group=cluster)) + geom_bar(position = 'stack') + geom_text(stat='count', aes(label = ..count..), vjust = -.5, size = 5)
dev.copy(png,'cluster_contentk6.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ normalized ellipses
fviz_cluster(cars_clusters6, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk6_norm.png') #We save the plot to working directory!
dev.off()

#Clustering plot w/ convex ellipses
fviz_cluster(cars_clusters6, data = cars_nn, 
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)
dev.copy(png,'clustering_plotk6_conv.png') #We save the plot to working directory!
dev.off()

#Final comparison plot w/ k=3, k=4, k=5, k=6

#Convex ellipse
#k=3
plot1 <- fviz_cluster(cars_clusters3, data = cars_nn, main = 'Cluster Plot, k=3',
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=4
plot2 <- fviz_cluster(cars_clusters, data = cars_nn, main = 'Cluster Plot, k=4',
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = "convex",addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=5
plot3 <- fviz_cluster(cars_clusters5, data = cars_nn, main = 'Cluster Plot, k=5',
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=6
plot4 <- fviz_cluster(cars_clusters6, data = cars_nn, main = 'Cluster Plot, k=6',
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'convex',addlabels = TRUE, labelsize = 10, stand = FALSE)

#Create collage and save to disk
grid.arrange(plot1,plot2,plot3,plot4,nrow=2, ncol=2)
dev.copy(png,'cluster_plot_compare_conv.png') #We save the plot to working directory!
dev.off()

#Normalized ellipse
#k=3
plot5 <- fviz_cluster(cars_clusters3, data = cars_nn, main = 'Cluster Plot, k=3',
geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=4
plot6 <- fviz_cluster(cars_clusters, data = cars_nn, main = 'Cluster Plot, k=4', geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = "norm",addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=5
plot7 <- fviz_cluster(cars_clusters5, data = cars_nn, main = 'Cluster Plot, k=5', geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)

#k=6
plot8 <- fviz_cluster(cars_clusters6, data = cars_nn, main = 'Cluster Plot, k=6', geom = c('point', 'text'), repel = TRUE, ellipse = TRUE, 
ellipse.type = 'norm',addlabels = TRUE, labelsize = 10, stand = FALSE)

#Create collage and save to disk
grid.arrange(plot5,plot6,plot7,plot8, nrow=2, ncol=2)
dev.copy(png,'cluster_plot_compare_norm.png') #We save the plot to working directory!
dev.off()

#Extra clustering plots just for fun!!!
install.packages('mclust')
library('mclust')

#A menu will pop-up on the terminal and you can select option (1-4) and confirm with 0 to see the plot.
fit <- Mclust(cars_nn)
plot(fit)
dev.copy(png,'mclust_density.png') #We save the plot to working directory!
dev.off()