# importing dataset
dataset <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/datasets/pca_sc_dataset.dat')
dataset <- subset(dataset, select = -c(Fig4DE.genecluster, lineage))
View(dataset)

# install packages
install.packages('useful')
install.packages('ggplot2')
library(useful)
library(ggplot2)

# choosing the right number of clusters
Checkcluster <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/datasets/pca_sc_dataset.dat')

# it is important to note that the funtion FitKMeans only work with values, so we need to remove every column without values, therefore, the columns (Fig4DE.genecluster, gene, and lineage) are removed before running the function
Checkcluster <- subset(Checkcluster, select = -c(Fig4DE.genecluster, gene, lineage))
View(Checkcluster)
Perf_dataset <- FitKMeans(Checkcluster, max.clusters=20)
View(Perf_dataset)
PlotHartigan(Perf_dataset)
# therefore, 14 was the last value of cluster with "True", hence, our best cluster is considered 14

# for the purpose of visualization, two columns that correlates are being plotted, and these can be any of the columns
cor(Checkcluster)
plot(Checkcluster$E5.pre.lineage, Checkcluster$EPI)
plot(Checkcluster$EPI, Checkcluster$EPI.2)

# for consistency of result, I will set a seed
set.seed(102)

# To perform KMEAN Clustering
KMCheckcluster <- kmeans(x = Checkcluster, centers = 14)

# adding a new column to our table with the newly created clusters
Checkcluster$clusters <- c(KMCheckcluster$cluster)

# visualizing the cluster information of each cor (EPI and EPI.2)
a <- ggplot(Checkcluster, aes(x = EPI, y = EPI.2, color = factor(clusters)))+
  geom_point()+
  theme_bw()

#reducing the y limit to 400
a + coord_cartesian(ylim = c(0, 400))

# visualizing the cluster information of each cor (E5.pre.lineage and EPI)
b <- ggplot(Checkcluster, aes(x = E5.pre.lineage, y = EPI, color = factor(clusters)))+
  geom_point()+
  theme_bw()

#reducing the y limit to 400
b + coord_cartesian(ylim = c(0, 400))

# called out the gene column from the original dataset
Checkcluster.gene <- dataset$gene

# added the newly formed gene column to our already existing table 
Checkcluster$gene <- Checkcluster.gene

#plotting the table
plot(KMCheckcluster, data = Checkcluster)
# plotting using class = gene will be impossible because we have over 300 genes and the maximum number of class should be max. 6


#Performing Hierarchial Clustering
# Recall the Checkcluster function
Checkcluster2 <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/datasets/pca_sc_dataset.dat')
Checkcluster2 <- subset(Checkcluster2, select = -c(Fig4DE.genecluster, gene, lineage))
View(Checkcluster2)

#checking the distance between each column
d = dist(Checkcluster2)
hcCheckcluster <- hclust(d, method = 'complete')
plot(hcCheckcluster)

# using a different type (triangle) for the plot
hc1 = as.dendrogram(hcCheckcluster)
plot(hc1, type = 'triangle')




