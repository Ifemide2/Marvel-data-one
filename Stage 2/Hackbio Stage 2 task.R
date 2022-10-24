# importing dataset
dataset <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/datasets/pca_sc_dataset.dat')
dataset <- subset(dataset, select = -c(Fig4DE.genecluster, E5.pre.lineage))
View(dataset)


# installing the required packages for pca
install.packages('ggplot2')
install.packages('factoextra')
install.packages('FactoMineR')
install.packages('devtools')
install_github('kassambara/ggpubr')
install.packages('RColorBrewer')

# enabling the previously installed packages
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(devtools)
library(ggpubr)
library(RColorBrewer)

# the pca() is then adopted for the creation of a standardized pca table with [, -c(1:2)] to omit the first and second column
a <- PCA(dataset[,-c(1:2)], length(dataset), scale.unit = TRUE, graph = FALSE)


# a summary of the pca is then made using the command prompt to show the importance of the components about the derived pca table
summary(a)


# making a pca plot of the earlier realized data (i.e 'a'), we use the fviz_eig function
fviz_eig(a, addlabels = TRUE, ylim = c(0, 70))
# the limit of my y-axis was pegged at 70 to allow full capture of my highest % of variance level in the plot


# a variable correlation plot is required to have a good understand of the correlation if it is positive or negative, hence, the fviz_pca_var function is used
fviz_pca_var(a, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)
# a positive correlation was observed with no slightest negative correlation, this can be summarily said that the variables are well correlated


# To find a pattern or marker, we could view our data in a different light by either plotting the  cell types or gene expression
b <- PCA(t(dataset[,-1]), scale.unit = TRUE, graph = FALSE)


# For visualization, the fviz_pca_ind function is then used
c <- fviz_pca_ind(b, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)
# It can be deduced that bacteria sample E3 and E4  has no correlation with the member cell types on the table
 

# adding a label to the previous plot
ggpar(c,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cos2", legend.position = "top",
      ggtheme = theme_minimal())


#recalling the plot of bacteria cell type but with an inclusion of noncentrality parameter
d <- PCA(dataset[, -c(1:2)], scale.unit = TRUE, ncp = 2, graph = FALSE)
summary(d)


# To color the gene in the PCA plot we will be using the first column (Lineage), it divided the cells into three groups. First we need to convert the column to a factor by the following command
dataset$lineage <- as.factor(dataset$lineage)


# Using the fviz_pca_ind() function to create a PCA plot
e <- fviz_pca_ind(d, col.ind = dataset$lineage, addEllipses = TRUE)


#labels are added for easy classification
ggpar(e, 
      title = "Principal Component Analysis", xlab = "PC1", ylab = "PC2", 
      legend.title = "Cell Types", legend.position = "top", 
      ggtheme = theme_minimal())

