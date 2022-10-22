# importing dataset
Mcb_dataset <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/mcb/microbial_stationary_phase.dat')
View(Mcb_dataset)

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

# the pca() is then adopted for the creation of a standardized pca table 
a <- PCA(Mcb_dataset, scale.unit = TRUE, graph = FALSE)


# a summary of the pca is then made using the command prompt to show the importance of the components about the derived pca table
summary(a)


# making a pca plot of the earlier realized data (i.e 'a'), we use the fviz_eig function
fviz_eig(a, addlabels = TRUE, ylim = c(0, 100))
# the limit of my y-axis was pegged at 100 to allow full capture of my highest % of variance level in the plot


# increase in the max.overlaps is highly recommended required to avoid unlabeled data points
options(ggrepel.max.overlaps = Inf)


# a variable correlation plot is required to have a good understand of the correlation if it is positive or negative, hence, the fviz_pca_var function is used
fviz_pca_var(a, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)
# a positive correlation was observed with no slightest negative correlation, this can be summarily said that the variables are well correlated


# To find a pattern or marker, we could view our data in a different light by either plotting the bacteria cell types or time
b <- PCA(t(Mcb_dataset), scale.unit = TRUE, graph = FALSE)


# For visualization, the fviz_pca_ind function is then used
c <- fviz_pca_ind(b, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)
# It can be deduced that bacteria sample A1 has no correlation with the member bacteria sample on the table
 

# adding a label to the previous plot
ggpar(c,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cos2", legend.position = "top",
      ggtheme = theme_minimal())


#recalling the plot of bacteria cell type but with an inclusion of noncentrality parameter
d <- PCA(Mcb_dataset, scale.unit = TRUE, ncp = 2, graph = FALSE)
summary(d)


# A1 is the obvious log phase(exponential phase) of the bacteria curve and it is expected the column be converted to a factor, this will be responsible for obvious observation of the stationary phase while using color.
Mcb_dataset$A1 <- as.factor(Mcb_dataset$A1)


# Using the fviz_pca_ind() function to create a PCA plot
e <- fviz_pca_ind(d, col.ind = Mcb_dataset$A1, addEllipses = TRUE)


#labels are added for easy classification
ggpar(e, 
      title = "Principal Component Analysis", xlab = "PC1", ylab = "PC2", 
      legend.title = "Bacteria Curve", legend.position = "top", 
      ggtheme = theme_minimal())

