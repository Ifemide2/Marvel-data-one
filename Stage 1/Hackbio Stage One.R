# importing epidemiology dataset from github
Epidata <- read.delim('https://raw.githubusercontent.com/Ifemide2/public_datasets/850fb1e1f57c246d0b8fc00e05b82769b423cdaa/R/datasets/epidemiology_dataset')
# running the view function on my dataset to view what my data looks like
View(Epidata)

# Packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('choroplethr')
install.packages('choroplethrMaps')
install.packages('openintro')
install.packages('fiftystater')
install.packages('colorplaner')
install.packages('RColorBrewer')

# Activating packages
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(fiftystater)
library(colorplaner)
library(RColorBrewer)

# Dataset Filtering
# filtering the data to have only death_date variable
Epidata$death_date

# filtering data to show selected date
a <- Epidata %>%
  filter(death_date=='2/23/19' | death_date=='3/26/19' | death_date=='4/15/19')
View(a)

#Arranging Dataset
#  further arranging my selected date in descending order of clone
b <- Epidata %>%
  filter(death_date=='2/23/19' | death_date=='3/26/19' | death_date=='4/15/19') %>%
  arrange(desc(clone))
View(b)

# Arranging dataset in descending order of host time point
c <- Epidata %>%
  arrange(desc(host_time_point))
View(c)

#Mean calculation
# calculating the mean and median of clone in the dataset
d <- Epidata %>%
  summarise(Avg_clone = mean(clone),
            median_clone = median(clone),
            total = n())
View(d)

#Histogram Visualization
# visualizing the dataset using histogram with focus on variables - host time point
e <- hist(Epidata$host_time_point, col ='blue', main = 'Host time point representation',
          xlab = 'Host time point')

#visualizing the dataset using histogram with focus on variables - para time point
f <- hist(Epidata$para_time_point, col ='red', main = 'Para time point representation',
          xlab = 'Para time point')

# visualizing the dataset using histogram with focus on variables - lifespan
g <- hist(Epidata$lifespan, col ='green', main = 'Lifespan representation',
          xlab = 'Lifespan')

#Line/Horizontal Plot Visualization
# visualizing the dataset using horizontal plot with focus on the host time point with y-axis limit of 0 to 5
h <- plot(Epidata$para_time_point[0:100], type = 'h', ylim = c(0,3.5))

# visualizing the dataset using line plot with focus on the host time point with y-axis limit of 0 to 5
i <- plot(Epidata$para_time_point[0:100], type = 'l', ylim = c(0,3.5))

# visualizing the dataset using line plot with focus on the host time point
j <- plot(Epidata$host_time_point[0:100], type = 'l')

# visualizing the dataset using horizontal plot with focus on the host time point
k <- plot(Epidata$host_time_point[0:100], type = 'h')

# visualizing the dataset using horizontal plot with focus on lifespan using y-axis limit of 0 - 80
l <- plot(Epidata$lifespan, type = 'h', ylim = c(0,80))

#Boxplot Visualization
# Using the boxplot visualizing tool for my variables (Host Time Point, Para Time Point, and Life Span)
m <- boxplot( Epidata$host_time_point, Epidata$para_time_point,Epidata$lifespan,
              col = 5:7, notch = F, outline = F, main = 'Representaion of Host Time Point, Para Time Point & Life Span',
              xaxt = 'n', xlab = 'Infection Cycle', ylab = 'Position')

# adding a name to my axis and creating keypoints for each varibales as legends
axis(side = 1, at = c(1,2,3), labels = c('Host_TP', 'Para_TP', 'Lifespan'))
legend('topleft', legend = c('Host_TP', 'Para_TP', 'Lifespan'), col = 5:7, pch = 15)

# Using the boxplot visualizing tool for my variables (Host Time Point and Para Time Point)
n <- boxplot( Epidata$host_time_point, Epidata$para_time_point,
              col = 5:6, notch = F, outline = F, main = 'Representation of Host Time Point & Para Time Point',
              xaxt = 'n', xlab = 'Time Point Cycle', ylab = 'Position')

# adding a name to my axis and creating keypoints for each varibales as legends
axis(side = 1, at = c(1,2), labels = c('Host_TP', 'Para_TP'))
legend('bottomleft', legend = c('Host_TP', 'Para_TP'), col = 5:6, pch = 15)

# Scatterplot Visualization
# visuaizing the para time point and host time point using the scatter point
o <- plot(x = Epidata$host_time_point, y = Epidata$para_time_point, col = 'red',
          pch = 19, xlab = 'Host_TP', ylab = 'Para_TP', main = 'Time Point (Host Vs. Para)')

# Visualization of the Host time point with pie chart at a radius of 1.0
table(Epidata$host_time_point)
Rep <- unique(Epidata$host_time_point)
RCount <- as.vector(table(Epidata$host_time_point))
pie(x = RCount, labels = Rep, radius = 1.0, main = 'Pie chart of Host_TP')

# Visualization of the Para time point with pie chart at a radius of 1.0
table(Epidata$para_time_point)
RepP <- unique(Epidata$para_time_point)
RPCount <- as.vector(table(Epidata$para_time_point))
pie(x = RPCount, labels = Rep, radius = 1.0, main = 'Pie Chart of Para_TP')

# visualizing the dataset using heatmap for Host time point, Para time point & Lifespan
heatmap(as.matrix(Epidata[8:10]), Colv = NA, scale = 'col',
        margins = c(7,7), main = 'Heatmap Plot of Host_TP, Para_TP & Lifespan',
        col = colorRampPalette(brewer.pal(8, 'Dark2'))(25))


par(mfrow = c(1,1))