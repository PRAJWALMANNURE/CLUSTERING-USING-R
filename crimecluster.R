#Load the data
crime <- read.csv(file.choose())
View(crime)
crime <- data.frame(crime)

######EXPLORATORY DATA ANALYSIS############
summary(crime)

#mode 
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(crime$Murder)    # [1] 13.2
getmode(crime$Assault)   # [1] 120
getmode(crime$UrbanPop)  # [1] 80
getmode(crime$Rape)      # [1] 16.3

# meassures of dispersion
var(crime$Murder)    # [1] 18.97047
var(crime$Assault)   # [1] 6945.166
var(crime$UrbanPop)  # [1] 209.5188
var(crime$Rape)      # [1] 87.72916

sd(crime$Murder)    # [1] 4.35551
sd(crime$Assault)   # [1] 83.33766
sd(crime$UrbanPop)  # [1] 14.47476
sd(crime$Rape)      # [1] 9.366385

range(crime$Murder)    # [1]  0.8 17.4
range(crime$Assault)   # [1]   45 337
range(crime$UrbanPop)  # [1]   32 91
range(crime$Rape)      # [1]  7.3 46.0

# meassures of skewness
library(moments)

skewness(crime$Murder)    # [1] 0.3820378
skewness(crime$Assault)   # [1] 0.2273179
skewness(crime$UrbanPop)  # [1] -0.2191719
skewness(crime$Rape)      # [1] 0.7769613


#meassures of kurtosis
kurtosis(crime$Murder)    # [1] 2.135329
kurtosis(crime$Assault)   # [1] 1.93098
kurtosis(crime$UrbanPop)  # [1] 2.21579
kurtosis(crime$Rape)      # [1] 3.201898


##Graphical Representation
boxplot(crime$Murder,horizontal = T)    # no outliers
boxplot(crime$Assault,horizontal = T)   # no outliers
boxplot(crime$UrbanPop,horizontal = T)  # no outliers
boxplot(crime$Rape,horizontal = T)      # outliers are present

hist(crime$Murder)    
hist(crime$Assault)   
hist(crime$UrbanPop)
hist(crime$Rape) 

#GG-plot
library(ggpubr)

ggdensity(crime$Murder)
ggdensity(crime$Assault)
ggdensity(crime$UrbanPop) 
ggdensity(crime$Rape)    

#excluding the X coloumn for normalizing
normalized_data <- scale(crime[,(2:5)])

#Distance matrix
d <- dist(normalized_data,method = 'euclidean')

# Hyrarchial clustering
fit <- hclust(d, method = 'complete')

#dendrogram
plot(fit,hang=-1)
rect.hclust(fit,k=4,border = 'red') 
group <- cutree(fit,k=4)
crimegroup <- as.matrix(group)

#final model
f1 <- data.frame(crime,crimegroup)
f2 <- f1[,c(ncol(f1),1:(ncol(f1)-1))]
View(f2)
