library(readxl)
airlines <-read_excel('C://Users/PJ/Desktop/Assignmens/clustering/EastWestAirlines.xlsx',sheet = 2)
View(airlines)

###EXPLORATORY DATA ANALYSIS#######################
summary(airlines[,-1])


#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(airlines$Balance)                # [1] 1000
getmode(airlines$Qual_miles)             # [1] 0
getmode(airlines$cc1_miles)              # [1] 1
getmode(airlines$cc2_miles)              # [1] 1
getmode(airlines$cc3_miles)              # [1] 1
getmode(airlines$Bonus_miles)            # [1] 0
getmode(airlines$Bonus_trans)            # [1] 0
getmode(airlines$Flight_miles_12mo)      # [1] 0
getmode(airlines$Flight_trans_12)        # [1] 0
getmode(airlines$Days_since_enroll)      # [1] 8296
getmode(airlines$`Award?`)               # [1] 0


####Meassures of Dispersion####
var(airlines$Balance)                    # [1] 10155734648
var(airlines$Qual_miles)                 # [1] 598555.7
var(airlines$cc1_miles)                  # [1] 1.895907
var(airlines$cc2_miles)                  # [1] 0.0218006
var(airlines$cc3_miles)                  # [1] 0.03811896
var(airlines$Bonus_miles)                # [1] 583269247
var(airlines$Bonus_trans)                # [1] 92.23317
var(airlines$Flight_miles_12mo)          # [1] 1960586
var(airlines$Flight_trans_12)            # [1] 14.38816
var(airlines$Days_since_enroll)          # [1] 4264781
var(airlines$`Award?`)                   # [1] 0.2332473


sd(airlines$Balance)                    # [1] 100775.7
sd(airlines$Qual_miles)                 # [1] 773.6638
sd(airlines$cc1_miles)                  # [1] 1.376919
sd(airlines$cc2_miles)                  # [1] 0.1476503
sd(airlines$cc3_miles)                  # [1] 0.1952408
sd(airlines$Bonus_miles)                # [1] 24150.97
sd(airlines$Bonus_trans)                # [1] 9.60381
sd(airlines$Flight_miles_12mo)          # [1] 1400.209
sd(airlines$Flight_trans_12)            # [1] 3.793172
sd(airlines$Days_since_enroll)          # [1] 2065.135
sd(airlines$`Award?`)                   # [1] 0.4829568



range(airlines$Balance)                    # [1] 1704838
range(airlines$Qual_miles)                 # [1] 0 11148
range(airlines$cc1_miles)                  # [1] 1 5
range(airlines$cc2_miles)                  # [1] 1 3
range(airlines$cc3_miles)                  # [1] 1 5
range(airlines$Bonus_miles)                # [1] 0 263685
range(airlines$Bonus_trans)                # [1] 0 86
range(airlines$Flight_miles_12mo)          # [1] 0 30817
range(airlines$Flight_trans_12)            # [1] 0 53
range(airlines$Days_since_enroll)          # [1] 2 8296
range(airlines$`Award?`)                   # [1] 0 1

###Meassures of skewness######
library(moments)

skewness(airlines$Balance)                    # [1] 5.00231, positively skewed
skewness(airlines$Qual_miles)                 # [1] 7.509577 positively skewed
skewness(airlines$cc1_miles)                  # [1] 0.8572472 positively skewed
skewness(airlines$cc2_miles)                  # [1] 11.20625 positively skewed
skewness(airlines$cc3_miles)                  # [1] 17.18908  positively skewed
skewness(airlines$Bonus_miles)                # [1] 2.841027  positively skewed
skewness(airlines$Bonus_trans)                # [1] 1.156928  positively skewed
skewness(airlines$Flight_miles_12mo)          # [1] 7.448871  positivley skewed
skewness(airlines$Flight_trans_12)            # [1] 5.488402  positively skewed
skewness(airlines$Days_since_enroll)          # [1] 0.1201285 positively skewed
skewness(airlines$`Award?`)                   # [1] 0.5369989 positively skewed

###Meassures of kurtosis##########


kurtosis(airlines$Balance)                    # [1] 47.10124, positive
kurtosis(airlines$Qual_miles)                 # [1] 70.60325  positive
kurtosis(airlines$cc1_miles)                  # [1] 2.250927  positive 
kurtosis(airlines$cc2_miles)                  # [1] 136.6178  positive 
kurtosis(airlines$cc3_miles)                  # [1] 311.2674  positive
kurtosis(airlines$Bonus_miles)                # [1] 16.61195  positive 
kurtosis(airlines$Bonus_trans)                # [1] 5.740805  positive 
kurtosis(airlines$Flight_miles_12mo)          # [1] 97.64108  positive 
kurtosis(airlines$Flight_trans_12)            # [1] 45.92294  positive 
kurtosis(airlines$Days_since_enroll)          # [1] 2.032204 positive  
kurtosis(airlines$`Award?`)                   # [1] 1.288368 positive


#####Graphical Representation#########
boxplot(airlines$Balance,horizontal = T)           #Too many outliers 
boxplot(airlines$Qual_miles,horizontal = T)        #Too many outliers 
boxplot(airlines$cc1_miles,horizontal = T)         # no outliers
boxplot(airlines$cc2_miles,horizontal = T)         #Too many outliers
boxplot(airlines$cc3_miles,horizontal = T)         #Too many outliers
boxplot(airlines$Bonus_miles,horizontal = T)       #Too many outliers
boxplot(airlines$Bonus_trans,horizontal = T)       #Too many outliers
boxplot(airlines$Flight_miles_12mo,horizontal = T) #Too many outliers
boxplot(airlines$Flight_trans_12,horizontal = T)   #Too many outliers
boxplot(airlines$Days_since_enroll,horizontal = T) #NO outliers
boxplot(airlines$`Award?`,horizontal = T)          # No outliers

###Histogram########
hist(airlines$Balance)          
hist(airlines$Qual_miles)         
hist(airlines$cc1_miles)         
hist(airlines$cc2_miles)        
hist(airlines$cc3_miles)         
hist(airlines$Bonus_miles)     
hist(airlines$Bonus_trans)      
hist(airlines$Flight_miles_12mo) 
hist(airlines$Flight_trans_12)   
hist(airlines$Days_since_enroll) 
hist(airlines$`Award?`)

###QQ plot##
qqnorm(airlines$Balance)
qqline(airlines$Balance)

qqnorm(airlines$Qual_miles)
qqline(airlines$Qual_miles)

qqnorm(airlines$cc1_miles)
qqline(airlines$cc1_miles)

qqnorm(airlines$cc2_miles)
qqline(airlines$cc2_miles)

qqnorm(airlines$cc3_miles)
qqline(airlines$cc3_miles)

qqnorm(airlines$Bonus_miles)
qqline(airlines$Bonus_miles)

qqnorm(airlines$Bonus_trans)
qqline(airlines$Bonus_trans)

qqnorm(airlines$Flight_miles_12mo)
qqline(airlines$Flight_miles_12mo)

qqnorm(airlines$Flight_trans_12)
qqline(airlines$Flight_trans_12)

qqnorm(airlines$Days_since_enroll)
qqline(airlines$Days_since_enroll)

qqnorm(airlines$`Award?`)
qqline(airlines$`Award?`)

######################################################################################

##HYRARCHIAL CLUSTERING###

##Normalizing the data to make them unit less &  to normalize
normalized_data <- scale(airlines[,-1])

#calculating the distance matrix
D <- dist(normalized_data,method = 'euclidian')

# clustering
fit <- hclust(D,method = 'complete')
plot(fit,hang = -1) #dendrogram
rect.hclust(fit,k=10,border = 'red')
groups <- cutree(fit,k=10) #cutree

membership<-as.matrix(groups) # groups or cluster numbers

#Final model 
final <- data.frame(airlines, membership)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
