library(ggplot2)
library(bootstrap)
install.packages("GGally")
library(GGally)
library(ggplot2)

#QUESTION 1
#a
meteorology<-read.csv("C:/Users/Pati/OneDrive - Aberystwyth University/STATS/WORKBOOKS/PCA/Datasets/meteorology.csv",row.names=1)
met.pca<-prcomp(meteorology, scale=TRUE)

met.pca$center
met.pca$x
met.pca$rotation
met.pca$sdev

#b
proportions <- met.pca$sdev^2/sum(met.pca$sdev^2)
proportions

#SCREE PLOT
qplot(c(1:5), proportions) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

met.pca.scores.df <- data.frame(met.pca$x)
p1 <- ggplot(met.pca.scores.df, aes(x=PC1, y=PC2)) +
  geom_point() +
  labs(x=paste0("PC1 (", 100*round(proportions[1], 2), "% of variability)"), y=paste0("PC2 (", 100*round(proportions[2], 2), "% of variability)")) + 
  ggtitle("PC2 against PC1")
p1

#QUESTION 2
head(scor)   # head() function shows you the top six rows – useful for getting a sense of the shape of a data set

scor.pca <- prcomp(scor, scale=TRUE)

proportions <- scor.pca$sdev^2/sum(scor.pca$sdev^2)
proportions

#plot 1
scor.pca.scores.df <- data.frame(scor.pca$x)

plot1 <- ggplot(scor.pca.scores.df, aes(x=PC1, y=PC2)) +
  geom_point() +
  labs(x=paste0("PC1 (", 100*round(proportions[1], 2), "% of variability)"), y=paste0("PC2 (", 100*round(proportions[2], 2), "% of variability)")) + 
  ggtitle("PC2 against PC1")

plot1

#plot 2
scor.pca.scores.df <- data.frame(scor.pca$x)

plot2 <- ggplot(scor.pca.scores.df, aes(x=PC1, y=PC3)) +
  geom_point() +
  labs(x=paste0("PC1 (", 100*round(proportions[1], 2), "% of variability)"), y=paste0("PC3 (", 100*round(proportions[3], 2), "% of variability)")) + 
  ggtitle("PC3 against PC1")

plot2

#plot 3
scor.pca.scores.df <- data.frame(scor.pca$x)

plot3 <- ggplot(scor.pca.scores.df, aes(x=PC2, y=PC3)) +
  geom_point() +
  labs(x=paste0("PC2 (", 100*round(proportions[2], 2), "% of variability)"), y=paste0("PC3 (", 100*round(proportions[3], 2), "% of variability)")) + 
  ggtitle("PC3 against PC2")

plot3

#SCREE PLOT
qplot(c(1:5), proportions) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#b
rotation <- scor.pca$rotation
rotation

#c
# e_scaled <- eigen(cov(scor.pca$x, scor.pca$y))
# e_scaled$values
# 
# e_scaled <- eigen(cor(scor))
# e_scaled$values

e_scaled <-scor.pca$sdev^2
e_scaled

p <- 5
k <- 3
n <- 88

# k <- 3
a <- sum(e_scaled[4:5])/(p-k)
a

g <- prod(e_scaled[4:5])^(1/(p-k)) #prod() multiplies 4:5 values 
g

#Number of rows and PC
dim(scor)


h <- (n-((2*p+11)/6))*(p-k)*log(a/g)
h

df <- (p-k+2)*(p-k-1)/2
df

p_sq <- pchisq(h, df, lower.tail = FALSE)
p_sq

# k <- 2
k <- 2

a <- sum(e_scaled[3:5])/(p-k)
a

g <- prod(e_scaled[3:5])^(1/(p-k)) 
g


h <- (n-((2*p+11)/6))*(p-k)*log(a/g)
h

df <- (p-k+2)*(p-k-1)/2
df

p_sq <- pchisq(h, df, lower.tail = FALSE)
p_sq


# k <- 1
k <- 1

a <- sum(e_scaled[2:5])/(p-k)
a

g <- prod(e_scaled[2:5])^(1/(p-k))
g


h <- (n-((2*p+11)/6))*(p-k)*log(a/g)
h

df <- (p-k+2)*(p-k-1)/2
df

p_sq <- pchisq(h, df, lower.tail = FALSE)
p_sq

#QUESTION 3
#Method 1
blood_chem <- read.csv("C:/Users/Pati/OneDrive - Aberystwyth University/STATS/WORKBOOKS/PCA/blood_chem.csv")
head(blood_chem)

dim(blood_chem)

blood_chem.pca<-prcomp(blood_chem, scale=TRUE)

proportions <- blood_chem.pca$sdev^2/sum(blood_chem.pca$sdev^2)
proportions

e_scaled1 <-blood_chem.pca$sdev^2
e_scaled1

cumsum(proportions)   #cumulative sum

#Method 2

var <- sqrt(blood_chem.pca$sdev)
var

diag(var(blood_chem.pca$x)) #variance of PCAs themselves 

#Method 3
#SCREE PLOT
qplot(c(1:8), proportions) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#QUESTION 5
# Descriptive statistics
sparrows <- read.csv("C:/Users/Pati/OneDrive - Aberystwyth University/STATS/WORKBOOKS/PCA/sparrows.csv")

boxplot(sparrows[,3:11], las = 2)

# for the next two blocks of codes, 
# you need to have a folder called figs in your 
# RStudio project folder for the workbook.

pdf("C:/Users/Pati/OneDrive - Aberystwyth University/STATS/WORKBOOKS/PCA/Figures/sparrows_pairs_plot_by_sex.pdf", width=10)
ggpairs(sparrows, mapping = aes(color=factor(sex), alpha=0.7), columns=3:11)
dev.off()

pdf("C:/Users/Pati/OneDrive - Aberystwyth University/STATS/WORKBOOKS/PCA/Figures/sparrows_pairs_plot_by_survive.pdf", width=10)
ggpairs(sparrows, mapping = aes(color=factor(survive), alpha=0.7), columns=3:11)
dev.off()

# Some more code that will help with the preliminary analysis.
apply(sparrows[,3:11],2,summary)
apply(sparrows[,3:11],2,sd)
table(sparrows[,1:2])


## PCA based on correlation matrix

sparrows.pca<-prcomp(sparrows[,3:11],scale=TRUE,retx=TRUE)
summary(sparrows.pca)
screeplot(sparrows.pca,main="",type="lines")

proportions<-sparrows.pca$sdev^2/sum(sparrows.pca$sdev^2)
proportions


#SCREE PLOT
qplot(c(1:9), proportions) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


cumsum(proportions)

loadings<-sparrows.pca$rotation
round(loadings[,1:5],3)

sparrows_scores.df <- data.frame(sparrows.pca$x)
sparrows_scores.df$sex <- sparrows$sex
sparrows_scores.df$survive <- sparrows$survive

# A sample plot that might be useful.
ggplot(sparrows_scores.df, 
       aes(x=PC1, y=PC2, col=factor(survive))) +
  geom_point() + 
  labs(col = "survive")

# Remember to include proportions of variability 
# explained by PCs and appropriate axis labels and title

sparrows.pca_scores.df <- data.frame(sparrows.pca$x)
sparrows.pca_scores.df$survive <- sparrows$survive
ggplot(sparrows.pca_scores.df, 
       aes(x=PC1, y=PC2, col=factor(survive))) +
  geom_point() +
  labs(x=paste0("PC1 (", 100*round(proportions[1], 2), "% of variability)"), y=paste0("PC2 (", 100*round(proportions[2], 2), "% of variability)") ,col = "survived") +
  ggtitle("PC2 against PC1")


var1 <- sqrt(sparrows.pca$sdev)
var1

diag(var(sparrows.pca$x))
