install.packages("GGally")
library(GGally)
library(ggplot2)

## Descriptive statistics

sparrows<-read.csv("data/sparrows.csv")

boxplot(sparrows[,3:11], las = 2)

# for the next two blocks of codes, 
# you need to have a folder called figs in your 
# RStudio project folder for the workbook.

pdf("figs/sparrows_pairs_plot_by_sex.pdf", width=10)
ggpairs(sparrows, mapping = aes(color=factor(sex), alpha=0.7), columns=3:11)
dev.off()

pdf("figs/sparrows_pairs_plot_by_survive.pdf", width=10)
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

