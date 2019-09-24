install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

install.packages(c("FactoMineR", "factoextra"))
library("factoextra")

install.packages("corrplot")
library(corrplot)

#data <- read.csv(file="/u/edwardhu/Desktop/all_metrics.csv",header = TRUE, sep = ",")
data <- read.csv(file="/home/ed/Desktop/Rodinia/cuda/all_metrics.csv",header = TRUE, sep = ",")
num_bench <- 23
#data.pr <- princomp(data[1:4, 1:4], cor = FALSE, scores = TRUE)
#num_data <- subset(data, select = -c(1))
print(data[,!apply(data, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))])
new_d <- data[,!apply(data, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
new_d.pr <- prcomp(new_d[1:num_bench, 2:ncol(new_d)], center=TRUE, scale = TRUE)
print(new_d[1:num_bench, 2:ncol(new_d)])


#plot(new_d.pr$x[,1],new_d.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
#plot(new_d.pr$x[,1], new_d.pr$x[,2])
#biplot(new_d.pr)


#text(new_d.pr$x[,1],new_d.pr$x[,2],labels = data[, 1], pos = 4, offset = 0.5)


ggbiplot(new_d.pr, choices = c(1,2), labels = new_d[, 1], var.axes = F, labels.size = 3)
fviz_eig(new_d.pr, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(new_d.pr)
#corrplot(var$contrib, is.corr = FALSE, )

fviz_contrib(new_d.pr, choice = "var", axes = 2, top = 50)


summary(new_d.pr)
