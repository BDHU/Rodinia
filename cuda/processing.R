#This might be for SHOC

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
install.packages("rgl")
library(rgl)
install.packages("plot3D")
library("plot3D")

install.packages(c("FactoMineR", "factoextra"))
library("factoextra")
library(FactoMineR)

install.packages("corrplot")
library(corrplot)
library(data.table)


data_small <- read.csv(file="/home/ed/Desktop/SHOC/src/cuda/all_small_metrics.csv",header = TRUE, sep = ",")
data_big <- read.csv(file="/home/ed/Desktop/SHOC/src/cuda/all_big_metrics.csv",header = TRUE, sep = ",")
num_bench <- 14

new_d_small <- data_small[,!apply(data_small, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
new_d_big <- data_big[,!apply(data_big, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))]


#final
final_big <- subset(new_d_big, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes, local_memory_overhead, flop_dp_efficiency))
final_small <- subset(new_d_small, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes))


final_small.pr <- prcomp(final_small[1:num_bench, 2:ncol(final_small)], center=TRUE, scale = TRUE)
final_big.pr <- prcomp(final_big[1:num_bench, 2:ncol(final_big)], center=TRUE, scale = TRUE)
#final_big <- subset(new_d_big, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes))

scores_small <- as.data.frame(final_small.pr$x)
scores_big <- as.data.frame(final_big.pr$x)

finalbig2 <- final_big[,-1]
rownames(finalbig2) <- final_big[,1]
finalsmall2 <- final_small[,-1]
rownames(finalsmall2) <- final_small[,1]
require("ggrepel")
#binding

df1 <- data.frame(PC1=final_small.pr$x[,1], PC2=final_small.pr$x[,2])
df2 <- data.frame(PC1=final_big.pr$x[,1], PC2=final_big.pr$x[,2])
df1$group=1
df2$group=2
df = rbind(df1, df2)


chulls <- ddply(df, .(group), function(df) df[chull(df$PC1, df$PC2), ])

g <- ggplot(df, aes(x=PC1, y=PC2,group=as.factor(group), color=as.factor(group),fill=as.factor(group),shape=as.factor(group)))+theme_light() + theme(legend.position="none")
g <-g +
  geom_polygon(show.legend = F, data = chulls,aes(x=PC1,y=PC2,fill=as.factor(group),color=NA), alpha = 0.5) + theme(legend.position="none")

g <- g + geom_point(show.legend = F)+
  scale_fill_manual(values=c("#999999", "#56B4E9"))+
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.title = element_blank(),
        legend.box.background = element_rect(colour = "gray"),
        legend.position = c(.85, .15),
        legend.box.margin = margin(1, 1, 1, 1))+
  annotate("text", x = -4, y = -7, label = "md5hash")+
  annotate("text", x = 9, y = -2, label = "md")+
  annotate("text", x = 0, y = 5, label = "qtclustering")+
  annotate("text", x = -5, y = 1.2, label = "neuralnet")+
  annotate("text", x = -10, y = 4.5, label = "qtclustering", color='#56B4E9')+
  annotate("text", x = 5.5, y = 10, label = "gemm", color='#56B4E9')+
  annotate("text", x = 3, y = -1.6, label = "fft", color='#56B4E9')+
  annotate("text", x = -1, y = -3, label = "reduction", color='#56B4E9')

g
autoplot(final_small.pr, frame = TRUE) + theme_light()+ geom_text_repel(aes(label=rownames(finalsmall2)), size = 3.6) #+ geom_point(data=final_big.pr,colour='red')# 
#ggplot(final_small.pr) +
#  geom_point(aes(x=final_small.pr$x[,1], y=final_small.pr$x[,2])) +geom_text_repel(aes(x=final_small.pr$x[,1], y=final_small.pr$x[,2],label=rownames(finalsmall2)), size = 3.6)
 # geom_point(aes(x=final_big.pr$x[,1], y=final_big.pr$x[,2]), color='red')
#ggplot(final_big.pr)
autoplot(final_big.pr, frame = TRUE) + theme_light() + geom_text_repel(aes(label=rownames(finalbig2)), size = 3.6)
#final_big.hcpc <- HCPC(final_big.pr, graph = FALSE)
#print(new_d[1:num_bench, c("inst_per_warp", "ipc")])
#print(ncol(final_small))
par3d(windowRect = c(2309, 160, 2808, 594))
plot3d(scores_small[, 1:3], col=c(1:1),size=6,type='p')

plot3d(scores_big[, 1:3], col=(2:2),size=6,type='p',add=TRUE)
#plot(new_d.pr$x[,1],new_d.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
#plot(new_d.pr$x[,1], new_d.pr$x[,2])
#biplot(new_d.pr)


#text(new_d.pr$x[,1],new_d.pr$x[,2],labels = data[, 1], pos = 4, offset = 0.5)


#ggbiplot(final.pr, choices = c(1,2), labels = final[, 1], var.axes = F, labels.size = 3)
view3d( theta = -55, phi = 23, zoom=0.78)
par3d("windowRect")
first <- c(1,2,3,4,5,6,8,9,12,13,17,18,19,20,21,22,23,24,25,27,28,16,30,31,32,33)
second <- c(2,9,15)
text3d(scores_small[, 1:3], texts=data_small[,1], pos = 3)
text3d(scores_big, texts=data_big[second,1], pos = 3, color="red")
#score_to_disp <- subset(scores, select=-c(1))
#label_to_disp <- data[-c(1),]
#text3d(scores[-c(1,3,4,5,10,20,22,23),1:3],texts=data[-c(1,3,4,5,10,20,22,23),1], pos=3)
#rgl.postscript("pca_shoc_small.eps", "eps")
#rgl.postscript("pca_shoc_big.pdf", "pdf")
rgl.postscript("altis_pca.eps", "eps")
#text3d(scores[1:10,1:3],texts=data[1:10, 1], pos=3)
#text3d(scores[,1]+2, scores[,2]+10, scores[,3]+2,texts=final[1, ])
#print(data[,1])
#scatter3D(scores[,1], scores[,2], scores[,3], phi = 1, bty = "g", pch = 10, cex = 0.5)
#text3D(scores[,1], scores[,2], scores[,3], labels = data[, 1],
#add = TRUE, colkey = TRUE, cex = 0.7)

library(cowplot)
fviz_eig(final_big.pr, addlabels = TRUE, ylim = c(0, 25))

var <- get_pca_var(final_big.pr)
#corrplot(var$contrib, is.corr = FALSE, )
par(mfrow=c(1,2))
fviz_contrib(final_big.pr, choice = "var",fill = "lightgray", color = "black", axes = 1:2, top = 20,xtickslab.rt = 90,ggtheme = theme_minimal()) + theme(axis.title.x=element_text(size=10))
#  theme(axis.text.x = element_text(angle=45))
fviz_contrib(final_big.pr, choice = "var", fill = "lightgray", color = "black", axes = 3:4, top = 10, xtickslab.rt = 90, ggtheme = theme_minimal())
plot_grid(dim12, dim34, labels = "")
#fviz_contrib(final_big.pr, choice = "var", axes = 5:6, top = 10,xtickslab.rt = 65)
#fviz_contrib(final_big.pr, choice = "var", axes = 4, top = 10, xtickslab.rt = 65)






# This is the section for extracting utilization values
#data <- read.csv(file="/home/ed/Desktop/Rodinia/cuda/all_metrics.csv",header = TRUE, sep = ",")
data <- read.csv(file="/home/ed/Documents/all_big_metrics.csv",header = TRUE, sep = ",")
data2 <- read.csv(file="/home/ed/Desktop/SHOC/src/cuda/all_big_metrics.csv",header = TRUE, sep = ",")
num_bench <- 33

data <- data[,!apply(data, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))] # remove duplicates
data2 <- data2[,!apply(data, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))] # remove duplicates

final_big <- subset(data, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes, sysmem_write_transactions, sysmem_write_bytes, inst_executed_global_atomics, sysmem_read_throughput,sysmem_read_transactions,sysmem_read_utilization,sysmem_read_bytes,sysmem_read_throughput, sysmem_read_transactions, local_memory_overhead, flop_dp_efficiency))
#Rodinia#
#final_big <- subset(data, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes, local_memory_overhead, flop_dp_efficiency))
#SHOC 
#final_big <- subset(data, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes, local_memory_overhead, flop_dp_efficiency))
final_big2 <- subset(data2, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes, local_memory_overhead, flop_dp_efficiency))

#final_big <- subset(data, select=c(dram_utilization, l2_utilization, shared_utilization, tex_utilization, cf_fu_utilization, ldst_fu_utilization, tex_fu_utilization, special_fu_utilization, single_precision_fu_utilization, double_fu_precision_utilization))

data_gg <- final_big[,-1]
data_gg2 <- final_big2[,-1]
rownames(data_gg) <- final_big[,1]
rownames(data_gg2) <- final_big2[,1]
t_mtcars <- transpose(data_gg)
t_mtcars2 <- transpose(data_gg2)

#M<-cor(t_mtcars)
#corrplot(M, method="circle")

# get row and colnames in order
colnames(t_mtcars) <- rownames(data_gg)
colnames(t_mtcars2) <- rownames(data_gg2)
rownames(t_mtcars) <- colnames(data_gg)
rownames(t_mtcars2) <- colnames(data_gg2)
cormat <- round(cor(t_mtcars),2)
cormat2 <- round(cor(t_mtcars2),2)
head(cormat)
#melted_cormat <- melt(cormat)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_lower_tri2<-function(cormat2){
  cormat[upper.tri(cormat2)] <- NA
  return(cormat2)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
get_upper_tri2 <- function(cormat2){
  cormat[lower.tri(cormat2)]<- NA
  return(cormat2)
}

upper_tri <- get_upper_tri(cormat)
upper_tri2 <- get_upper_tri(cormat2)

melted_cormat <- melt(upper_tri, na.rm = TRUE)
melted_cormat2 <- melt(upper_tri2, na.rm = TRUE)
require(gridExtra)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white",show.legend = TRUE)+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_gray()+ 
  theme(axis.text.x = element_text(hjust=1,angle = 90, 
                                   size=7.5),
        axis.text.y = element_text( vjust=1,
                                   size=7.5 ,hjust = 1),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0))+
  coord_fixed() + theme(legend.title = element_blank()) + theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +   theme(legend.key.size = unit(0.25, "cm"),
                                                                              legend.position="right", legend.box = "horizontal", legend.title = element_blank()) 

plot2<-ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white",show.legend = FALSE)+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size=7.5 ,hjust = 1), axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y = element_text(vjust = 1, 
                                   size=7.5 ,hjust = 1)) +
  coord_fixed()+theme(plot.margin = unit(c(0,0,0,0), "cm"))


grid.arrange(plot1, plot2, ncol=2)


ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#final_big.pr <- prcomp(final_big[1:num_bench, 2:ncol(final_big)], center=TRUE, scale = TRUE)
res.pca <- PCA(data_gg, ncp = 8, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          #palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = FALSE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          #labels_track_height = 0.8,      # Augment the room for labels
          horiz=TRUE,
          color_labels_by_k = TRUE
)
head(res.pca$ind$coord)
#cleanup <- data[,!apply(data, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))] # remove duplicates

x <- c(5,17,31,9,17,10,30,28,16,29,14,34)
y <- c(1,2,3,4,5,6,7,8,9,10,11,12)
day <- c(1,2,3,4,5,6,7,8,9,10,11,12)


df1 <- data.frame(x, y, day)


install.packages("ggthemes")
devtools::install_github("baptiste/egg")
library(ggplot2)
library(reshape2)
library(ggthemes)
data <- read.csv(file="/home/ed/Documents/all_big_metrics.csv",header = TRUE, sep = ",")
data_ff <- data[,-1]
which(names(data)=="benchmark.name")
rownames(data_ff) <- data[,1]
zemaitis_feature_on <- subset(data, select = c(benchmark.name, achieved_occupancy))
ncol(zemaitis_feature_on)
# change for 1080
#zemaitis_feature_on[9, "l2_utilization"] <- 1
#zemaitis_feature_on[9, "shared_utilization"] <- 1
#zemaitis_feature_on[9, "tex_utilization"] <- 1
#zemaitis_feature_on[2, "double_precision_fu_utilization"] <- 10
# change for m60
#zemaitis_feature_on[9, "l2_utilization"] <- 1
#zemaitis_feature_on[9, "shared_utilization"] <- 1
#zemaitis_feature_on[9, "tex_utilization"] <- 1

#insert mummergpu
#zemaitis_feature_on <- rbind(zemaitis_feature_on, mummergpu)
zemaitis_utilization_plot <- melt(zemaitis_feature_on, id.vars = "benchmark.name")

ggplot(zemaitis_utilization_plot, aes(x=benchmark.name, y=value, fill=variable)) +
  ylab("eligible warps per cycle") + theme_minimal() +
  geom_bar(stat='identity', position='dodge') + 
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1),
        #legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm")) +
  scale_fill_brewer(palette = "Paired", name = "")


ggplot(zemaitis_utilization_plot, aes(x=benchmark.name, y=value, fill=variable)) +
  ylab("achieved Occupancy") + theme_gray() +
  geom_bar(stat='identity', position='dodge') + 
  #scale_fill_discrete(name = "", labels = c("DRAM", "L2", "Shared", "Unified Cache", "Control Flow", "Load/Store", "Tex", "Special", "Single P.", "Double P.")) +
  #labs(fill = "") + 
  theme(legend.key.size = unit(0.25, "cm"),
        legend.position="top", legend.box = "horizontal", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-10,-10)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm")) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_brewer(palette = "Paired", name = "",
                    labels = c("DRAM", "L2", "Shared", "Unified Cache", "Control Flow", "Load/Store", "Tex", "Special", "Single P.", "Double P."))




#zemaitis_feature_turned_on <- table(data_ff$dram_utilization, data_ff$l2_utilization, data_ff$shared_utilization, data_ff$tex_utilization, data_ff$cf_fu_utilization, data_ff$ldst_fu_utilization, data_ff$tex_fu_utilization, data_ff$special_fu_utilization, data_ff$single_precision_fu_utilization, data_ff$double_precision_fu_utilization)
#zemaitis_feature_on <- subset(data_ff, select = c(dram_utilization, l2_utilization, shared_utilization, tex_utilization, cf_fu_utilization, ldst_fu_utilization, tex_fu_utilization, special_fu_utilization, single_precision_fu_utilization, double_precision_fu_utilization))
#gplot(zemaitis_feature_on, aes(x=rownames(zemaitis_feature_on), y = value)) + geom_bar(stat="identity", position="dodge") + scale_fill_brewer(palette = "Set1")

#barplot(zemaitis_feature_turned_on, main="Car Distribution by Gears and VS",
#        xlab="Number of Gears", col=c("darkblue","red"),
#        legend = colnames(data_ff), beside=TRUE)
summary(new_d.pr)

