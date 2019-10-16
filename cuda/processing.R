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

#data <- read.csv(file="/u/edwardhu/Desktop/all_metrics.csv",header = TRUE, sep = ",")
data_big <- read.csv(file="/home/ed/Desktop/all_big_metrics.csv",header = TRUE, sep = ",")
num_bench <- 32
#data.pr <- princomp(data[1:4, 1:4], cor = FALSE, scores = TRUE)
#num_data <- subset(data, select = -c(1))

#print(data[,!apply(data, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))])
new_d_big <- data_big[,!apply(data_big, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))] # remove duplicates
#new_d.pr <- prcomp(new_d[1:num_bench, 2:ncol(new_d)], center=TRUE, scale = TRUE)
#new_d.pr <- prcomp(new_d[1:num_bench, c("cf_issued", "cf_executed", "inst_issued", "inst_executed","inst_control","flop_count_sp_fma","inst_per_warp","inst_bit_convert","inst_integer","flop_count_sp","shared_store_transactions","inst_fp_32","flop_count_sp_add","inst_executed_shared_stores","flop_count_dp_fma","flop_count_sp_special","inst_executed_tex_ops","texture_load_requests","flop_count_dp","inst_compute_ld_st","stall_not_selected","dram_write_throughput","inst_fp_64","ldst_executed","l2_tex_read_transactions","flop_sp_efficiency","l2_read_transactions","flop_count_dp_mul","ldst_executed","shared_load_transactions","inst_executed_shared_loads","flop_count_sp_mul","tex_cache_transactions","tex_utilization","")], center=TRUE, scale = TRUE)
#final <- subset(new_d, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests))

# remove throughputs
#final <- subset(new_d, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput))

#remove transactions
#final <- subset(new_d, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions))

#final
final_big <- subset(new_d_big, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes))

#final_big <- subset(new_d_big, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes))


final_big.pr <- prcomp(final_big[1:num_bench, 2:ncol(final_big)], center=TRUE, scale = TRUE)

head(final_big.pr$x)
#final_big.hcpc <- HCPC(final_big.pr, graph = FALSE)
#print(new_d[1:num_bench, c("inst_per_warp", "ipc")])
#print(ncol(final_small))
scores_big <- as.data.frame(final_big.pr$x)
summary(final_big.pr)

#plot(new_d.pr$x[,1],new_d.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
#plot(new_d.pr$x[,1], new_d.pr$x[,2])
#biplot(new_d.pr)


#text(new_d.pr$x[,1],new_d.pr$x[,2],labels = data[, 1], pos = 4, offset = 0.5)


#ggbiplot(final.pr, choices = c(1,2), labels = final[, 1], var.axes = F, labels.size = 3)
par3d(windowRect = c(2309, 160, 2808, 594))
plot3d(scores_big[, 1:3], col=(1:1),size=6,type='p')
view3d( theta = -55, phi = 23, zoom=0.78)
par3d("windowRect")
text3d(scores_big[, 1:3], texts=data_big[,1], pos = 3, color="red")
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


fviz_eig(final_big.pr, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(final_big.pr)
#corrplot(var$contrib, is.corr = FALSE, )

fviz_contrib(final_big.pr, choice = "var", axes = 1, top = 35, xtickslab.rt = 65)
fviz_contrib(final_big.pr, choice = "var", axes = 2, top = 35, xtickslab.rt = 65)
fviz_contrib(final_big.pr, choice = "var", axes = 3, top = 35, xtickslab.rt = 65)
fviz_contrib(final_big.pr, choice = "var", axes = 4, top = 35, xtickslab.rt = 65)






# This is the section for extracting utilization values
data <- read.csv(file="/home/ed/Desktop/all_big_metrics.csv",header = TRUE, sep = ",")
num_bench <- 32
data_gg <- data[,-1]
rownames(data_gg) <- data[,1]
#new_d_big <- data_big[,!apply(data_big, MARGIN = 2, function(y) max(y, na.rm = TRUE) == min(y, na.rm = TRUE))] # remove duplicates
#final_big <- subset(new_d_big, select=-c(inst_misc, cf_issued, cf_executed, inst_issued, inst_executed, issue_slots, nvlink_total_data_received, nvlink_total_data_transmitted, pcie_total_data_transmitted, pcie_total_data_received, nvlink_receive_throughput, nvlink_transmit_throughput, stall_other, global_load_requests, local_load_requests, global_store_requests, local_store_requests, global_reduction_requests, texture_load_requests, gld_requested_throughput, gst_requested_throughput, gld_throughput, gst_throughput, dram_read_throughput, dram_write_throughput, tex_cache_throughput, l2_tex_read_throughput, l2_tex_write_throughput, l2_read_throughput, l2_write_throughput, sysmem_write_throughput, local_load_throughput, local_store_throughput, shared_load_throughput, shared_store_throughput, l2_atomic_throughput,                 shared_load_transactions_per_request, shared_store_transactions_per_request,local_store_transactions_per_request,gld_transactions_per_request,gst_transactions_per_request,shared_store_transactions,shared_load_transactions,local_load_transactions,local_store_transactions, gld_transactions, gst_transactions, l2_read_transactions, l2_write_transactions, dram_read_transactions, dram_write_transactions, tex_cache_transactions, atomic_transactions, atomic_transactions_per_request, l2_atomic_transactions, l2_tex_read_transactions, l2_tex_write_transactions, l2_global_load_bytes, l2_local_load_bytes, dram_read_bytes, dram_write_bytes, l2_local_global_store_bytes))
#final_big.pr <- prcomp(final_big[1:num_bench, 2:ncol(final_big)], center=TRUE, scale = TRUE)
res.pca <- PCA(data_gg, ncp = 8, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = FALSE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8,      # Augment the room for labels
          horiz=TRUE
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
data <- read.csv(file="/home/ed/Desktop/Rodinia/cuda/all_metrics.csv",header = TRUE, sep = ",")
data_ff <- data[,-1]
rownames(data_ff) <- data[,1]
zemaitis_feature_on <- subset(data, select = c(benchmark.name, dram_utilization, l2_utilization, shared_utilization, tex_utilization, cf_fu_utilization, ldst_fu_utilization, tex_fu_utilization, special_fu_utilization, single_precision_fu_utilization, double_precision_fu_utilization))

zemaitis_utilization_plot <- melt(zemaitis_feature_on, id.vars = "benchmark.name")

g1 <- ggplot(zemaitis_utilization_plot, aes(x=benchmark.name, y=value, fill=variable)) +
  ylab("Utilization") + theme_gray() +
  geom_bar(stat='identity', position='dodge') + 
  #scale_fill_discrete(name = "", labels = c("DRAM", "L2", "Shared", "Unified Cache", "Control Flow", "Load/Store", "Tex", "Special", "Single P.", "Double P.")) +
  #labs(fill = "") + 
  theme(legend.key.size = unit(0.25, "cm"),
        legend.position="top", legend.box = "horizontal", legend.title = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.text=element_text(size=8),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-10,-10)) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_brewer(palette = "Paired", name = "",
                    labels = c("DRAM", "L2", "Shared", "Unified Cache", "Control Flow", "Load/Store", "Tex", "Special", "Single P.", "Double P."))
ggplot(zemaitis_utilization_plot, aes(x=benchmark.name, y=value, fill=variable)) +
  ylab("Utilization") + theme_gray() +
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


g1grob <- ggplotGrob(g1)
g2grob <- ggplotGrob(g2)

grid.arrange(g1grob, g2grob)


#zemaitis_feature_turned_on <- table(data_ff$dram_utilization, data_ff$l2_utilization, data_ff$shared_utilization, data_ff$tex_utilization, data_ff$cf_fu_utilization, data_ff$ldst_fu_utilization, data_ff$tex_fu_utilization, data_ff$special_fu_utilization, data_ff$single_precision_fu_utilization, data_ff$double_precision_fu_utilization)
#zemaitis_feature_on <- subset(data_ff, select = c(dram_utilization, l2_utilization, shared_utilization, tex_utilization, cf_fu_utilization, ldst_fu_utilization, tex_fu_utilization, special_fu_utilization, single_precision_fu_utilization, double_precision_fu_utilization))
#gplot(zemaitis_feature_on, aes(x=rownames(zemaitis_feature_on), y = value)) + geom_bar(stat="identity", position="dodge") + scale_fill_brewer(palette = "Set1")

#barplot(zemaitis_feature_turned_on, main="Car Distribution by Gears and VS",
#        xlab="Number of Gears", col=c("darkblue","red"),
#        legend = colnames(data_ff), beside=TRUE)
summary(new_d.pr)

