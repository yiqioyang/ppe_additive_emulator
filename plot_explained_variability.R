library(RColorBrewer)
library(ggplot2)
rm(list = ls())

myPallette <-
  c(rev(colorRampPalette(brewer.pal(9,"YlGnBu")[3:9])(11)),"khaki1",
    colorRampPalette(brewer.pal(9, "YlOrRd")[3:9])(11))

##################################################
res_dir = "~/Documents/code_proj/ppesage-main/"
setwd(res_dir)
inp = read.table("./data/cam6_parameter.csv", sep = ",", header=TRUE)





case_name = "SWCF_so_stratocumulus_djf"
setwd(file.path(res_dir, "cases", case_name))
out_nm = list.dirs(".",full.names = FALSE)
var_name = out_nm[-1]
#target_climatologies = c("variable1", "variable2", "variable4", "variable6", "variable9")
##################################################
explained_var_mat = data.frame(matrix(nrow = 1, ncol = 12))
colnames(explained_var_mat) = c("inp1", "inp2", "inp3", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "sd_diff", "var")

for(var_ind in 1:length(var_name)){
  var = var_name[var_ind]
  temp_file = file.path(var, "sd_reduce_optimized.csv")
  temp_exp_sd = read.table(temp_file, sep = ",", header=TRUE)
  temp_exp_sd = cbind(temp_exp_sd, var)
  
  explained_var_mat = rbind(explained_var_mat, temp_exp_sd)
}

explained_var_mat = explained_var_mat[-1,]
##################################################

explained_var_mat_single = subset(explained_var_mat, is.na(V2))
explained_var_mat_single$x_label = explained_var_mat_single$inp1

explained_var_mat_pair = subset(explained_var_mat, !is.na(V2) & sd_diff > 0.01)
explained_var_mat_pair$x_label = paste(explained_var_mat_pair$inp1, explained_var_mat_pair$inp2, sep = "----")

explain_var_plot = rbind(explained_var_mat_single, explained_var_mat_pair)

explain_var_plot_aggre = aggregate(explain_var_plot$sd_diff, by = list(explain_var_plot$x_label, explain_var_plot$var), sum)


x_label = c(colnames(inp), unique(explained_var_mat_pair$x_label))
x_label = factor(x_label , levels = x_label)

explain_var_plot_aggre = subset(explain_var_plot_aggre, Group.2 %in% unique(explain_var_plot_aggre$Group.2))
x_label = x_label[x_label!= "Sample_nmb"]
#####################
ggplot() + 
  geom_point(data = subset(explain_var_plot_aggre, x > 0.05), 
             aes(y = Group.1, x = Group.2, fill = x, shape = "> 0.05"), size = 4) + 
  geom_point(data = subset(explain_var_plot_aggre, x <= 0.05 & x > 0.02), 
             aes(y = Group.1, x = Group.2, fill = x, shape = "0.02-0.05"), size = 4, fill = NA) + 
  
  geom_point(data = subset(explain_var_plot_aggre, x <= 0.02 & x > 0), 
             aes(y = Group.1, x = Group.2, fill = x, shape = "0-0.02"), size = 4, fill = NA) + 
  
  geom_point(data = subset(explain_var_plot_aggre, x <= 0), 
             aes(y = Group.1, x = Group.2, shape = "< 0"), size = 4) + 
  
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5)) + scale_y_discrete(limits = x_label)+
  scale_fill_gradientn(colors = myPallette[1:21], limits = c(0.049999, 0.4)) + xlab("") + ylab("") + labs(fill = "Explained variability") +
  scale_shape_manual(values = c("> 0.05"=21, "0.02-0.05" = 22, "0-0.02" = 23, "< 0" = 4), labs(shape = "By category"))

#####################
#explain_var_plot_aggre$ev = explain_var_plot_aggre$x


x_label_col = x_label[x_label %in% unique(explain_var_plot_aggre$Group.1)]

filtered_explain_var_plot_aggre <- explain_var_plot_aggre %>%
  group_by(Group.1) %>%
  filter(max(x) > 0.04)




colors = c("#DC143C","#4169E1","#F0E68C","#228B22","#DAA520","#9932CC","#00BFFF","#8B4513")


ggplot() + 
  geom_col(data = filtered_explain_var_plot_aggre,
           aes(x = Group.1, y = x, fill = Group.2), width = 0.8, position = position_dodge2(width = 1.5, preserve = "single")) + 
  ylim(c(-0.03,0.35))+ xlab("") + ylab("Explained variability") + theme_bw() +
  labs(fill = "Climatologies") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5, size = 12), 
        legend.position = "none")  +
        #legend.text = element_text(size = 14))
        scale_fill_manual(values =colors)



        
      
  
        
