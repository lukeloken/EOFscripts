

# ############################################
# Plot soil data for 2016
# ###########################################

soil_df <- readRDS(file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_Spring.rds')))


soil_0_15 <- readRDS(file=file_out(file.path(path_to_data, 'soil', 'cleaned_data', 'Soil2016_0_to_15cm.rds')))



soilvars <- names(soil_df3)[5:ncol(soil_df3)]

i=1
soil_dots_list <- list()
soil_box_list <- list()

for (i in 1:length(soilvars)){
  soil_dots_list[[i]] <- ggplot(data=soil_df3, 
                                aes_string(x="Site", y=soilvars[i], group="Depth")) +
    geom_jitter(aes(col=Depth), width=.15,height=0, size=1.5) +
    # geom_point(aes(col=Depth)) +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position='none') +
    facet_wrap(~Manure, scales='free_x')
  
  # soil_box_list[[i]] <- ggplot(data=soil_df3, 
  #                             aes_string(x="Manure", y=soilvars[i])) +
  # geom_jitter(aes(col=Manure), alpha=.5, width=.2, height=0) + 
  # geom_boxplot(aes(fill=Manure), alpha=.3, width=.7, outlier.shape = NA) +
  # theme_bw() +
  # theme(axis.title.x = element_blank(), legend.position='none') 
  
  soil_box_list[[i]] <- ggplot(data=soil_df3, 
                               aes_string(x="Manure", y=soilvars[i], fill='Depth', col='Depth')) +
    geom_point(position=position_jitterdodge(dodge.width=.5), alpha=.5) + 
    geom_boxplot(alpha=.3, width=.5, outlier.shape = NA) +
    theme_bw() +
    theme(axis.title.x = element_blank(), legend.position='none') 
  
}

plot_withlegend <- soil_dots_list[[2]] + 
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

mylegend<-g_legend(plot_withlegend)

rm(plot_withlegend)

plot_grid <- grid.arrange(grobs=soil_dots_list, ncol=4, as.table = F)

soil_dots_list[[length(soil_dots_list)+1]] <- mylegend

png(file_out(file.path(path_to_results, "Figures", "Soil", "Spring2016.png")), res=400, width=12, height=10, units='in')

grid.arrange(grobs=soil_dots_list, ncol=4, as.table = F)

# grid.arrange(plot_grid, mylegend, nrow=2, heights=c(15,1))

dev.off()





#Boxplots
plot_withlegend <- soil_box_list[[2]] + 
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) +
  labs(fill = 'Soil depth (cm)', color = 'Soil depth (cm)')

mylegend<-g_legend(plot_withlegend)

rm(plot_withlegend)

box_grid <- grid.arrange(grobs=soil_box_list, ncol=6, as.table = F)

soil_box_list[[length(soil_box_list)+1]] <- mylegend

png(file_out(file.path(path_to_results, "Figures", "Soil", "Boxplots_Spring2016.png")), res=400, width=12, height=8, units='in')

# grid.arrange(grobs=soil_box_list, ncol=5, as.table = F)

grid.arrange(box_grid, mylegend, nrow=2, heights=c(15,1))

dev.off()



###################################
#Mostly copy and paste from old code
#PCA
# ##################################

pca_df<-soil_df3[,c("Site", "Depth", "Manure", soilvars)]
Manure_temp <- factor(pca_df$Manure, c('Manure', "No Manure"))
Manure_temp <- as.numeric(Manure_temp)
Manure_temp[which(Manure_temp == 2)] <- 0
pca_df$Manure_binary <- Manure_temp

# Subset to 0 to 15 cm depth and include infiltration data
# create pca object 
# pca_df_0_15 <- soil_joined %>%
#   filter(Depth == "0-15")

pca_df_0_15 <- soil_0_15

Manure_temp2 <- factor(pca_df_0_15$Manure, c('Manure', "No Manure"))
Manure_temp2 <- as.numeric(Manure_temp2)
Manure_temp2[which(Manure_temp2 == 2)] <- 0
pca_df_0_15$Manure_binary <- Manure_temp2


pca_df_0_15 <- pca_df_0_15[colSums(!is.na(pca_df_0_15)) > 0] %>%
  group_by() %>%
  na.omit()

#For big analysis remove rows with any NAs
pca_df<-na.omit(pca_df)
pca_df2 <- select(pca_df, -Site, -Manure, -Depth)

#Make PCAs
pca <- prcomp(pca_df2, center = TRUE, scale. = TRUE, rank=6) 

pca_0_15 <- pca_df_0_15 %>%
  dplyr::select(-Manure, -Depth, -Site, -Date, -wateryear) %>%
  prcomp(center = TRUE, scale. = TRUE, rank=6) 

rownames(pca_df_0_15) <- pca_df_0_15$Site



plot(pca, type='l')
abline(h=1)
summary(pca)
pca
PerVar<-(pca$sdev^2)/sum((pca$sdev^2))*100
CumVar<-cumsum(PerVar)
plot(CumVar, ylab='Cumulative variance (%)', type='o', ylim=c(0,100))


png(file_out(file.path(path_to_results, "Figures", "Soil", "SoilPCA_Spring2016.png")), res=400, width=7, height=7, units='in')

par(mfrow=c(1,2))
par(mar=c(2.5,3.5,.5,0), oma=c(.5,0,0,0))
par(mgp=c(2, .5, 0))

plot(pca, type='l', main='')
abline(h=1, lty=3)
box(which='plot')
mtext('PCA #',1,2)

#corr plot
corrange <- c(NA, NA)
corrange[1] <- floor(min(pca$rotation)*10)/10
corrange[2] <- ceiling(max(pca$rotation)*10)/10
if (mean(corrange) != 0) {
  corrange[which.min(abs(corrange))] <- corrange[which.max(abs(corrange))]*(-1)
}
corrplot(pca$rotation, is.corr=FALSE, mar=c(0,0,0,1.5), oma=c(0,0,0,0), tl.col='black', cl.pos='r', cl.ratio=0.5, col=brewer.pal(10, 'RdYlBu'), cl.lim=corrange)

mtext('Correlation', 4, -2)
mtext('Predictor variable', 2, 1.5)

# legend(-7,par('usr')[4], c('Predictor variable'), bty='n')

dev.off()


res <- cor(pca_df2)
round(res, 2)

png(file_out(file.path(path_to_results, "Figures", "Soil", "SoilCorrelation_Spring2016.png")), res=400, width=7, height=7, units='in')

par(mfrow=c(1,1))
par(mar=c(2.5,3.5,.5,0), oma=c(.5,0,0,0))
par(mgp=c(2, .5, 0))


corrplot(res, type='upper', cl.lim=c(-1, 1))

dev.off()


png(file_out(file.path(path_to_results, "Figures", "Soil", "SoilCorrelation2_Spring2016.png")), res=400, width=7, height=7, units='in')

par(mfrow=c(1,1))
par(mar=c(2.5,3.5,.5,0), oma=c(.5,0,0,0))
par(mgp=c(2, .5, 0))

chart.Correlation(pca_df2, histogram=TRUE, pch=19)

dev.off()



# pca <- prcomp(pca_df, center = TRUE, scale. = TRUE, rank=5) 


#plot axes 1 and 2 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v1.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_1v2<-autoplot(pca, x=2, y=1, data=pca_df, size=3, colour = "Manure", shape = 'Depth',
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  scale_shape_manual(values=c(1,16)) + 
  theme_bw() + 
  theme(legend.position='none', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(p_1v2)  

dev.off()


#plot axes 1 and 3 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v2.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_3v2<-autoplot(pca, x=3, y=1, data=pca_df, size=3, colour = "Manure", shape = 'Depth',
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  scale_shape_manual(values=c(1,16)) + 
  theme_bw() + 
  theme(legend.position='bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p_3v2)  

dev.off()


#plot first three axes of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v3.png")), res=400, width=5, height=10, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

grid.arrange(p_1v2, p_3v2, ncol=2)

grid.newpage()
grid.draw(rbind(ggplotGrob(p_1v2), ggplotGrob(p_3v2), size = "last"))

dev.off()





#Depth = 0 to 15 cm

png(file_out(file.path(path_to_results, "Figures", "Soil", "SoilPCA_0_15cm.png")), res=400, width=7, height=7, units='in')

par(mfrow=c(1,2))
par(mar=c(2.5,3.5,.5,0), oma=c(.5,0,1.5,0))
par(mgp=c(2, .5, 0))

plot(pca_0_15, type='l', main='')
abline(h=1, lty=3)
box(which='plot')
mtext('PCA #',1,2)

#corr plot
corrange <- c(NA, NA)
corrange[1] <- floor(min(pca_0_15$rotation)*10)/10
corrange[2] <- ceiling(max(pca_0_15$rotation)*10)/10
if (mean(corrange) != 0) {
  corrange[which.min(abs(corrange))] <- corrange[which.max(abs(corrange))]*(-1)
}
corrplot(pca_0_15$rotation, is.corr=FALSE, mar=c(0,0,0,1.5), oma=c(0,0,0,0), tl.col='black', cl.pos='r', cl.ratio=0.5, col=brewer.pal(10, 'RdYlBu'), cl.lim=corrange)

mtext('Correlation', 4, -2)
mtext('Predictor variable', 2, 1.5)
mtext('0 to 15 cm depth', 3, 0, outer=T)

# legend(-7,par('usr')[4], c('Predictor variable'), bty='n')

dev.off()






#plot axes 1 and 2 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_v1_0_15.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_1v2<-autoplot(pca_0_15, x=2, y=1, data=pca_df_0_15, size=3, colour = "Manure",
                label=TRUE, label.size=4, shape=FALSE,
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  scale_shape_manual(values=c(1,16)) + 
  theme_bw() + 
  theme(legend.position='none', 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

print(p_1v2)  

dev.off()



#plot axes 1 and 3 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_v2_0_15.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


# p_3v2<-autoplot(pca_0_15, x=3, y=1, data=pca_df_0_15, size=3, colour = "Manure",
#                 loadings = TRUE, loadings.colour = 'grey', 
#                 loadings.label = TRUE, loadings.label.colour='black', 
#                 loadings.label.size = 3) + 
#   scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
#   scale_shape_manual(values=c(1,16)) + 
#   theme_bw() + 
#   theme(legend.position='bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_3v2<-autoplot(pca_0_15, x=3, y=1, data=pca_df_0_15, size=3, colour = "Manure", 
                label=TRUE, label.size=4, shape=FALSE,
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3, labels=TRUE) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  scale_shape_manual(values=c(1,16)) + 
  theme_bw() + 
  theme(legend.position='bottom', panel.grid.major = element_blank(), panel.grid.minor = element_blank())


print(p_3v2)  

dev.off()


#plot first three axes of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_v3_0_15.png")), res=400, width=5, height=10, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

grid.arrange(p_1v2, p_3v2, ncol=2)

grid.newpage()
grid.draw(rbind(ggplotGrob(p_1v2), ggplotGrob(p_3v2), size = "last"))

dev.off()




