
#load soil quality/health data.
#Input is one or more csv files from UW-Green Bay and Purdue


soil_df <- read.csv(file_in(file.path(path_to_data, 'soil', 'raw_data', "EoF_SoilData_2016-2017.csv")), header=T, stringsAsFactors = F)

#When more than one file is included, add code here

#First file is missing dates
#These are for the Spring 2016 data only
soil_dates<-data.frame(Site = c("WI-01", "WI-02", "WI-03", "WI-04", "WI-05", "OH-01", "MI-01", "MI-02", "IN-01", "IN-02", "NY-01", "NY-02", "NY-03", "NY-04" ), 
                       Date = as.Date(c("2016-06-21", "2016-06-21", "2016-06-21", "2016-06-22", "2016-06-22", "2016-06-14", "2016-06-14", "2016-06-14", "2016-06-13", "2016-06-13", "2016-06-16", "2016-06-16", "2016-06-16", "2016-06-16" ))) 
soil_dates$Type <- rep('Spring', nrow(soil_dates))
  
#Add dates to spring
soil_df <- full_join(soil_dates, soil_df) 


#Calculate percent silt and sand for 0-15 combined depth
soil_spring <- filter(soil_df, Type=='Spring') %>%
  select(Site, SamplePt, Depth, P_Clay, P_Silt, P_Sand, Bulk_Den) %>%
  gather(variable, value, -(Site:Depth)) %>%
  unite(temp, variable, Depth) %>%
  spread(temp, value) %>%
  mutate(`P_Sand_0-15` = CombineDepthPercent(P_Sand_05, Bulk_Den_05, 5, P_Sand_15, Bulk_Den_15, 10),
         `P_Silt_0-15` = CombineDepthPercent(P_Silt_05, Bulk_Den_05, 5, P_Silt_15, Bulk_Den_15, 10),
         `Bulk_Den_0-15` = Bulk_Den_05/3 + Bulk_Den_15*2/3) %>%
  tidyr::gather(variable, value, 3:18) %>%
  separate(variable, into=c("variable1", "variable2", "Depth"), sep='_') %>%
  unite(variable, variable1, variable2) %>%
  spread(variable, value) %>%
  mutate(Type = 'Spring') 

soil_df2 <- left_join(soil_df[,-which(names(soil_df) %in% c("P_Clay", "P_Silt", "P_Sand", "Bulk_Den", "Project_Year", "SampleID"))], soil_spring)

#summarize Spring data by site/date
#Calculate means of reps
soil_df3 <- soil_df2 %>%
  filter(Type == 'Spring') %>%
  mutate(Depth = factor(Depth, c( '05', '0-15', '15', '30'))) %>%
  dplyr::group_by(Site, Date, Depth, Manure) %>%
  summarize_at(vars(OM:P_Silt), mean, na.rm=T) %>%
  select_if(~sum(!is.na(.)) > 0)
  







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
  geom_point(position=position_jitterdodge(jitter.width=.1, jitter.height=0), alpha=.5) + 
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
#Copy and paste from old code
#PCA

pca_df<-soil_spring[,c("Manure", soilvars)]
Manure_temp <- factor(pca_df$Manure, c('Manure', "No Manure"))
Manure_temp <- as.numeric(Manure_temp)
Manure_temp[which(Manure_temp == 2)] <- 0
pca_df$Manure_binary <- Manure_temp
pca_df<-na.omit(pca_df)

pca_df2 <- select(pca_df, -Manure)

pca <- prcomp(pca_df2, center = TRUE, scale. = TRUE, rank=6) 


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


p_1v2<-autoplot(pca, x=2, y=1, data=pca_df, size=3, colour = "Manure",
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  theme_bw() + 
  theme(legend.position=c(0.15,0.12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank())

print(p_1v2)  

dev.off()


#plot axes 1 and 3 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v2.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_3v2<-autoplot(pca, x=3, y=1, data=pca_df, size=3, colour = "Manure",
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=c('#d95f02', '#1b9e77')) + 
  theme_bw() + 
  theme(legend.position='none', panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p_3v2)  

dev.off()


#plot first three axes of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v3.png")), res=400, width=10, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

grid.arrange(p_1v2, p_3v2, ncol=2)

dev.off()

