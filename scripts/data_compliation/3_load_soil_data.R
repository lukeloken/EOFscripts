


soil_df <- read.csv(file_in(file.path(path_to_data, 'soil', 'raw_data', "EoF_SoilData_2016-2017.csv")), header=T, stringsAsFactors = F)

str(soil_df)

unique(soil_df$Depth)

soil_spring <- soil_df %>%
  dplyr::filter(Type == "Spring") %>%
  group_by(Site, Depth, Manure) %>%
  summarize_at(vars(P_Clay:NAG_nmol), mean, na.rm=T) %>%
  select_if(~sum(!is.na(.)) > 0)



soilvars <- names(soil_spring[4:ncol(soil_spring)])

i=1
soil_dots_list<- list()

for (i in 1:length(soilvars)){
soil_dots_list[[i]] <- ggplot(data=soil_spring, 
                              aes_string(x="Site", y=soilvars[i], group="Depth")) +
  geom_point(aes(col=Depth)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), legend.position='none')

}

plot_withlegend <- soil_dots_list[[1]] + 
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

mylegend<-g_legend(plot_withlegend)

rm(plot_withlegend)

plot_grid <- grid.arrange(grobs=soil_dots_list, ncol=4, as.table = F)

grid.arrange(plot_grid, mylegend, nrow=2, heights=c(15,1))




#Copy and paste from old code


#PCA

pca_df<-soil_spring[,soilvars]
pca_df<-na.omit(pca_df)

pca <- prcomp(pca_df, center = TRUE, scale. = TRUE, rank=6) 


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
corrange[which.max(abs(corrange))] <- corrange +c(-.1, .1)

corrplot(pca$rotation, is.corr=FALSE, mar=c(0,0,0,1.5), oma=c(0,0,0,0), tl.col='black', cl.pos='r', cl.ratio=0.5, col=brewer.pal(10, 'RdYlBu'), cl.lim=corrange)

mtext('Correlation', 4, -2)
mtext('Predictor variable', 2, 1.5)

# legend(-7,par('usr')[4], c('Predictor variable'), bty='n')

dev.off()


res <- cor(pca_df)
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

chart.Correlation(pca_df, histogram=TRUE, pch=19)

dev.off()




pca <- prcomp(pca_df, center = TRUE, scale. = TRUE, rank=5) 


#plot axes 1 and 2 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v1.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_1v2<-autoplot(pca, x=2, y=1, data=pca_df, size=3, 
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=rev(viridis(4))) + 
  theme_bw() + 
  theme(legend.position=c(0.83,0.15), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p_1v2)  

dev.off()


#plot axes 1 and 3 of PCA
png(file_out(file.path(path_to_results, "Figures", "Soil", "PredictorVarsPCA_Spring2016_v2.png")), res=400, width=5, height=5, units='in')
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_3v2<-autoplot(pca, x=3, y=1, data=pca_df, size=3, 
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=rev(viridis(4))) + 
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

