


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



