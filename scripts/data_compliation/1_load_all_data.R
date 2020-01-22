

#Loop through all site folders and load edge of field runoff data
library(drake)


print(path_to_data)


data_list <-list()
folders<-list.files(path_to_data)

folder_nu<-1
for (folder_nu in 1:length(folders)){
  folder<-folders[folder_nu]
  
  cached_files<-list.files(file.path(path_to_data, folder, 'data_cached'))
  mod_file <- cached_files[grep("mod_dat" , cached_files)]
  
  site_name <- paste(unlist(strsplit(mod_file, "_"))[1], unlist(strsplit(mod_file, "_"))[2], sep="_")

  mod_path <- file.path(path_to_data, folder, 'data_cached', mod_file)
  
  data_i <- read.csv(file_in(mod_path), stringsAsFactors = F, header=T)
  
  data_list[[folder_nu]]<-data_i
  names(data_list)[[folder_nu]] <- site_name
  
}

head(data_list)
