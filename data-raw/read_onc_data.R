master_folder <- "~/data/SEA/south_pacific"
subfolders <- list.files(master_folder, pattern = "S[0-9]{3}")

for (i in 1:length(subfolders)) {

  root_folder <- file.path(master_folder,subfolders[i])

  a <- sea::package_data(root_folder)

  assign(subfolders[i],a)

}

devtools::use_data(S258, overwrite = TRUE)
devtools::use_data(S265, overwrite = TRUE)
devtools::use_data(S272, overwrite = TRUE)
devtools::use_data(S278, overwrite = TRUE)


