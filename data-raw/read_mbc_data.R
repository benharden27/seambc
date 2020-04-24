master_folder <- "~/data/SEA/MBC"
subfolders <- list.files(master_folder, pattern = "(S|C)[0-9]{3}")

for (i in length(subfolders)) {

  # set route folder
  root_folder <- file.path(master_folder,subfolders[i])

  # Package all data by standard routine
  a <- sea::package_data(root_folder)

  assign(subfolders[i],a)

}

usethis::use_data(C259, overwrite = TRUE)
usethis::use_data(C266, overwrite = TRUE)
usethis::use_data(C273, overwrite = TRUE)
usethis::use_data(C279, overwrite = TRUE)
usethis::use_data(C285, overwrite = TRUE)

