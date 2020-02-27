master_folder <- "~/data/SEA/south_pacific"
subfolders <- list.files(master_folder, pattern = "S[0-9]{3}")

for (i in 1:length(subfolders)) {

  # set route folder
  root_folder <- file.path(master_folder,subfolders[i])

  # Package all data by standard routine
  a <- sea::package_data(root_folder)

  # Add in the size fraction chl-a from S278
  if (i == 4) {
    surf <- sea::read_datasheet(file.path(root_folder,"SHIPDATA/S278_surfsamp.xlsm"))
    a$surfsamp <- dplyr::mutate(a$surfsamp,
                         chla_45 = as.numeric(surf$`Chl.a........0.45um....(ug/l)`),
                         chla_120 = as.numeric(surf$`Chl.a...1.2um...(ug/l)`),
                         chla_800 = as.numeric(surf$`Chl.a...8.0um...(ug/l)`))
  }


  for (j in 1:length(a$ctd)) {
    # find possible oxygen values
    ii <- stringr::str_which(names(a$ctd[[j]]@data),"oxygen")
    mean_o2 <- colMeans(sapply(a$ctd[[j]]@data[ii], unlist),na.rm = T)
    oval <- ii[which(mean_o2 < 10)][1]

    if(is.null(a$ctd[[j]]@data$par)) {
      par = NA
    } else {
      par <- a$ctd[[j]]@data$par
    }

    if (subfolders[i] == "S272") {
      a$ctd[[j]]@metadata$station <- as.numeric(tail(stringr::str_split(a$ctd[[j]]@metadata$filename,"_")[[1]],2)[1])
    }

    station <- paste0(subfolders[i],"_",stringr::str_pad(a$ctd[[j]]@metadata$station,3,pad = "0"))


    ctd_add <- tibble::tibble(dep = a$ctd[[j]]@data$depth,
                      temp = a$ctd[[j]]@data$temperature,
                      theta = a$ctd[[j]]@data$theta,
                      sigtheta = a$ctd[[j]]@data$sigmaTheta,
                      sal = a$ctd[[j]]@data$salinity,
                      fluor = a$ctd[[j]]@data$fluorescence,
                      par = par,
                      oxygen = a$ctd[[j]]@data[[oval]],
                      lon = a$ctd[[j]]@metadata$longitude,
                      lat = a$ctd[[j]]@metadata$latitude,
                      station = station,
                      cruise = subfolders[i])
    if (j == 1){
      ctd <- ctd_add
    } else {
      ctd <- dplyr::bind_rows(ctd,ctd_add)
    }
  }

  a$ctd2 <- ctd

  assign(subfolders[i],a)

}

usethis::use_data(S258, overwrite = TRUE)
usethis::use_data(S265, overwrite = TRUE)
usethis::use_data(S272, overwrite = TRUE)
usethis::use_data(S278, overwrite = TRUE)
usethis::use_data(S285, overwrite = TRUE)

