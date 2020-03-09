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


  # a$adcp$u <- ifelse(a$adcp$u>1,NA,a$adcp$u)
  # a$adcp$v <- ifelse(a$adcp$v>1,NA,a$adcp$v)
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

    distance_to_adcp <- oce::geodDist(a$adcp$lon,a$adcp$lat,
                                      a$ctd[[j]]@metadata$longitude,a$ctd[[j]]@metadata$latitude)
    ii <- which.min(distance_to_adcp)
    if(distance_to_adcp[ii] < 50) {
      step = 6 # 3 == 1 hour, 6 == 2 hours (4 hour window total)
      if(i == 5){
        step = step * 4 # account for S285 data being STA (5min) rather than LTA (20min)
      }

      umean <- colMeans(a$adcp$u[(ii-step):(ii+step),],na.rm = T)
      vmean <- colMeans(a$adcp$v[(ii-step):(ii+step),],na.rm = T)
      n <- colMeans(is.na(a$adcp$u[(ii-step):(ii+step),]))
      umean[n>0.6] <- NA
      vmean[n>0.6] <- NA
      uadd <- approx(a$adcp$d,umean,a$ctd[[j]]@data$depth)$y
      vadd <- approx(a$adcp$d,vmean,a$ctd[[j]]@data$depth)$y
      # testing script
      # plot(umean, type = 'l', col = "red", lwd = 5,ylim = c(-1,1))
      # for (l in -step:step)
      #   points(a$adcp$u[ii+l,])
      # lines(umean, col = "red", lwd = 5)
      #
      # plot(a$adcp$u[(ii-step):(ii+step),1])

    } else {
      uadd <- vadd <- NA
    }


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
                      u = uadd,
                      v = vadd,
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

