################################################################################
## Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    ##
## see COPYRIGHT file.                                                        ##
##                                                                            ##
## This file is part of LandInG and licensed under GNU AGPL Version 3 or      ##
## later. See LICENSE file or go to http://www.gnu.org/licenses/              ##
## Contact: https://github.com/PIK-LPJmL/LandInG/                             ##
################################################################################

################################################################################
## Function to set up which crops are included in "nes" groups?               ##
## Tries to match names (punctuation and white space is not always            ##
## consistently used in FAOSTAT)                                              ##
## Parameters:                                                                ##
## croplist: character vector of crop names for which to set up nes groups    ##
## item_group_def: data.frame with FAOSTAT production item group definitions  ##
## drop_crops: optional character vector with crop names to be dropped from   ##
##             list (see fao_drop_crops in ../landuse_setup.R)                ##
################################################################################
setup_nes_groups <- function(croplist, item_group_def, drop_crops = NULL) {
  nes_crops <- list()
  # Berries nes, all "*berries" & Currants
  crop_name <- nes_name(croplist, "Berries[[:punct:]|[:blank:]]*nes")
  if (!is.null(crop_name)) {
    nes_crops[[crop_name]] <- setdiff(
      # Match any "*berries" except nes group name itself
      grep(
        "berries|currant",
        croplist,
        value = TRUE,
        ignore.case = TRUE
      ),
      crop_name
    )
  }
  # Cereals nes, all crops within Cereals, Total group (except Cereals nes
  # itself)
  crop_name <- nes_name(croplist, "Cereals[[:punct:]|[:blank:]]*nes")
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Cereals[[:punct:]|[:blank:]]*Total",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Fruit, citrus nes, all in Citrus Fruit, Total group (except nes crop itself)
  crop_name <- nes_name(
    croplist,
    "Fruit[[:punct:]|[:blank:]]*citrus[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Citrus[[:punct:]|[:blank:]]*Fruit[[:punct:]|[:blank:]]*Total",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Fruit, pome nes (apples, pears, quinces)
  crop_name <- nes_name(
    croplist,
    "Fruit[[:punct:]|[:blank:]]*pome[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    nes_crops[[crop_name]] <- grep(
      paste(c("^apple", "^pear", "^quince"), collapse = "|"),
      item_group_def$Item,
      value = TRUE,
      ignore.case = TRUE
    )
  }
  # Fruit, stone nes (apricots, cherries, sour cherries, peaches and nectarines,
  # plums and sloes)
  crop_name <- nes_name(
    croplist,
    "Fruit[[:punct:]|[:blank:]]*stone[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    nes_crops[[crop_name]] <- grep(
      paste(c("apricot", "cherries", "peach", "plum"), collapse = "|"),
      item_group_def$Item,
      value = TRUE,
      ignore.case = TRUE
    )
  }
  # Fruit, tropical nes (avocados, dates, mangoes, mangosteens, guavas, papayas,
  # pineapples)
  crop_name <- nes_name(
    croplist,
    paste0(
      "Fruit[[:punct:]|[:blank:]]*tropical[[:punct:]|[:blank:]]*fresh",
      "[[:punct:]|[:blank:]]*nes"
    )
  )
  if (!is.null(crop_name)) {
    nes_crops[[crop_name]] <- grep(
      paste(c("avocado", "dates", "mango","papaya", "pineapple"), collapse = "|"),
      item_group_def$Item,
      value = TRUE,
      ignore.case = TRUE
    )
  }
  # Nuts nes, all crops in Treenuts, Total group (except nes crop itself)
  crop_name <- nes_name(croplist, "Nuts[[:punct:]|[:blank:]]*nes")
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Tree[[:punct:]|[:blank:]]*Nuts[[:punct:]|[:blank:]]*Total",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Oilseeds nes, all in Oilcrops, Oil Equivalent (except nes crop itself)
  crop_name <- nes_name(
    croplist,
    "Oil[[:punct:]|[:blank:]]*seeds[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Oilcrops[[:punct:]|[:blank:]]*Oil[[:punct:]|[:blank:]]*Equivalent",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Pulses nes, all in Pulses, Total group (except nes crop itself)
  crop_name <- nes_name(croplist, "Pulses[[:punct:]|[:blank:]]*nes")
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Pulses[[:punct:]|[:blank:]]*Total",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Roots and tubers nes, all in Roots and Tubers, Total group (except nes crop
  # itself)
  crop_name <- nes_name(
    croplist,
    "Roots (and|&) tubers[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Roots (and|&) Tubers[[:punct:]|[:blank:]]*Total",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  # Spices nes (pepper, dry chillies and peppers, vanilla, cinnamon, cloves,
  # nutmeg, mace and cardamons, anise, badian, fennel, coriander, ginge)
  crop_name <- nes_name(croplist, "Spices[[:punct:]|[:blank:]]*nes")
  if (!is.null(crop_name)) {
    nes_crops[[crop_name]] <- grep(
      paste(
        c("^pepper$", "^pepper ", "chillies.*dry", "vanilla", "cinnamon",
          "cloves", "nutmeg", "anise"),
        collapse = "|"
      ),
      item_group_def$Item,
      value = TRUE,
      ignore.case = TRUE
    )
  }
  # Vegetables, fresh nes, all in Vegetables Primary group (except nes crop
  # itself)
  crop_name <- nes_name(
    croplist,
    "Vegetables[[:punct:]|[:blank:]]*fresh[[:punct:]|[:blank:]]*nes"
  )
  if (!is.null(crop_name)) {
    total_name <- grep(
      "Vegetables[[:punct:]|[:blank:]]*Primary",
      item_group_def$Item.Group,
      value = TRUE,
      ignore.case = TRUE
    )
    index <- which(item_group_def$Item.Group %in% total_name)
    nes_crops[[crop_name]] <- setdiff(
      item_group_def$Item[index],
      crop_name
    )
  }
  for (n in names(nes_crops)) {
    # Remove any crops that may be listed in drop_crops
    if (!is.null(drop_crops)) {
      nes_crops[[n]] <- setdiff(nes_crops[[n]], drop_crops)
    }
    # Check that all crops belonging to nes group are in
    # fao_monfreda_production_array
    crops <- intersect(
      nes_crops[[n]],
      croplist
    )
    if (length(crops) < length(nes_crops[[n]])) {
      stop(
        "Not all crops belonging to ", sQuote(n),
        " according to item_group_def are found in",
        " croplist.\nPlease check assignment"
      )
    }
  }
  nes_crops
}

# Detect exact name of *nes crop. Return NULL if *nes crop not found.
nes_name <- function(croplist, pattern) {
  crop_name <- grep(
    pattern,
    croplist,
    value = TRUE,
    ignore.case = TRUE
  )
  if (length(crop_name) == 0) {
    warning(
      "No crop name found matching pattern ", sQuote(pattern),
      immediate. = TRUE
    )
    return (NULL)
  } else if (length(crop_name) > 1) {
    warning(
      length(crop_name), "crop names found matching pattern ", sQuote(pattern),
      ": ", toString(sQuote(crop_name)),
      immediate. = TRUE
    )
    return(NULL)
  }
  crop_name
}
