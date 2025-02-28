packages <- c("openxlsx", "tidyverse")

install_and_load_packages <- function(packages) {
  for(package in packages) {
    if(!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

install_and_load_packages(packages)

# Loading and organizing data
all_data <- read.xlsx("Elected articles.xlsx", startRow = 2)

all_data <- all_data[, -c(2, 4:7, 42, 46, 55)] # Removing collected variables that are not important for or were not analyzed in this work.

# Modifying data.
data <- all_data  |>  
  mutate_if(is.character, as.factor)  |>  
  dplyr::rename( 
    source = Source.Title, 
    pphy = Parasite.phylum, 
    pclass = Parasite.class, 
    porder = Parasite.order, 
    pfamily = Parasite.family, 
    pgen = Parasite.genus, 
    psp = Parasite.species, 
    porigin = Parasite.origin, 
    repro = Reproduction, 
    pstyle =  Parasite.specialization, 
    kind = Euc.proc.or.virus,
    methodology = Identification.methodology, 
    host.type = Life.stage,
    site = Infection.site, 
    hphy = Invaded.host.phylum, 
    hclass = Invaded.host.class, 
    horder = Invaded.host.order, 
    hfamily = Invaded.host.family, 
    hgenus = Invaded.host.genus, 
    hsp = Invaded.host.species,
    invaded.host.origin = Invaded.host.indigenous.to,
    original.host.continent = Original.host.Indigenous.to.Continent,
    host.feeding = Invaded.host.Feeding.behaviour,
    status = IUCN.Red.List.Status,
    htroph = Trophic.level,
    shchooling = Shoaling.or.Schooling,
    ambiguous = Ambiguous.native.range,
    ambstudy = Study.environment, 
    lab = Experiment, 
    factor.ind = Factor.inducing.invasion,
    continent = Invaded.continent, 
    country = Invaded.country, 
    meltdown = Invasion.meltdown
  )  |>  
  dplyr::select( 
    ID, Year, source, Native.host, pphy, pclass, porder, pfamily, pgen, psp, porigin, Life.cycle, repro, pstyle, N.host.orders, N.host.families, Ecto.or.Endo, methodology, host.type, site, hphy, hclass, horder, hfamily, hgenus, hsp, invaded.host.origin, original.host.continent, host.feeding, status, htroph, Migrant, shchooling, ambiguous, ambstudy, lab, factor.ind, Spillmode, continent, country, meltdown, Invaded.area
  )


table(data$factor.ind)
data$factor.ind <- gsub("(Eel.*|eel.*|fish farm.*)", "Pisciculture", data$factor.ind)
data$factor.ind <- gsub("fisherman", "Fisherman", data$factor.ind)
data$factor.ind <- gsub("(Not known|Not mentioned|Unknow)", "Not mentioned or unknown", data$factor.ind)
data$factor.ind <- gsub(".*rism", "Aquarism", data$factor.ind)
data$factor.ind <- gsub(".*ol", "Biological control", data$factor.ind)
table(data$factor.ind)

table(data$repro)
data$repro <- gsub("\\?", "Unknown", data$repro)
table(data$repro)

table(data$pstyle)

table(data$Life.cycle)

table(data$host.feeding)
data$host.feeding <- gsub("\\?", "Unknown", data$host.feeding)
table(data$host.feeding)

table(data$htroph)
data$htroph <- gsub("\\?", "Unknown", data$htroph)
table(data$htroph)

table(data$status)

table(data$Migrant)
data$Migrant <- gsub("\\?", "Unknown", data$Migrant)
table(data$Migrant)

table(data$shchooling)
data$shchooling <- gsub("\\?", "Unknown", data$shchooling)
table(data$shchooling)

# Exporting data to directory.
write.xlsx(data, file = "All_Data.xlsx")

