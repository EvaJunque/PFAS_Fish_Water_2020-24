# Calculate some derived values from the water measurements
library(readr)
library(dplyr)
library(tidyr)
library(cowplot)
library(ggplot2)
library(maps)
library(sf)
library(patchwork)

# don't need the appended long/lat data so read the smaller/simpler file
D <- read_csv("DB/sampled.fluor_with_locations.csv")
# D <- read_csv("DB/sampled.fluor_combined.csv") 
# View(D)

# might call into the URL structure directly instead
# somewhat slow before the cache populates, should then be fine
source("get_location_to_coord.R")

print("suspicious values - in the channel?")
suspicious <- D[D$sample.samplingPoint.northing < 10,]
print(unique(suspicious$sample.samplingPoint.label))

D<- D[D$sample.samplingPoint.northing>10,]

# The EA data collection method changed from only looking for two PFAS compounds
# to checking for lots of them, sometime in 2021. Individual measurements are not
# tagged to show which collection method they correspond to, but the change in the
# data is quite sharp and occurs at this point. Some samples from the early method
# may be mislabelled if they were taken after the change over point.
method_change_date <- ISOdatetime(2021, 6, 23, 10, 40, 0, tz="UTC")
D$MethodName <- ifelse(D$sample.sampleDateTime < method_change_date, "Original", "Extended")

# Discard the columns that carry no information or are not in use
if (!all(D$sample.samplingPoint == D$sample.samplingPoint.notation)) stop("Not a duplicate column, don't delete it")
if (!all(is.na(D$codedResultInterpretation.interpretation))) stop("wasn't all na")
if (!all(D$determinand.unit.label == "ug/l")) stop("not all ug/l")
if (any(D$sample.isComplianceSample)) stop("Some were compliance samples")
if (length(D$X.id) != length(unique(D$X.id))) stop("Id not unique per row")


for (l in unique(D$determinand.label))
{
  rows <- D[D$determinand.label == l,]
  if (length(unique(rows$determinand.notation)) != 1)
  {
    stop("Determinand label corresponds to multiple notations")
  }
  if (length(unique(rows$determinand.definition)) != 1)
  {
    stop("Determinand label corresponds to multiple definitions")
  }
}

for (l in unique(D$determinand.notation))
{
  rows <- D[D$determinand.notation == l,]
  if (length(unique(rows$determinand.label)) != 1)
  {
    stop("Determinand notation corresponds to multiple labels")
  }
}

for (l in unique(D$determinand.definition))
{
  rows <- D[D$determinand.definition == l,]
  if (length(unique(rows$determinand.label)) != 1)
  {
    stop("Determinand definition corresponds to multiple labels")
  }
}

for (l in unique(D$sample.samplingPoint))
{
  rows <- D[D$sample.samplingPoint == l,]
  if (length(unique(rows$sample.samplingPoint.label)) != 1)
  {
    stop("Sample.sampling point corresponds to multiple sample point labels")
  }
  if (length(unique(rows$sample.samplingPoint.easting)) != 1)
  {
    # Turns out one of the sample points, SW-RSN0237, has two distinct east/northing
    # associated with it. Thus not ideal to use sample point as a uuid
    # not causing problems yet as the timestamps are distinct
    # stop("Sample.sampling point corresponds to multiple sample point eastings")
  }
  if (length(unique(rows$sample.samplingPoint.northing)) != 1)
  {
    # stop("Sample.sampling point corresponds to multiple sample point northings")
  }
}

for (l in unique(D$sample.samplingPoint.label))
{
  rows <- D[D$sample.samplingPoint.label == l,]
  if (length(unique(rows$sample.samplingPoint)) != 1)
  {
    stop("sample.samplingpoint.label corresponds to multiple samplePoints")
  }
}


# Create a column of acronyms based on this lookup
renametable <- c()
renametable["pFoctanoate"] = "PFOA anion"
renametable["PFHxS-B"] = "PFHxS-B" 
renametable["PFHxS-L"] = "PFHxS-L"
renametable["PFHxSA"] =	"PFHxSA"
renametable["PFHxS"] = "PFHxS" # not in the water dataset, will be derived from the other PFHxS*
renametable["PFTrDA"] = "PFTrDA"
renametable["PFPeS"] = "PFPeS"
renametable["FOSA"] =	"PFOSA"
renametable["PFNS"] =	"PFNS"
renametable["PFHpS"] =	"PFHpS"
renametable["PFDoS"]=	"PFDoS"
renametable["PFDS"] =	"PFDcS"
renametable ["FBSA"] =	"PFBSA"
renametable ["PFBS"] =	"PFBuS"
renametable["PFecHS"] =	"PFECHS"
renametable["N-MeFOSA"] =	"NMeFOSAA"
renametable["EtFOSA"] =	"NEtFOSAA"
renametable["HFPO-DA"] =	"HPFO-DA"
renametable["9Cl-PF3ONS"] =	"9Cl-PF3ONS"
renametable["8:2 FTSA"] =	"8:2FTS"
renametable["6:2 FTSA"] =	"6:2FTS"
renametable["4:2 FTSA"] =	"4:2FTS"
renametable["ADONA"] =	"ADONA"
renametable["5:3 FTCA"] =	"5:3 FTCA"
renametable["11Cl-PF3OUdS"] =	"11Cl-PF3OUdS"
renametable["PFBA"] =	"PFBuA"
renametable["PFpentncAcid"] =	"PFPeA"
renametable["PFhexncAcid"] =	"PFHxA"
renametable["PFheptncAcid"] =	"PFHpA"
renametable["PFoctncAcid"] =	"PFOA"
renametable["PFnonncAcid"]	= "PFNA"
renametable["PFdecncAcid"] =	"PFDcA"
renametable["PFundencAcid"] =	"PFUnA"
renametable["PFdodencAcid"] =	"PFDoDA"
renametable["PFtetdncAcid"] = "PFTeDA"
renametable["PFEESA"] =	"PFEESA"
renametable["7:3 FTCA"] = "7:3 FTCA"
renametable["3:3 FTCA"] =	"3:3 FTCA"
renametable["PFOS (T)"] =	"PFOS"

# could drop the .label as well but it's used further down

# Some locations have different sample.sampledMaterialType.label values, e.g. groundwater and river water in the same place
D$determinand_acronyms <- renametable[D$determinand.label]

# Drop duplicate or redundant columns from the input dataset, leave easting/northing present
D <- subset(D, select=-c(X.id,
                         sample.samplingPoint.notation, # just a copy of sample.samplingPoint
                         sample.samplingPoint.label, # equivalent to sample.samplingPoint
                         codedResultInterpretation.interpretation,
                         sample.isComplianceSample, # all FALSE
                         determinand.unit.label, # all ug/l 
                         determinand.notation, # equivalent to determinand.label
                         determinand.definition, # also equivalent to determinand.label
                         longitude, latitude)) # don't need these columns



pfas_acronyms_unique <- unique(renametable) 
pfas_acronyms <- character(3*length(pfas_acronyms_unique))
for (i in 1:length(pfas_acronyms_unique))
{
  pfas_acronyms[3*i - 2] <- sprintf("%s.measured", pfas_acronyms_unique[i])
  pfas_acronyms[3*i - 1] <- sprintf("%s.qualifier", pfas_acronyms_unique[i])
  pfas_acronyms[3*i] <- pfas_acronyms_unique[i]
}

# length datetime won't be right, but will mean the initial table filling runs faster
size_guess <- length(unique(D$sample.sampleDateTime)) # * length(unique(D$sample.samplingPoint)) / 32 # trying to guess roughly how big it'll be
leading_names <- c("sample.samplingPoint",
                   "sample.sampleDateTime",
                   "sample.sampledMaterialType.label",
                   "water type",
                   "easting",
                   "northing",
                   "longitude",
                   "latitude",
                   "area",
                   "subarea",
                   "Total PFAS",
                   "Max PFAS")
rot_names <- c(leading_names, pfas_acronyms)
rot <- data.frame(matrix(ncol=length(rot_names), nrow=size_guess), stringsAsFactors=FALSE)
colnames(rot) <- rot_names

next_free_row <- 1 # instead of nrow to append, write directly to the next unused one
list_NA <- rep(list(NA), length(rot_names) - length(leading_names))

count_sample_points <- length(unique(D$sample.samplingPoint))

counter <- 0
for (loc in unique(D$sample.samplingPoint)) {
  counter <- counter + 1
  
  print(sprintf("Loc %s consider %i / %i, %s%%", loc, counter, count_sample_points, round((100*counter) / count_sample_points, 2)))
  
  D1 <- D[D$sample.samplingPoint == loc,]
  for (time in unique(D1$sample.sampleDateTime)) {
    D2 <- D1[D1$sample.sampleDateTime == time,]
    for (mat in unique(D2$sample.sampledMaterialType.label))
    {
      subtab <- D2[D2$sample.sampledMaterialType.label == mat,]
      nr <- nrow(subtab)
      if (nr == 0) next
      
      # place to insert into
      selected <- !is.na(rot$sample.samplingPoint) &
        rot$sample.samplingPoint == loc &
        rot$sample.sampleDateTime == time &
        rot$sample.sampledMaterialType.label == mat
      
      row_count <- sum(selected)
      if (row_count > 1) stop("Multiple equivalent rows?")
      
      selected_row <- ifelse(row_count == 0, next_free_row, which(selected)[1])
      next_free_row <- next_free_row + (row_count == 0)
      if (row_count == 0)
      {
        # don't have a row for this tuple yet, create it with NA for all measurements
        # bit hacky here, we're using the knowledge that long/lat are functions of loc
        # and loc is constant for the subtable
        
        if (length(unique(subtab$sample.samplingPoint.easting)) != 1) {
          stop("multiple eastings associated with same point")
        }
        if (length(unique(subtab$sample.samplingPoint.northing)) != 1) {
          stop("multiple northings associated with same point")
        }
        
        rot[selected_row,] <- c(list(loc,
                                     format(time),
                                     mat,
                                     "unknown",
                                     subtab[1,]$sample.samplingPoint.easting,
                                     subtab[1,]$sample.samplingPoint.northing,
                                     0, # longitude
                                     0, # latitude,
                                     "", # area
                                     "", # subarea
                                     0,0 # total pfas, to fill in later
        ),
        list_NA)
      }
      
      for (i in 1:nr)
      {
        r <- subtab[i,] # to insert from
        
        if (!is.na(rot[selected_row, r$determinand_acronyms]))
        {
          stop("Error, duplicate measurement, will need averaging or similar")
        }
        
        if (is.na(r$result)) stop("NA in input dataset result field?")
        
        # Assign this value to the specific entry
        val <- r$result
        qual <- r$resultQualifier.notation
        # need to decide how to handle values below or above the limits of the equipment
        
        modified <- ifelse(is.na(qual), val,
                           ifelse(qual == "<", val/2,
                                  ifelse(qual == ">", val,
                                         NA)))
        if (is.na(modified))
        {
          stop("Unexpected result qualifier")
        }
        
        rot[selected_row, r$determinand_acronyms] <- modified
        rot[selected_row, sprintf("%s.qualifier",r$determinand_acronyms)] <- qual
        rot[selected_row, sprintf("%s.measured",r$determinand_acronyms)] <- val
      }
    }
  }
}

rot <- rot[!is.na(rot$sample.samplingPoint),] # discard unused-but-allocated rows

# For all the sample points of interest, append some location information
# derived from URL if necessary, could calculate longitude/latitude from easting instead
# probably better to work in terms of easting/northing consistently
st <- get_location_to_coord(unique(rot$sample.samplingPoint))
rot$longitude <- as.numeric(sapply(rot$sample.samplingPoint, function (x) { st$get(x)[1] }))
rot$latitude <- as.numeric(sapply(rot$sample.samplingPoint, function (x) { st$get(x)[2]}))
rot$area <- sapply(rot$sample.samplingPoint, function (x) { st$get(x)[3] })
rot$subarea <- sapply(rot$sample.samplingPoint, function (x) { st$get(x)[4] })




# Water DB records these separately and fish analysis combines them
# Sanity check that the .measured and resolved values are !NA in the corresponding places
if (!all(!is.na(rot$`PFHxS-B`) == !is.na(rot$`PFHxS-B.measured`))) stop("bad PFHxS-B data")
if (!all(!is.na(rot$`PFHxS-L`) == !is.na(rot$`PFHxS-L.measured`))) stop("bad PFHxS-L data")

if (!all(is.na(rot$PFHxS))) stop("Unexpected entry for PFHxS in water db")
rot$PFHxS <- ifelse(is.na(rot$`PFHxS-B`) & is.na(rot$`PFHxS-L`),
                    NA, # both NA, this is NA
                    ifelse(is.na(rot$`PFHxS-B`) & !is.na(rot$`PFHxS-L`),
                           rot$`PFHxS-L`, # Only one measurement, use it
                           ifelse(!is.na(rot$`PFHxS-B`) & is.na(rot$`PFHxS-L`),
                                  rot$`PFHxS-B`, # Other measurement
                                  rot$`PFHxS-L` + rot$`PFHxS-B`))) # Both measurements



# Populate the totals column
for (i in 1:nrow(rot))
{
  acc <- 0
  max <- 0
  for (det in pfas_acronyms_unique)
  {
    inc <- ifelse(is.na(rot[i, det]), 0, rot[i,det])
    max <- base::max(max, rot[i,det], na.rm = T)
    acc <- acc + inc
  }
  rot[i, ]$`Total PFAS` <- acc
  rot[i, ]$`Max PFAS` <- max
}





# Collapse various material types onto fewer categories
{
  is_fresh <- ((rot$sample.sampledMaterialType.label == "RIVER / RUNNING SURFACE WATER") | 
                 (rot$sample.sampledMaterialType.label == "POND / LAKE / RESERVOIR WATER") | 
                 (rot$sample.sampledMaterialType.label == "SURFACE DRAINAGE") )
  
  is_saline <- (rot$sample.sampledMaterialType.label == "SEA WATER")
  
  is_ground <- (rot$sample.sampledMaterialType.label == "GROUNDWATER")
  
  is_estuarine <- (rot$sample.sampledMaterialType.label == "ESTUARINE WATER")
  
  # And also a string version for easier use later
  rot$`water type` <- ifelse(is_fresh, 
                             "fresh",
                             ifelse(is_saline,
                                    "saline",
                                    ifelse(is_ground,
                                           "ground",
                                           ifelse(is_estuarine,
                                                  "estuarine",
                                                  "discarded"))))
}


if (FALSE) {
  write.csv(rot, "DB/rotated_water_fluor.csv", row.names = FALSE)
}


# Discard saline and estuarine from the dataset
rot <- rot [! (rot$`water type` == "estuarine" | rot$`water type` == "saline"), ]

# from the first pfas column to the end, stepping over the qualifier and measured fields
#REDO this
# drop the qualifier and measured columns, keep the resolved one
rot_without_qualifier <- rot[,c(1:(length(leading_names)-1), seq(length(leading_names), length(rot_names), 3))]



summarizer <- function (x) {
  m <- ifelse(all(is.na(x)), NA, base::mean(x, na.rm = T))
}

# average the easting and northing for a given subarea
# the explicit ungroup is needed to get back to a flat structure from two group-by
subarea_mean <- rot_without_qualifier %>% group_by(subarea) %>%
  summarise(across(c(easting, northing), base::mean),
            across(-c(1:10), summarizer))


# Calculate long/lat coordinates from the averaged easting/northings
GeomTmp <-
  subarea_mean %>%
  st_as_sf(coords = c("easting", 
                      "northing"),
           crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates()
subarea_mean$longitude <- GeomTmp[,"X"]
subarea_mean$latitude <- GeomTmp[,"Y"]
subarea_mean <- relocate(subarea_mean, c(longitude, latitude), .after = northing)

# Append a column for derived values
subarea_mean <-  mutate(subarea_mean, mean = rowMeans(select(subarea_mean, -c(1:6)), na.rm=T))


# Draw the UK AREAS
plot <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  coord_fixed(ratio = 1.3, # todo, work out what ratio matches the usual presentation
              xlim = c(-7,3), ylim = c(50, 57) # carve part of the UK out of the world map dataset
  )

# UK, roughly, is xlim = c(-10,3), ylim = c(50.3, 59)

if (T)
{
  hulls <-
    rot %>% 
    group_by(subarea) %>%
    slice(chull(longitude, latitude))
  
  plot <- plot + 
    aes(fill = factor(subarea)) + 
    theme(legend.position = "none") +
    geom_polygon(data = hulls, aes(x = longitude, y = latitude), alpha = 0.5)
}


plot <- plot +  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "fresh",],
                           aes(x = longitude,
                               y = latitude,
                               shape = 0,
                               size = PFOA)) + scale_shape_identity()

plot <- plot +  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "ground",],
                           aes(x = longitude,
                               y = latitude,
                               shape = 2,
                               size = PFOA)) + scale_shape_identity()
plot


##FRESH


fresh <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  coord_fixed(ratio = 1.3, # todo, work out what ratio matches the usual presentation
              xlim = c(-7,3), ylim = c(50, 57) # carve part of the UK out of the world map dataset
  )
fresh <- fresh +  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "fresh",],
                             aes(x = longitude,
                                 y = latitude,
                                 size = PFOA))
fresh




ground <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black') +
  coord_fixed(ratio = 1.3, # todo, work out what ratio matches the usual presentation
              xlim = c(-7,3), ylim = c(50, 57) # carve part of the UK out of the world map dataset
  )
ground <- ground +  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "ground",],
                               aes(x = longitude,
                                   y = latitude,
                                   size = PFOA)) 
ground

#####MAP WITH PFOA freshwater and groundwater######

#GROUND
#Create the map
ground <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black', size = 0.2) +  # Ajustar el grosor de los límites
  coord_fixed(ratio = 1.3, 
              xlim = c(-7, 3), ylim = c(50, 57)) +  # Ajustar límites del mapa
  theme_minimal() +  # Usar un tema minimalista
  theme(
    panel.grid = element_blank(),  # Eliminar las cuadrículas
    axis.text = element_blank(),  # Eliminar los textos de los ejes
    axis.title = element_blank(),  # Eliminar los títulos de los ejes
    plot.margin = unit(c(1,1,1,1), "cm")  # Ajustar márgenes
  )

# Add data points with size and color based on PFOA
ground <- ground + 
  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "ground",],
             aes(x = longitude, y = latitude, size = PFOA, color = PFOA), 
             alpha = 0.7) +  # Use alpha for transparency
  scale_size_continuous(range = c(1, 10), name = "PFOA Concentration") +  # Point size scale
  scale_color_gradient(low = "blue", high = "red", name = NULL) +  # Color scale
  labs(title = "Distribution of groundwater PFOA levels",
       caption = "Source: Open data from the Environment Agency") +  # Titles and legend
  theme(
    legend.position = "bottom",  # Move the legend
    legend.title = element_text(size = 10),  # Change the legend size
    legend.text = element_text(size = 8),  #Change the legend text size
    plot.title = element_text(size = 14, hjust = 0.5)  # Center the title
  )

# Show the map
ground

#FRESH
# Create the map for freshwater ("fresh")
fresh <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black', size = 0.2) +  
  coord_fixed(ratio = 1.3, 
              xlim = c(-7, 3), ylim = c(50, 57)) + 
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text = element_blank(),  
    axis.title = element_blank(),  
    plot.margin = unit(c(1,1,1,1), "cm")  
  )

# Add data points with size and color based on PFOA
fresh <- fresh + 
  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "fresh",],
             aes(x = longitude, y = latitude, size = PFOA, color = PFOA), 
             alpha = 0.7) +  
  scale_size_continuous(range = c(1, 10), name = "PFOA Concentration") +  
  scale_color_gradient(low = "blue", high = "red", name = NULL) +  
  labs(title = "Distribution of freshwater PFOA levels") +
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 14, hjust = 0.5)  
  )

# Show the map
fresh


# Juntar los dos mapas asegurando que solo haya una leyenda
combined_plot <- fresh + ground + 
  plot_layout(ncol = 2, guides = "keep") 

# Combine the two maps ensuring there is only one legend
combined_plot

# Save the combined image
ggsave("Plotting/combined_map.png", plot = combined_plot, dpi = 300, width = 12, height = 6)

##saved as 1200x900





#####MAP WITH TOTAL PFAS#####

# Create the map
ground <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black', size = 0.2) +  
  coord_fixed(ratio = 1.3, 
              xlim = c(-7, 3), ylim = c(50, 57)) +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text = element_blank(),  
    axis.title = element_blank(),  
    plot.margin = unit(c(1,1,1,1), "cm")  
  )

# Add data points with size and color based on PFOA
ground <- ground + 
  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "ground",],
             aes(x = longitude, y = latitude, size = `Total PFAS`, color = `Total PFAS`), 
             alpha = 0.7) +  
  scale_size_continuous(range = c(1, 10), name = "Total PFAS Concentration") +  
  scale_color_gradient(low = "blue", high = "red", name = NULL) +  
  labs(title = "Distribution of groundwater total PFAS levels",
       caption = "Source: UK Environment Agency") + 
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 14, hjust = 0.5)  
  )

# Show the map
ground



#FRESH improved
# Create the map for freshwater "fresh"
fresh <- ggplot() + 
  geom_polygon(data = map_data('world'), 
               aes(x = long, y = lat, group = group),
               fill = 'gray90', 
               color = 'black', size = 0.2) +  
  coord_fixed(ratio = 1.3, 
              xlim = c(-7, 3), ylim = c(50, 57)) +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text = element_blank(),  
    axis.title = element_blank(),  
    plot.margin = unit(c(1,1,1,1), "cm")  #
  )

# Add data points with size and color based on PFOA
fresh <- fresh + 
  geom_point(data = rot_without_qualifier[rot_without_qualifier$`water type` == "fresh",],
             aes(x = longitude, y = latitude, size = `Total PFAS`, color = `Total PFAS`), 
             alpha = 0.7) +  # Usar alfa para transparencias
  scale_size_continuous(range = c(1, 10), name = "Total PFAS Concentration") +  
  scale_color_gradient(low = "blue", high = "red", name = NULL) +  
  labs(title = "Distribution of freshwater total PFAS levels",
       caption = "Source: Open data from the Environment Agency") + 
  theme(
    legend.position = "bottom",  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    plot.title = element_text(size = 14, hjust = 0.5)  
  )


fresh
#saved 1200 x900

# Combine the two maps, ensuring there is only one legend
combined_plot <- fresh + ground + 
  plot_layout(ncol = 2, guides = "keep") 

# Show the combined plot
combined_plot

# Save the combined image
ggsave("Plotting/combined_totalPFAS_map.png", plot = combined_plot, dpi = 300, width = 12, height = 6)





#####PLOTS WITH FRESH AND GROUND########


# PFHxS doesn't exist in the water set, PFHxSA, PFHxS-L, PFHxS-B do.
# Derived PFHxS from PFHxS-L and PFHxS-B (by sum, roughly)
pfas_order <- c("PFOA", "PFBuA", "PFPeA", "PFHxA", "PFHpA", "PFBuS", "PFHxS", "PFOS", "PFBSA", "6:2FTS")

# Discarding compounds other than these simplifies the plot,
# but also reduces the maximum concentration figure

reduced_set_compounds <- TRUE
want <- c(leading_names, pfas_order)
rot_without_qualifier2 <- rot_without_qualifier[, want]


# rotate structure
rot_without_qualifier2 <- pivot_longer(rot_without_qualifier2,
                                       cols = (length(leading_names)+1) : length(rot_without_qualifier2),
                                       values_to = "Value",
                                       names_to = "PFAS")

muscle <- rot_without_qualifier2%>%
  group_by(`water type`, PFAS)%>%
  summarise(meanVal=mean(Value, na.rm = T))


muscleSum <- muscle%>%
  group_by(`water type`)%>%
  summarise(sumVal=sum(meanVal, na.rm = T))


muscle<-merge(muscle, muscleSum, by="water type")

#calculate relative concentration
muscle<-muscle%>%mutate(perVal=meanVal/sumVal*100)




#improved
water_row <- muscle %>%
  ggplot(aes(x = `water type`, y = meanVal)) +
  geom_col(aes(fill = factor(PFAS, levels = pfas_order)), width = 0.7) +
  theme_minimal() +
  xlab("Water type") +
  ylab("Concentration (µg/L)") +
  scale_y_continuous(limits = c(0, 0.05)) +
  scale_x_discrete(limits = rev(unique(muscle$`water type`))) +  # Orden inverso si es necesario
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E",  "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B" , "#7F7F7F", "#E377C2","#17BECF", "#BCBD22"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

ggsave("Plotting/water_row.png", plot = water_row, dpi = 300, width = 10, height = 6)



water_percentage <- muscle %>%
  ggplot(aes(x = `water type`, y = perVal)) +
  geom_col(aes(fill = factor(PFAS, levels = pfas_order)), width = 0.7) +
  theme_minimal() +
  xlab("Water type") +
  ylab("Composition (%)") +
  scale_y_continuous(limits = c(0, 101)) +
  scale_x_discrete(limits = rev(unique(muscle$`water type`))) +  # Invierte el orden si es necesario
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E",  "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B", "#7F7F7F", "#E377C2","#17BECF", "#BCBD22")) +
  coord_flip() +  # Esto gira la gráfica 90 grados
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

ggsave("Plotting/water_percentage.png", plot = water_percentage, dpi = 300, width = 10, height = 6)





water_legend <- cowplot::get_plot_component(water_row  + theme(legend.position = "bottom"), "guide-box", return_all = TRUE)[[3]]

water_row <- water_row + theme(legend.position = "none")
water_percentage <- water_percentage + theme(legend.position="none")


prow <- plot_grid(water_row, water_percentage, nrow=1)
both <- plot_grid(prow, water_legend, nrow=2,  rel_heights = c(1, 0.25))

both



# ggsave("Plotting/water.png", plot = british_row, dpi = 300, width = 10, height = 6)
#saved at 700 and mantain ratio







#####MOST CONTAMINATED SAMPLES########


worst_data2 <- rot_without_qualifier[rot_without_qualifier$`Total PFAS` > 2,]

worst_data <- pivot_longer(worst_data2,
                           cols = (length(leading_names)+1) : length(worst_data2),
                           values_to = "Value",
                           names_to = "PFAS")


worst <- worst_data%>%
  group_by(sample.sampleDateTime, PFAS)%>%
  summarise(meanVal=mean(Value, na.rm = T))


worstSum <- worst%>%
  group_by(sample.sampleDateTime)%>%
  summarise(sumVal=sum(meanVal, na.rm = T))


worst<-merge(worst,worstSum, by="sample.sampleDateTime")



# Calculate the relative concentration
worst <- worst %>%
  mutate(perVal = meanVal / sumVal * 100)

# Define the desired order of the samples
sample_order <- c("1687435500", "1698316800", "1698317460", "1719238080","1728998700", "1719831240",
                  "1689932160","1636019220","1666100460","1625236080")


# Convert sample.sampleDateTime to a factor with the desired order
worst <- worst %>%
  mutate(sample.sampleDateTime = factor(sample.sampleDateTime, levels = sample_order))

# Create the plot
worst_row <- ggplot(worst, aes(x = sample.sampleDateTime, y = meanVal, fill = PFAS)) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Sampling point") +
  ylab("Concentration (µg/L)") +
  scale_x_discrete(labels = c("1687435500" = "A", 
                              "1698316800"= "A", 
                              "1698317460"="A",  
                              "1719238080"= "A",
                              "1728998700"="A", 
                              "1719831240"="B",
                              "1689932160"="C",
                              "1636019220"="D",
                              "1666100460"="E", 
                              "1625236080"="E")) +
  
  scale_fill_manual(values = c(
    "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825",
    "#AEC7E8", "#FFBB78", "#98DF8A", "#D62728", "#FF9896",
    "#C5B0D5", "#F7B6D2", "#C49C94", "#F1C6AA", "#FF6666", 
    "#66B3FF", "#FF8C00", "#99CC99", "#AA4D4D", "#003366",
    "#FF1493", "#3CB371", "#6A5ACD", "#FFD700", "#800000",
    "#8A2BE2", "#00BFFF", "#FF6347", "#3B9C9C", "#BDB76B", 
    "#D2691E", "#ADFF2F", "#8B0000", "#808000"
  )) 
theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotar etiquetas X
  legend.title = element_blank(),  # Quitar título de la leyenda
  legend.text = element_text(size = 12),  # Tamaño de la leyenda
  plot.title = element_text(size = 16, hjust = 0.5)  # Centrar título
)

# Show the plot
worst_row


ggsave("Plotting/worst_row.png", plot = worst_row, dpi = 300, width = 10, height = 6)



worst_percentage <- worst %>%
  ggplot(aes(x = sample.sampleDateTime, y = perVal)) +
  geom_col(aes(fill = PFAS), width = 0.7) +
  theme_minimal() +
  xlab("Worst Type") +
  ylab("Composition (%)") +
  scale_y_continuous(limits = c(0, 101)) +  # Adjusted Y-axis limits
  scale_x_discrete(labels = c("1687435500" = "TH-PEVR0052", 
                              "1698316800"= "TH-PEVR0052", 
                              "1698317460"="TH-PEVR0052",  
                              "1719238080"= "TH-PEVR0052",
                              "1728998700"="TH-PEVR0052", 
                              "1719831240"="NE-RSN0140",
                              "1689932160"="TH-PCHR0148",
                              "1636019220"="SW-70550004",
                              "1666100460"="NW-88008943", 
                              "1625236080"="NW-88008943")) +
  scale_fill_manual(values = c(
    "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825",
    "#AEC7E8", "#FFBB78", "#98DF8A", "#D62728", "#FF9896",
    "#C5B0D5", "#F7B6D2", "#C49C94", "#F1C6AA", "#FF6666", 
    "#66B3FF", "#FF8C00", "#99CC99", "#AA4D4D", "#003366",
    "#FF1493", "#3CB371", "#6A5ACD", "#FFD700", "#800000",
    "#8A2BE2", "#00BFFF", "#FF6347", "#3B9C9C", "#BDB76B", 
    "#D2691E", "#ADFF2F", "#8B0000", "#808000"
  )) +  # Colores personalizados (39 colores)  
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate the X-axis labels
    axis.text.y = element_text(size = 12),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 12),  a
    plot.title = element_text(size = 16, hjust = 0.5),  
    plot.margin = unit(c(1, 1, 1, 1), "cm")  
  )
worst_percentage

ggsave("Plotting/worst_percentage.png", plot = worst_percentage, dpi = 300, width = 10, height = 6)




worst_legend <- cowplot::get_plot_component(
  worst_row + theme(legend.position = "bottom") +
    guides(fill = guide_legend(reverse = TRUE)),  #Invert the order of the legend
  "guide-box", return_all = TRUE
)[[3]]

worst_row <- worst_row + theme(legend.position = "none")
worst_percentage <- worst_percentage + theme(legend.position="none")


prow <- plot_grid(worst_row, worst_percentage, nrow=1)
both <- plot_grid(prow, worst_legend, nrow=2,  rel_heights = c(1, 0.5))

both



#ggsave("Plotting/worst.png", plot = british_row, dpi = 300, width = 10, height = 6)




#### MORETON -IN- MARSH ########

# Filter the data to keep only those with 'Total PFAS' > 2
worst_data2 <- rot_without_qualifier[rot_without_qualifier$`Total PFAS` > 2,]

# Pivot the data from wide format to long format
worst_data <- pivot_longer(worst_data2,
                           cols = (length(leading_names)+1):length(worst_data2),
                           values_to = "Value",
                           names_to = "PFAS")

# alculate the mean values for each combination of sample.sampleDateTime and PFAS
worst <- worst_data %>%
  group_by(sample.sampleDateTime, PFAS) %>%
  summarise(meanVal = mean(Value, na.rm = TRUE)) %>%
  filter(!is.na(meanVal))  # Eliminar NA en meanVal

# Calculate the sum of the means per sample
worstSum <- worst %>%
  group_by(sample.sampleDateTime) %>%
  summarise(sumVal = sum(meanVal, na.rm = TRUE)) %>%
  filter(!is.na(sumVal))  # Eliminar NA en sumVal

# Join the worst data with worstSum to include the sums per sample
worst <- merge(worst, worstSum, by = "sample.sampleDateTime")

# Calculate the relative concentration
worst <- worst %>%
  mutate(perVal = meanVal / sumVal * 100)

# Define the desired order of the samples
sample_order <- c("1687435500")

# Convert sample.sampleDateTime to a factor with the desired order and remove NA
worst <- worst %>%
  mutate(sample.sampleDateTime = factor(sample.sampleDateTime, levels = sample_order)) %>%
  filter(!is.na(sample.sampleDateTime))  # Eliminar NA en sample.sampleDateTime


# Create the plot with centered labels on the X-axis
worst_row <- ggplot(worst, aes(x = sample.sampleDateTime, y = meanVal, fill = PFAS)) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Moreton-in-Marsh") +
  scale_x_discrete(labels = c("1687435500" = "")) +
  ylab("Concentration (µg/L)") +
  scale_fill_manual(values = c(
    "#1F77B4", "#FF7F0E", "#FF0000", "#2CA02C", "#0000FF", 
    "#9467BD", "#E377C2", "#7F7F7F", "#BCBD22", "#00CC66", "#FF6600",
    "#AEC7E8", "#FFBB78", "#98DF8A", "#D62728", "#FF9896",
    "#C5B0D5", "#F7B6D2", "#C49C94", "#F1C6AA", "#FF6666", 
    "#66B3FF", "#FF8C00", "#99CC99", "#AA4D4D", "#003366",
    "#FF1493", "#3CB371", "#6A5ACD", "#FF69B4", "#FFD700",
    "#8A2BE2", "#00BFFF", "#FF6347", "#3B9C9C", "#BDB76B", 
    "#D2691E", "#ADFF2F", "#8B0000", "#808000"
  )) + 
  theme(
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 0.5, angle = 0),  
    axis.text.y = element_text(size = 14),  
    axis.title.x = element_text(size = 18), 
    axis.title.y = element_text(size = 18),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 16),  
    plot.title = element_text(size = 14, hjust = 0.5),  
    plot.caption = element_text(hjust = 1, vjust = -1, size = 10),  
    plot.margin = margin(t = 10, r = 20, b = 30, l = 10)  
  )

print(worst_row)


ggsave("Plotting/MoretoninMarsh.png", plot = worst_row, dpi = 300, width = 10, height = 6)


# Guardar la imagen con mejor calidad
#ggsave("Plotting/worst.png", plot = british_row, dpi = 300, width = 10, height = 6)









if (FALSE) {
  smallvals <- rot[rot$`Total PFAS` < 5,]
  ggplot(smallvals, aes(as.numeric(sample.sampleDateTime), `Total PFAS`)) + geom_point()
  ggsave("Plotting/change_in_total_against_time.png")
}

# Set up some logical fields for the groupings
D$is_fresh <- ((D$sample.sampledMaterialType.label == "RIVER / RUNNING SURFACE WATER") | 
                 (D$sample.sampledMaterialType.label == "POND / LAKE / RESERVOIR WATER") | 
                 (D$sample.sampledMaterialType.label == "SURFACE DRAINAGE") )

D$is_saline <- (D$sample.sampledMaterialType.label == "SEA WATER")

D$is_ground <- (D$sample.sampledMaterialType.label == "GROUNDWATER")

D$is_discarded <- !(D$is_fresh | D$is_saline | D$is_ground)

# And also a string version for easier use later
D$group <- ifelse(D$is_fresh, 
                  "fresh",
                  ifelse(D$is_saline,
                         "saline",
                         ifelse(D$is_ground,
                                "ground",
                                "discarded")))

# Treating "<" detection as zero and ">" as whatever the maximum value was
D$detected <- is.na(D$resultQualifier.notation) | (D$resultQualifier.notation != "<")


# Like a data frame but with the fields named
setClass("Summary",
         slots = list(
           # table = "tbl_df",
           sample_count = "numeric",
           detected_count = "numeric",
           quantification_percentage = "numeric",
           mean = "numeric",
           median = "numeric",
           min = "numeric",
           minnonzero = "numeric",
           max = "numeric",
           sd = "numeric"
         ))

derived <- c()

# View(derived)

for (group in c("fresh", "saline", "ground"))
{
  for (method in c("Original", "Extended"))
  {
  for (det in unique(D$determinand.label))
  {
    sel <- (D$determinand.label == det) & (D$group == group) & (D$MethodName == method)
    subset <- D[sel,]
    detected_subset <- D[sel & D$detected ,]
    
    count <- nrow(subset)
    detected_count <- nrow(subset[subset$detected, ])
    
    # Some of the compounds have no detected samples, avoid warning messages from R
    mean <- 0
    median <-0
    min <- 0
    minnonzero <- 0
    max <- 0
    sd <- 0
    if (detected_count != 0) 
    {
      mean <- mean(detected_subset$result)
      median <- median(detected_subset$result)
      min <- min(detected_subset$result)
      nonzero <- detected_subset[detected_subset$result > 0 ,]
      if (nrow(nonzero) != 0)
      {
        minnonzero <- min(nonzero$result)
      }
      max <- max(detected_subset$result)
      sd <- sd(detected_subset$result)
    }
    
    derived[[group]][[det]]  <-
      new("Summary", 
          # table = subset,
          sample_count = count,
          detected_count = detected_count,
          quantification_percentage = ifelse(count == 0, 0, (100.0*detected_count) / count),
          mean = mean,
          sd = sd,
          median = median, 
          min = min,
          minnonzero = minnonzero,
          max = max
      )
  }
  
  
  # Write summary files
  f <- data.frame(Name = c("% quantification", "mean", "sd","median", "minnonzero", "min", "max"))
  
  
  # All of them would be:
  # for (det in unique(D$determinand.label))
  
  # Only these ones, in this order
  for (det in 
       c("pFoctanoate", 
         "PFHxS-B",
         "PFTrDA",
         "PFPeS",
         "FOSA",
         "PFNS",
         "PFHxS-L",
         "PFHxSA",
         "PFHpS",
         "PFDoS",
         "PFDS",
         "FBSA",
         "PFBS",
         "PFecHS",
         "N-MeFOSA",
         "EtFOSA",
         "HFPO-DA",
         "9Cl-PF3ONS",
         "8:2 FTSA",
         "6:2 FTSA",
         "4:2 FTSA",
         "ADONA",
         "5:3 FTCA",
         "11Cl-PF3OUdS",
         "PFBA",
         "PFpentncAcid", 
         "PFhexncAcid",
         "PFheptncAcid",
         "PFoctncAcid",
         "PFnonncAcid",
         "PFdecncAcid",
         "PFundencAcid",
         "PFdodencAcid",
         "PFtetdncAcid",
         "PFEESA",
         "7:3 FTCA",
         "3:3 FTCA",
         "PFOS (T)"))
  
  {
    val <- derived[[group]][[det]]
    f[renametable[det]] = c(val@quantification_percentage, val@mean, val@sd, val@median, val@minnonzero, val@min, val@max)
    
  }
  
  
  
  write.csv(f, paste("DB/", method, "_", group, "_water.csv", sep=""), row.names = FALSE)
}
}

# can have multiple sample locations that happen to have the same time recorded
cols <- c("sample.sampleDateTime", "sample.samplingPoint")
print(sprintf("%i groundwater",nrow(unique(select(D[D$is_ground,], cols))) ))
print(sprintf("%i saline",nrow(unique(select(D[D$is_saline,], cols))) ))
print(sprintf("%i fresh",nrow(unique(select(D[D$is_fresh,], cols))) ))

