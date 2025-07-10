library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(tibble)
library(dplyr)


srcfilepath <- "./Original EA Fish DB/"
dstfilepath <- "./Extended EA Fish DB/"

filenames <- c("Freshwater fish PFAS analysis 2022",
               "PFAS Fish Spain UoB_IDAEA",
               "PFAS Fish UK UoB_IDAEA",
               "Saline Fish PFAS analysis 2020",
               "Saline Fish PFAS analysis 2022",
               "Saline Fish Tissue PFAS analysis 2022")

dir.create(dstfilepath, showWarnings="FALSE")

# Got all the input excel files
sources <- map(filenames, function (n) {
    path <- paste(srcfilepath, n, ".xlsx", sep="")
    read_xlsx(path)
}) %>% set_names(filenames)

# Do something with them
mutated <- sources




is_pfas_name <- function (N) {
    ## conservative and based on the known column headers, don't want to
    ## miss out any of the pfas measurements    
    !(
        (any (N == c("Lat", "Long", "River basin", "Wild or farmed", "Replicate", "Supermarket", "...1"))) |    
        (startsWith(N, "Sample")) |
        (startsWith(N, "Tissue")) |
        (startsWith(N, "Species")) |
        (startsWith(N, "Date")) |
        (startsWith(N, "SMPT_")) |
        (startsWith(N, "Number")) |
        (startsWith(N, "Weight")) |
        (startsWith(N, "Grams")) |
        (startsWith(N, "Unit")) |
        (startsWith(N, "Fish")) |
        (startsWith(N, "Total"))        
        )
}



mutated <- map(mutated, function (D) {
    N <- names(D)
    pfas_names <- N[unlist(map(N, is_pfas_name))]

    for (n in pfas_names) {
        det <-paste(n, "detected")         
        raw <- D[[n]]

        D[[det]] <- ifelse(startsWith(as.character(raw), "<"), 0, as.numeric(raw))
    }

    # calculate the new column names in the desired order
    m <- c()
    for (n in N) {
        m <- c(m, n)            
        if (is_pfas_name(n)) {
            m <- c(m, paste(n, "detected")) 
        }
    }

    D <- D[, m]
    
    D
})



# Write them out as csv
map(filenames, function (n) {
    ## write.csv works but puts quotes everywhere
    ## write would work once whatever unexpected data type has been found and removed
    ## this will do for now
    path <- paste(dstfilepath, n, ".csv", sep="")
    write.table(mutated[[n]], sep=",", path, row.names=FALSE )
})

