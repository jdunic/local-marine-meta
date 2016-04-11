# Standardizing units

library(datamart)
library(plyr)

# Load modified uncov function set
source("datamart_UnitSetManager2_source.r")
source("datamart_mashup_source.r")


test_data <- read.csv('Data/fullData_v0.4-20141213.csv')

# Remembering to change things to the correct data type
test_data$SiSz..units. <- tolower(as.character(test_data$SiSz..units.))
test_data$PltSz..units. <- tolower(as.character(test_data$PltSz..units.))

test_data$SiSz <- as.numeric(as.character(test_data$SiSz))
test_data$PltSz <- as.numeric(as.character(test_data$PltSz))

#str(test_data)

#unique(c(test_data$SiSz..units., test_data$PltSz..units.))

# Standardize units
test_data$SiSz..units. <- gsub('acres', 'ac', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('acres', 'ac', test_data$PltSz..units.)
test_data$SiSz..units. <- gsub('km2', 'km^2', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('km2', 'km^2', test_data$PltSz..units.)
test_data$SiSz..units. <- gsub('m2', 'm^2', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('m2', 'm^2', test_data$PltSz..units.)
test_data$SiSz..units. <- gsub('m3', 'm^3', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('m3', 'm^3', test_data$PltSz..units.)
test_data$SiSz..units. <- gsub('nmi', 'nautical mile', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('nmi', 'nautical mile', test_data$PltSz..units.)
print('Double check that \'nm\' is nautical miles if you run this script')
test_data$SiSz..units. <- gsub('nm', 'nautical mile', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('nm', 'nautical mile', test_data$PltSz..units.)
test_data$SiSz..units. <- gsub('liters', 'l', test_data$SiSz..units.)
test_data$PltSz..units. <- gsub('liters', 'l', test_data$PltSz..units.)
test_data$PltSz..units.[which(test_data$PltSz..units. == 'unspecified')] <- NA


# Convert units
unique(c(test_data$SiSz..units., test_data$PltSz..units.))

convert_units <- function(value, unit) {
  if (is.na(unit)) {
    out_value <- NA
    out_unit <- NA
  } else if (unit %in% uconvlist()$Length) {
      out_value <- uconv(x = value, from = unit, to = 'm', uset = 'Length')
      out_unit <- 'm'
    } else if (unit %in% uconvlist()$Area) {
        out_value <- uconv(x = value, from = unit, to = 'm^2', uset = 'Area')
        out_unit <- 'm^2'
      } else if (unit %in% uconvlist()$Volume) {
          out_value <- uconv(x = value, from = unit, to = 'm^3', uset = 'Volume')
          out_unit <- 'm^3'
        } else if (!unit %in% c(uconvlist()$Length, uconvlist()$Area, uconvlist()$Volume)) {
            print(unit)
            stop('unit not in uconvlist of Length, Area, or Volume')
        }
    df <- data.frame('new_val' = as.numeric(out_value), 'new_unit' = as.character(out_unit))
    return(df)
}


test_data2 <- adply(test_data, 1, function(x) convert_units(x$SiSz, x$SiSz..units.))
colnames(test_data2)[colnames(test_data2)=="new_val"] <- "SiteSize"
colnames(test_data2)[colnames(test_data2)=="new_unit"] <- "SiteSizeUnits"

test_data3 <- adply(test_data2, 1, function(x) convert_units(x$PltSz, x$PltSz..units.))
colnames(test_data3)[colnames(test_data3)=="new_val"] <- "PlotSize"
colnames(test_data3)[colnames(test_data3)=="new_unit"] <- "PlotSizeUnits"
