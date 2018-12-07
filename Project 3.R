# Project 3
## Lori Kim

#### Part A: Map
# setwd("/Volumes/LEXAR/DATA SCIENCE/Projects/Project 3")
setwd("/Volumes/LEXAR/DATA SCIENCE/Projects/Project 3")

install.packages("ggplot2") 
library(ggplot2)
library(readxl) 
library(dplyr)

install.packages("maps")
library(maps)

cnty = map_data("county")
gusa = map_data("state")
head(cnty)
head(gusa)
head(county.fips) # fips is the id of the county
cnty

cnty2 = cnty %>% mutate(polyname=paste(region,subregion, sep=",")) %>% # mutate creates a new variable 'polyname' and gets that from the data set
  left_join(county.fips, by="polyname")
head(cnty2)

chci = read.csv("chci.csv")
colnames(chci)[2]="fips"   # change the column name of "id" to "fips"
cnty3 = cnty2 %>% left_join(chci, by="fips")
head(cnty3)

# ggplot(cnty3, aes(long,lat,group=group)) + geom_polygon(aes(fill = chci09), colour = rgb(1,1,1,0.2)) + coord_quickmap() 

# Without County boundary --> colour = NA
# ggplot(cnty3, aes(long,lat,group=group)) + 
#  geom_polygon(aes(fill = chci16), colour = NA) + coord_quickmap() +
#  scale_fill_gradient(low="lightyellow2", high="darkgreen")

install.packages("RColorBrewer")
library(RColorBrewer)

# Generating a divergent color scheme
qt1=quantile(cnty3$chci09, probs=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), na.rm=T)
cnty3$chci09a = cut(cnty3$chci09, breaks=qt1, labels=paste(qt1[-1]))

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci16a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu")

# Other palettes-bidirection:Spectral,RdYlGn,RdYlBu,RdGy,PuOr,PRGn,PiYG, BrBG
# Other palettes-onedirection:YlOrRd,YlOrBr,YlGnBu,YlGn,Reds,RdPu,Purples,PuRd,
# PuBuGn,PuBu,OrRd,Oranges,Greys,Greens,GnBu,BuGn,Blues

state_layer=geom_polygon(aes(long,lat,group=group), fill=NA, data=gusa,color = "black") 

ggplot(cnty3, aes(long,lat,group=group)) + 
  geom_polygon(aes(fill = chci09a), colour = rgb(1,1,1,0.2)) + coord_quickmap() +
  scale_fill_brewer(palette = "RdBu") + state_layer

### Part A: Fertility Chart
install.packages("tidyverse") 
library(tidyverse)

wdi = data.frame(read_excel("W03b_wdi.xlsx"))     # World Development Indicators

# Get variables for life expetancy, total population, and fertility rate
wdi.le = subset(wdi, Indicator.Code == "SP.DYN.LE00.IN")
wdi.pop = subset(wdi, Indicator.Code == "SP.POP.TOTL")
wdi.fert = subset(wdi, Indicator.Code == "SP.DYN.TFRT.IN")

wdi3v.60a=merge(wdi.le[,c("Country.Name","Country.Code","X1960")], wdi.pop[,c("Country.Code","X1960")], by="Country.Code")

wdi3v.60b=merge(wdi3v.60a, wdi.fert[,c("Country.Code","X1960")], by="Country.Code")

# Remove those rows that are not countries but regions
wdi3v.60=wdi3v.60b[!wdi3v.60b$Country.Code %in% c("ARB",	"CSS",	"CEB",	"EAR",	"EAS",	"EAP",	"TEA",	"EMU",	"ECS",
                                                  "ECA",	"TEC",	"EUU",	"FCS",	"HPC",	"HIC",	"IBD",	"IBT",	"IDB",	
                                                  "IDX",	"IDA",	"LTE",	"LCN",	"LAC",	"TLA",	"LDC",	"LMY",	"LIC",	
                                                  "LMC",	"MEA",	"MNA",	"TMN",	"MIC",	"NAC",	"INX",	"OED",	"OSS",	
                                                  "PSS",	"PST",	"PRE",	"SST",	"SAS",	"TSA",  "SSF",	"SSA",	"TSS",
                                                  "UMC",	"WLD"),]

colnames(wdi3v.60)=c("code", "country", "life","pop","fertility")
p6=ggplot(wdi3v.60, aes(x=life, y=fertility, size=pop)) + geom_point(alpha=0.4) + scale_size_continuous(range=c(0.5, 20)) 
p6

### Part B:
## two linear regressions
# logerror

setwd("/Volumes/LEXAR/DATA SCIENCE/Projects/Project 3/zillow prize project data")

# Variables
properties <- read.csv('properties_2016.csv')
transactions <- read.csv('train_2016_v2.csv')
sample_submission <- read.csv('sample_submission.csv')

# Rename the variable names
# FunctionX(dataA) is the same as dataA %>% functionX / is the same as doing properties$parcelid, multiple times

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

# Convert dummary variables (Y and N) to (1 and 0)
properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

# Take a look at the data
properties <- properties %>% select(id_parcel, build_year, starts_with("area_"), 
                                    starts_with("num_"), starts_with("flag_"), starts_with("region_"), everything())

# Take a look at the transaction data
tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))

# Absolute logerror
transactions <- transactions %>% mutate(abs_logerror = abs(logerror))

cor_tmp <- transactions %>% left_join(properties, by="id_parcel")

# logerror
y1 = lm (logerror ~ tax_property + area_live_finished + num_garage + num_bedroom + num_bathroom, data=cor_tmp)
summary(y1)

### the model for logerror shows that property tax and the finished area lived very statistically significant
### and the adjusted r-squared shows that this model is not, overall, highly significant.

# Absolute logerror
y2 = lm (abs_logerror ~ tax_property + area_live_finished + num_garage + num_bedroom + num_bathroom, data=cor_tmp)
summary(y2) 

### this model for the absolute logerror has changed and shows an increase in statistical significance for the variables.
### For example, the number of bathrooms in the house shows a higher level of significance.
### Also, the Adjusted R-squared has also increased to 0.0049 which shows that the combination of these variables have a greater
### correlation with the dependent variable
