##################################################################################
### SQL company data #####
# This script loads the company data from the KIO SQL servers and performs basic manipulations
# If you do not have access to this SQL server, I recommend that you skip the SQL calls and load the respective datasets from disc.
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
#if (!require("haven")) install.packages("haven"); library("haven")
if (!require("RODBC")) install.packages("RODBC"); library("RODBC")

#### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
if (!dir.exists(paste0(map_data_analysis, "step1/company/"))) dir.create(paste0(map_data_analysis, "step1/company/"))
map_output_here <- paste0(map_data_analysis, "step1/company/")

##### set SQL connection ###### 
server_id <- NA # specify server ID here
database_id <- NA # specify database ID here 

if (is.na(server_id) | is.na(database_id)) warning("You need to specify the server and database ids for this script to run. Alternatively, you could remove the SQL calls and load the respective datasets from disc.")

# open SQL connection
dbhandle <- odbcDriverConnect(paste0("driver={SQL Server};server=", server_id, ";database=", database_id, ";trusted_connection = yes"))

##################################################################################
##################################################################################

#### Sales (SBS + Baseline?) #####
query <- "SELECT [Vbe_Identificatie] as SBEID
,[jaar] as year
,[omzet_jj] as sales_bedrijfsecondata
FROM [POC_EED_ANA].[dbo].[bedrijfs_econ_data]"

bedrijfsecondata <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
bedrijfsecondata[, sales_bedrijfsecondata := as.numeric(sales_bedrijfsecondata)/1000]

#### BDK (for BEGINBEID) #####
query <- paste0(
  "SELECT *
  FROM [IOH_DATA_ANA].[AMUR].[BDK_verrijkt_vanaf2007]
  ")
bdk <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))
saveRDS(bdk, paste0(map_output_here, "bdk.rds"), compress=T)

#### company groups BEIDs ######
# Note 02.10.23: Years 2005-2023 available

query <- paste0(
  "SELECT [JAAR] as year
  ,[BEID] as SBEID
  ,[OGID] as OG_BEID
  FROM [POC_EED_ANA].[dbo].[LBRU_KoppelingBeOgNormaal]
  ")
beog <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))

#### Nace Code / size class ###### 
# Note 02.10.23: Years 2005-2023 available
query <- paste0("
                select p.Vbe_Identificatie, p.jaar, p.kwartaal, p.gk_strat, sbi08pub_kw 
                from [POC_EED_ANA].[dbo].pab15_jaar p
                ")

beidkenmerken <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
beidkenmerken <- beidkenmerken[nchar(Vbe_Identificatie) == 8]
setnames(beidkenmerken, 1:5, c("SBEID", "year", "quarter", "size_class", "sbi08pub_kw"))

# keep latest industry classification per SBEID, year
setorderv(beidkenmerken, cols=c("SBEID", "year", "quarter"))
beidkenmerken <- beidkenmerken[, .(nace_sbi08 = sbi08pub_kw[.N], size=size_class[.N]), by=c("SBEID", "year")]

#### Firm birth ########
# Note 02.10.23: available up to 2023
query <- paste0("
                SELECT * FROM [POC_EED_ANA].[dbo].[Ontstaansdatum_BE]
                ")
firmbirth <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))

# manipulate 
firmbirth[, Oprichtingsdatum  := as.IDate(Oprichtingsdatum)]
firmbirth[, firm_birthyear := as.integer(year(Oprichtingsdatum))]
firmbirth[, SBEID := as.integer(BE_ID)]
firmbirth <- firmbirth[, c("SBEID", "firm_birthyear")]


#### Exports/Imports on commodity code level ########
# years 2010 to 2021
GSRT <- list()
i <- 0
for (J in 2010:2021){
  # message
  cat(paste(Sys.time(), J), "\n")
  i <- i+1
  query <- paste0("
                select * from [IOH_DATA_ANA].[Data].[IHG_ABR_Koppeling_", J, "]
                ")
  GSRT[[i]] <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))
}
GSRT <- rbindlist(GSRT)

# prepare
GSRT <- GSRT[!is.na(BEID), ]
GSRT[GSRT=="        ", GSRT := NA_character_]

# create values
GSRT[IMP_EXP=="I", import := as.numeric(WAARDE)/1000]
GSRT[IMP_EXP=="U", export := as.numeric(WAARDE)/1000]
# remove reexports from exports
GSRT[!is.na(export) & !is.na(WEDERUITVOER), export := export-as.numeric(WEDERUITVOER)/1000]
# correct negative values
GSRT[export < 0 | is.na(export), export := 0] # 4 observations with NA
GSRT[import < 0 | is.na(import), import := 0] # 2 observations with NA

# calculate total imports/exports; and number of goods
import_export <- GSRT[, .(import = sum(import), import_goods = uniqueN(GSRT[IMP_EXP=="I" & !is.na(GSRT)]), 
                          export = sum(export), export_goods = uniqueN(GSRT[IMP_EXP=="U"  & !is.na(GSRT)])),  
                      .(SBEID=BEID, year=JAAR)]

##### MNEs (2010 - 2021) ######

# get overall MNEs source (2010-2022)
query <- paste0("
                SELECT *
                FROM [IOH_DATA_ANA].[Data].[Multinationals]
                ")
MNEs <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE)) 
MNEs <- MNEs[, .(SBEID = as.integer(BEID), year=as.integer(jaar), BUI_MUL=as.logical(BUI_MUL), NED_MUL=as.logical(as.integer(NED_MUL)))]
MNEs[year %in% 2010:2021, keep := BUI_MUL|NED_MUL, by=SBEID]
MNEs <- MNEs[keep==T, !c("keep")]

saveRDS(MNEs, file=paste0(map_output_here, "MNEs_2010_2021.rds"))

##### save above ##### 
saveRDS(beog, paste0(map_output_here, "beog.rds"), compress=T)
saveRDS(beidkenmerken, paste0(map_output_here, "beidkenmerken.rds"), compress=T)
saveRDS(firmbirth, paste0(map_output_here, "firmbirth.rds"), compress=T)
saveRDS(import_export, paste0(map_output_here, "ihg.rds"), compress=T)
saveRDS(bedrijfsecondata, paste0(map_output_here, "bedrijfsecondata.rds"), compress=T)