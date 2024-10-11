##################################################################################
### Investment data ####
# This script loads the investment data from the KIO SQL servers 
# It also identifies spikes in investment data (Computers, Software, Machinery)
# If you do not have access to this SQL server, I recommend that you skip the SQL calls and load the respective datasets from disc.

#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
#if (!require("haven")) install.packages("haven"); library("haven")
if (!require("RODBC")) install.packages("RODBC"); library("RODBC")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (packageVersion("ggplot2")!="3.4.2") warning("Analysis ran in ggplot2 version 3.4.2 - consider up- or downgrading")

#### output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis, "step1/"))
if (!dir.exists(paste0(map_data_analysis, "step1/company/"))) dir.create(paste0(map_data_analysis, "step1/company/"))
map_output_raw <- paste0(map_data_analysis, "step1/company/")

if (!dir.exists(paste0(map_data_analysis, "step4/"))) dir.create(paste0(map_data_analysis, "step4/"))
map_output_here <- paste0(map_data_analysis, "step4/")

#### load data #### 
selected_firms <- readRDS(paste0(map_data_analysis, "step3/selected_firms_2010_2021.rds")) # selected firms panel
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021

##### set SQL connection ###### 
server_id <- NA # specify server ID here
database_id <- NA # specify database ID here 

if (is.na(server_id) | is.na(database_id)) warning("You need to specify the server and database ids for this script to run. Alternatively, you could remove the SQL calls and load the respective datasets from disc.")

# open SQL connection
dbhandle <- odbcDriverConnect(paste0("driver={SQL Server};server=", server_id, ";database=", database_id, ";trusted_connection = yes"))

##################################################################################
##################################################################################
### get investment data from SQL server ###

# years 2012 to 2020
investment <- list()
i <- 0
for (J in 2012:2020){
  # message
  cat(paste(Sys.time(), J), "\n")
  i <- i+1
  query <- paste0("
                SELECT [beid] as SBEID
                , Kw_1
                , Kw_2
                , [C010004] as Computers_inv
                , [C010003] as Self_Computers_inv

                , [C015004] as Software_inv
                , [C015002] as Custom_Software_inv
                , [C015003] as Self_Software_inv

                , [C012004] as Machinery_inv
                , [C012003] as Self_Machinery_inv

                , [C011004] as Communication_inv
                , [C014004] as Total_inv_material
                , [C017004] as Total_inv_immaterial
                FROM [IOH_DATA_ANA].[Data].[Investeringen_VasteActiva_Nationaal_", J, "_(d)]
                ")
  investment[[i]] <- setDT(sqlQuery(channel = dbhandle, query = query, as.is = TRUE))
  investment[[i]][, year := J]
}

# combine
investment <- rbindlist(investment)

# change SBEID to integer
investment[, SBEID := as.integer(SBEID)]

# Save raw data
saveRDS(investment, paste0(map_output_raw, "investments_2012_2020.rds"), compress = T)

##################################################################################
### clean investment data ###
## only keep good responses ("A" Response in survey; "C" uitbijter)
investment <- investment[Kw_1=="A" & Kw_2 %in% c("A"), !c("Kw_2", "Kw_1")]
# calculate total investments
investment[, Total_inv := Total_inv_material + Total_inv_immaterial]
# calculate IT investments
investment[, IT_inv := Computers_inv + Software_inv + Communication_inv]

### subset to firms in selected_firms ####
investment[, .(.N, uniqueN(SBEID))]
investment[, keep := SBEID %in% selected_firms[, unique(SBEID)]]
investment[, .(.N, uniqueN(SBEID)), by=keep]
investment <- investment[keep ==T, !c("keep")]

##################################################################################
### identify investment spikes (Computers, Software, Machinery) ###

# set vars for calculating spikes
vars <- c("IT_inv", "Machinery_inv")

# work on a copy
investment_2 <- copy(investment)

# match with firms
firms <- merge(selected_firms[year>=2012 & year<=2020, c("SBEID", "year", "MNE", "workers_fte")], investment_2, by=c("SBEID", "year"), all.x=T)
firms[, remove := all(is.na(Total_inv)), by=SBEID]
firms <- firms[remove==F, !c("remove")]

## keep firms with at least 8 years of observed data --> time frame needed for event study
firms[, keep := .N>=8, by=SBEID]
firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
firms <- firms[keep==T, !c("keep")]

## keep firms with at least 3 years of investment data 
firms[, keep := sum(!is.na(Total_inv))>=3, by=SBEID]
firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
firms <- firms[keep==T, !c("keep")]

# What is the distribution of gaps in investments?
tt <- firms[order(SBEID, year), .(ceiling((sum(!is.na(Total_inv))/.N)^(-1))), by=.(SBEID, MNE)][, .(SBEID, MNE, V1=fifelse(V1==Inf, 0, V1))][, .N, by=.(V1, MNE)][order(V1), .(V1, cumsum(N/sum(N)*100)), by=MNE]
ggplot(data = tt, aes(x=V1, y=V2, color=MNE)) + geom_point() + 
  xlab("Observed every x year") + ylab("Cumulative distribution")

# keep firms that are observed at least every third year on average
firms[order(SBEID, year), keep := {t <- (sum(!is.na(Total_inv))/.N)^(-1)
t <- fifelse(t==Inf, 1, t)
t <= 3}
, by=.(SBEID)]
firms[, .(.N, firms=uniqueN(SBEID), MNEs=uniqueN(SBEID[MNE]), Domestic = uniqueN(SBEID[!MNE])), by=keep]
firms <- firms[keep==T, !c("keep")]
investment_2 <- firms[!is.na(Total_inv), c("SBEID", "year", "workers_fte", vars, "Total_inv", "Total_inv_material", "Total_inv_immaterial"), with=F]

# add real investment expenditures
investment_2 <- merge(investment_2, cpi, by="year")
investment_2[, paste0("real_", c(vars, "Total_inv")) := lapply(.SD, function(x) x*factor), .SDcols=c(vars, "Total_inv")]

# calculate real spending per (average) worker
investment_2[, paste0("real_", vars, "_pw") := lapply(.SD, function(x) x*factor/mean(workers_fte)), by=SBEID, .SDcols=c(vars)]

#### identify spikes #####

# calculate investment shares
# Note that some firms only one type of investment throughout; or they have mainly one type of investment
# --> this complicates mean(real_Total_inv-x) // For now I just accept that shares can explode
investment_2[, paste0("share_", vars) := lapply(.SD, function(x) x / mean(real_Total_inv-x)), by=SBEID, .SDcols=paste0("real_", vars)]

investment_2[, paste0("share_", vars) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols=paste0("share_", vars)]
investment_2[, paste0("share_", vars) := lapply(.SD, function(x) fifelse(x==Inf, 1, x)), .SDcols=paste0("share_", vars)] # set share to one when only expenditure observed

# calculate comparison ratio (ratio of current year's share to average share in other years)
investment_2[, paste0("ratio_", vars) := lapply(.SD, function(x) {avg <- (sum(x)-x)/(.N-1) # average investments excluding current row
ratio <- fifelse(x==0, 0, fifelse(avg == 0, 999, x/avg)) # take care of 0's --> where x == 0 and avg == 0; AND where x > 0 and avg == 0 
return(ratio)}),  # i.e. I am allowing for one-shot investments here by setting the ratio to a high number
by="SBEID",
.SDcols = paste0("share_", vars)] 
# spike if thrice the average expenditure of other years (in a year with observed expenditures). 
investment_2[, paste0("spike_", vars) := lapply(.SD, function(x) fifelse(x>= 3, T, F)), .SDcols = paste0("ratio_", vars)] 

investment_2[, lapply(.SD, sum), .SDcols = paste0("spike_", vars)]

# drop spike if level of expenditure is minimum observed at firm level
# Note that with costs/worker this can never be the case
investment_2[, paste0("spike_", vars) := lapply(vars, function(x) fifelse(get(paste0("real_", x))<=min(get(paste0("real_", x))), 
                                                                          F, get(paste0("spike_", x)))), by=SBEID]

investment_2[, lapply(.SD, sum), .SDcols = paste0("spike_", vars)]

# see Bessen scripts: drop spikes if less than 25th percentile of non-zero shares
investment_2[, paste0("spike_", vars) := lapply(vars, function(x) fifelse(get(paste0("share_", x)) < quantile(get(paste0("share_", x))[get(paste0("share_", x))>0], p = 0.25), 
                                                                          F, get(paste0("spike_", x))))]
investment_2[, lapply(.SD, sum), .SDcols = paste0("spike_", vars)]

# identify adopters 
investment_2[, paste0("adopter_", vars) := lapply(.SD, any), by=SBEID, .SDcols = paste0("spike_", vars)]
investment_2[, paste0("change_year_", vars) := lapply(.SD, function(x) fifelse(any(x), min(year[x]), NA_integer_)), by=SBEID, .SDcols = paste0("spike_", vars)]

# create dummy for firm-year represented in investment data
investment_2[, invest_data := T]

### save investment spike data #### 
saveRDS(investment_2, file=paste0(map_output_here, "investment_spikes.rds"), compress = T)
