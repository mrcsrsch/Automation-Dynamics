##################################################################################
### Extract SBEID-years from POLIS #####
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")

### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step2/"))) dir.create(paste0(map_data_analysis, "step2/"))
map_output_here <- paste0(map_data_analysis, "step2/")

##################################################################################
#### load POLIS (no internships; age 18-65) #######
POLIS <- list()
k <- 0
for (current_year in 2010:2021){ 
  cat(paste(Sys.time(), ":", current_year), "\n")
  k <- k +1
  POLIS[[k]] <- readRDS(paste0(map_data_analysis, "step1/SPOLIS_yearly/POLIS_", current_year, ".rds"))
}
rm(current_year, k)

POLIS <- rbindlist(POLIS)
gc()

#### extract SBEID-years and save #####
POLIS_firms <- POLIS[, .(workers_fte = sum(deeltijdfactor)), by=c("SBEID", "year")]
saveRDS(POLIS_firms, paste0(map_output_here, "POLIS_firms.rds"), compress=T)


