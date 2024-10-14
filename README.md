# Replication Package for 'Post-Automation Workforce Dynamics in (Non-)Multinationals'

**Last update: Oct 14, 2024**

## Overview

This replication package contains the scripts required to replicate the results of the paper [*"Post-Automation Workforce Dynamics in (Non-)Multinationals"*](https://mrcsrsch.github.io/research/3_2024_automation/) by Marcus Roesch. Due to the confidential nature of the data, the analyses must be executed on the servers of Statistics Netherlands (CBS). For more details on accessing the data, see the "Source Data Access" section.

## Folder Structure

- `scripts/`: Contains R scripts to run the analysis.
- `data/`: Stores intermediate datasets created during the analysis.
- `data/source_data/`: Place the (confidential) source data files here (see "Source Data Access").
- `outputs/`: Stores result figures and tables for the paper.

## Replication Instructions

### Data Preparation
1. Place the source data in the `data/source_data/` folder. Ensure that the file `data/source_data/EDUC/oplrichting_definities.csv` and the packages in `data/packages/` are also present.
2. Specify the SQL server connection for the firm-level source data in the `01_company_data.R` and `02D_IT_Machinery_spikes.R` scripts (see comments in the scripts for guidance).
3. Adjust the file paths in `00_main.R` to match your directory structure.

### Running the Analysis
4. Run `00_main.R` from the `scripts/` folder in R. This will:
   - Build the analysis datasets from the source data.
   - Reproduce the result figures and tables from the paper.
5. The resulting tables and figures will be saved in the `outputs/` folder.

### Software and Hardware Requirements
- **R version >= 4.0.0**
- The analysis was performed on a server with an 8-core processor running at 3.19 GHz and 48 GB of memory. Ensure that the server at CBS has sufficient computational resources to handle large datasets and long runtimes.
- Key R packages: `data.table`, `fixest`, `ggplot2`

## Source Data Access

This project uses confidential datasets provided by Statistics Netherlands (CBS). The specific datasets are:

### Worker-Level Datasets
- **SPOLISBUS** (employer-employee data): 2010 - 2021
- **GBAPERSOON** (worker demographics): 2021
- **NIET_GBAPERSOON** (worker demographics): 2021
- **SECMBUS** (socio-economic category): 2021
- **HOOGSTEOPLTAB** (highest educational attainment) and **OPLEIDINGSNRREFV30** (reference book for education level and subject translations): 2021
- **BESTUURDERS** (high-level management according to KVK registration): 2010 - 2021
- **EBB** (occupation codes for a sample): 2010 - 2021

### Firm-Level Datasets
- **Productiestatistiek** (financial data of firms, especially the variable "BEDRLST348400 - Automatiseringskosten"): 2010 - 2021
- **Investeringen** (investments in material and immaterial assets by firms): 2010 - 2021
- **Multinationals** (foreign and domestic multinationals): 2010 - 2021
- **BDK** (firm demographics, especially "BeginBEID" for consistent firm tracking): 2010 - 2021
- **Abr** (company group IDs): 2010 - 2021
- **Abr_pab** (NACE industry codes): 2010 - 2021
- **ihg** (export and import statistics of firms): 2010 - 2021
- **bedrijfs_econ_data** (firm sales data): 2010 - 2021

Due to confidentiality, we cannot share the data directly. To replicate the analysis, researchers can obtain access to these datasets through the [CBS Microdata Service](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research). Please note that access is subject to CBS's terms and fees. All data processing must occur on CBS servers, and exported outputs are subject to privacy and confidentiality checks.

To request access, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl).

### Important Note

We had employee access to CBS data, which did not require requesting data through the Microdata Service. However, to the best of our knowledge, the analyses can be replicated with Microdata Service access using the provided code. If any of the individual datasets are incomplete or unavailable through the Microdata Service, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl) with your specific request.