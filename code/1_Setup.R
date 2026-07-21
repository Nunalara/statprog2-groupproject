# env_setup.R - Environment Setup Script
# this script is necessary to download all the needed packages and the data itself

# Define necessary packages
packages <- c( "readxl",
              "tidyr",
              "dplyr",
              "ggplot2",
              "maps")

# Pakete installieren und laden
for (pkgs in packages) {
  if (!require(pkgs, character.only = TRUE)) {
    install.packages(pkgs)
    library(pkgs, character.only = TRUE)
  }
}

## Download data
data <- readxl::read_excel(
  "data-raw/EPLP_Dataset_Workbook_v2.xlsx",
  sheet = "Dataset",
  skip = 1
)

glimpse(data) ## a lot if NAs encoded as -98, -99

key_vars <- data |>
  select(country, year,
         mat_m_ld_bb, mat_m_ld_ab,
         mat_v_ld_bb, mat_v_ld_ab,
         par1_ld, par1_fr, par1_for_whom,
         par2_ld, par2_fr, par2_for_whom,
         par3_ld, par3_rr, par3_for_whom,
         currency)


## final data set
data_expl <- data |>
  select (country, year,
          mat_m_ld_bb, mat_m_ld_ab,
          mat_v_ld_bb, mat_v_ld_ab,
          par1_ld, par1_fr, par1_for_whom,
          par2_ld, par2_fr, par2_for_whom,
          par3_ld, par3_rr, par3_for_whom,
          currency) |>
  ## filter out NAs for numeric variables
  mutate(across(where(is.numeric), ~ na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -99))) |> 
  ## filter NAs for char variables
  mutate(par1_for_whom = na_if(par3_for_whom, "-98"),
         par2_for_whom = na_if(par3_for_whom, "-98"),
         par3_for_whom = na_if(par3_for_whom, "-98")) |> 
  ## calculate maternity leave
  mutate(
    mat_ld_total = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab, mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
    mat_mandatory = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab)), na.rm = TRUE),
    mat_voluntary = rowSums(across(c(mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
  )


## check how NAs work for new maternity variables 
data_expl |>
  filter(is.na(mat_m_ld_bb) & is.na(mat_m_ld_ab) & is.na(mat_v_ld_bb) & is.na(mat_v_ld_ab)) |>
  select(country, year, mat_ld_total)
## No case of all NAs


## fixed colors
mat_type <- c(
  "Voluntary" = "grey",
  "Mandatory" = "darkred"
)

