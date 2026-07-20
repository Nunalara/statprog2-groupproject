# env_setup.R - Environment Setup Script
# Dieses Skript richtet die Arbeitsumgebung für das Projekt ein.

# Notwendige Pakete definieren (können wir ja einfach erweitern falls wir mehr brauchen)
packages <- c( "readxl",
              "tidyr",
              "dplyr",
              "ggplot2",
              "skimr",
              "maps",
              "forcats",
              "scales")

# Pakete installieren und laden
for (pkgs in packages) {
  if (!require(pkgs, character.only = TRUE)) {
    install.packages(pkgs)
    library(pkgs, character.only = TRUE)
  }
}



data <- readxl::read_excel(
  "data-raw/EPLP_Dataset_Workbook_v2.xlsx",
  sheet = "Dataset",
  skip = 1
)

glimpse(data)

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
  mutate(across(where(is.numeric), ~ na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -99))) |>
  ## calculate maternity leave
  mutate(
    mat_ld_total = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab, mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
    mat_manditory = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab)), na.rm = TRUE),
    mat_voluntary = rowSums(across(c(mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
  )


## check how NAs work
data_expl |>
  filter(is.na(mat_m_ld_bb) & is.na(mat_m_ld_ab) & is.na(mat_v_ld_bb) & is.na(mat_v_ld_ab)) |>
  select(country, year, mat_ld_total)
## No case of all NAs


## fixed color scale
