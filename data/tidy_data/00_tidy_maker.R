# libraries -------
pacman::p_load(tidyverse, here, unpivotr, tidyxl)

# cleaning --------
## ratio ----------
xlsx_sheet_names(path = here("data", "raw_data", "ratio_alumnos_secundaria.xlsx"))
xlsx_cells(path = here("data", "raw_data", "ratio_alumnos_secundaria.xlsx"),
           sheets = "Distrital") |> 
  filter(row > 5, row <= 1886) |> 
  filter(!is_blank) |> 
  select(row, col, data_type, character, numeric, local_format_id) -> all_cells

xlsx_formats(path = here("data", "raw_data", "ratio_alumnos_secundaria.xlsx")) -> formats

### just-one-mini-tab
all_cells |> 
  filter(between(row, 6, 1886)) |> 
  filter(between(col, 27, 31)) -> mini_tab

mini_tab |> 
  behead("up", year) |> 
  behead("left", codigo) |> 
  behead("left", department) |> 
  behead("left", provincia) |> 
  behead("left", distrito) |> 
  select(row, col, numeric, codigo, department, provincia, distrito) |> 
  filter(!is.na(numeric)) 

unpivot <- function(cells){
  cells |> behead("up", year) |> 
    behead("left", codigo) |> 
    behead("left", department) |> 
    behead("left", provincia) |> 
    behead("left", distrito) |> 
    select(row, col, numeric, codigo, department, provincia, distrito, year) |> 
    filter(!is.na(numeric))
}


### partition-the-excel-sheet 

all_cells |> 
  filter(character == "Código") |> 
  select(row, col) -> corners

all_cells |> partition(corners) -> partitions
partitions |> 
  mutate(
    cells = map(cells, unpivot)
  ) |> unnest(cells) |> 
  select(-c(corner_row, corner_col)) -> data_ratio

### write-tidy-file ----
# write_rds(data_ratio, file = here("data", "tidy_data", "tidy_ratio.rds"))

## ece ----------
xlsx_sheet_names(path = here("data", "raw_data", "ece_2s_15-19.xlsx")) |> 
  pluck(2) -> target_sheet
xlsx_cells(path = here("data", "raw_data", "ece_2s_15-19.xlsx"),
           sheets = target_sheet) -> all_cells_2
all_cells_2 |> filter(col <= 165) |> 
  filter(!is_blank) |> 
  select(row, col, data_type, character, numeric, local_format_id) -> all_cells_2

all_cells_2 |> 
  filter(col == 9, row >= 4) |> 
  select(row, col, departamento = character) -> row_header_1

all_cells_2 |> 
  filter(col == 10, row >= 4) |> 
  select(row, col, provincia = character) -> row_header_2

all_cells_2 |> 
  filter(col == 11, row >= 4) |> 
  select(row, col, distrito = character) -> row_header_3

all_cells_2 |> 
  filter(col == 13, row >= 4) |> 
  select(row, col, gestion = character) -> row_header_4


all_cells_2 |> 
  filter(row == 1, col >= 5) |> 
  select(row, col, year = numeric) -> col_header_1

all_cells_2 |> 
  filter(row == 2) |> 
  select(row, col, col_header_2 = character) -> col_header_2

all_cells_2 |> 
  filter(row == 3, col >= 16) |> 
  select(row, col, col_header_3 = character) -> col_header_3


all_cells_2 |> 
  filter(row >= 4, col >= 16) |> 
  filter(data_type == "numeric") |> 
  select(row, col, measurement = numeric) -> data_cells


data_cells |> 
  enhead(row_header_1, "left") |> 
  enhead(row_header_2, "left") |> 
  enhead(row_header_3, "left") |> 
  enhead(row_header_4, "left") |> 
  enhead(col_header_1, "up-left") |> 
  enhead(col_header_2, "up-left") |> 
  enhead(col_header_3, "up") -> data_ece

data_ece |> select(-c(row, col)) |> distinct(col_header_3) |> 
  pull(col_header_3) |> pluck(10) -> target_col



data_ece |> filter(col_header_3 == target_col) |>
  mutate(across(.cols = c("departamento", "provincia", "distrito"),
                .fns = ~.x |> str_squish())) |> 
  mutate(departamento = ifelse(departamento == "LIMA METROPOLITANA", "LIMA", departamento)) |> 
  select(departamento, provincia, distrito, year, measurement, col_header_2) |> 
  group_by(departamento, provincia, distrito, year, col_header_2) |> 
  summarise(sapo = mean(measurement, na.rm = T)) |> 
  pivot_wider(names_from = col_header_2, values_from = sapo) |> 
  ungroup() -> data_to_join_1

library(tidylog)
data_ece |>
  filter(col_header_3 == target_col) |>
  mutate(across(.cols = c("departamento", "provincia", "distrito"),
                .fns = ~.x |> str_squish())) |> 
  mutate(departamento = ifelse(departamento == "LIMA METROPOLITANA", "LIMA", departamento)) |> 
  group_by(departamento, provincia, distrito, year) |> 
  summarise(perc_estatal = mean(gestion == "Estatal", na.rm = T)) |> 
  select(year, everything()) |> ungroup() -> data_to_join_2

data_to_join_2

data_to_join_1 |> 
  left_join(data_to_join_2, by = c("departamento", "provincia", "distrito", "year")) -> data_f_ece 

data_f_ece

### write-tidy-file ------
# write_rds(data_f_ece, here("data", "tidy_data", "01_tidy_ece.rds"))

## enaho -----------
read_rds(here("data", "raw_data", "workfile_data.rds")) -> data_enaho
data_enaho |> 
  pluck("tibble_2", 1) |> 
  select(departamento, provincia, distrito, pobreza, factor07, mieperho) |> 
  mutate(pobreza = case_when(pobreza == 3 ~ 0,
                             pobreza == 2 ~ 1,
                             pobreza == 1 ~ 1)) |> 
  mutate(factor_personas = factor07*mieperho) |> 
  group_by(departamento) |> 
  summarise(pobreza = weighted.mean(pobreza, factor_personas, na.rm = T))

data_enaho |> 
  mutate(tibble_2 = map(tibble_2, .f = function(tibble_2){
    tibble_2 |> 
      select(departamento, provincia, distrito, pobreza, inghog1d, factor07, mieperho) |> 
      mutate(pobreza = case_when(pobreza == 3 ~ 0,
                                 pobreza == 2 ~ 1,
                                 pobreza == 1 ~ 1)) |> 
      mutate(factor_personas = factor07*mieperho,
             ing_prom = inghog1d/mieperho) |> 
      group_by(departamento, provincia, distrito) |> 
      summarise(pobreza = weighted.mean(pobreza, factor_personas),
                ing_prom = weighted.mean(ing_prom, factor07))
  })) -> data_enaho

data_enaho |> pluck("tibble", 1) |> 
  select(departamento, provincia, distrito, dominio, estrato) |> 
  mutate(area = case_when(dominio == 8 | (dominio >= 1 & dominio <= 7) & (estrato >= 1 & estrato <= 5) ~ "Urbano",
                          (dominio >= 1 & dominio <= 7) & (estrato >= 6 & estrato <= 8) ~ "rural"
                          ) ) |> 
  group_by(departamento, provincia, distrito) |> 
  summarise(area = mean(area == "Urbano")) |> 
  mutate(urbano = area > .5) 

data_enaho |> 
  mutate(tibble = map(.x = tibble, .f = function(tibble){
    tibble |> select(departamento, provincia, distrito, dominio, estrato) |> 
      mutate(area = case_when(dominio == 8 | (dominio >= 1 & dominio <= 7) & (estrato >= 1 & estrato <= 5) ~ "Urbano",
                              (dominio >= 1 & dominio <= 7) & (estrato >= 6 & estrato <= 8) ~ "rural")) |> 
      group_by(departamento, provincia, distrito) |> 
      summarise(area = mean(area == "Urbano")) |> 
      ungroup() |> 
      mutate(urbano = area > .5)
  }))  -> data_enaho

data_enaho |> pluck("tibble_2", 1)
data_enaho |> pluck("tibble", 1)

data_enaho |> mutate(tibble_2 = map2(.x = tibble, .y = tibble_2,
                                     .f = function(tibble, tibble_2){
                                       tibble_2 |> left_join(tibble, 
                                                             by = c("departamento",
                                                                    "provincia",
                                                                    "distrito"))
                                     })) -> data_enaho
data_enaho |> pluck("tibble_2", 1)
### write-tidy-file -------
# write_rds(data_enaho, here("data", "tidy_data", "03_enaho-tidy.rds"))
