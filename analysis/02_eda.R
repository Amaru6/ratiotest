# libraries ------#
pacman::p_load(tidyverse, here, fixest, skimr)
source(here("functions", "julio_theme.R"))
theme_set(theme_julio())
library(tidyxl)
library(unpivotr)
# data ------#
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

data_ece |> glimpse()
data_ece |> distinct(col_header_3)

data_ece |> filter(col_header_3 == "Medida promedio de la IE (media=500 ; d.e=100)") |> 
  distinct(col_header_2) |> mutate(rn = row_number()) |> filter(rn %in% c(1,2)) |> pull(col_header_2
                                ) -> target_col_2 
target_col_2

data_ece |> filter(col_header_3 == "Medida promedio de la IE (media=500 ; d.e=100)") |> 
  filter(col_header_2 %in% target_col_2) |> 
  select(-c(col, row)) |> 
  mutate(logro_bucket = case_when(col_header_2 == target_col_2 |> pluck(1) & measurement < 505.4 ~ "Previo al inicio",
                                  col_header_2 == target_col_2 |> pluck(1) & (measurement >= 505.14 & measurement < 580.61) ~ "En inicio",
                                  col_header_2 == target_col_2 |> pluck(1) & (measurement >= 580.61 & measurement < 641.25 )~ "En proceso",
                                  col_header_2 == target_col_2 |> pluck(1) & measurement >= 641.25 ~ "Satisfactorio",
                                  col_header_2 == target_col_2 |> pluck(2) & measurement < 519.67 ~ "Previo al inicio",
                                  col_header_2 == target_col_2 |> pluck(2) & (measurement >= 505.14 & measurement < 595.96) ~ "En inicio",
                                  col_header_2 == target_col_2 |> pluck(2) & (measurement >= 595.96 & measurement < 649.38) ~ "En proceso",
                                  col_header_2 == target_col_2 |> pluck(2) & (measurement >= 649.38) ~ "Satisfactorio"
                                 
                                  )
         ) |> 
  select(departamento, provincia, distrito, year, logro_bucket, col_header_2) |> 
  group_by(logro_bucket, year, col_header_2) |> 
  summarise(n = n()) |> 
  relocate(col_header_2, .before = 1) |> 
  arrange(year) |> 
  group_by(year, col_header_2) |> 
  mutate(frac = n/sum(n)*100) |> View()


data_ece |> distinct(col_header_2) |> rownames_to_column() |> 
  filter(rowname %in% c(2, 4)) |> pull(col_header_2) -> target_col_3

data_ece |> filter(col_header_2 %in% target_col_3) |> 
  filter(col_header_3 == col_header_3 |> pluck(5)) |> count(year, col_header_2) # everything ok...


data_ece |> filter(col_header_2 %in% target_col_3) |> 
  filter(col_header_3 == col_header_3 |> pluck(5)) |> 
  select(-c(row, col)) |> 
  distinct(col_header_2) |> pluck("col_header_2", 1) -> lectura

data_ece |> filter(col_header_2 %in% target_col_3) |> 
  filter(col_header_3 == col_header_3 |> pluck(5)) |> 
  select(-c(row, col)) |> 
  distinct(col_header_2) |> pluck("col_header_2", 2) -> matematica

lectura
matematica

target_col_3

data_ece |> filter(col_header_2 %in% target_col_3) |>
  filter(year != 2017) |> 
  filter(col_header_3 == col_header_3 |> pluck(5)) |> 
  select(-c(row, col)) |> 
  mutate(logro_bucket = case_when(col_header_2 == lectura & measurement < 505.14 ~ "Previo al inicio",
                                  col_header_2 == lectura & (measurement >= 505.14 & measurement < 580.61) ~ "En inicio",
                                  col_header_2 == lectura & (measurement >= 580.61 & measurement < 641.25) ~ "En proceso",
                                  col_header_2 == lectura & measurement >= 641.25 ~ "Satisfactorio",
                                  col_header_2 == matematica & measurement < 519.67 ~ "Previo al inicio",
                                  col_header_2 == matematica  & (measurement >= 519.67 & measurement < 595.96) ~ "En inicio",
                                  col_header_2 == matematica & (measurement >= 595.96 & measurement < 649.38) ~ "En proceso",
                                  col_header_2 == matematica & measurement >= 649.38 ~ "Satisfactorio")) |> 
  select(departamento, provincia, distrito, year, logro_bucket, col_header_2) |> 
  mutate(col_header_2 = col_header_2 |> str_squish()) |>
  group_by(year, logro_bucket, col_header_2) |> 
  summarise(n = n()) |> 
  group_by(year, col_header_2) |> 
  mutate(frac = n/sum(n)*100) |> 
  mutate(col_header_2 = col_header_2 |> str_extract(pattern = "Lectura|Matemática")) |> 
  mutate(year = year |> as.factor()) |> 
  ungroup() |> 
  ggplot(aes(x = year, y = frac, fill = logro_bucket)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_wrap(vars(col_header_2)) + 
  scale_fill_manual(values = MetBrewer::met.brewer("Tiepolo", 4)) + 
  scale_y_continuous(expand = c(0,0),
                     labels = scales::percent_format(scale = 1)) + labs(x = "", y = "") + 
  theme(strip.background = element_rect(fill = "white", colour = "black"),
        legend.title = element_blank()) + 
  geom_text(aes(label = scales::percent(round(frac, 2), scale = 1)),
            position = position_fill(vjust = .5))

