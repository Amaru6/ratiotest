# libraries ------#
pacman::p_load(tidyverse, here, fixest, skimr)
source(here("functions", "julio_theme.R"))
theme_set(theme_julio())
library(tidyxl)
library(unpivotr)

# data -------#
xlsx_cells(here("data", "raw_data", "gasto_publico_pbi.xlsx"),
           sheets = "B3") |> 
  filter(!is_blank) |> 
  select(row, col, data_type, character, numeric, local_format_id) |> 
  filter(row >=4, row <= 33) |> 
  filter(!(row %in% c(6,7))) -> all_cells

all_cells |> 
  filter(row == 5) |> select(row, col, year = numeric) -> col_header_1

all_cells |> 
  filter(col == 2) |> select(row, col, departamento = character) -> row_header_1


all_cells |> 
  filter(between(row, 8, 33),
         between(col, 3, 8)) |> 
  select(row, col, measurement = numeric) -> data_cells

data_cells |> enhead(col_header_1, "up") |> 
  enhead(row_header_1, "left") |> select(-c(row, col)) -> data_to_plot

data_to_plot |> 
  filter(departamento %in% c("Ancash", "Ayacucho", "Huancavelica",
                             "Lima Metropolitana", "Tacna",
                             "Moquegua")) |> 
  ggplot(aes(x = year, y = measurement, colour = departamento)) + 
  geom_line(size = 1) + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 6)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(x = "", y = "") + theme(legend.title = element_blank()) -> plot_gasto
plot_gasto + 
  geom_text(data = pluck(plot_gasto, "data") |> 
              filter(year %in% c(2016)),
            aes(label = scales::percent(round(measurement, 0),scale = 1)),
            hjust = 1, colour = "black", family = "special") + 
  geom_text(data = pluck(plot_gasto, "data") |> 
              filter(year %in% c(2020)),
            aes(label = scales::percent(round(measurement, 0),scale = 1)),
            hjust = -.5, colour = "black", family = "special")

