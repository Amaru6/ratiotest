pacman::p_load(tidyverse, here, fixest, skimr)
source(here("functions", "julio_theme.R"))
theme_set(theme_julio())
# data ---------
read_rds(here("data", "workfile", "01_workfile_tbl.rds")) -> workfile
workfile |> nrow()
workfile
workfile |> distinct(departamento, provincia,
                     distrito, year) |> nrow() 

workfile |> rename(ratio = numeric) -> workfile
workfile |> janitor::clean_names() -> workfile
workfile |> ungroup() -> workfile
# analysis -----
## 1.A1 -------#
workfile |> mutate(key = str_glue("{departamento}-{provincia}-{distrito}")) -> workfile
workfile |> nrow()
workfile |> distinct(key, year) |> nrow()
workfile |> 
  group_by(key) |> 
  filter(n() == 3) -> workfile
workfile |> paint::paint()
workfile |> 
  select(key, year, starts_with("resultado"), ratio, ing_prom) |>  ungroup() |> 
  select(key, year, 3, 4, ratio, ing_prom) |> 
  rename(lectura = resultado_en_lectura_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas,
         matematica = resultado_en_matematica_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas
  ) |> 
  mutate(test = (lectura + matematica)/2) -> workfile
workfile |> 
  mutate(year = year |> as.factor()) -> workfile

## 1.A2 ------#
workfile |> 
  drop_na() -> workfile_pruned

workfile_pruned |> 
  separate(col = "key",
           into = c("departamento", "provincia", "distrito"),
           sep = "-") |> 
  filter(departamento == "LIMA",
         provincia == "LIMA") 
## 1.A3 -------# DROP THIS
read_rds(here("data", "tidy_data", "02_tidy_ratio.rds")) -> tidy_ratio
tidy_ratio |> mutate(year = year |> as.numeric()) -> tidy_ratio
tidy_ratio |> 
  filter(department == "LIMA") |> 
  filter(provincia == "LIMA") |> 
  select(-c(row, col)) |> 
  select(departamento = department, provincia, distrito, year, ratio = numeric) -> tidy_ratio_lima

tidy_ratio_lima |>
  mutate(year = year |> as.numeric()) |> 
  ggplot(aes(x = year, y = ratio, colour = distrito)) + 
  geom_line() -> plot_1
plot_1 |> pluck("data") |> distinct(distrito) |> head(20)

tidy_ratio |> filter(year %in% c(2019, 2020, 2021)) |> 
  select(department, provincia, distrito, year, ratio = numeric) |> 
  pivot_wider(names_from = year,
              values_from = ratio) |> janitor::clean_names() |> 
  mutate(change_2020 = ((x2020-x2019)/x2019)*100,
         change_2021 = ((x2021-x2020)/x2020)*100) -> tidy_ratio_change
  
### AVG_CHANGE_PER_YEAR
### all the country
tidy_ratio_change |> 
  summarise(avg_change_2020 = mean(change_2020, na.rm = T), avg_change_2021 = mean(change_2021,
                                                                                   na.rm = T))


### by department
tidy_ratio_change |> 
  group_by(department) |> 
  summarise(across(.cols = c("change_2020", "change_2021"),
                   .fns = ~mean(., na.rm = T),
                   .names = "{col}")) |> 
  arrange(change_2020) # now this is interesting!


### RATIO PER DEPARTMENT -----#
library(tidyxl)
library(unpivotr)

xlsx_cells(path = here("data", "raw_data", "ratio_alumnos_secundaria.xlsx"),
           sheets = "Regional") |> 
  filter(!is_blank) |> 
  select(row, col, data_type, character, numeric, local_format_id) |> 
  filter(between(row, 4, 42)) |> 
  filter(!(row %in% c(5:15))) -> all_cells

all_cells |> filter(row == 4, data_type == "numeric") |> 
  select(row, col, year = numeric) -> col_header_1
col_header_1

all_cells |> filter(col == 2, row >= 17, row <= 42) |> 
  select(row, col, departamento = character) -> row_header_1

all_cells |> filter(between(row, 17, 42),
                    between(col, 3, 8)) |> 
  filter(data_type == "numeric") |> 
  select(row, col, measurement = numeric) -> data_cells

data_cells |> enhead(row_header_1, "left") |> 
  enhead(col_header_1, "up") |> 
  select(-c(row, col)) -> data_departamental

library(gghighlight)
library(latex2exp)
#### plot_1
data_departamental |> group_by(departamento) |> 
  mutate(diff_y = (measurement - lag(measurement))/lag(measurement)) |> 
  ggplot(aes(x = year, y = diff_y, colour = departamento)) + geom_line(size = 1.5) + 
  gghighlight(departamento %in% c("Lima Metropolitana",
                                  "Callao",
                                  "Huancavelica",
                                  "Piura"),
              label_params = list(vjust = 1,
                                  family = "special",
                                  colour = "black"))   + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 4)) + 
  scale_x_continuous(limits = c(2017, 2021)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  ylab(TeX("$\\Delta$ Ratio alumnos por maestro")) + xlab("") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100))


#### plot_1_refined
data_departamental |> group_by(departamento) |> 
  mutate(diff_y = (measurement - lag(measurement))/lag(measurement)) |> 
  filter(departamento %in% c("Lima Metropolitana",
                             "Callao",
                             "Huancavelica",
                             "Piura")) |> 
  ggplot(aes(x = year, y = diff_y, colour = departamento)) + geom_line(size = 1.5)  + 
  theme(legend.title = element_blank(),
        legend.position = "top")  + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 4)) + 
  scale_x_continuous(limits = c(2017, 2021)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  ylab(TeX("$\\Delta$ Ratio alumnos por maestro")) + xlab("") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(-0.1, 0.05))


#### plot_2_refined
data_departamental |> filter(departamento %in% c("Lima Metropolitana",
                                                 "Callao",
                                                 "Huancavelica",
                                                 "Piura")) |> 
  ggplot(aes(x = year, y = measurement, colour = departamento)) + 
  geom_line(size = 1.5) + 
  theme(legend.title = element_blank(),
        legend.position = "top") + labs(x = "", y = "") + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 4))

#### plot_3_refined
data_departamental |> filter(departamento %in% c("Lima Metropolitana",
                                                 "Callao",
                                                 "Huancavelica",
                                                 "Piura")) |> 
  mutate(year = year |> as.factor()) |> 
  ggplot(aes(x = year, y = measurement, fill = departamento)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 20)) + 
  scale_fill_manual(values = MetBrewer::met.brewer("Tiepolo", 4)) + 
  geom_text(aes(label = round(measurement, 0)), vjust = -.5,
            family = "special",
            position = position_dodge(width = .9)
            ) + theme(legend.position = "top") + labs(x = "", y = "") + 
  theme(legend.title = element_blank())

#### plot_4_refined 
data_departamental |> 
  group_by(departamento) |> 
  summarise(var_x = var(measurement)) |> 
  arrange(desc(var_x)) |> 
  slice(1:4) -> data_to_join

data_departamental |> 
  semi_join(data_to_join, by = "departamento") |> 
  bind_rows(data_departamental |> 
              filter(departamento == "Lima Metropolitana")) |> 
  ggplot(aes(x = year, y = measurement, fill = departamento)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 20)) + 
  scale_fill_manual(values = MetBrewer::met.brewer("Tiepolo", 5)) + 
  geom_text(aes(label = round(measurement, 0)), vjust = -.5,
            family = "special",
            position = position_dodge(width = .9)
  ) + theme(legend.position = "top") + labs(x = "", y = "") + 
  theme(legend.title = element_blank())

#### plot_5_refined

data_departamental |> 
  group_by(departamento) |> 
  summarise(var_x = var(measurement)) |> 
  arrange(desc(var_x)) |> 
  slice(1:6) -> data_to_join_2

data_departamental |> 
  semi_join(data_to_join_2, by = "departamento") |> 
  bind_rows(data_departamental |> 
              filter(departamento %in% c("Lima Metropolitana", "Callao", "Moquegua"))) |> 
  group_by(departamento) |> mutate(diff_y = measurement - lag(measurement)) |> 
  ggplot(aes(x = year, y = diff_y, colour = departamento)) + 
  geom_line(size = 1.5) + 
  theme(legend.title = element_blank(),
        legend.position = "top") + 
  scale_x_continuous(limits = c(2017, 2021))  + 
  ylab(TeX("$\\Delta$\\% Ratio alumnos por profesor")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, 
             colour = "grey20") + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 9)) -> plot_ratio_dep 

plot_ratio_dep + geom_text(data = pluck(plot_ratio_dep, "data") |> 
                             filter(year %in% c(2017)),
                           family = "special",
                           mapping = aes(label = as.character(round(measurement, 0))),
                           size = 5, colour = "black", hjust = 1) + 
  geom_text(data = pluck(plot_ratio_dep, "data") |> 
              filter(year %in% c(2021)),
            family = "special",
            mapping = aes(label = as.character(round(measurement, 0))),
            size = 5, colour = "black", hjust = -.5) + labs(x = "")

#### plot_6_finished
read_rds(here("data", "tidy_data", "01_tidy_ece.rds"))

### plot_7
data_departamental |> 
  group_by(departamento) |> 
  summarise(var_x = var(measurement)) |> 
  arrange(desc(var_x))

data_departamental |> 
  semi_join(data_departamental |> 
              group_by(departamento) |> 
              summarise(var_x = var(measurement)) |> 
              arrange(desc(var_x)) |> 
              slice(1:5)
  ) |> bind_rows(data_departamental |> 
                   filter(departamento %in% c("Lima Metropolitana",
                                              "Callao"))) |> 
  ggplot(aes(x = year, y  = measurement, colour = departamento)) + 
  geom_line(size = 1.2) + theme(legend.title = element_blank()) + 
  labs(x = "", y = "") + 
  scale_colour_manual(values = MetBrewer::met.brewer("Tiepolo", 7)) + 
  geom_point() + geom_text(aes(label = round(measurement, 0)))

