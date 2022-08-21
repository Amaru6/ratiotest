pacman::p_load(tidyverse, here, fixest)
library(modelsummary)
## data ---------
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
  select(key, year, starts_with("resultado"), ratio, ing_prom, area, urbano) |>  ungroup() |> 
  select(key, year, 3, 4, ratio, ing_prom, area, urbano) |> 
  rename(lectura = resultado_en_lectura_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas,
         matematica = resultado_en_matematica_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas
         ) |> 
  mutate(test = (lectura + matematica)/2) -> workfile
workfile |> 
  mutate(year = year |> as.factor()) -> workfile


## 1.A2 -------#
### fixed effects --#
setFixest_estimation(panel.id = c("key", "year"))

workfile |> 
  feols(test ~ ratio|key, data = _,
        cluster = "key") -> reg_1
reg_1

workfile |> 
  feols(test ~ ratio + year|key, data = _,
        cluster = "key") -> reg_2

reg_2

## 1.A3 ---------#


workfile |> 
  feols(test ~ ratio + year + ing_prom|key, data =_,
        cluster = "key") -> reg_3

reg_3

## 1.A4 -------#

etable(reg_1, reg_2, reg_3, drop = c("year", "ing_prom")) 
table_coef <- tribble(~raw, ~clean, ~fmt,
                      "nobs", "N", 0,
                      "r.squared", "R-cuadrado", 2)

modelsummary(list(reg_1, reg_2, reg_3),
             gof_map = table_coef,
             coef_omit = "year|ing_prom",
             stars = T, output = "kableExtra",
             coef_rename = c("ratio" = "Alumnos por profesor"),
             fmt = 2, escape = F) -> tabla_reg

tabla_reg
## 1.A5 -----------#
### imputation -------#
library(recipes)
workfile |> mutate(urbano = urbano |> as.factor()) |> 
  select(key, year, lectura, matematica, ratio, ing_prom, urbano) |> 
  recipe(~.) |> 
  step_impute_median(ing_prom) |> 
  step_impute_knn(urbano, impute_with = c("lectura", "matematica", "ing_prom", "ratio")) |> 
  prep() |> bake(new_data = NULL) -> workfile_imputed

workfile_imputed |> mutate(test = (matematica + lectura)/2) -> workfile_imputed

workfile |> mutate(urbano = urbano |> as.factor()) |> 
  recipe(~.) |> 
  step_impute_median(ing_prom) |>
  step_normalize(lectura, matematica, ratio, ing_prom) |> 
  step_impute_knn(urbano, impute_with = c("lectura", "matematica", "ratio", "ing_prom")) |> 
  prep() |> bake(new_data = NULL) |> 
  select(key, urbano) -> workfile_col
workfile_col |> rename(urbano_pruned = urbano) -> workfile_col

workfile_imputed |> left_join(workfile_col, by = "key") |> 
  select(-urbano) -> workfile_imputed
## 1.A6 ------#
workfile_imputed |> feols(
  test ~ ratio + year + ing_prom | key, data = _,
  cluster = "key"
) -> reg_3_imp
reg_3_imp
## 1.A7 -------#
workfile_imputed |> 
  feols(test ~ ratio + year + ing_prom + urbano_pruned| key, data = _,
        cluster = "key") -> reg_4

reg_4
etable(reg_4)
## 1.A8 ------#

workfile_imputed |> 
  feols(log(test) ~ ratio + year + ing_prom + urbano_pruned|key, data = _,
        cluster = "key") |> etable()


### FINAL WORKFILE ------#
read_rds(here("data", "workfile", "workfile_2_final.rds")) -> workfile2_final
workfile2_final |> nrow()
workfile2_final |> distinct(departamento, provincia, distrito, year) |> nrow()

workfile2_final |> rename(ratio = numeric) |> janitor::clean_names() |> 
  mutate(key = str_glue("{departamento}-{provincia}-{distrito}")) -> workfile2_final

workfile2_final |> 
  group_by(key) |> 
  filter(n() == 3) |> ungroup() |> 
  select(key, year, starts_with("resultado"), ratio, ingreso_proveniente_del_trabajo_mensual, area, urbano) |>  ungroup() |> 
  select(key, year, 3, 4, ratio, ingreso_proveniente_del_trabajo_mensual, area, urbano) |> 
  rename(lectura = resultado_en_lectura_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas,
         matematica = resultado_en_matematica_porcentaje_de_estudiantes_en_cada_nivel_de_logro_solo_se_reportan_en_el_caso_de_haber_sido_evaluados_10_o_mas
  ) |> 
  mutate(test = (lectura + matematica)/2) |> 
  mutate(year = year |> as.factor()) -> workfile2_final


#### ESTIMACION

setFixest_estimation(panel.id = c("key", "year"))
workfile2_final |> count(key)

workfile2_final |> 
  feols(test ~ ratio + ingreso_proveniente_del_trabajo_mensual + year| key,
        data = _, cluster = "key") 

workfile2_final |> 
  separate(key, into = c("departamento",
                          "provincia",
                         "distrito"),
           sep = "-") |> 
  group_by(departamento, provincia, year) |> 
  summarise(ratio = mean(ratio, na.rm = T),
            test = mean(test, na.rm = T),
            ingreso = mean(ingreso_proveniente_del_trabajo_mensual, na.rm = T)) |> 
  feols(test ~ ratio + ingreso + year | provincia, cluster = "provincia")
