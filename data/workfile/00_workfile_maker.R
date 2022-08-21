pacman::p_load(tidyverse, here)

# ece -----
read_rds(here("data", "tidy_data", "01_tidy_ece.rds")) -> tidy_ece
tidy_ece |> distinct(departamento, provincia, distrito, year) |> nrow()
tidy_ece |> filter(departamento |> str_detect("LIM"))
tidy_ece |> glimpse()
tidy_ece |> 
  mutate(key = str_glue("{departamento}-{provincia}-{distrito}-{year}")) -> tidy_ece
tidy_ece |> distinct(departamento) # Tildes! NOT GOOD

## stringi package to the rescue!!!

tidy_ece |> distinct(departamento) |> 
  mutate(departamento_2 = stringi::stri_trans_general(str = departamento,
                                                      id = "Latin-ASCII")) |> View() # it worked!

tidy_ece |> 
  mutate(across(.cols = c("departamento", "provincia", "distrito"),
                .fns = ~stringi::stri_trans_general(str = .,
                                                    id = "Latin-ASCII"))) -> tidy_ece


tidy_ece |> distinct(departamento, provincia, distrito, year) |> nrow()
tidy_ece # lima metropolitana appears as LIMA METROPOLITANA
tidy_ece |> janitor::get_dupes(departamento, provincia, distrito, year) |> View()
tidy_ece |> filter(departamento |> str_detect("LIM")) |> distinct(departamento)

tidy_ece |> distinct(departamento, provincia, distrito, year) |> View()
tidy_ece |> distinct(departamento, provincia, distrito, year) |> nrow()

tidy_ece |> 
  group_by(departamento, provincia, distrito, year) |>  # get unique IDs.
  filter(n() == 1)  -> tidy_ece
tidy_ece
# ratio -----
read_rds(here("data", "tidy_data", "02_tidy_ratio.rds")) -> tidy_ratio
tidy_ratio |> mutate(year = year |> as.double()) -> tidy_ratio
tidy_ratio |> 
  mutate(key = str_glue("{department}-{provincia}-{distrito}-{year}") # nice
         )   -> tidy_ratio

tidy_ratio |> 
  mutate(across(.cols = c("department", "provincia", "distrito"),
                .fns = ~stringi::stri_trans_general(str = .,
                                                    id = "Latin-ASCII"))) -> tidy_ratio
tidy_ratio
tidy_ratio |> distinct(department, provincia, distrito, year) |> nrow() # good!
tidy_ratio # lima metropolitana appears as LIMA
tidy_ratio |> filter(department |> str_detect("LIM")) |> distinct(department)
tidy_ratio |> distinct(key)
tidy_ratio
# enaho -----
read_rds(here("data", "tidy_data", "03_enaho-tidy.rds")) |> 
  select(year, tibble_2) |> 
  unnest(tibble_2) -> tidy_enaho
tidy_enaho |> 
  mutate(key = str_glue("{departamento}-{provincia}-{distrito}-{year}")) -> tidy_enaho

tidy_enaho |> 
  mutate(across(.cols = c("departamento", "provincia", "distrito"),
                .fns = ~stringi::stri_trans_general(str = .,
                                                    id = "Latin-ASCII"))) -> tidy_enaho
tidy_enaho
tidy_enaho |> distinct(departamento, provincia, distrito, year) |> nrow()
# join them ------
library(tidylog)
tidy_ece |> 
  inner_join(tidy_ratio, by = c("departamento" = "department",
                               "provincia", "distrito", "year")) |> # 5285 observations matched 
  left_join(tidy_enaho, by = c("departamento", "provincia", "distrito", "year") # 3701 matched
            ) -> workfile_tbl 
workfile_tbl |> ungroup() -> workfile_tbl
workfile_tbl |> distinct(departamento, provincia, distrito, year)
# write --------
# write_rds(workfile_tbl, here("data", "workfile", "01_workfile_tbl.rds"))


# ENAHO ingresos ---------------
list.files(here("data", "raw_data", "enaho_ingresos"),
           full.names = T) -> data_ingresos_import
tibble(data_ingresos_import) |> 
  mutate(data_ingresos = map(.x = data_ingresos_import,
                             .f = function(data_ingresos_import){
                               data_ingresos_import |> haven::read_dta()
                             })) -> tbl_ingresos_import
tbl_ingresos_import |> pluck("data_ingresos", 1) |> 
  select(ubigeo) |> 
  mutate(departamento_inei = str_sub(ubigeo, 1L, 2L),
         provincia_inei = str_sub(ubigeo, 3, 4),
         distrito_inei = str_sub(ubigeo, 5, 6))

tbl_ingresos_import |> 
  mutate(data_ingresos = map(.x = data_ingresos,
                             .f = function(data_ingresos){
                               data_ingresos |> 
                                 mutate(departamento_inei = str_sub(ubigeo, 1L, 2L),
                                        provincia_inei = str_sub(ubigeo, 3, 4),
                                        distrito_inei = str_sub(ubigeo, 5, 6))
                             })) -> tbl_ingresos_import

read_rds(here("data", "raw_data", "ubigeos_datos_pruned.rds")) -> ubigeos_data
ubigeos_data

tbl_ingresos_import |> mutate(year = c(2016, 2018, 2019)) |> 
  select(year, data_ingresos) -> tbl_ingresos_import

tbl_ingresos_import |> 
  mutate(data_ingresos = map(.x = data_ingresos,
                             .f = function(data_ingresos){
                               data_ingresos |> 
                                 left_join(ubigeos_data,
                                           by = c("departamento_inei",
                                                  "provincia_inei",
                                                  "distrito_inei"))
                             })) -> tbl_ingresos_import
tbl_ingresos_import |> 
  mutate(data_ingresos = map(.x = data_ingresos,
                             .f = function(data_ingresos){
                               data_ingresos |> 
                                 mutate(
                                   area = case_when(estrato < 5 ~ "Urbano",
                                                    between(estrato, 6, 8) ~ "Rural")
                                 ) |> 
                                 mutate(etnia = case_when(p558c < 4 | p558c == 9 ~ "Indigena",
                                                          p558c == 4 ~ "Afroperuano",
                                                          p558c == 6 ~ "Mestizo",
                                                          p558c == 5 | p558c == 7 ~ "Otro",
                                                          p558c == 8 ~ "No sabe")) |> 
                                 mutate(
                                   edad = case_when(
                                  (p208a > 13 & p208a < 25) ~ "14 a 24",
                                   (p208a > 24 & p208a < 45) ~ "25 a 44",
                                   (p208a > 44 & p208a < 60 )~ "45 a 59",
                                   (p208a > 59 & p208a < 65) ~ "60 a 64",
                                   (p208a > 64) ~ "65 y más"
                                 )) |> 
                                 mutate(
                                   estado_civil = case_when(
                                     p209 == 1 ~ "Conviviente",
                                     p209 == 2 ~ "Casado/a",
                                     p209 == 3 | p209 == 4 | p209 == 5 ~ "Alguna vez unido",
                                     p209 == 6 ~ "Soltero/a"
                                   )
                                 ) |> 
                                 mutate(
                                   residente = ifelse(
                                     (p204 == 1 & p205 == 2) | (p204 == 2 & 206 == 1), TRUE, FALSE
                                   )
                                 ) 
                                    })) -> tbl_ingresos_import
tbl_ingresos_import
library(tidytable)
tbl_ingresos_import |> as_tidytable() -> tbl_ingresos_import

tbl_ingresos_import |> 
  mutate(data_ingresos = map(data_ingresos,
                             .f = function(data_ingresos){
                               data_ingresos |> 
                                 mutate(
                                   ingreso_proveniente_trabajo = i524a1 + d529t + i530a + d536 +
                                     i538a1 + d540t + i541a + d543 + d544t
                                 ) |> 
                                 mutate(ingreso_proveniente_trabajo_mensual =
                                          ingreso_proveniente_trabajo/12)
                             })) -> tbl_ingresos_import

tbl_ingresos_import |> 
  mutate.(data_ingresos = map.(data_ingresos,
                               .f = function(data_ingresos){
                                 data_ingresos |> 
                                   mutate_rowwise.(
                                     ingreso_proveniente_trabajo = 
                                       c_across.(c("i524a1", "d529t", "i530a", "d536",
                                                   "i538a1","d540t", "i541a", "d543", "d544t")) |> 
                                       sum(na.rm = T)
                                   ) |> 
                                   mutate(ingreso_proveniente_del_trabajo_mensual = 
                                            ingreso_proveniente_trabajo/12)
                               })) -> tbl_ingresos_import

## CHECK IF CORRECT -----#
tbl_ingresos_import |> pluck("data_ingresos", 2) |> 
  filter(ocu500 == 1 & ingreso_proveniente_del_trabajo_mensual > 0) |> 
  filter(residente == T) |> 
  summarise.(sapo_logne = weighted.mean(ingreso_proveniente_del_trabajo_mensual,
                                       fac500a),
             .by = etnia) # good

tbl_ingresos_import |> pluck("data_ingresos", 2) |> 
  filter(ocu500 == 1 & ingreso_proveniente_del_trabajo_mensual > 0) |> 
  filter(residente == T) |> 
  summarise.(sapo_logne = weighted.mean(ingreso_proveniente_del_trabajo_mensual,
                                        fac500a),
             .by = estado_civil) # good

tbl_ingresos_import |> pluck("data_ingresos", 2) |> 
  filter(ocu500 == 1 & ingreso_proveniente_del_trabajo_mensual > 0) |> 
  filter(residente == T) |> 
  summarise.(sapo_logne = weighted.mean(ingreso_proveniente_del_trabajo_mensual,
                                        fac500a),
             .by = c(departamento, p207)) |> View() # good

## USE THIS -------#
tbl_ingresos_import |> 
  mutate(data_ingresos = map(.x = data_ingresos,
                             .f = function(data_ingresos){
                               data_ingresos |> 
                                 select(ingreso_proveniente_del_trabajo_mensual,
                                        departamento, provincia, distrito,
                                        fac500a)
                             })) |> unnest(data_ingresos) |> 
  summarise.(ingreso_proveniente_del_trabajo_mensual = weighted.mean(ingreso_proveniente_del_trabajo_mensual,
                                                                     fac500a),
             .by = c("year", "departamento", "provincia", "distrito")) |> 
  drop_na() -> tbl_ingresos_import
tbl_ingresos_import

## FINAL WORKFILE -----#
read_rds(here("data", "workfile", "01_workfile_tbl.rds")) -> workfile_parte_1
workfile_parte_1 |> 
  left_join(tbl_ingresos_import, by = c("year", "departamento", "provincia",
                                        "distrito")) -> workfile_parte_1


write_rds(workfile_parte_1, here("data", "workfile", "workfile_2_final.rds"))
