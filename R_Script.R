library(basedosdados)
library(tidyverse)
library(ggplot2)
library(sf)
library(gridExtra)

# pegando os dados
set_billing_id("<your billing ID>")
query <- "SELECT*FROM basedosdados.br_isp_estatisticas_seguranca.evolucao_mensal_cisp"

isp_cisp = basedosdados::read_sql(query)

## escrevendo csv file
write.table(isp_cisp, file = "isp_cisp.csv", row.names = FALSE, sep = ";")

# organizando os dados por tipo e ano
df_roubos <- isp_cisp %>%
  select(ano, regiao_rj, roubo_veiculo, roubo_celular, roubo_transeunte, roubo_em_coletivo) %>%
  filter(regiao_rj == "capital") %>%
  group_by(ano) %>%
  summarise(veic_s = sum(roubo_veiculo, na.rm = TRUE), cel_s = sum(roubo_celular, na.rm = TRUE),
            tran_s = sum(roubo_transeunte, na.rm = TRUE), cole_s = sum(roubo_em_coletivo, na.rm = TRUE))
cols.num <- c("veic_s", "cel_s", "tran_s", "cole_s", "ano")
df_roubos[cols.num] <- sapply(df_roubos[cols.num], as.numeric)

## escrevendo csv file
write.table(df_roubos, file = "df_roubos.csv", row.names = FALSE, sep = ";")

# plots
## celular
plot_roubo_cel <- df_roubos %>%
  ggplot(aes(x=ano, y=cel_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(2500, 5000, 10000, 14622)) +
  labs(title = "Roubo de celular por ano a cada 100 mil habitantes \nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## veciulos
plot_roubo_veiculo <- df_roubos %>%
  ggplot(aes(x=ano, y=veic_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(6164, 10000, 15000, 20000, 25894)) +
  labs(title = "Roubo de veiculos por ano a cada 100 mil habitantes \nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## transeunte
plot_roubo_transeunte <- df_roubos %>%
  ggplot(aes(x=ano, y=tran_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(11064, 15000, 20000, 25000, 30000, 35000, 40000, 45518)) +
  labs(title = "Roubo de transeunte por ano a cada 100 mil habitantes \nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## coletivo
plot_roubo_coletivo <- df_roubos %>%
  ggplot(aes(x=ano, y=cole_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(2684, 5000, 7000, 9775)) +
  labs(title = "Roubo em coletivo por ano a cada 100 mil habitantes \nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

# pegando o numero de roubo por cisp da capital
roubo_cisp <- isp_cisp %>%
  select(ano, cisp, regiao_rj, roubo_celular, roubo_transeunte, roubo_veiculo, roubo_em_coletivo) %>%
  filter( cisp %in% c(1:44)) %>%
  group_by(ano, cisp) %>%
  summarise(cel_c_s = sum(roubo_celular, na.rm = TRUE), tran_c_s = sum(roubo_transeunte, na.rm = TRUE),
            veic_c_s = sum(roubo_veiculo, na.rm = TRUE), col_c_s = sum(roubo_em_coletivo, na.rm = TRUE))

## escrevendo csv file
write.table(roubo_cisp, file = "roubo_cisp.csv", row.names = FALSE, sep = ";")

# baixando shapefile para mapa do estado
## instrucoes para baixar estao no README.md
rj_cisp <- st_read("lm_dp_2019.shp")

## renomeando primeira coluna
colnames(rj_cisp)[1] <- "cisp"

## grafico do estado
plot_rj_cisp <- rj_cisp %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), alpha = 5) +
  labs(title = "DivisÃ£o das CISP no Estado do Rio de Janeiro") +
  theme_bw()

## criando mapa para capital
cap_cisp <- rj_cisp %>%
  filter(cisp %in% c(1:44))

plot_cap_cisp <- cap_cisp %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), alpha = 5) +
  labs(title = "DivisÃ£o das CISP na Cidade do Rio de Janeiro") +
  theme_bw()

### mapa para roubo na capital
#### 2011
cap_cisp_11 <- roubo_cisp %>%
  select(ano, cisp, cel_c_s, veic_c_s) %>%
  filter(ano == "2011") %>%
  group_by(cisp)
cap_cisp_11$cisp <- as.numeric(cap_cisp_11$cisp)
cap_cisp_11$cel_c_s <- as.numeric(cap_cisp_11$cel_c_s)
cap_cisp_11$veic_c_s <- as.numeric(cap_cisp_11$veic_c_s)
cap_cisp_roubos_11 <- left_join(cap_cisp, cap_cisp_11, by = "cisp")

#####escrevendo csv file
write.table(cap_cisp_11, file = "cap_cisp_11.csv", row.names = FALSE, sep = ";")

##### grafico
plot_c_11 <- cap_cisp_roubos_11 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cel_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Roubos", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Celular na Cidade do Rio de Janeiro", subtitle = "2011")

plot_v_11 <- cap_cisp_roubos_11 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = veic_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "x", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Veiculos da Cidade do Rio de Janeiro",
       subtitle = "2011")

#### 2015
cap_cisp_15 <- roubo_cisp %>%
  select(ano, cisp, cel_c_s, veic_c_s) %>%
  filter(ano == "2015") %>%
  group_by(cisp)
cap_cisp_15$cisp <- as.numeric(cap_cisp_15$cisp)
cap_cisp_15$cel_c_s <- as.numeric(cap_cisp_15$cel_c_s)
cap_cisp_15$veic_c_s <- as.numeric(cap_cisp_15$veic_c_s)
cap_cisp_roubos_15 <- left_join(cap_cisp, cap_cisp_15, by = "cisp")

##### escrevendo csv file
write.table(cap_cisp_15, file = "cap_cisp_15.csv", row.names = FALSE, sep = ";")

##### grafico
plot_c_15 <- cap_cisp_roubos_15 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cel_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Roubos", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Celular na Cidade do Rio de Janeiro", subtitle = "2015")

plot_v_15 <- cap_cisp_roubos_15 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = veic_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "x", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Veiculos da Cidade do Rio de Janeiro",
       subtitle = "2015")

#### 2019
cap_cisp_19 <- roubo_cisp %>%
  select(ano, cisp, cel_c_s, veic_c_s) %>%
  filter(ano == "2019") %>%
  group_by(cisp)
cap_cisp_19$cisp <- as.numeric(cap_cisp_19$cisp)
cap_cisp_19$cel_c_s <- as.numeric(cap_cisp_19$cel_c_s)
cap_cisp_19$veic_c_s <- as.numeric(cap_cisp_19$veic_c_s)
cap_cisp_roubos_19 <- left_join(cap_cisp, cap_cisp_19, by = "cisp")

##### escrevendo csv file
write.table(cap_cisp_19, file = "cap_cisp_19.csv", row.names = FALSE, sep = ";")

##### grafico
plot_c_19 <- cap_cisp_roubos_19 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cel_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Roubos", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Celular na Cidade do Rio de Janeiro", subtitle = "2019")

plot_v_19 <- cap_cisp_roubos_19 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = veic_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "x", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Veiculos da Cidade do Rio de Janeiro",
       subtitle = "2019")

#### 2020
cap_cisp_20 <- roubo_cisp %>%
  select(ano, cisp, cel_c_s, veic_c_s) %>%
  filter(ano == "2020") %>%
  group_by(cisp)
cap_cisp_20$cisp <- as.numeric(cap_cisp_20$cisp)
cap_cisp_20$cel_c_s <- as.numeric(cap_cisp_20$cel_c_s)
cap_cisp_20$veic_c_s <- as.numeric(cap_cisp_20$veic_c_s)
cap_cisp_roubos_20 <- left_join(cap_cisp, cap_cisp_20, by = "cisp")

##### escrevendo csv file
write.table(cap_cisp_20, file = "cap_cisp_20.csv", row.names = FALSE, sep = ";")

##### grafico
plot_c_20 <- cap_cisp_roubos_20 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = cel_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Roubos", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Celular na Cidade do Rio de Janeiro", subtitle = "2020")

plot_v_20 <- cap_cisp_roubos_20 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = veic_c_s), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "x", labels = scales::comma) +
  labs(title = "Mapa dos Roubos de Veiculos da Cidade do Rio de Janeiro",
       subtitle = "2020")

###### combinando graficos
mapas_cel <- grid.arrange(plot_c_11, plot_c_15, plot_c_19, plot_c_20, ncol=2)
linhas <- grid.arrange(plot_roubo_cel, plot_roubo_transeunte, plot_roubo_coletivo,
                       plot_roubo_veiculo, ncol=2)
mapas_veiculo <- grid.arrange(plot_v_11, plot_v_15, plot_v_19, plot_v_20, ncol = 2)
