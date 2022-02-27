library(ggplot2)
library(dplyr)

# pegando os dados
isp_cisp <- read.csv("CSV_files/isp_cisp.csv",
                     sep = ";")

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
  labs(title = "Roubo de celular por ano a cada 100 mil habitantes /nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## veciulos
plot_roubo_veiculo <- df_roubos %>%
  ggplot(aes(x=ano, y=veic_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(6164, 10000, 15000, 20000, 25894)) +
  labs(title = "Roubo de veiculos por ano a cada 100 mil habitantes /nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## transeunte
plot_roubo_transeunte <- df_roubos %>%
  ggplot(aes(x=ano, y=tran_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(11064, 15000, 20000, 25000, 30000, 35000, 40000, 45518)) +
  labs(title = "Roubo de transeunte por ano a cada 100 mil habitantes /nna cidade do Rio de Janeiro",
       y = "Total de roubos a cada 100 mil habitantes",
       x = "Anos") +
  theme_bw()

## coletivo
plot_roubo_coletivo <- df_roubos %>%
  ggplot(aes(x=ano, y=cole_s)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(2684, 5000, 7000, 9775)) +
  labs(title = "Roubo em coletivo por ano a cada 100 mil habitantes /nna cidade do Rio de Janeiro",
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