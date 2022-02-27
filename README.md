# Projeto Final para a aula de Dados do Departamento de Economia da PUC-Rio

## Introdução
Esse repositório apresenta os códigos e inputs utilizados para a realização do trabalho de ciência de dados, assim como os outputs (plots) gerados.
O intuito do trabalho foi analisar roubos na cidade do Rio de Janeiro ao longo dos anos, procurando tendências no aumento ou diminuição no número de roubos por ano e região da cidade.

## Metodologia
Os dados são do Instituto de Segurança Pública do Rio de Janeiro e foram retirados a partir do pacote 'basedosdados' para o R. Para sua análise foram utilizados também outros pacotes: tidyverse, sf, ggplot2, gridExtra
Para os mapas foram utilizados os arqvuios shapefiles baixados do site do Instituto de Segurança Pública. Para rodar o código é necessário que todos os arquivos da pasta "Shapefile" desse repositório estejam baixados no diretório.
Todas as tabelas geradas pelo código podem ser encontradas em formato .csv na pasta "CSV files"

## Outputs
Como resultado do projeto foram montados diversos gráficos que podem ser encontrados na pasta "Plots". Dentre eles estão os gráficos de evolução anual de roubo e mapas da cidade do Rio de Janeiro por nível de roubo em cada CISP da capital.
