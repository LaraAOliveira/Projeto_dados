# Projeto Final para a aula de Dados do Departamento de Economia da PUC-Rio

## Introdução
Esse repositório apresenta os códigos e inputs utilizados para a realização do trabalho de ciência de dados, assim como os outputs (plots) gerados.
O intuito do trabalho foi analisar roubos na cidade do Rio de Janeiro ao longo dos anos, procurando tendências no aumento ou diminuição no número de roubos por ano e região da cidade.

## Metodologia
Os dados são do Instituto de Segurança Pública do Rio de Janeiro e foram retirados a partir do pacote 'basedosdados' para o R (https://CRAN.R-project.org/package=basedosdados). Para sua análise foram utilizados também outros pacotes: tidyverse, sf, ggplot2, gridExtra

Para os mapas foram utilizados os arqvuios shapefiles baixados do site do Instituto de Segurança Pública. Para rodar o código é necessário que todos os arquivos da pasta "Shapefile" desse repositório estejam baixados no diretório. Para baixar arquivos diretamente do site: http://www.ispdados.rj.gov.br/Conteudo.html (escolher baixar shapefile de Bases Cartográficas Digitais – Circunscrições Integradas de Segurança Pública (CISP) - Limites de 2019)

O script do R com todos os códigos está na página inicial do repositório sob o nome R_Script.R

Todas as tabelas geradas pelo código podem ser encontradas em formato .csv na pasta "CSV files"

### Arquivo de apoio
A pasta Apoio contém um documento do Instituto de Segurança Pública com a relação entre CISP-AISP-Unidade Territorial-Município-Região de Governo.

### Projeto final
Pasta onde se encontra o arquivo pdf com maiores detalhes sobre metodologia e análise dos dados.

## Outputs
Como resultado do projeto foram montados diversos gráficos que podem ser encontrados na pasta "Plots". Dentre eles estão os gráficos de evolução anual de roubo e mapas da cidade do Rio de Janeiro por nível de roubo em cada CISP da capital.

## Docker
O script para ser usado no docker e o Dockerfile estão na pasta "Docker". Por conta de erros para utilizar os pacotes 'basedosdados' e 'sf' no docker, o Projeto_docker.R foi alterado para não precisar de nenhum desses pacotes. Desse modo, a segunda parte do trabalho, onde são montados os mapas, não pôde ser incluída no script alterado. 
O script original segue inalterado na página principal do Repositório (R_Script.R)
