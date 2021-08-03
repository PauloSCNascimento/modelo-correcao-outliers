# Descrição
A estrutura “modelo-correcao-outliers”, apresenta um modelo de filtro probabilístico para correção de *missings* e *outliers*. O objetivo do modelo é tratar dados faltantes e aberrantes em series temporais de geração e vento verificados das usinas eólicas. As medidas de geração e vento são transmitidas ao SCADA (*Supervisory Control and Data Acquisition*) para os bancos de dados dos centros de operações. Os dados faltantes e aberrantes ocorrem quando há falhas de medição ou de aferição de equipamentos ou transmissão de dados. Este modelo foi desenvolvido em linguagem R.

# Documentação
O modelo de correção baseia-se num processo de identificação de *outliers* através de informações de padrões característicos. Desta forma, os códigos apresentam um método de reconhecimento de padrões. A correção dos dados que foram considerados como *outliers* é realizada substituindo esses dados, transformados em faltantes, por valores mais plausíveis. Esses valores previstos são obtidos pelo modelo proposto baseado no filtro de Kalman adaptado. Existem algumas variações do filtro de Kalman, neste modelo é utilizado uma formulação em espaço de estados de nível e tendência local. Sendo que o valor observado da série é presumido como a soma de duas componentes. O nível da série, somado a uma componente aleatória de tendência, cuja distribuição de probabilidades é a normal.

__Parte 01: Descrição teórica__ 
* Um vídeo com a descrição teórica do modelo pode ser encontrado no link:
https://www.youtube.com/watch?v=kwkTTWZEbUI
* A apresentação em pdf do vídeo é encontrada na raiz do diretório “modelo-correcao-outliers”.

**Parte 02: Descrição dos algoritmos**
* Todos os códigos relevantes ao modelo estão contidos no seguinte diretório: ~\Codigos\R.
* A execução do código “Filtrando_dados.R” já captura automaticamente o diretório raiz após o download  de “modelo-correcao-outliers”, desde que mantida a estrutura de pasta do modelo.
* O manual de execução e descrição dos algoritmos podem ser visualizados no vídeo:
https://www.youtube.com/watch?v=mE4uCv73sbU

**Links necessários**
* Download R para Windows:
https://cran.r-project.org/bin/windows/base/
* Download Rstudio:
https://www.rstudio.com/products/rstudio/download/#download

