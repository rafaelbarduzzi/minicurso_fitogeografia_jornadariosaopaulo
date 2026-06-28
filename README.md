### Minicurso: Análises fitogeográficas no ambiente computacional R

Yago Barros-Souza, Monique Maianne, Rafael F. Barduzzi

A biodiversidade está distribuída de maneira heterogênea ao longo do espaço e do tempo, sendo influenciada por fatores intrínsecos (e.g., capacidade de dispersão dos organismos) e/ou extrínsecos (e.g., filtros ambientais). Identificar padrões espaciais e processos que moldam a distribuição da biodiversidade é o objetivo central de análises biogeográficas. Neste minicurso, integraremos dados de ocorrência, filogenéticos e abióticos para investigar padrões fitogeográficos.

*Materiais necessários*: Computador com R Studio instalado (https://rstudio-education.github.io/hopr/starting.html)

*N° de vagas*: 30

### Descrição de arquivos

1.data: Pasta disponibilizada para os participantes atráves de um link do Google Drive. Contém:
- ne_countries_small: Dados espaciais globais baixados do ArcGIS;
- occurrence_clean.csv: Dados de ocorrências globais de espécies de Mimosa baixadas do GBIF, specieslink e idigbio;
- bioclim_mercator e soil_world_mercator: Dados bioclimáticos e de solo globais baixados do WorldClim 2.1 e SoilGrids;
- tree-Vasconcelos2020.txt: Filogenia do gênero Mimosa L. inferida por Vasconcelos et al. 2020.

2.script: Pasta com scripts extras para aqueles interessados na obtenção dos dados para o tutorial. Contém:
- download_abiotic_shapefile: realiza o download dos dados espaciais e dados bioclimáticos e de solo globais utilizados no tutorial.

3.output: Pasta com arquivos gerados durante o tutorial. Contém:
- zonal_statistics.rds: Estatísticas zonais, com variáveis atribuídas a cada uma das células de grade;
- phylogenetic_diversity_stats.rds: Diversidade filogenética para cada comunidade (células de grade).

biomas_america_do_sul_fontana: Classificação de biomas da América do Sul de acordo com Fontana et al. 2012.

lookup_table.csv: Tabela com informações acerca dos dados abióticos utilizados.

tutorial-ptbr.Rmd: Roteiro para tutorial do minicurso (executável).

tutorial-ptbr.html: Roteiro para tutorial do minicurso (visualização).

### Referências

- Vasconcelos, T. N., Alcantara, S., Andrino, C. O., Forest, F., Reginato, M., Simon, M. F., & Pirani, J. R. (2020). Fast diversification through a mosaic of evolutionary histories characterizes the endemic flora of ancient Neotropical mountains. Proceedings of the Royal Society B: Biological Sciences, 287(1923), 20192933.

- Fontana, S. L., Bianchi, M. M., & Bennett, K. D. (2012). Palaeoenvironmental changes since the Last Glacial Maximum: Patterns, timing and dynamics throughout South America. The Holocene, 22(11), 1203-1206.