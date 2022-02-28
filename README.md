
## Sumário

1. Links Relevantes
2. Pastas e arquivos

## 1. Links Relevantes

* [Dados de covid](https://brasil.io/dataset/covid19/files/)
* [Base com discursos da cpi da covid](https://basedosdados.org/dataset/br-senado-cpipandemia)
* [Dados dos candidatos](https://basedosdados.org/dataset/br-tse-eleicoes)

As bases podem ser acessadas diretamente pelos link acima ou clicando [aqui](https://drive.google.com/drive/folders/13ozE9GhQaxSmfw-wjO_2QT_frnw5nHxU?usp=sharing)

## 2. Pastas e arquivos

##### `/codigo`
* 2 arquivos:
  * Um R Script, com o tratamento das bases, modelo ML e todos os entregáveis
  * Um RMD, que gera o pdf e reproduz os gráficos e tabelas que tinham sido gerados no R Script.
  
##### `/input`
- Arquivos rda com bases limpas utilizadas para gerar as tabelas e gráficos do RMD
- 2 arquivos csv:
  * 1 com a amostra aleatória a ser preenchida para input do modelo de ML
  * 1 que é o mesmo arquivo de cima, porém preenchido manualmente
  
##### `/output`
- PDF com trabalho final
- Inclui as 3 bases de dados geradas na subpasta `/CSV` (mais detalhes ver o pdf final)
- Figuras incluídas na subpasta `/figures` como png
- Tabelas incluídas na subpast `/tables` como png

## Citação para o template utilizado

cff-version: 1.2.0
message: "If you use this software, please cite it as below."
authors:
- family-names: "Dahis"
  given-names: "Ricardo"
title: "Template Repository for Research Papers"
date-released: 2021-11-10
url: "https://github.com/rdahis/paper_template"
