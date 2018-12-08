
[![Travis build status](https://travis-ci.org/jjesusfilho/tjdft.svg?branch=master)](https://travis-ci.org/jjesusfilho/tjdft) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/tjdft?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/tjdft)

<!-- README.md is generated from README.Rmd. Please edit that file -->
tjdft
=====

Este pacote baixa e lê decisões de primeira e segunda instância do TJDFT

Instalação
----------

Como os demais da série tribunais, este pacote só tem versão em desenvolvimento:

``` r
devtools::install_github("jjesusfilho/tjdft")
```

Primeira instância
------------------

Para baixar decisões de primeira instância, use a função `baixar_tjdft_cpopg`:

``` r
baixar_tjdft_cpopg("00050191120168070016",diretorio = ".") # Esse número foi obtido aleatóriamente, qualquer coincidência é obra do acaso.
```

Para ler esta mesma decisao, basta indicar o diretório:

``` r
arquivo <- ler_tjdft_cpopg(diretorio = ".")
```

Segunda instância
-----------------

Ainda não incluí leitura de decisões de segunda instância pelo número do processo, por ora é basca livre. Se for uma frase, coloque TRUE para aspas.

``` r
baixar_tjdft_cjsg("homicídio", aspas = FALSE)
```
