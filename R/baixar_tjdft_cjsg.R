#' Baixar consulta jurisprudencial de segunda instancia do TJDFT
#'
#'
#'
#' @param buscalivre palavra ou expressao a ser buscada.
#' @param aspas TRUE. Coloca a expressao entre aspas
#' @keywords Courts, Decisions, Jurimetry, Webscraping
#' @export

baixar_tjdft_cjsg <- function(buscalivre, aspas = TRUE) {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  buscalivre <- stringr::str_replace_all(buscalivre, "\\s+", "+")

  if (aspas == TRUE)
    buscalivre <- paste0("%22", buscalivre, "%22")

  url1 <-
    "https://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?argumentoDePesquisa=indulto&visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=buscaLivre&comando=pesquisar&internet=1&camposSelecionados=ESPELHO&COMMAND=ok&quantidadeDeRegistros=20&tokenDePaginacao=1"

  url1 <- urltools::param_set(url1, "argumentoDePesquisa", buscalivre)

  hits <- httr::GET(url1) %>%
    httr::content("parsed") %>%
    xml2::xml_find_all("//*[@class='conteudoComRotulo']") %>%
    xml2::xml_text(trim = T) %>%
    .[3] %>%
    as.numeric()

  paginas <- hits %>%
    '/'(20) %>%
    ceiling()

  url2 <-
    "http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=buscaLivre2&buscaPorQuery=1&baseSelecionada=BASE_ACORDAO_TODAS&ramoJuridico=&baseDados=[BASE_ACORDAOS,%20TURMAS_RECURSAIS,%20BASE_ACORDAO_PJE,%20BASE_HISTORICA]&argumentoDePesquisa=indulto&desembargador=&indexacao=&tipoDeNumero=&tipoDeRelator=&camposSelecionados=[ESPELHO]&numero=&tipoDeData=&dataFim=&dataInicio=&ementa=&orgaoJulgador=&legislacao=&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaPaginaAtual=&quantidadeDeRegistros=20&totalHits=" %>%
    urltools::param_set("argumentoDePesquisa", buscalivre) %>%
    urltools::param_set("totalHits", hits)

  df <- tibble::tibble()
  for (i in 1:paginas) {
    tryCatch({
      url2 <- urltools::param_set(url2, "numeroDaPaginaAtual", i)

      codigo <- url2 %>%
        httr::GET() %>%
        httr::content() %>%
        xml2::xml_find_all("//*[@id='id_link_abrir_dados_acordao']") %>% xml2::xml_text()

      url3 <-
        paste0(
          "https://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&controladorId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.ControladorBuscaAcordao&visaoAnterior=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=resultado&comando=abrirDadosDoAcordao&enderecoDoServlet=sistj&historicoDePaginas=buscaLivre&quantidadeDeRegistros=20&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaUltimaPagina=1&buscaIndexada=1&mostrarPaginaSelecaoTipoResultado=false&totalHits=1&internet=1&numeroDoDocumento=",
          codigo
        )

      b1 <- purrr::map(url3, function(x) {
        x %>% httr::GET() %>%
          httr::content()

      })

      processo <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Classe')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE)
        })

      registro <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Registro')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE)
        })

      data_julgamento <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Julgamento')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE)
        })

      orgao_julgador <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Julgador')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE)
        })


      relator <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Relator:')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE)
        })

      data_publicacao <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Intima\u00e7\u00e3o')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim = TRUE) %>%
            stringr::str_extract("\\d+/\\d+/\\d+") %>%
            lubridate::dmy()
        })



      decisao <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Decis\u00e3o')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim=TRUE)
        })

      ementa <- b1 %>%
        purrr::map_chr(function(x) {
          x %>% xml2::xml_find_all(
            "//*[@class='rotulo'][contains(.,'Ementa')]/ancestor::div[1]/div[@class='conteudoComRotulo']"
          ) %>%
            xml2::xml_text(trim=TRUE)
        })

      df1 <-
        tibble::tibble(
          processo,
          registro,
          data_julgamento,
          data_publicacao,
          relator,
          orgao_julgador,
          ementa,
          decisao,
          pagina = i
        )
      df <- rbind(df, df1)
    }, error = function(m) {
      m
    }, finally = {
      next
    })

  }
  return(df)
}
