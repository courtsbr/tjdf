#' Lê dados processuais
#'
#' @param diretorio onde se encontram os htmls
#' @param plano check `future::plan`
#'
#' @return tibble com as informações processuais.
#' @export
#'
#'
ler_tjdft_cpopg <- function(diretorio = ".", plano = NULL) {

  info <- fs::dir_info(diretorio) %>%
    dplyr::filter(size > 0)

  processo <- stringr::str_extract(info$path, "\\d{20}(?=\\.html)")

  if (!is.null(plano)) {
    future::plan(plano)
  }

  furrr::future_map2(info$path, processo, purrr::possibly( ~ {
    conteudo <- xml2::read_html(.x)


    id <- conteudo %>%
      xml2::xml_find_all('//span[contains(@id,"i_")]') %>%
      xml2::xml_attr("id")

    descricao <- conteudo  %>%
      xml2::xml_find_all('//span[contains(@id,"i_")]') %>%
      xml2::xml_text()

    andamento <- conteudo %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::flatten_dfr() %>%
      purrr::set_names(.[2, ] %>% unlist) %>%
      tail(-3) %>%
      dplyr::mutate(processo = .y)


    list(dados = tibble::tibble(id, descricao, processo = .y),
         andamento = andamento)

  }, NULL)) %>%
    purrr::set_names(processo)
}