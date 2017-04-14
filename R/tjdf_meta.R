#' Function tjdf_meta
#'
#' Esta função extrai os metadados das decisões judiciais do Tribunal de Justiça do Distrito Federal
#'
#' @param url A forma mais conveniente de obter o url é fazer a busca jurisprudencial
#'      no TJDF, copiar a url da barra de endereço e transformá-la em uma string do R.
#' @param form Lista com informações para o body.
#' @keywords Tribunal de Justiça, Jurisprudência, Decisão Judicial, Webscraping
#' @export
#' @examples
#' tjdf_meta(url,form)

tjdf_meta<-function(url,form){
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
## Primeira etapa. Carrega os dados da busca e obtêm no número de decisões.
  a<-GET(url)
  b<-htmlParse(a)
  val<-xpathSApply(b,"//*[@title]",xmlValue)[4]
  val<-str_extract(val,"\\d+")
  num<-as.numeric(val)
  max_pag<-ceiling(num/20)
## Segunda etapa. Com o número de decisões, inicia a extração do número do registro (id do acórdão)
  url2<-"http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj"
  registro<-NULL
  for (i in 1:max_pag){
    form$numeroDaPaginaAtual<-i
    d<-POST(url2,set_cookies(unlist(a$cookies)),body=form)
    e<-htmlParse(d,encoding="UTF-8")
    r<-xpathSApply(e,"//*[@id='id_link_abrir_dados_acordao']",xmlValue)
    registro<-c(registro,r)
    Sys.sleep(1)
  }
## Terceira etapa. com o número dos registros, cria as urls para extrair os metadados de cada decisão.
  u<-paste0("http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj?visaoId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&controladorId=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.ControladorBuscaAcordao&visaoAnterior=tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao&nomeDaPagina=resultado&comando=abrirDadosDoAcordao&enderecoDoServlet=sistj&historicoDePaginas=buscaLivre&quantidadeDeRegistros=20&baseSelecionada=BASE_ACORDAO_TODAS&numeroDaUltimaPagina=1&buscaIndexada=1&mostrarPaginaSelecaoTipoResultado=false&totalHits=1&internet=1&numeroDoDocumento=",registro)
  df<-data.frame()
  for (i in 1:length(u)){
    s<-GET(u[i])
    b<-htmlParse(s,encoding = "UTF-8")
    c<-xpathSApply(b,"//*[@class='conteudoComRotulo']",xmlValue,trim=T)[1:9]
    processo<-c[1]
    registro<-c[2]
    data.julgamento<-c[3]
    relator<-c[4]
    orgao.julgador<-c[5]
    publicacao<-c[6]
    ementa<-c[7]
    decisao<-c[8]
    resultado<-c[9]
    df1<-data.frame(processo,registro,data.julgamento,relator,orgao.julgador,publicacao,ementa, decisao,resultado)
    df<-rbind(df,df1)
    Sys.sleep(1)
  }
  return(df)
}


