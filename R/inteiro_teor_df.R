#' Function inteiro_teor_df
#'
#' Esta função baixa os pdfs com o inteiro teor dos acórdãos do Tribunal de Justiça do Distrito Federal
#'
#' @param registro número de registro obtido com a função tjdf_meta.
#' @keywords Tribunal de Justiça, Jurisprudência, Decisão Judicial, Inteiro Teor
#' @export
#' @examples
#' registro<-1005250
#' inteiro_teor_df(registro)


inteiro_teor_df<-function(registro){
  set_config(config(ssl_verifypeer = FALSE))
  
  body<-list(visaoId = "tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao", 
             controladorId = "tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.ControladorBuscaAcordao", 
             idDoUsuarioDaSessao = "", nomeDaPagina = "dadosDoAcordao", 
             comando = "downloadInteiroTeor", enderecoDoServlet = "sistj", 
             visaoAnterior = "tjdf.sistj.acordaoeletronico.buscaindexada.apresentacao.VisaoBuscaAcordao", 
             skin = "", tokenDePaginacao = "1", historicoDePaginas = "buscaLivre", 
             historicoDePaginas = "resultado", argumentoDePesquisa = "", 
             hppert = "", numero = "", tipoDeNumero = "", tljjat = "", 
             desembargador = "", kahufx = "", tipoDeRelator = "", orgaoJulgador = "", 
             ementa = "", decisaoLivre = "", indexacao = "", talkls = "", 
             tipoDeData = "", tipoDeBusca = "", camposSelecionados = "ESPELHO", 
             camposDeAgrupamento = "", numeroDoDocumento = "", 
             quantidadeDeRegistros = "20", dataInicio = "", dataFim = "", 
             campoDeOrdenacao = "", baseDados = "BASE_ACORDAOS", baseDados = "TURMAS_RECURSAIS", 
             baseDados = "BASE_ACORDAO_PJE", baseDados = "BASE_HISTORICA", 
             numeroDaUltimaPagina = "1", idDespacho = "", buscaIndexada = "1", 
             mostrarPaginaSelecaoTipoResultado = "false", totalHits = "1", 
             tipoDeDocumento = "", query = "{}", buscaPorQuery = "", argumentoDePesquisa = "", 
             jurisprudenciaAdministrativa = "", faixaInicial = "", faixaFinal = "", 
             indexados = "", decisao = "", decisaoLivre = "", loginDoUsuario = "", 
             nomeDoUsuario = "", idClasse = "", descricaoClasse = "", 
             qrwywx = "", indiceAcordaoAtual = "0", numeroDoDocumento = "", 
             numeroDaPaginaAtual = "1", caminhoDoAcordao = "", baseSelecionada = "BASE_ACORDAO_TODAS", 
             quantidadeDeRegistros = "20", numeroDaUltimaPagina = "1", 
             totalHits = "1", tipoDeDocumento = "", jurisprudenciaAdministrativa = "", 
             faixaInicial = "", faixaFinal = "", indexados = "", decisao = "", 
             decisaoLivre = "", loginDoUsuario = "", nomeDoUsuario = "", 
             idClasse = "", descricaoClasse = "", idDoAcordao = "", viqoev = "aberto", 
             idJanelaAbrirAjax = "", idJanelaAbrirIsModalAjax = "false", 
             internet = "1", iebzrz = "<?xml version='1.0' encoding='ISO-8859-1'?><comando><camposParaRedesenhar></camposParaRedesenhar><camposParaRecarregar><campo>idJanelaAbrirAjax</campo><campo>idJanelaAbrirIsModalAjax</campo></camposParaRecarregar><campoComMensagem></campoComMensagem><timeoutDeMensagem>0</timeoutDeMensagem><campoComFoco></campoComFoco><comandoDeSaida></comandoDeSaida></comando>")
  
  
  for (i in seq_along(registro)){
    tryCatch({
      body$numeroDoDocumento<-registro[i]
      s<-POST("http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/sistj",body=body)
      
      idd <-  s %>%  httr::content('text') %>% 
        stringr::str_extract('idd=[0-9a-zA-Z]+')
      
      u_teor <- paste0(
        "http://pesquisajuris.tjdft.jus.br/IndexadorAcordaos-web/infra/Download.jsp?",
        idd)
      
      httr::GET(u_teor, httr::write_disk(paste0(registro[i],".pdf"), overwrite = TRUE))
    }, error = function(m) {
      m
    }, finally = {
      next
    })
    Sys.sleep(1)
    
  }
  
}