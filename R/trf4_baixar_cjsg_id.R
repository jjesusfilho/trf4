#' Baixar julgados de segundo grau conforme id
#'
#' @param id Id do processo
#' @param diretorio Diretório
#'
#' @details Há uma sequência de ids que podem ser baixados. Vc pode formá-la, se quiser.
#'
#' @return htmls
#' @export
#'
trf4_baixar_cjsg_id <- function(id = NULL, diretorio = "."){


  docs_pagina <- "100"

  prefixo <- "TRF4"

  sufixo <- id %>%
    stringr::str_pad(8,"left","0") %>%
    stringr::str_c(prefixo,.)

  pb <- progress::progress_bar$new(total = length(sufixo))

  purrr::walk(sufixo,purrr::possibly(~{

    pb$tick()

    url <- "https://jurisprudencia.trf4.jus.br/pesquisa/resultado_pesquisa.php"

    body <- list(
      parcial = "aqui",
      pesquisaLivre = "",
      registrosSelecionados = "",
      vetPaginacao = .x,
      paginaAtual = "0",
      totalRegistros = "",
      rdoCampoPesquisa = "I",
      chkAcordaos = "on",
      chkDecMono = "on",
      textoPesqLivre = "",
      chkDocumentosSelecionados = "",
      numProcesso = "",
      cboRelator = "",
      dataIni = "",
      dataFim = "",
      tipodata = "DEC",
      docsPagina = "",
      arrorgaos = "",
      arrclasses = "",
      hdnAcao = "",
      hdnTipo = "1"
    )

    arquivo <- file.path(diretorio,paste0(.x,".html"))

    httr::POST(url,body= body, encode="form",httr::write_disk(arquivo,overwrite = TRUE))


  },NULL))

}
