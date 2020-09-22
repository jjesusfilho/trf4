#' Baixar julgados de segundo grau conforme id
#'
#' @param id Id do processo. Pode ser o id completo ou o número.
#' @param diretorio Diretório
#'
#' @details Há uma sequência de ids que podem ser baixados. Vc pode formá-la, se quiser.
#'
#' @return htmls
#' @export
#'
trf4_baixar_cjsg_id <- function (id = NULL, diretorio = ".")
{

  pb <- progress::progress_bar$new(total = length(id))

  purrr::walk(id, purrr::possibly(~{

    pb$tick()

    if (stringr::str_detect(.x, "TRF4", negate = TRUE)) {
      .x <- .x %>% stringr::str_pad(8, "left", "0") %>%
        stringr::str_c("TRF4", .)
    }

    url <- "https://jurisprudencia.trf4.jus.br/pesquisa/resultado_pesquisa.php"

    body <- list(parcial = "aqui", pesquisaLivre = "", registrosSelecionados = "",
                 vetPaginacao = .x, paginaAtual = "0", totalRegistros = "",
                 rdoCampoPesquisa = "I", chkAcordaos = "on", chkDecMono = "on",
                 textoPesqLivre = "", chkDocumentosSelecionados = "",
                 numProcesso = "", cboRelator = "", dataIni = "",
                 dataFim = "", tipodata = "DEC", docsPagina = "",
                 arrorgaos = "", arrclasses = "", hdnAcao = "", hdnTipo = "1")

    arquivo <- file.path(diretorio, paste0("cjsg_",.x, ".html"))

    httr::POST(url, body = body, encode = "form", httr::write_disk(arquivo,
                                                                   overwrite = TRUE))
  }, NULL))
}
