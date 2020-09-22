#' Lê urls dos  processos de segundo grau do TRF4 a partir dos hmtls do cjsg
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informar vetor de arquivos
#'
#' @return tibble
#' @export
#'
trf4_ler_url_processo <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x,"TRF\\d+")

    x <- xml2::read_html(.x)

    url <- x %>%
      xml2::xml_find_first("//td[b='Processo:']/following-sibling::td/a") %>%
      xml2::xml_attr("href")

    tibble::tibble(id = id, processo_url = url)

  },NULL))

}
