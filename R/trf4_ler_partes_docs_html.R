#' Lê partes dos htmls dos julgados
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar arquivos
#'
#' @return tibble
#' @export
#'
trf4_ler_partes_docs_html <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x,"TRF4\\d+")

    x <- .x %>%
      xml2::read_html()


    var <- x %>%
      xml2::xml_find_all("//section[@data-nome='partes']//span[@class='tipo_parte']") %>%
      xml2::xml_text()

    val <- x %>%
      xml2::xml_find_all("//section[@data-nome='partes']//span[@class='nome_parte']") %>%
      xml2::xml_text()

    tibble::tibble(id, variavel = var, valor = val)

  },NULL))

}
