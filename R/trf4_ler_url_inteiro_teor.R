#' Lê urls dos inteiros teores do TRF4
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se não informar vetor de arquivos
#'
#' @return tibble
#' @export
#'
trf4_ler_url_inteiro_teor <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

 purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x,"TRF\\d+")

    x <- xml2::read_html(.x)

    url <- x %>%
         xml2::xml_find_first("//td[b='Inteiro Teor:']/following-sibling::td/a") %>%
         xml2::xml_attr("href")

    tibble::tibble(id = id, inteiro_teor_url = url)

 },NULL))

}
