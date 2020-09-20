#' Ler julgados de segundo grau do TRF4
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio  Diretório, se não informar arquivos
#' @param wide Converte para formato largo
#'
#' @return tibble
#' @export
#'
trf4_ler_cjsg <- function(arquivos = NULL, diretorio = ".", wide = TRUE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  df <- purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    id <- stringr::str_extract(.x,"TRF\\d+")

    x <- xml2::read_html(.x)

    tipo <- x %>%
      xml2::xml_find_all("//table//td[@class='td_resultado']//table/../../td[1]") %>%
      xml2::xml_text()

    detalhes <- x %>%
      xml2::xml_find_all("//table//td[@class='td_resultado']//table//td") %>%
      #xml2::xml_siblings() %>%
      xml2::xml_text("text") %>%
      stringr::str_replace("^$","v1")

    var1 <- detalhes %>%
      stringr::str_extract(".+?(?=:)")

    val1 <- detalhes %>%
      stringr::str_extract("(?<=:).+") %>%
      stringr::str_trim()

    outros <- x %>%
      xml2::xml_find_all("//table//td/table/../../following-sibling::tr") %>%
      xml2::xml_text("text")

    var2 <- outros %>%
      stringr::str_extract(".+\n") %>%
      stringr::str_trim()
    val2 <- outros %>%
      stringr::str_extract("(?<=\n)\\X+") %>%
      stringr::str_trim()

    variaveis <-c(var1,var2)
    valores <- c(val1,val2)

    tibble::tibble(id = id, tipo = tipo, variavel = variaveis, valor = valores)


  },NULL))

  if (wide){

    df <- df %>%
      dplyr::filter(!is.na(variavel)) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id)

  }

  return(df)

}
