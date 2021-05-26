



`%>%` <- magrittr::`%>%`



# Importação dos Dados -----------------------------------------------------------------------------
dados_gd <- readxl::read_xlsx('./dados/2021_05_22_base.xlsx',

                              col_types = c("text", "text", "text", "text",

                                            "text", "text", "text", "numeric",

                                            "text", "text", "date", "text",

                                            "text", "numeric")) %>%

  dplyr::rename('concessionaria' = NomAgente,

         'cod_concessionaria' = SigAgente,

         'id_instalacao' = CodGD,

         'id_proprietario' = CPF_CNPJ_encriptografado,

         'classe' = DscClasseFornecimento,

         'grupo' = DscGrupoFornecimento,

         'modalidade' = DscModalidade,

         'qtde_consumidores' = QtdUCRecebeCredito,

         'regiao' = NomRegiao,

         'data_instalacao' = DthConexao,

         'tipo' = SigTipoGeracao,

         'combustivel' = DscCombustivel,

         'potencia_kw' = MdaPotenciaInstalada) %>%

  tidyr::separate(NomMunicipio, into = c('municipio', 'estado'), sep = " - ")




#---------------------------------------------------------------------------------------------------



# [Gráfico] Evolução das Instalações e Consumidores ------------------------------------------------
g1 <- dados_gd %>%

  dplyr::select(data_instalacao, estado, qtde_consumidores) %>%

  dplyr::mutate(data_instalacao = lubridate::year(data_instalacao)) %>%

  dplyr::group_by(data_instalacao, estado) %>%

  dplyr::summarise(instalacoes = dplyr::n(),

                   consumidores = sum(qtde_consumidores)) %>%

  dplyr::ungroup() %>%

  dplyr::summarise(data = data_instalacao,

                   estado = estado,

                   instalacoes = cumsum(instalacoes),

                   consumidores = cumsum(consumidores)) %>%

  dplyr::filter(estado == 'MG',

                data %in% seq(2012,2020, 1)) %>%

  ggplot2::ggplot() +

  ggplot2::geom_col(mapping = ggplot2::aes(x = data, y = instalacoes),

                    fill = '#0F2E3D',

                    colour = 'white') +

  ggplot2::geom_label(mapping = ggplot2::aes(x = data, y = instalacoes, label = instalacoes),

                      size = 4.8,

                      fill = '#0F2E3D',

                      colour = 'white',

                      fontface = 'bold',

                      family = 'Fantasy font',

                      vjust = -0.2) +

  ggplot2::labs(title = 'Quantidade de Instalações',

                y = 'Acumulado',

                x = '') +

  ggplot2::scale_y_continuous(limits = c(0 , 400000),

                              labels = scales::number_format(big.mark = '.')) +

  ggplot2::scale_x_continuous(breaks = seq(2012, 2021, 1)) +

  ggplot2::theme(text = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                              size = 14,  family = 'apple-system'),

                 axis.text.y = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                                     size = 11,  family = 'apple-system'),

                 axis.text.x = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                                     size = 11,  family = 'apple-system'))



g2 <- dados_gd %>%

  dplyr::select(data_instalacao, estado, qtde_consumidores) %>%

  dplyr::mutate(data_instalacao = lubridate::year(data_instalacao)) %>%

  dplyr::group_by(data_instalacao, estado) %>%

  dplyr::summarise(instalacoes = dplyr::n(),

                   consumidores = sum(qtde_consumidores)) %>%

  dplyr::ungroup() %>%

  dplyr::summarise(data = data_instalacao,

                   estado = estado,

                   instalacoes = cumsum(instalacoes),

                   consumidores = cumsum(consumidores)) %>%

  dplyr::filter(estado == 'MG',

                data %in% seq(2012,2020, 1)) %>%

  ggplot2::ggplot() +

  ggplot2::geom_col(mapping = ggplot2::aes(x = data, y = consumidores),

                    fill = '#0F2E3D',

                    colour = 'white') +

  ggplot2::geom_label(mapping = ggplot2::aes(x = data, y = consumidores, label = consumidores),

                      size = 4.8,

                      fill = '#0F2E3D',

                      colour = 'white',

                      fontface = 'bold',

                      family = 'Fantasy font',

                      vjust = -0.2) +

  ggplot2::labs(title = 'Quantidade de Consumidores',

                y = 'Acumulado',

                x = '') +

  ggplot2::scale_y_continuous(limits = c(0 , 400000),

                              labels = scales::number_format(big.mark = '.')) +

  ggplot2::scale_x_continuous(breaks = seq(2012, 2021, 1)) +

  ggplot2::theme(text = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                              size = 14,  family = 'apple-system'),

                 axis.text.y = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                                     size = 11,  family = 'apple-system'),

                 axis.text.x = ggplot2::element_text(colour = '#292A2E', face = 'bold',

                                                     size = 11,  family = 'apple-system'))



gridExtra::grid.arrange(g1, g2, nrow = 2)


#---------------------------------------------------------------------------------------------------
