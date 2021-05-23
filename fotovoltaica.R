


`%>%` <- magrittr::`%>%`



dados_gd <- readxl::read_xlsx('2021_05_22_base.xlsx',
                              
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


tibble::glimpse(dados_gd)


teste <- dados_gd %>% 
  
  dplyr::select(data_instalacao, estado, qtde_consumidores) %>% 
  
  dplyr::mutate(data_instalacao = zoo::as.yearqtr(data_instalacao),
                
                qtde_instalacoes = 1) %>% 

  dplyr::group_by(data_instalacao, estado) %>% 
  
  dplyr::summarise(dplyr::across(.cols = c(qtde_consumidores, qtde_instalacoes), .funs = ~sum(.x))) %>% 

  tibble::view()
  
  



dplyr::summarise(dplyr::across(.cols = where(is.double), .funs = sum) %>% 


,






  ggplot2::ggplot() +
  
  ggplot2::geom
  
  








img <- png::readPNG('fotovoltaica.png')

g <- grid::rasterGrob(img, interpolate=TRUE)




ggplot2::ggplot(mtcars) +
  
  ggplot2::annotation_custom(g,
                             
                             xmin = -Inf, 
                             
                             xmax = Inf, 
                             
                             ymin = -Inf, 
                             
                             ymax = Inf) +
  
  ggplot2::geom_bar(mapping = ggplot2::aes(x = cyl),
                    
                    colour = 'red',
                    
                    fill = 'red',
                    
                    alpha = 0.5) 




