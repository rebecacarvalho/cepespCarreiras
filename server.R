
# Titulo: Shiny do aplicativo de Carreiras
# Autor: Rebeca Carvalho



# Objetivos

#'         - Criar um servidor que seja ágil e coerente nos resultados apresentados.



# 1. Server ---------------------------------------------------------------


server <- function(input, output, session){
    
  
 
   #Sobre
    
    output$Note <- renderUI({
        note <- paste0("
                  <h2 align = 'center'>
                   <font size ='6' color = 'black'><strong>
                   
                   SOBRE </font></h2>
                   
                   <font size = '1' color = 'black'>
                    <h4 align = 'justify'><br />
                   <p style='line-height:150%'>O CepespCarreiras foi criado a partir de dados do TSE, tratados e agregados pela equipe do CepespData, 
                   permitindo a consulta aos cargos já concorridos e ocupados por um mesmo político.  Atualmente, inclui 
                   as eleições nacionais e estaduais (1998 a 2018). As eleições a nível municipal serão incorporadas futuramente.</p></h4></font>")
        HTML(note)
    })
    


# 1.1. Opcoes -------------------------------------------------------------

    updateSelectizeInput(session, 
                         'CANDIDATO', 
                         choices = candidatos, 
                         selected = NULL,
                         options = list(
                           placeholder = 'Digite o nome do candidato'),
                         server = TRUE)    
    
   
    
# 1.2. Perfil do candidato -----------------------------------------------------------
    
    
    output$perfil <- DT::renderDataTable(
      bperfil()
    )
   
    
    bperfil <- eventReactive(input$BCAL1, {
        datatable(
            rownames = FALSE,
            caption =  htmltools::tags$caption(
            style = 'text-align: center;
                    font-weight: bold;
                    font-size: 20px;
                    color: black;',
                'Informações sociodemográficas  do(a) candidato(a)'),
                  options = list(dom = 't', 
                                 paging = FALSE, 
                                 scrollX = TRUE,
                                 columnDefs = list(list(
                                     className = 'dt-center',
                                     targets="_all")),
                                 autoWidth = FALSE,
                                 ordering = FALSE),{
            candidato <- req(input$CANDIDATO)
            if(length(candidato) > 0){
               df %>% 
                    dplyr::filter(`Nome de urna2` == input$CANDIDATO &
                                  `Ano da Eleição` == max(`Ano da Eleição`)) %>% 
                    head(1) %>% 
                    dplyr::select(Nome, CPF, `Número do Título Eleitoral`, Sexo, `Cor ou Raça`, `Grau de Instrução`, `Ocupação`,
                                  `Estado Civil`, Nacionalidade, `Estado de Nascimento`, `Município de Nascimento`) %>% 
                    unique()
                
            } else {
                return()
            }    
        })
    })
    
# 1.3. Eleicoes -----------------------------------------------------------     
    
    output$eleicoes <- DT::renderDataTable(
      beleicoes()
      
    )
    
    
    beleicoes <- eventReactive(input$BCAL1, {
        datatable(
            rownames = FALSE,
            caption =  htmltools::tags$caption(
                style = 'text-align: center;
                    font-weight: bold;
                    font-size: 20px;
                    color: black;',
                'Cargos eletivos disputados e exercidos pelo(a) candidato(a)'),
            options = list(dom = 't', 
                                 autoWidth = FALSE,
                                 paging = FALSE, 
                           columnDefs = list(list(
                               className = 'dt-center',
                               targets="_all")),
                                 scrollX = TRUE,
                                 ordering = FALSE),{
            candidato <- req(input$CANDIDATO)
            if(length(candidato) > 0){
                df %>% 
                    dplyr::filter(`Nome de urna2` == input$CANDIDATO) %>% 
                    dplyr::select(`Ano da Eleição`, `Nº do Turno`, Cargo, `Sigla da Unidade Eleitoral`, `Situação da Candidatura`, 
                                  `Situação de Totalização do Turno`,`Número de urna`, `Sigla do Partido`,
                                  `Composição da Coligação`, `Quantidade de Votos`)
            } else{
                return()
                
            }
        })
    })
    
}


