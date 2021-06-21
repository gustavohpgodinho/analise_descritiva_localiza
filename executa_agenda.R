validando_agenda <- function(agenda, hr_trabalho){
  
  # deixando na agenda somente os compromissos que comecam entre o horario de entrada 
  # e terminam ate o horario de saida do funcionario
  agenda_valida <- list()
  for(i in agenda){
    if(i[1] >= hr_trabalho[1] & i[2] <= hr_trabalho[2]){
      
      k <- length(agenda_valida) + 1
      agenda_valida[[k]] <- i
      
    }
  }
  
  return(agenda_valida)
  
}

checando_horarios_livres <- function(agenda, hr_min, hr_max){
  
  # verificando os horarios livres do funcionario
  
  horarios_livres <- list()
  ref_hora <- hr_min
  
  for(i in agenda){
    
    if(i[1] <= ref_hora){
      
      ref_hora <- i[2]
      next
      
    } else{
      
      j <- length(horarios_livres) + 1
      horarios_livres[[j]] <- c(ref_hora, i[1])
      ref_hora <- i[2]
      
    }
  }
  if(ref_hora < hr_max){
    
    j <- length(horarios_livres) + 1
    horarios_livres[[j]] <- c(ref_hora, hr_max)
    
  }
  
  return(horarios_livres)
}

ind_combinando_intervalos_em_comum <- function(intervalo_a, intervalo_b){
  
  # essa funcao cria apenas uma indicacao se os horarios possuem intersecao ou nao.
  # retorna TRUE ou FALSE
  
  intervalo_a <- unlist(intervalo_a)
  intervalo_b <- unlist(intervalo_b)
  
  if((intervalo_a[1] >= intervalo_b[1] & intervalo_a[1] < intervalo_b[2])|
     (intervalo_b[1] >= intervalo_a[1] & intervalo_b[1] < intervalo_a[2])){
    
    return(TRUE)
    
  } else{
    
    return(FALSE)
    
  }
}

combinando_intervalos_em_comum <- function(intervalo_a, intervalo_b){
  
  # dado dois intervalos, essa funcao verifica se ha alguma intersecao, mesmo que 
  # minima entre eles
  intervalos_em_comum <- list()
  if(length(intervalo_a) > 0 & length(intervalo_b) > 0){
    
    for(i in intervalo_a){
      for(j in intervalo_b){
        
        if(ind_combinando_intervalos_em_comum(i, j)){
          
          k <- length(intervalos_em_comum) + 1
          intervalos_em_comum[[k]] <- list(c(i), c(j))
        } else{
          next
        }
      }
      
    }
    
  }
  
  return(intervalos_em_comum) 
}

define_intersecao_intervalos <- function(intervalo_a, intervalo_b){

  # define a intersecao existente entre os intervalos
  if(ind_combinando_intervalos_em_comum(intervalo_a, intervalo_b)){
    c(max(intervalo_a[1], intervalo_b[1]), min(intervalo_a[2], intervalo_b[2]))
  } else{
    NULL
  }
  
  
}    

intersecao_intervalos <- function(intervalos){
  
  # cria a lista em os intervalos livres em comum dos funcionarios
  intervalos_disponiveis <- list()
  for(i in intervalos){
    k <- length(intervalos_disponiveis) + 1
    intervalos_disponiveis[[k]] <- define_intersecao_intervalos(i[[1]], i[[2]])
  }
  
  return(intervalos_disponiveis)
}

transforma_horario <- function(horario){
  
  # transformando os horarios em minutos, para conseguir calcular a diferenca entre eles
  60*as.integer(stringr::str_sub(horario, 1, 2)) + as.integer(stringr::str_sub(horario, 4, 5))
}

verificando_intervalo_valido <- function(intervalos, duracao){
  
  #verificando se os intervalos disponiveis possuem a duracao necessario
  horarios_validos <- list()
  for(i in intervalos){
    
    dif_tempo <- transforma_horario(i[2]) - transforma_horario(i[1])
    
    if(dif_tempo >= duracao){
      
      k <-length(horarios_validos) + 1
      horarios_validos[[k]] <- i
    }
    
  }
  
  return(horarios_validos)
}


verificando_horarios_disponiveis <- function(horario_trabalho_pessoa1, horario_trabalho_pessoa2, 
                                             agenda_pessoa1, agenda_pessoa2, duracao_reuniao){
  
  if(length(horario_trabalho_pessoa1) == 0 | length(horario_trabalho_pessoa2) == 0){
    
    "É preciso informar os horários de trabalho dos 2 funcionários."
    
  } else{
    
    # definindo o intervalo em comum que os funcionarios trabalham
    min_hora_reuniao <- define_intersecao_intervalos(horario_trabalho_pessoa1, horario_trabalho_pessoa2)[1]
    max_hora_reuniao <- define_intersecao_intervalos(horario_trabalho_pessoa1, horario_trabalho_pessoa2)[2]
    
    # validando a agenda dos funcionarios, excluindo compromissos fora do trabalho de trabalho
    agenda_pessoa1 <- validando_agenda(agenda_pessoa1, horario_trabalho_pessoa1)
    agenda_pessoa2 <- validando_agenda(agenda_pessoa2, horario_trabalho_pessoa2)
    
    # se nao existe intervalo em comum entao nao ha como marcar a reuniao
    if(is.null(min_hora_reuniao)){
      'Os horários de trabalho não coincidem.'
    } else{
      
      # se existe, precisamos definir os horarios livres dos dois funcionarios
      intervalos_livres_pessoa1 <- checando_horarios_livres(agenda_pessoa1, min_hora_reuniao, max_hora_reuniao)
      intervalos_livres_pessoa2 <- checando_horarios_livres(agenda_pessoa2, min_hora_reuniao, max_hora_reuniao)
      
      # combinando os intervalos livres dos dois funcionarios que possua a minima intersecao
      intervalos_em_comum <- combinando_intervalos_em_comum(intervalos_livres_pessoa1, intervalos_livres_pessoa2)  
      
      # se nao ha intervalos em comum, entao nao ha possibilidade para marcacao da reuniao
      if(length(intervalos_em_comum) == 0){
        'Não há espaço comum nas agendas para reunião hoje.'
      } else{
        # definindo a intersecao existente entre os intervalos em comum dos funcionarios
        horarios_em_comum <- intersecao_intervalos(intervalos_em_comum)
        # verificando se os intervalos disponiveis possuem espaço para a reuniao com a duracao informada
        horarios_validos <- verificando_intervalo_valido(horarios_em_comum, duracao_reuniao)
        # se nao ha, entao ha intervalos para a reuniao, mas nao na duracao informada
        if(length(horarios_validos) == 0){
          'Não há espaco comum nas agendas para uma reunião tão longa.'
        } else{
          # se sim, entao em mensagem final esta os horarios disponiveis para a reuniao em forma de string
          horarios_finais <- c()
          for(i in horarios_validos){horarios_finais <- c(horarios_finais, paste0(i, collapse = '-'))}
          
          mensagem_final <- paste0('Os horários disponíveis para a reunião são: ', paste0(horarios_finais, collapse = ', '))
          mensagem_final
        }
      }
    }
    
  }

  
}


verificando_horarios_disponiveis(c(), 
                                 c('08:05','21:30'), 
                                 list(c('09:00','12:30'), c('12:00','13:00'),c('16:00','18:00')), 
                                 list(), 
                                 5)
