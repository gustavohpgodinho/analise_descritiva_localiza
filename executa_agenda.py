
def validando_agenda(agenda, hr_trabalho):
    # deixando na agenda somente os compromissos que comecam entre o horario de entrada 
    # e terminam ate o horario de saida do funcionario
    agenda_valida = []
    for item in agenda:
        if (item[0] >= hr_trabalho[0]) & (item[1] <= hr_trabalho[1]):

            agenda_valida.append(item)

    return tuple(agenda_valida)

def checando_horarios_livres(agenda, hr_min, hr_max):

    # verificando os horarios livres do funcionario
    horarios_livres = []
    ref_hora = hr_min

    for item in agenda:
        if item[0] <= ref_hora:
            ref_hora = item[1]
            next
        else:
            horarios_livres.append((ref_hora, item[0]))
            ref_hora = item[1]
     
    if ref_hora < hr_max:
        horarios_livres.append((ref_hora, hr_max))

    return horarios_livres

def ind_combinando_intervalos_em_comum(intervalo_a, intervalo_b):

    # essa funcoo cria apenas uma indicacao se os horarios possuem 
    # intersecao ou nao. Retorna TRUE ou FALSE
    condicao1 = (intervalo_a[0] >= intervalo_b[0]) & (intervalo_a[0] < intervalo_b[1])
    condicao2 = (intervalo_b[0] >= intervalo_a[0]) & (intervalo_b[0] < intervalo_a[1])
    if condicao1|condicao2:
        return True
    else:
        return False

def combinando_intervalos_em_comum(intervalo_a, intervalo_b):

    # dado dois intervalos, essa funcao verifica se ha alguma 
    # intersecao, mesmo que minima entre eles
    intervalos_em_comum = []
    if (len(intervalo_a) > 0) & (len(intervalo_b) > 0):

        for item1 in intervalo_a:
            for item2 in intervalo_b:

                if ind_combinando_intervalos_em_comum(item1, item2):
                    intervalos_em_comum.append((item1, item2))
                else:
                    next

    return intervalos_em_comum

def define_intersecao_intervalos(intervalo_a, intervalo_b):

    # define a intersecao existente entre os intervalos
    if ind_combinando_intervalos_em_comum(intervalo_a, intervalo_b) == True:
        return (max(intervalo_a[0], intervalo_b[0]), min(intervalo_a[1], intervalo_b[1]))
    else:
        return (None, None)

def intersecao_intervalos(intervalos):
  # cria a lista em os intervalos livres em comum dos funcionarios    
  intervalos_disponiveis = []
  for item in intervalos:
      intervalos_disponiveis.append(define_intersecao_intervalos(item[0], item[1]))

  return intervalos_disponiveis

def transforma_horario(horario):

    # transformando os horarios em minutos, para conseguir calcular a diferenca entre eles
    tmin = 60 * int(horario[0] + horario[1]) + int(horario[-2] + horario[-1])
    return tmin

def verificando_intervalo_valido(intervalos, duracao):

    #verificando se os intervalos disponiveis possuem a duracao necessario
    horarios_validos = []
    for item in intervalos:

        dif_tempo = transforma_horario(item[1]) - transforma_horario(item[0])
        if dif_tempo >= duracao:
            horarios_validos.append(item)

    return horarios_validos

def verificando_horarios_disponiveis(horario_trabalho_pessoa1, horario_trabalho_pessoa2, agenda_pessoa1, agenda_pessoa2, duracao_reuniao):

    if (len(horario_trabalho_pessoa1) == 0)|(len(horario_trabalho_pessoa1) == 0):
        return "É preciso informar os horários de trabalho dos 2 funcionários."   
    
    else:
        # definindo o intervalo em comum que os funcionarios trabalham
        min_hora_reuniao = define_intersecao_intervalos(horario_trabalho_pessoa1, horario_trabalho_pessoa2)[0]
        max_hora_reuniao = define_intersecao_intervalos(horario_trabalho_pessoa1, horario_trabalho_pessoa2)[1]
        
        # validando a agenda dos funcionarios, excluindo compromissos fora do trabalho de trabalho
        agenda_pessoa1 = validando_agenda(agenda_pessoa1, horario_trabalho_pessoa1)
        agenda_pessoa2 = validando_agenda(agenda_pessoa2, horario_trabalho_pessoa2)

        # se nao existe intervalo em comum entao nao ha como marcar a reuniao
        if min_hora_reuniao is None:
            return 'Os horários de trabalho não coincidem.'
        else:
            # se existe, precisamos definir os horarios livres dos dois funcionarios
            intervalos_livres_pessoa1 = checando_horarios_livres(agenda_pessoa1, min_hora_reuniao, max_hora_reuniao)
            intervalos_livres_pessoa2 = checando_horarios_livres(agenda_pessoa2, min_hora_reuniao, max_hora_reuniao)

            # combinando os intervalos livres dos dois funcionarios que possua a minima intersecao
            intervalos_em_comum = combinando_intervalos_em_comum(intervalos_livres_pessoa1, intervalos_livres_pessoa2)  

            # se nao ha intervalos em comum, entao nao ha possibilidade para marcacao da reuniao
            if len(intervalos_em_comum) == 0:
                return 'Não há espaço comum nas agendas para reunião hoje.'
            else:
                # definindo a intersecao existente entre os intervalos em comum dos funcionarios
                horarios_em_comum = intersecao_intervalos(intervalos_em_comum)
                # verificando se os intervalos disponiveis possuem espaço para a reuniao com a duracao informada
                horarios_validos = verificando_intervalo_valido(horarios_em_comum, duracao_reuniao)

                # se nao ha, entao ha intervalos para a reuniao, mas nao na duracao informada
                if len(horarios_validos) == 0:
                    return 'Não há espaco comum nas agendas para uma reunião tão longa.'
                else:
                    # se sim, entao em mensagem final esta os horarios disponiveis para a reuniao em forma de string
                    horarios_finais = [i[0] + '-' + i[1] for i in horarios_validos]

                    mensagem_final = 'Os horários disponíveis para a reunião são: '
                    for item in horarios_finais:
                        mensagem_final += item + '; '

                    return mensagem_final


print(verificando_horarios_disponiveis(('10:00','21:00'), ('08:05','21:30'), (('09:00','11:30'), ('12:00','13:00'),('16:00','18:00')), (),10))