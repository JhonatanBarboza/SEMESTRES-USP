import os
import pickle
import subprocess
from class_USP import UnidadeUSP, Curso, Disciplina

# Caminho do arquivo com os dados serializados
ARQUIVO_DADOS = "dados_usp.pkl"
cont_tentativa = 1


# ---------------------------------------------------------------
# -------------- INICIALIZAÇÃO E COLETA DE DADOS ----------------
# ---------------------------------------------------------------


# inicializa os dados e carrega os cursos da USP
def inicializar_dados():
    global dados, todos_cursos, cont_tentativa
    # Carrega os dados
    subprocess.run(["python", "bot.py"])

    # Verifica se o arquivo de dados existe
    if not os.path.exists(ARQUIVO_DADOS) and cont_tentativa < 3:
        cont_tentativa += 1
        inicializar_dados()

    if cont_tentativa >= 3:
            print("Erro ao carregar os dados. Verifique a conexão com a internet e tente novamente.")
            exit(1)

    cont_tentativa = 1
    # Cria lista plana de cursos
    todos_cursos = []
    with open(ARQUIVO_DADOS, "rb") as f:
        dados = pickle.load(f)
    for cursos in dados.values():
        todos_cursos.extend(cursos)



# ---------------------------------------------------------------
# --------------------- FUNÇÕES DE CONSULTA ---------------------
# ---------------------------------------------------------------



# Função principal do menu
def menu():
    # Verifica se o arquivo de dados existe e o exclui se necessário
    global ARQUIVO_DADOS
    if os.path.exists(ARQUIVO_DADOS):
        os.remove(ARQUIVO_DADOS)
        
    # Inicializa os dados
    inicializar_dados()

    # exclui o arquivo de dados se existir
    if os.path.exists(ARQUIVO_DADOS):
        os.remove(ARQUIVO_DADOS)

    # Variáveis globais
    global dados, todos_cursos 

    # Menu de opções
    while True:

        # Exibe o menu de opções
        print(
            "\n================ MENU DE CONSULTA ================\n\n"
            "0. Consulta detalhada de cursos\n"
            "1. Listar cursos por unidade\n"
            "2. Consulta por nome do curso\n"
            "3. Exibir dados completos de uma unidade\n"
            "4. Consulta por disciplina\n"
            "5. Disciplinas que são usadas em mais de um curso\n"
            "6. Recarregar dados da web\n"
            "7. Sair\n\n"
            "==================================================\n"
        )

        escolha = input("\nEscolha uma opção: ")            

        # Verifica a escolha do usuário e chama a função correspondente
        if escolha == "0":
            consulta_detalhada()
        elif escolha == "1":
            listar_cursos_por_unidade()
        elif escolha == "2":
            consulta_por_nome_curso()
        elif escolha == "3":
            exibir_dados_unidade_completa()
        elif escolha == "4":
            consulta_por_disciplina()
        elif escolha == "5":
            disciplinas_multiplos_cursos()
        elif escolha == "6":
            inicializar_dados()
        elif escolha == "7":
            sair()
        else:
            print("\nOpção inválida. Tente novamente.")



# ---------------------------------------------------------------
# --------------- IMPLEMENTAÇÃO DAS CONSULTAS -------------------
# ---------------------------------------------------------------



# Opção 0, consulta detalhada de cursos
def consulta_detalhada():

    # Exibe as unidades disponíveis
    unidades = list(dados.keys())
    print("\nUnidades disponíveis:")
    for i, nome in enumerate(unidades):
        print(f"{i}. {nome}")

    # Permite ao usuário escolher uma unidade
    try:
        idx = int(input("\nEscolha o número da unidade: "))
        unidade_nome = unidades[idx]
        cursos = dados[unidade_nome]

        # Exibe os cursos da unidade escolhida
        print(f"\nCursos da unidade '{unidade_nome}':")
        for j, curso in enumerate(cursos):
            print(f"{j}. {curso.nome}")

        # Permite ao usuário escolher um curso
        curso_idx = int(input("\nEscolha o número do curso: "))
        curso = cursos[curso_idx]

        exibir_detalhes_curso(curso)

    except (ValueError, IndexError):
        print("\nEntrada inválida.")


# Opção 1, listar cursos por unidade
def listar_cursos_por_unidade():

    # Exibe as unidades disponíveis
    unidades = list(dados.keys())
    print("\nUnidades disponíveis:")
    for i, nome in enumerate(unidades):
        print(f"{i}. {nome}")

    # Permite ao usuário escolher uma unidade
    try:
        idx = int(input("\nEscolha o número da unidade: "))
        unidade_nome = unidades[idx]
        cursos = dados[unidade_nome]

        # Exibe os cursos da unidade escolhida
        print(f"\nCursos da unidade '{unidade_nome}':")
        for curso in cursos:
            print(f"- {curso.nome}")

    except (ValueError, IndexError):
        print("\nEntrada inválida.")


# Opção 2, consulta por nome do curso
def consulta_por_nome_curso():
    # Recebe o nome do curso e busca nos cursos disponíveis
    nome_busca = input("\nDigite o nome do curso (ou parte): ").lower()
    encontrados = [c for c in todos_cursos if nome_busca in c.nome.lower()]

    # Se não encontrar nenhum curso, informa ao usuário
    if not encontrados:
        print("\nCurso não encontrado.")
        return

    # Exibe os cursos encontrados
    print(f"\n{len(encontrados)} curso(s) encontrados:")
    for i, curso in enumerate(encontrados):
        print(f"{i}. {curso.nome} ({curso.unidade.nome})")

    try:
        # Permite ao usuário escolher um curso para ver os detalhes
        idx = int(input("\nEscolha o número do curso para ver os detalhes: "))
        exibir_detalhes_curso(encontrados[idx])
    except (ValueError, IndexError):
        print("\nEntrada inválida.")


# Opção 3, exibir dados completos de uma unidade
def exibir_dados_unidade_completa():
    # Exibe as unidades disponíveis
    unidades = sorted(dados.keys())
    print("\nUnidades disponíveis:")
    for idx, nome_unidade in enumerate(unidades):
        print(f"{idx + 1}. {nome_unidade}")

    try:
        # Permite ao usuário escolher uma unidade
        escolha = int(input("\nDigite o número da unidade desejada: "))
        if not (1 <= escolha <= len(unidades)):
            print("Escolha inválida.")
            return
    except ValueError:
        print("Entrada inválida.")
        return

    # Exibe os cursos da unidade escolhida
    unidade_escolhida = unidades[escolha - 1]
    cursos = dados.get(unidade_escolhida, [])

    print(f"\n--- Cursos da Unidade: {unidade_escolhida} ---")

    for curso in cursos:
        exibir_detalhes_curso(curso)

        print("\n" + "-" * 40)


# Opção 4, consulta por disciplina
def consulta_por_disciplina():
    # Recebe o nome da disciplina e busca nos cursos disponíveis
    nome_disciplina = input("\nDigite o nome da disciplina (ou parte): ").lower().strip()
    
    if not nome_disciplina:
        print("\nNome da disciplina não pode estar vazio.")
        return
    
    # Busca disciplinas que contenham o termo pesquisado
    disciplinas_encontradas = []
    
    for curso in todos_cursos:
        # Busca em disciplinas obrigatórias
        for disciplina in curso.disciplinas_obrigatorias:
            if nome_disciplina in disciplina.nome.lower():
                disciplinas_encontradas.append((disciplina, curso, "Obrigatória"))
        
        # Busca em disciplinas optativas livres
        for disciplina in curso.disciplinas_optativas_livres:
            if nome_disciplina in disciplina.nome.lower():
                disciplinas_encontradas.append((disciplina, curso, "Optativa Livre"))
        
        # Busca em disciplinas optativas eletivas
        for disciplina in curso.disciplinas_optativas_eletivas:
            if nome_disciplina in disciplina.nome.lower():
                disciplinas_encontradas.append((disciplina, curso, "Optativa Eletiva"))
    
    if not disciplinas_encontradas:
        print(f"\nNenhuma disciplina encontrada com o termo '{nome_disciplina}'.")
        return
    
    # Remove duplicatas baseando-se no código da disciplina
    disciplinas_unicas = {}
    for disciplina, curso, tipo in disciplinas_encontradas:
        codigo = disciplina.codigo
        if codigo not in disciplinas_unicas:
            disciplinas_unicas[codigo] = {
                'disciplina': disciplina,
                'cursos': []
            }
        disciplinas_unicas[codigo]['cursos'].append((curso, tipo))
    
    print(f"\n{len(disciplinas_unicas)} disciplina(s) encontrada(s):")
    
    # Lista as disciplinas encontradas para seleção
    disciplinas_lista = list(disciplinas_unicas.items())
    for i, (codigo, info) in enumerate(disciplinas_lista):
        disciplina = info['disciplina']
        print(f"{i}. {codigo} - {disciplina.nome}")
    
    try:
        if len(disciplinas_lista) == 1:
            # Se há apenas uma disciplina, exibe automaticamente
            escolha_idx = 0
        else:
            escolha_idx = int(input("\nEscolha o número da disciplina para ver os detalhes: "))
        
        if not (0 <= escolha_idx < len(disciplinas_lista)):
            print("\nEscolha inválida.")
            return
        
        codigo_escolhido, info_escolhida = disciplinas_lista[escolha_idx]
        disciplina_escolhida = info_escolhida['disciplina']
        cursos_disciplina = info_escolhida['cursos']
        
        # Exibe os detalhes da disciplina
        print(f"\n====== Detalhes da Disciplina ======")
        print(f"Código: {disciplina_escolhida.codigo}")
        print(f"Nome: {disciplina_escolhida.nome}")
        print(f"Créditos Aula: {disciplina_escolhida.creditos_aula}")
        print(f"Créditos Trabalho: {disciplina_escolhida.creditos_trabalho}")
        print(f"Total de Créditos: {disciplina_escolhida.creditos_aula + disciplina_escolhida.creditos_trabalho}")
        print(f"Carga Horária: {disciplina_escolhida.carga_horaria}h")
        
        # Exibe cargas horárias adicionais se existirem
        if disciplina_escolhida.carga_horaria_estagio > 0:
            print(f"Carga Horária de Estágio: {disciplina_escolhida.carga_horaria_estagio}h")
        if disciplina_escolhida.carga_horaria_praticas > 0:
            print(f"Carga Horária de Práticas: {disciplina_escolhida.carga_horaria_praticas}h")
        if disciplina_escolhida.carga_horaria_teorico_praticas > 0:
            print(f"Carga Horária Teórico-Práticas: {disciplina_escolhida.carga_horaria_teorico_praticas}h")
        
        # Agrupa cursos por unidade para melhor organização
        cursos_por_unidade = {}
        for curso, tipo in cursos_disciplina:
            unidade = curso.unidade.nome
            if unidade not in cursos_por_unidade:
                cursos_por_unidade[unidade] = []
            cursos_por_unidade[unidade].append((curso.nome, tipo))
        
        # Exibe os cursos que incluem a disciplina
        print(f"\n======= Cursos que incluem esta disciplina ========")
        for unidade, cursos_info in sorted(cursos_por_unidade.items()):
            print(f"\n{unidade}:")
            for nome_curso, tipo in cursos_info:
                print(f"  - {nome_curso} ({tipo})")
        
        print(f"\nTotal: {len(cursos_disciplina)} curso(s)")
        
    except (ValueError, IndexError):
        print("\nEntrada inválida.")


# Opção 5, disciplinas que são usadas em mais de um curso
def disciplinas_multiplos_cursos():
    # Cria um dicionário para contar quantas vezes cada disciplina aparece
    contagem_disciplinas = {}

    for curso in todos_cursos:
        for disciplina in curso.disciplinas_obrigatorias + curso.disciplinas_optativas_livres + curso.disciplinas_optativas_eletivas:
            if disciplina.codigo not in contagem_disciplinas:
                contagem_disciplinas[disciplina.codigo] = {
                    'disciplina': disciplina,
                    'cursos': []
                }
            contagem_disciplinas[disciplina.codigo]['cursos'].append(curso.nome)

    # Filtra as disciplinas que aparecem em mais de um curso
    disciplinas_repetidas = {k: v for k, v in contagem_disciplinas.items() if len(v['cursos']) > 1}

    if not disciplinas_repetidas:
        print("\nNenhuma disciplina encontrada que seja usada em mais de um curso.")
        return

    print(f"\n{len(disciplinas_repetidas)} disciplina(s) encontrada(s) usada(s) em mais de um curso:\n")
    for codigo, info in disciplinas_repetidas.items():
        disciplina = info['disciplina'] 
        cursos = info['cursos']
        print(f"\n{disciplina.nome} ({codigo}):\n")
        for curso in cursos:
            print(f"- {curso}")


# opção 7, sair do programa e excluir os dados carregados
def sair():
    print("Encerrando o programa...")
    # Excluir os dados carregados
    dados.clear()
    todos_cursos.clear()
    # Exclui o arquivo de dados
    if os.path.exists(ARQUIVO_DADOS):
        os.remove(ARQUIVO_DADOS)
    exit()


# Função auxiliar
def exibir_detalhes_curso(curso: Curso):
    print(f"\n================= Detalhes do Curso =================")
    print(f"Nome: {curso.nome}")
    print(f"Unidade: {curso.unidade.nome}")
    print(f"Duração ideal: {curso.duracao_ideal} semestres")
    print(f"Duração mínima: {curso.duracao_minima} semestres")
    print(f"Duração máxima: {curso.duracao_maxima} semestres")

    print(f"\nDisciplinas obrigatórias ({len(curso.disciplinas_obrigatorias)}):")
    for d in curso.disciplinas_obrigatorias:
        print(f"- {d.codigo} - {d.nome}")

    print(f"\nDisciplinas optativas livres ({len(curso.disciplinas_optativas_livres)}):")
    for d in curso.disciplinas_optativas_livres:
        print(f"- {d.codigo} - {d.nome}")

    print(f"\nDisciplinas optativas eletivas ({len(curso.disciplinas_optativas_eletivas)}):")
    for d in curso.disciplinas_optativas_eletivas:
        print(f"- {d.codigo} - {d.nome}")

# Executa o menu
if __name__ == "__main__":
    menu()