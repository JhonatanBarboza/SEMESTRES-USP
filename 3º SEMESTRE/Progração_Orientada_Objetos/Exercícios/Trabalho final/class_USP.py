
# ----------------------------------------------------------------------------------------------
#  Definir classes para representar um curso e suas disciplinas associadas a uma unidade da USP.
# ----------------------------------------------------------------------------------------------

# Classe Disciplina representa uma disciplina com seus atributos e métodos.
class Disciplina:
    def __init__(self, codigo: str, nome: str, creditos_aula: int, creditos_trabalho: int,
                 carga_horaria: int, carga_horaria_estagio: int = 0,
                 carga_horaria_praticas: int = 0, carga_horaria_teorico_praticas: int = 0):
        self.codigo = codigo
        self.nome = nome
        self.creditos_aula = creditos_aula
        self.creditos_trabalho = creditos_trabalho
        self.carga_horaria = carga_horaria
        self.carga_horaria_estagio = carga_horaria_estagio
        self.carga_horaria_praticas = carga_horaria_praticas
        self.carga_horaria_teorico_praticas = carga_horaria_teorico_praticas

    def __str__(self):
        return f"{self.codigo} - {self.nome} ({self.creditos_aula + self.creditos_trabalho} créditos)"

# Classe Curso representa um curso na USP, contendo informações sobre a unidade, duração e disciplinas.
class Curso:
    def __init__(self, nome: str, unidade: 'UnidadeUSP', duracao_ideal: int,
                 duracao_minima: int, duracao_maxima: int):
        self.nome = nome
        self.unidade = unidade
        self.duracao_ideal = duracao_ideal
        self.duracao_minima = duracao_minima
        self.duracao_maxima = duracao_maxima
        self.disciplinas_obrigatorias = []
        self.disciplinas_optativas_livres = []
        self.disciplinas_optativas_eletivas = []
        
        # Adiciona este curso à lista de cursos da unidade
        self.unidade.adicionar_curso(self)

    def adicionar_disciplina_obrigatoria(self, disciplina: Disciplina):
        self.disciplinas_obrigatorias.append(disciplina)

    def adicionar_optativa_livre(self, disciplina: Disciplina):
        self.disciplinas_optativas_livres.append(disciplina)

    def adicionar_optativa_eletiva(self, disciplina: Disciplina):
        self.disciplinas_optativas_eletivas.append(disciplina)

    def __str__(self):
        return f"{self.nome} ({self.unidade.nome})"

# Classe UnidadeUSP representa uma unidade da USP, contendo informações sobre o nome e os cursos associados.
class UnidadeUSP:
    def __init__(self, nome: str):
        self.nome = nome
        self.cursos = []

    def adicionar_curso(self, curso: Curso):
        if curso not in self.cursos:
            self.cursos.append(curso)

    def listar_cursos(self):
        return [curso.nome for curso in self.cursos]

    def __str__(self):
        return f"Unidade USP: {self.nome} ({len(self.cursos)} cursos)"