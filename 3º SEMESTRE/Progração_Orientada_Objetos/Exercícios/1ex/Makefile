# Compilador Java
JAVAC = javac

# Diretório de origem (.java)
SRC_DIR = src

# Diretório de saída (.class)
BIN_DIR = bin

# Classe principal (contém o método main)
MAIN_CLASS = Main

# Lista todos os arquivos .java em src/
SRCS = $(wildcard $(SRC_DIR)/*.java)

# Regra padrão (executa quando você digita apenas 'make')
all: compile run

# Compila os arquivos .java para .class
compile:
	@mkdir -p $(BIN_DIR)
	$(JAVAC) -d $(BIN_DIR) -cp $(SRC_DIR) $(SRCS)

# Executa o programa
run:
	java -cp $(BIN_DIR) $(MAIN_CLASS)

# Limpa os arquivos compilados
clean:
	@rm -rf $(BIN_DIR)

.PHONY: all compile run clean