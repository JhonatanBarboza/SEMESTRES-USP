import PyPDF2

# Função para extrair texto do PDF
def extrair_texto_pdf(caminho_pdf):
    with open(caminho_pdf, "rb") as file:
        reader = PyPDF2.PdfReader(file)
        texto = ""
        for page in reader.pages:
            texto += page.extract_text()
        return texto

# Caminho para o arquivo PDF
caminho_pdf = "/mnt/data/doc.pdf"  # Ajustar para o caminho correto

# Extrair o texto do PDF
documento_extraido = extrair_texto_pdf(caminho_pdf)

# Imprimir o texto extraído do PDF
print("\nTexto extraído do PDF:\n")
print(documento_extraido)

# Frase que queremos verificar no documento extraído
frase_verificar = """
ATESTO, atendendo a requerimento do interessado, que Jhonatan Barboza da Silva, RG 140402419, código USP 15645049, é aluno do curso de Bacharelado em Ciências de Computação, desta Unidade.
"""

# Verificar se a frase está presente no texto extraído do PDF
if frase_verificar.strip() in documento_extraido:
    print("\n\nA frase está presente no documento.\n\n")
else:
    print("\n\nA frase não foi encontrada no documento.\n\n")
