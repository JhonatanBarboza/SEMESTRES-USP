import PyPDF2
from transformers import RobertaTokenizer, RobertaModel
import torch
from torch.nn import functional as F

# Função para extrair texto do PDF
def extrair_texto_pdf(caminho_pdf):
    with open(caminho_pdf, "rb") as file:
        reader = PyPDF2.PdfReader(file)
        texto = ""
        for page in reader.pages:
            texto += page.extract_text()
        return texto

# Caminho para o arquivo PDF
caminho_pdf = "/home/jhonatan/Documentos/hack/doc2.pdf"  # Certifique-se de ajustar o caminho correto se necessário

# Extrair o texto do PDF
documento_extraido = extrair_texto_pdf(caminho_pdf)

# Documento modelo
documento_original = """
ATESTO, atendendo a requerimento do interessado, que xxxxxxxxxx xxxxxxxxx, RG 111111111, código USP 11111111, é aluno do curso de Bacharelado em xxxxxxxxxxxx xxxxxxxxxxxx, desta Unidade.
"""

# Carregar o modelo RoBERTa pré-treinado e o tokenizer
tokenizer = RobertaTokenizer.from_pretrained('roberta-base')
model = RobertaModel.from_pretrained('roberta-base')

# Tokenizar o documento original e o documento extraído
inputs_original = tokenizer(documento_original, return_tensors="pt", truncation=True, padding=True, max_length=512)
inputs_extraido = tokenizer(documento_extraido, return_tensors="pt", truncation=True, padding=True, max_length=512)

# Fazer inferência nos dois documentos
with torch.no_grad():
    outputs_original = model(**inputs_original)
    outputs_extraido = model(**inputs_extraido)

# Função para calcular a similaridade coseno entre dois vetores
def similaridade_coseno(vetor_a, vetor_b):
    return F.cosine_similarity(vetor_a.mean(dim=1), vetor_b.mean(dim=1))

# Calcular a similaridade coseno entre os dois documentos
similaridade = similaridade_coseno(outputs_original.last_hidden_state, outputs_extraido.last_hidden_state)

# Definir um limiar de similaridade para decidir se são "iguais"
limiar = 0.90
if similaridade.item() > limiar:
    print("\n\nDocumento valido.\n\n")
else:
    print("\n\nDocumento invalido.\n\n")
