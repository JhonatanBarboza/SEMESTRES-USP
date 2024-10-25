from transformers import RobertaTokenizer, RobertaForTokenClassification
from transformers import Trainer, TrainingArguments

# Carregar o tokenizer e o modelo pré-treinado para NER
tokenizer = RobertaTokenizer.from_pretrained('roberta-base')
model = RobertaForTokenClassification.from_pretrained('roberta-base', num_labels=5)  # 5 entidades como Nome, RG, Curso, etc.

# Exemplo de texto e rótulos NER
texts = ["Jhonatan Barboza da Silva, RG 140402419, código USP 15645049..."]
labels = [[0, 0, 0, 1, 1, 0, 0]]  # 1 = Nome, 2 = RG, 3 = Código USP, etc.

# Tokenizar o texto
encodings = tokenizer(texts, truncation=True, padding=True, max_length=512, return_offsets_mapping=True)

# Dataset de NER
class NERDataset(torch.utils.data.Dataset):
    def __init__(self, encodings, labels):
        self.encodings = encodings
        self.labels = labels

    def __getitem__(self, idx):
        item = {key: torch.tensor(val[idx]) for key, val in self.encodings.items()}
        item['labels'] = torch.tensor(self.labels[idx])
        return item

    def __len__(self):
        return len(self.labels)

dataset = NERDataset(encodings, labels)

# Parâmetros de treinamento
training_args = TrainingArguments(
    output_dir='./results',
    per_device_train_batch_size=4,
    num_train_epochs=3,
    logging_dir='./logs',
)

# Treinador para NER
trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=dataset,
)

# Treinar o modelo
trainer.train()
