
# Função para criar estrelas
def criar_estrelas(num_estrelas):
    estrelas = []
    if NUM_ESTRELAS > 0:
        x = ESPACO_VIRTUAL_LARGURA / 2
        y = ESPACO_VIRTUAL_ALTURA / 2
        massa = 2000000000
        raio = calcular_raio(massa)
        estrelas.append(Estrela(x, y, 0, 0, massa, raio))

    if NUM_ESTRELAS > 1:
        x = ESPACO_VIRTUAL_LARGURA / 2 - 30000
        y = ESPACO_VIRTUAL_ALTURA / 2 + 30000
        massa = 100000
        raio = calcular_raio(massa) * 20
        estrelas.append(Estrela(x, y, 300, 300, massa, raio))

    if NUM_ESTRELAS > 2:
        x = ESPACO_VIRTUAL_LARGURA / 2 + 30000
        y = ESPACO_VIRTUAL_ALTURA / 2 - 30000
        massa = 100000
        raio = calcular_raio(massa) * 20
        estrelas.append(Estrela(x, y, -300, -300, massa, raio))


    if NUM_ESTRELAS > 3:
        for _ in range(num_estrelas-3):
            x = ESPACO_VIRTUAL_LARGURA / 2 + random.randint(-100000, 100000)
            y = ESPACO_VIRTUAL_ALTURA / 2 + random.randint(-100000, 100000)
            massa = random.randint(100000, 100000000)
            raio = calcular_raio(massa)
            estrelas.append(Estrela(x, y, random.randint(-300, 300), random.randint(-300, 300), massa, raio))

    return estrelas
