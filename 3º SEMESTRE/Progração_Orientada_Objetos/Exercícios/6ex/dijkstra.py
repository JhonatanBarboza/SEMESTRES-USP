import heapq  # Importa o módulo heapq para trabalhar com fila de prioridade (heap)

def dijkstra(grafo, inicio):
    """
    Implementação do algoritmo de Dijkstra para encontrar os caminhos mais curtos
    a partir de um vértice inicial em um grafo com pesos não-negativos.
    
    Args:
        grafo (dict): Dicionário representando o grafo {vértice: {vizinho: peso}}
        inicio (str): Vértice de partida para o cálculo dos caminhos
        
    Returns:
        tuple: (distancias, anteriores) onde:
            - distancias: dicionário com as distâncias mínimas do início
            - anteriores: dicionário com os predecessores no caminho mínimo
    """
    
    # Inicializa todas as distâncias como infinito
    distancias = {vertice: float('infinity') for vertice in grafo}
    distancias[inicio] = 0  # A distância do início para ele mesmo é zero
    
    # Dicionário para armazenar os vértices anteriores no caminho mínimo
    anteriores = {vertice: None for vertice in grafo}
    
    # Fila de prioridade (heap) inicializada com o vértice de partida (distância 0)
    fila = [(0, inicio)]
    
    while fila:
        # Remove o vértice com menor distância atual da fila
        distancia_atual, vertice_atual = heapq.heappop(fila)
        
        # Se encontrarmos uma distância maior que a já registrada, pulamos
        if distancia_atual > distancias[vertice_atual]:
            continue
            
        # Para cada vizinho do vértice atual
        for vizinho, peso in grafo[vertice_atual].items():
            distancia = distancia_atual + peso  # Calcula nova distância
            
            # Se encontrou um caminho mais curto para o vizinho
            if distancia < distancias[vizinho]:
                distancias[vizinho] = distancia  # Atualiza a distância
                anteriores[vizinho] = vertice_atual  # Atualiza o predecessor
                heapq.heappush(fila, (distancia, vizinho))  # Adiciona na fila
    
    return distancias, anteriores

def construir_caminho(anteriores, destino):
    """
    Reconstrói o caminho mais curto do início até o destino usando
    o dicionário de predecessores.
    
    Args:
        anteriores (dict): Dicionário de predecessores do Dijkstra
        destino (str): Vértice final do caminho
        
    Returns:
        list: Lista ordenada do caminho (do início ao destino)
    """
    caminho = []
    atual = destino  # Começa pelo destino
    
    # Retrocede pelos predecessores até chegar no início (onde anterior é None)
    while atual is not None:
        caminho.append(atual)
        atual = anteriores[atual]
    
    # Inverte o caminho para ficar na ordem início->destino
    caminho.reverse()
    return caminho

def ler_grafo():
    """
    Lê a representação de um grafo da entrada padrão (stdin).
    
    Formato esperado:
    - Linha com nome do vértice
    - Linhas subsequentes com arestas no formato "destino peso"
    - Blocos separados por linhas vazias
    
    Returns:
        dict: Grafo no formato {vértice: {vizinho: peso}}
    """
    grafo = {}
    vertice_atual = None  # Controla o vértice atual sendo processado

    while True:
        try:
            linha = input().strip()
            if not linha:  # Linha vazia separa os blocos
                vertice_atual = None
                continue

            # Se não tem espaços, é um novo vértice
            if ' ' not in linha and '\t' not in linha:
                vertice_atual = linha
                grafo[vertice_atual] = {}  # Inicializa vizinhos
            else:  # É uma aresta (destino e peso)
                # Divide a linha no último espaço (para lidar com nomes com espaços)
                destino, peso = linha.rsplit(maxsplit=1)
                grafo[vertice_atual][destino] = int(peso)  # Adiciona aresta

        except EOFError:  # Termina quando não há mais entrada
            break

    return grafo

def main():
    """
    Função principal que coordena a execução do programa:
    1. Lê o grafo da entrada padrão
    2. Para cada cidade como origem:
       - Calcula caminhos mais curtos para todas as outras
       - Imprime as distâncias e caminhos
    3. Formata a saída
    """
    
    # Lê o grafo da entrada padrão
    grafo = ler_grafo()
    cidades = list(grafo.keys())  # Lista de todas as cidades
    
    # Para cada cidade como ponto de partida
    for cidade_inicio in cidades:
        # Calcula distâncias e predecessores usando Dijkstra
        distancias, anteriores = dijkstra(grafo, cidade_inicio)
        
        # Para cada cidade de destino
        for cidade_destino in cidades:
            if cidade_inicio == cidade_destino:  # Pula a mesma cidade
                continue
                
            # Constrói o caminho mais curto
            caminho = construir_caminho(anteriores, cidade_destino)
            
            # Imprime os resultados formatados
            print(f"{cidade_inicio} para {cidade_destino}")
            print(f"\tDistancia: {distancias[cidade_destino]:},0")
            print("\tCaminho:  -->", " --> ".join(caminho[1:]))  # Omite o início
        
        # Separador entre diferentes cidades de origem
        if cidade_inicio != cidades[-1]:
            print("-" * 45)
    print("-" * 45)  # Separador final

if __name__ == "__main__":
    main()  # Executa o programa quando chamado diretamente