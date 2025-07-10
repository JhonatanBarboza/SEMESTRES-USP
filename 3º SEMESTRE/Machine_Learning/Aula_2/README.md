# Machine Learning Notes: Margin Boundary and Optimization

## Margin Boundary

Na aula anterior, estudamos o algoritmo Perceptron, que funciona corretamente quando consegue dividir os dados linearmente. Porém, para garantir boa generalização, queremos que nossa **decision boundary** tenha uma margem de segurança simétrica em torno dela.

**Dois conceitos importantes:**
1. **Regularização**: Favorecer margin boundary o mais longe possível da decision boundary
2. **Loss**: Quantos exemplos ficariam dentro da margem ao afastá-la da decision boundary

A distância (d) entre a decision boundary e a margin boundary é dada por:

$$ d = \frac{1}{\|\theta\|} $$

## Função Gamma

A função gamma nos permite avaliar:
- Se um ponto foi classificado corretamente
- Se está dentro ou fora da margem

$$ \gamma(\theta,\theta_0) = \frac{y_i(\vec{\theta} \cdot \vec{x_i} + \theta_0)}{\|\vec{\theta}\|} $$

Interpretação:
- $$\(\gamma > 0\)$$: Exemplo corretamente classificado
- $$\(\gamma < 0\)$$: Exemplo classificado errado
- $$\(|\gamma| > \frac{1}{\|\theta\|}\)$$: Exemplo fora da margem
- $$\(0 < |\gamma| < \frac{1}{\|\theta\|}\)$$: Exemplo dentro da margem

## Função Loss (Hinge Loss)

Desenvolvemos o Perceptron para centralizar melhor a reta de decisão. A Hinge Loss é definida como:

$$ \text{Loss}_h(z) = \begin{cases} 0 & \text{se } z \geq 1 \\ 1 - z & \text{se } z < 1 \end{cases} $$
onde $$\( z = y_i(\vec{\theta} \cdot \vec{x_i} + \theta_0) \)$$

## Problema de Otimização

Minimizar apenas a loss não é suficiente (levaria ao Perceptron simples). Queremos:
1. Minimizar a loss
2. Maximizar a margem ($$\(\frac{1}{\|\theta\|}\)$$)

Que equivale a:

$$ \text{min}(Loss) \text{ e } \text{min}(\frac{1}{2}\|\theta\|^2) $$

A função objetivo combinada (com regularização L2):

$$ J(\theta,\theta_0) = \frac{1}{n}\sum_{i=1}^n \text{Loss}_h(y_i(\vec{\theta}\cdot\vec{x_i}+\theta_0)) + \frac{\lambda}{2}\|\theta\|^2 $$

## O Parâmetro Lambda (λ)

λ controla o trade-off entre margem e acurácia:
- **λ alto**: Margens maiores (melhor generalização)
- **λ baixo**: Maior complexidade (risco de overfitting)

O valor ideal $$\( C^* = \frac{1}{\lambda^*} \)$$ encontra o equilíbrio entre underfitting e overfitting.

## Gradiente Descendente

Para minimizar a função objetivo, usamos o gradiente descendente. O gradiente da nossa função é:

$$ \nabla_\theta J = \begin{cases} \lambda\theta & \text{se Loss} = 0 \\ -y_i\vec{x_i} + \lambda\theta & \text{se Loss} > 0 \end{cases} $$

$$ \nabla_{\theta_0} J = \begin{cases} 0 & \text{se Loss} = 0 \\ -y_i & \text{se Loss} > 0 \end{cases} $$

### Learning Rate (η)

Controla o tamanho dos passos na otimização. Começa alto e diminui com o tempo para evitar oscilações.

## Algoritmo SGD (Stochastic Gradient Descent)

```
1. θ = valor aleatório; θ₀ = valor aleatório
2. for t in range(T):
3.     sortear i em {x₁, x₂, ..., xₙ}
4.     if Loss > 0:
5.         θ = θ + η(xᵢyᵢ) - η(λθ)
6.         θ₀ = θ₀ + ηyᵢ
7.     else:
8.         θ = θ - η(λθ)
```

Este algoritmo combina:
1. Atualização baseada nos erros (quando Loss > 0)
2. Regularização (termo λθ) em todos os passos

## Visualização do Processo

```python
# Exemplo simplificado de implementação
def sgd(X, y, T=1000, eta=0.1, lambda_=0.1):
    theta = np.random.randn(X.shape[1])
    theta0 = 0
    for t in range(T):
        i = np.random.randint(len(X))
        xi, yi = X[i], y[i]
        z = yi * (np.dot(theta, xi) + theta0)
        if z < 1:  # Hinge Loss > 0
            theta = theta + eta * (xi * yi) - eta * lambda_ * theta
            theta0 = theta0 + eta * yi
        else:
            theta = theta - eta * lambda_ * theta
    return theta, theta0
```

Esta abordagem nos dá um modelo que:
1. Classifica bem os dados de treinamento
2. Mantém boa margem para generalização
3. É robusto a novos dados nunca vistos