from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, WebDriverException, NoSuchElementException
from bs4 import BeautifulSoup
from class_USP import UnidadeUSP, Curso, Disciplina
import time
import pickle

# Dicionário onde os dados de todas as unidades e cursos serão armazenados
todas_unidades = {}


# --------------------------------------------------------------------------------
# --------------------- FUNÇÕES DE CONFIGURAÇÃO E AUXILIARES ---------------------
# --------------------------------------------------------------------------------


# Configuração do driver do Selenium
def setup_driver():
    options = webdriver.ChromeOptions()
    options.binary_location = "/usr/bin/google-chrome"
    service = webdriver.chrome.service.Service(executable_path="./chromedriver")
    driver = webdriver.Chrome(service=service, options=options)
    return driver, WebDriverWait(driver, 5)

# Esperar a pagina carregar
def wait_full_load(wait):
    """Espera todos os elementos críticos carregarem"""
    try:
        # Espera o overlay desaparecer
        wait.until(EC.invisibility_of_element_located((By.CLASS_NAME, "blockUI")))

        # Espera o documento estar no estado "complete"
        wait.until(lambda d: d.execute_script("return document.readyState") == "complete")
        
        # Espera 0.5s extra para garantir
        time.sleep(0.5)
        
        return True
    except TimeoutException:
        print("Tempo limite excedido ao esperar carregamento")
        return False


# função para fechar o popup de erro 
def close_popup(driver, wait):
    """Função para lidar com o popup de erro"""
    try:     
        # Localiza o botão Fechar dentro do popup
        close_button = wait.until(EC.element_to_be_clickable((By.XPATH, ".//button[contains(., 'Fechar')]")))
        
        # Clica no botão usando JavaScript para evitar problemas de interceptação
        driver.execute_script("arguments[0].click();", close_button)
        print("Popup de erro fechado com sucesso")

        return True
        
    except TimeoutException:
        # Se o popup não aparecer, ou se o botão não for encontrado
        print("Nenhum popup de erro detectado dentro do tempo limite")
        return False

# Função para verificar se há algum overlay ativo
def overlay_ativo(driver):
    """Retorna True se qualquer overlay conhecido estiver visível."""
    for classe in ["ui-widget-overlay", "blockUI"]:
        try:
            overlay = driver.find_element(By.CLASS_NAME, classe)
            if overlay.is_displayed():
                return True
        except NoSuchElementException:
            continue
    return False

# Função para clicar com segurança em um elemento pelo ID, tenta clicar 3 vezes antes de retornar erro.
def safe_click(driver, wait, element_id, max_attempts=3):

    # Tenta clicar tres vezes, pois o site pode ter problemas de carregamento ou overlays
    for attempt in range(1, max_attempts + 1):
        try:
            # Verifica se há overlay na tela, se sim, espera desaparecer
            if overlay_ativo(driver):
                # print(f"[Tentativa {attempt}] Overlay ativo, aguardando desaparecer...")
                time.sleep(0.2)
                continue

            # Espera o elemento ser clicável e clica com JavaScript (para evitar interceptações)
            element = wait.until(EC.element_to_be_clickable((By.ID, element_id)))
            driver.execute_script("arguments[0].click();", element)
            return True

        except (TimeoutException, WebDriverException) as e:
            # print(f"[Tentativa {attempt}] Falha ao clicar em '{element_id}'")
            time.sleep(0.8)  # Espera um pouco antes de tentar de novo

    return False  # Significa que o overlay não desapareceu, logo o popup de erro esta ativo 



# ---------------------------------------------------------------------
# --------------------- FUNÇÕES PRINCIPAIS DO BOT ---------------------
# ---------------------------------------------------------------------



# Esta função é responsável por configurar o driver do Selenium e retornar o objeto driver e o WebDriverWait
def main():

    # Receber numero de unidades desejadas, se não for informado, carrega todas as unidades
    print("\n======== Bot de Coleta de Dados da USP ========\n")
    num_unidades = 0

    while (1):
        if (num_unidades := input("Digite o número de unidades desejadas: ").strip()):
            num_unidades = int(num_unidades)
            if num_unidades <= 0:
                print("Número inválido. Por favor, insira um número maior que 0.")
            else:
                break

    cont_unidades = 0

    # Configura o driver e o WebDriverWait
    driver, wait = setup_driver()

    try:

        # Abre a página inicial
        print("Iniciando a coleta de dados...")
        driver.get("https://uspdigital.usp.br/jupiterweb/jupCarreira.jsp?codmnu=8275")
        print("\nPágina inicial carregada")

        # Espera os combos carregarem
        wait_full_load(wait)
        # Conta o número de unidades disponíveis
        total_unidades = len(Select(driver.find_element(By.ID, "comboUnidade")).options)    

        for i in range(1, total_unidades):

            # Verifica se o número de unidades coletadas é igual ao número desejado
            if cont_unidades >= int(num_unidades):
                print(f"\n ======= Coletadas {cont_unidades} unidades com sucesso. =======")
                break

            # Espera carregar a pagina toda
            wait_full_load(wait)

            tent_uni = 0 # Tentativas de selecionar a unidade
            max_tent_uni = 3 # Número máximo de tentativas para selecionar a unidade

            # Tenta selecionar a unidade, com até 3 tentativas
            while tent_uni < max_tent_uni:
                try:
                    # Seleciona a unidade pelo índice
                    unidades = Select(driver.find_element(By.ID, "comboUnidade"))
                    unidades.select_by_index(i)
                    unidade_nome = unidades.first_selected_option.text.strip()
                    print(f"\n### Processando Unidade: {unidade_nome}")
                    cont_unidades += 1
                    break  # Sai do loop se der certo
                except:
                    tent_uni += 1
                    # print(f"[Tentativa: {tent_uni}] Aguardando carregamento Unidade...")
                    time.sleep(1)

            # Seleciona o combo de cursos
            cursos = Select(driver.find_element(By.ID, "comboCurso"))
            opcoes_validas = [opt for opt in cursos.options if opt.get_attribute("value").strip() != ""]

            # Interage sobre cada opção de curso valido
            for j, opcao in enumerate(opcoes_validas):
                
                tent_cur = 0 # Tentativas de selecionar o curso
                max_tent_cur = 3 # Número máximo de tentativas para selecionar o curso
                
                # Tenta selecionar o curso, com até 3 tentativas, por causa de problemas de carregamento ou overlays
                while tent_cur < max_tent_cur:
                    try:
                        # Seleciona o curso pelo texto visível
                        cursos = Select(driver.find_element(By.ID, "comboCurso"))
                        cursos.select_by_visible_text(opcao.text.strip())
                        curso_nome = cursos.first_selected_option.text.strip()
                        print(f"\n- Processando Curso: {curso_nome}")
                        break
                    except Exception as e:
                        tent_cur += 1
                        # print(f"[Tentativa {tent_cur}] Falha ao selecionar curso '{opcao.text}'")
                        time.sleep(1) # Espera um pouco antes de tentar de novo

                # Clica no botão Buscar
                safe_click(driver, wait, "enviar")
                   
                # Espera carregar a pagina de informações do curso
                wait_full_load(wait)

                # Navega para grade curricular
                if ( safe_click(driver, wait, "step4-tab") ):

                    try:
                        # Espera carregar a pagina toda
                        wait_full_load(wait)

                        # Processa a grade curricular

                        # 1. Espera o elemento de grade curricular estar presente
                        wait.until(lambda d: len(d.find_element(By.ID, "gradeCurricular").find_elements(By.TAG_NAME, "tr")) > 1)
                        grade = driver.find_element(By.ID, "gradeCurricular")

                        # 2. Verifica se a grade tem pelo menos uma linha de dados
                        soup = BeautifulSoup(driver.find_element(By.ID, "step4").get_attribute("innerHTML"), "html.parser")

                        # Coleta os dados de duração ideal, mínima e máxima
                        try:
                            duracao_ideal = int(soup.find("span", class_="duridlhab").text.strip())
                            duracao_minima = int(soup.find("span", class_="durminhab").text.strip())
                            duracao_maxima = int(soup.find("span", class_="durmaxhab").text.strip())
                        except Exception as e:
                            # Se não conseguir coletar as durações, define como 0
                            print(f"Erro ao coletar duração: {e}")
                            duracao_ideal = duracao_minima = duracao_maxima = 0

                        # Cria o curso com os dados agora corretamente preenchidos
                        unidade = UnidadeUSP(unidade_nome)
                        curso = Curso(curso_nome, unidade, duracao_ideal, duracao_minima, duracao_maxima)

                        # Coleta as linhas da tabela
                        linhas = soup.find_all("tr")[1:]

                        # pode ser "obrigatorias", "optativas_livres" ou "optativas_eletivas"
                        modo = "obrigatorias"  

                        # Itera sobre as linhas da tabela, coletando as disciplinas dos três modos
                        for linha in linhas:
                            colunas_td = linha.find_all("td")
                            if not colunas_td:
                                continue

                            # Verifica se a linha é um cabeçalho de seção
                            texto_cabecalho = colunas_td[0].get_text(strip=True)

                            # Verifica se mudou de seção
                            if "Disciplinas Optativas Livres" in texto_cabecalho:
                                modo = "optativas_livres"
                                continue
                            elif "Disciplinas Optativas Eletivas" in texto_cabecalho:
                                modo = "optativas_eletivas"
                                continue

                            # Coleta os dados da disciplina se houver ao menos 5 colunas (evita cabecalhos vazios)
                            if len(colunas_td) < 5 or not colunas_td[2].get_text(strip=True).isdigit():
                                continue

                            # Tenta extrair os dados da disciplina
                            try:
                                colunas = [td.get_text(strip=True) for td in colunas_td]

                                # Cria a disciplina com os dados extraídos
                                disciplina = Disciplina(
                                    codigo=colunas[0],
                                    nome=colunas[1],
                                    creditos_aula=int(colunas[2]),
                                    creditos_trabalho=int(colunas[3]),
                                    carga_horaria=int(colunas[4]) if colunas[4].isdigit() else 0,
                                    carga_horaria_estagio=int(colunas[5]) if len(colunas) > 5 and colunas[5].isdigit() else 0,
                                    carga_horaria_praticas=int(colunas[6]) if len(colunas) > 6 and colunas[6].isdigit() else 0,
                                    carga_horaria_teorico_praticas=int(colunas[7]) if len(colunas) > 7 and colunas[7].isdigit() else 0
                                )

                                # Adiciona a disciplina ao curso no modo correto
                                if modo == "obrigatorias":
                                    curso.adicionar_disciplina_obrigatoria(disciplina)
                                elif modo == "optativas_livres":
                                    curso.disciplinas_optativas_livres.append(disciplina)
                                elif modo == "optativas_eletivas":
                                    curso.disciplinas_optativas_eletivas.append(disciplina)

                            except Exception as e:
                                print(f"Erro ao processar disciplina: {e}")

                        # Exibe os dados do curso
                        print ("✓ Curso processado com sucesso!")  
                        print(f"\n{'─' * 70}")

                    except Exception as e:
                        print(f"Falha ao acessar grade curricular para {curso_nome}")


                    # Adiciona curso ao dicionário
                    if unidade_nome not in todas_unidades:
                        todas_unidades[unidade_nome] = []

                    todas_unidades[unidade_nome].append(curso)

                else:
                    # Trata popup de erro 
                    if not close_popup(driver, wait):
                        print("Nenhum popup para fechar ou erro ao fechar")

                # Volta para busca de uma nova unidade ou curso
                if not safe_click(driver, wait, "step1-tab"):
                    print("Recarregando página devido a falha ao voltar")
                    driver.get("https://uspdigital.usp.br/jupiterweb/jupCarreira.jsp?codmnu=8275")
                    wait.until(EC.presence_of_element_located((By.ID, "comboUnidade")))
                    Select(driver.find_element(By.ID, "comboUnidade")).select_by_index(i)
                    time.sleep(1)

        # Salva todos os dados em arquivo usando pickle
        with open("dados_usp.pkl", "wb") as f:
            pickle.dump(todas_unidades, f)


    except Exception:
        print("##### Erro geral no processamento dos dados. #####")
        exit()
 
    finally:
        driver.quit()

if __name__ == "__main__":
    main()