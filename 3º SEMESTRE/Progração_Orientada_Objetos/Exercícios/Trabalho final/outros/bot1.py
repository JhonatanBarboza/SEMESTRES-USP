from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException, TimeoutException
from selenium.common.exceptions import TimeoutException, WebDriverException, NoSuchElementException
from selenium.webdriver.common.action_chains import ActionChains
from bs4 import BeautifulSoup
from class_USP import UnidadeUSP, Curso, Disciplina
import time
import pickle

# Dicionário onde os dados de todas as unidades e cursos serão armazenados
todas_unidades = {}

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
        # 1. Espera o overlay desaparecer
        wait.until(EC.invisibility_of_element_located((By.CLASS_NAME, "blockUI")))
        
        # 2. Espera o documento estar no estado "complete"
        wait.until(lambda d: d.execute_script("return document.readyState") == "complete")
        
        # 3. Espera 0.5s extra para garantir
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
        print("Nenhum popup de erro detectado dentro do tempo limite")
        return False


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


def safe_click(driver, wait, element_id, max_attempts=3):
    """Tenta clicar com segurança em um elemento pelo ID. Retorna True se conseguir, False caso contrário."""
    for attempt in range(1, max_attempts + 1):
        try:
            # Verifica se há overlay na tela
            if overlay_ativo(driver):
                print(f"[Tentativa {attempt}] Overlay ativo, aguardando desaparecer...")
                time.sleep(0.2)
                continue

            # Espera o elemento ser clicável e clica com JavaScript (para evitar interceptações)
            element = wait.until(EC.element_to_be_clickable((By.ID, element_id)))
            driver.execute_script("arguments[0].click();", element)
            return True

        except (TimeoutException, WebDriverException) as e:
            print(f"[Tentativa {attempt}] Falha ao clicar em '{element_id}'")
            time.sleep(0.8)  # Espera um pouco antes de tentar de novo

    return False  # Nenhuma tentativa funcionou


def main():
    driver, wait = setup_driver()
    
    try:
        driver.get("https://uspdigital.usp.br/jupiterweb/jupCarreira.jsp?codmnu=8275")
        print("Página inicial carregada")

        # Espera os combos carregarem
        wait_full_load(wait)

        total_unidades = len(Select(driver.find_element(By.ID, "comboUnidade")).options)

        for i in range(1, total_unidades):

            # Espera carregar a pagina toda
            wait_full_load(wait)

            tent_uni = 0
            max_tent_uni = 3

            while tent_uni < max_tent_uni:
                try:
                    unidades = Select(driver.find_element(By.ID, "comboUnidade"))
                    unidades.select_by_index(i)
                    unidade_nome = unidades.first_selected_option.text.strip()
                    print(f"\n### Processando Unidade: {unidade_nome}")
                    break  # Sai do loop se der certo
                except:
                    tent_uni += 1
                    print(f"[Tentativa: {tent_uni}] Aguardando carregamento Unidade...")
                    time.sleep(1)



            cursos = Select(driver.find_element(By.ID, "comboCurso"))
            opcoes_validas = [opt for opt in cursos.options if opt.get_attribute("value").strip() != ""]

            for j, opcao in enumerate(opcoes_validas):
                tent_cur = 0
                max_tent_cur = 3
                while tent_cur < max_tent_cur:
                    try:
                        # Precisa recarregar o select em cada tentativa (DOM pode mudar)
                        cursos = Select(driver.find_element(By.ID, "comboCurso"))
                        cursos.select_by_visible_text(opcao.text.strip())
                        curso_nome = cursos.first_selected_option.text.strip()
                        print(f"\n--- Processando Curso: {curso_nome}")
                        break
                    except Exception as e:
                        tent_cur += 1
                        print(f"[Tentativa {tent_cur}] Falha ao selecionar curso '{opcao.text}'")
                        time.sleep(1)

                # Clica em Buscar
                safe_click(driver, wait, "enviar")
                   
                # Espera carregar a pagina
                wait_full_load(wait)

                # Navega para grade curricular
                if ( safe_click(driver, wait, "step4-tab") ): # Clica na grade curricular

                    try:
                        # Espera carregar a pagina toda
                        wait_full_load(wait)

                        # Processa a grade curricular
                        wait.until(lambda d: len(d.find_element(By.ID, "gradeCurricular").find_elements(By.TAG_NAME, "tr")) > 1)
                        grade = driver.find_element(By.ID, "gradeCurricular")

                        unidade = UnidadeUSP(unidade_nome)
                        curso = Curso(curso_nome, unidade, duracao_ideal=0, duracao_minima=0, duracao_maxima=0)

                        soup = BeautifulSoup(grade.get_attribute("innerHTML"), "html.parser")
                        disciplinas_processadas = 0

                        for linha in soup.find_all("tr")[1:]:
                            try:
                                colunas = [td.get_text(strip=True) for td in linha.find_all("td")]
                                if len(colunas) < 5 or not colunas[2].isdigit():
                                    continue

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
                                curso.adicionar_disciplina_obrigatoria(disciplina)
                                disciplinas_processadas += 1

                            except Exception as e:
                                print(f"Erro ao processar linha: {str(e)}")

                        print(f"Disciplinas processadas: {disciplinas_processadas}")

                    except Exception as e:
                        print(f"Falha ao acessar grade curricular para {curso_nome}")


                    # Adiciona curso ao dicionário
                    if unidade_nome not in todas_unidades:
                        todas_unidades[unidade_nome] = []

                    todas_unidades[unidade_nome].append(curso)

                    
        
                else:
                    # Trata popup de erro se aparecer
                    if not close_popup(driver, wait):
                        print("Nenhum popup para fechar ou erro ao fechar")

                # Volta para busca
                if not safe_click(driver, wait, "step1-tab"):
                    print("Recarregando página devido a falha ao voltar")
                    driver.get("https://uspdigital.usp.br/jupiterweb/jupCarreira.jsp?codmnu=8275")
                    wait.until(EC.presence_of_element_located((By.ID, "comboUnidade")))
                    Select(driver.find_element(By.ID, "comboUnidade")).select_by_index(i)
                    time.sleep(1)


        # Salva todos os dados em arquivo usando pickle
        with open("dados_usp.pkl", "wb") as f:
            pickle.dump(todas_unidades, f)

        print("\nDados salvos com sucesso em 'dados_usp.pkl'.")

    except Exception as erro:
        print("Erro geral no processamento:", erro)
        # Tira screenshot para debug
        driver.save_screenshot("error_fatal.png")
        print("Screenshot salvo como error_fatal.png")

    finally:
        driver.quit()
        print("Navegador finalizado")

if __name__ == "__main__":
    main()