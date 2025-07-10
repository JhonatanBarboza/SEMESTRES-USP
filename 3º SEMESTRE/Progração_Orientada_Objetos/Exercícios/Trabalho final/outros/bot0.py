from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.common.exceptions import TimeoutException, NoSuchElementException
import time
import traceback

UNIDADE_ALVO = "Escola de Comunicações e Artes - ( ECA )"
CURSO_ALVO = "Música - Bacharelado (Habilitação em Instrumento de Cordas - Ênfase em Violoncelo) - integral"

def setup_driver():
    options = Options()
    options.binary_location = "/usr/bin/google-chrome"
    service = Service(executable_path="./chromedriver")
    driver = webdriver.Chrome(service=service, options=options)
    return driver, WebDriverWait(driver, 15)

def safe_click(driver, wait, element_id, max_attempts=3):
    """Função segura para clicar em elementos"""
    for attempt in range(max_attempts):
        try:
            element = wait.until(
                EC.element_to_be_clickable((By.ID, element_id)))
            driver.execute_script("arguments[0].click();", element)
            return True
        except Exception as e:
            print(f"Tentativa {attempt + 1} falhou ao clicar em {element_id}: {str(e)}")
            time.sleep(1)
    return False

def handle_popup(driver, wait):
    """Função para lidar com o popup de erro"""
    try:
        # Espera até que o popup esteja visível
        popup = wait.until(EC.visibility_of_element_located(
            (By.XPATH, "//div[contains(@class, 'ui-dialog') and contains(@style, 'display: block')]")
        ))
        
        # Localiza o botão Fechar dentro do popup
        close_button = wait.until(EC.element_to_be_clickable(
            (By.XPATH, ".//button[contains(., 'Fechar')]")
        ))
        
        # Clica no botão usando JavaScript para evitar problemas de interceptação
        driver.execute_script("arguments[0].click();", close_button)
        print("Popup de erro fechado com sucesso")
        
        # Espera até que o popup desapareça
        wait.until(EC.invisibility_of_element(popup))
        return True
        
    except TimeoutException:
        print("Nenhum popup de erro detectado dentro do tempo limite")
        return False
    except Exception as e:
        print(f"Erro ao tentar fechar popup: {str(e)}")
        return False

def main():
    driver, wait = setup_driver()
    
    try:
        # 1. Acessa a página inicial
        driver.get("https://uspdigital.usp.br/jupiterweb/jupCarreira.jsp?codmnu=8275")
        print("\n=== Iniciando coleta de dados ===")

        # 2. Seleciona unidade
        wait.until(EC.presence_of_element_located((By.ID, "comboUnidade")))
        unidades = Select(driver.find_element(By.ID, "comboUnidade"))
        
        indice_unidade = next((i for i, opt in enumerate(unidades.options) if UNIDADE_ALVO in opt.text), None)
        if indice_unidade is None:
            raise ValueError(f"Unidade '{UNIDADE_ALVO}' não encontrada")
        
        unidades.select_by_index(indice_unidade)
        print(f"\n✓ Unidade selecionada: {unidades.options[indice_unidade].text.strip()}")

        # 3. Seleciona curso
        wait.until(lambda d: len(d.find_element(By.ID, "comboCurso").find_elements(By.TAG_NAME, "option")) > 1)
        cursos = Select(driver.find_element(By.ID, "comboCurso"))
        
        indice_curso = next((j for j, opt in enumerate(cursos.options) if CURSO_ALVO in opt.text), None)
        if indice_curso is None:
            raise ValueError(f"Curso '{CURSO_ALVO}' não encontrado")
        
        cursos.select_by_index(indice_curso)
        print(f"\n✓ Curso selecionado: {cursos.options[indice_curso].text.strip()}")

        # 4. Clica em Buscar
        driver.find_element(By.ID, "enviar").click()
        print("Clicou em Buscar - aguardando resultado...")

        # 5. Trata o popup de erro (se aparecer)
        if not handle_popup(driver, wait):
            print("Nenhum popup para fechar ou erro ao fechar - continuando...")
        
        # 6. Aqui você pode continuar com o resto do seu código
        print("Pronto para continuar com o processamento...")
        time.sleep(10)
    except Exception as e:
        print(f"\n Erro durante a execução: {str(e)}")
        traceback.print_exc()
        
        # Tira screenshot para debug
        driver.save_screenshot("error.png")
        print("Screenshot salvo como error.png")
    finally:
        driver.quit()
        print("\nNavegador finalizado")

if __name__ == "__main__":
    main()