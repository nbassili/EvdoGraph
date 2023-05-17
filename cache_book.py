from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
import sys

def cache_book():
    book_code = sys.argv[1]
    driver = webdriver.Chrome(executable_path="G:/Programs/chromedriver.exe")
    driver.get("https://service.eudoxus.gr/search/#a/id:"+book_code+"/0")
    element = WebDriverWait(driver, 5).until(EC.presence_of_element_located((By.CSS_SELECTOR, 'body > table > tbody > tr:nth-child(2) > td > table > tbody > tr:nth-child(3) > td > table > tbody > tr > td:nth-child(2) > table > tbody > tr:nth-child(1) > td > div')))
    pageSource = driver.page_source
    fileToWrite = open('./book_cache/'+book_code+'.html', "w")
    fileToWrite.write(pageSource)
    fileToWrite.close()
    driver.quit()
    return book_code+'.html'

    #driver.implicitly_wait(5)
    #driver.maximize_window()
    #print(book_code)
    #fileToRead = open("D:/book_cache/94700120.html", "r")
    #print(fileToRead.read())
    #fileToRead.close()


if __name__ == '__main__':
    print(cache_book())



