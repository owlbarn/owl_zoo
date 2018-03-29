import requests
import base64
import subprocess

#with open("panda_sq.ppm", "rb") as image_file:
    #encoded_string = base64.b64encode(image_file.read())
encoded_string = subprocess.check_output(['openssl', 'base64', '-in', 'panda.png'])

"""
#r = requests.get('http://172.17.0.2:9527/predict/infer', 
r1 = requests.get('http://127.0.0.1:9527/predict/infer', 
    params = {"input1":"panda_sq.ppm", "input2":encoded_string})

r2 = requests.get('http://127.0.0.1:9527/predict/to_json', 
    params = {"input1":r1.content})
#print r2.content
"""

r = requests.get('http://127.0.0.1:9527/predict/main',
    params = {"input1":"panda_sq.ppm", "input2":encoded_string})