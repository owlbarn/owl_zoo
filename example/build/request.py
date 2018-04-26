import requests
import base64
import subprocess

encoded_string = subprocess.check_output(['openssl', 'base64', '-in', 'panda.png'])

r1 = requests.get('http://127.0.0.1:9527/predict/infer',
    params = {"input1":"panda_sq.ppm", "input2":encoded_string})

r2 = requests.get('http://127.0.0.1:9527/predict/to_json',
    params = {"input1":r1.content})
print r2.content
