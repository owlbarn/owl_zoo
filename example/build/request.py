import requests
import base64
import subprocess

with open("panda.png", "rb") as f:
  encoded_string = base64.b64encode(f.read())

r1 = requests.get('http://127.0.0.1:9527/predict/infer',
    params = {"input1":"panda.png", "input2":encoded_string})

r2 = requests.get('http://127.0.0.1:9527/predict/to_json',
    params = {"input1":r1.content})
print r2.content
