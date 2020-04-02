import requests
import json

data_dir = "/home/ubuntu/covid_vis/our_data/test/test.csv"
url = "http://172.104.20.242:8080/api"

response = requests.get(url)
response_dict = json.loads(response.content)

obs_dict = response_dict["response"]

with open(data_dir, "a") as data:
    for ob in obs_dict:
        data.write(obs_dict[ob]["County"] + ',' + obs_dict[ob]["State"] + ',' + str(obs_dict[ob]["Lat"]) + ',' + str(obs_dict[ob]["Long"]) + ',' + str(obs_dict[ob]["Positive"]) + ',' + str(obs_dict[ob]["Updated"]) + ',' + str(obs_dict[ob]["Race"]) + ',' + str(obs_dict[ob]["Hispanic"]) + ',' + str(obs_dict[ob]["Age"]) + "\n")
