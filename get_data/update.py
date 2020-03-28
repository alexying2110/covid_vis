import requests
import json

data_dir = "/home/lofatdairy/code/sialab/covid_vis/our_data/test/test.csv"
url = "http://127.0.0.1:5000/api"

response = requests.get(url)
response_dict = json.loads(response.content)

obs_dict = response_dict["response"]

with open(data_dir, "a") as data:
    for ob in obs_dict:
        data.write(obs_dict[ob]["County"] + ',' + obs_dict[ob]["State"] + ',' + str(obs_dict[ob]["Lat"]) + ',' + str(obs_dict[ob]["Long"]) + ',' + str(obs_dict[ob]["Positive"]) + ',' + str(obs_dict[ob]["Updated"]) + "\n")
