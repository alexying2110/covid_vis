import flask
from flask import request, jsonify
import pandas as pd
import numpy as np
import time
import random
import math

data = pd.read_csv("./03-24-2020.csv")
data = data.loc[data["Country_Region"] == "US"]

def gen_response():
    obs = data.sample(200, replace = True, weights = "Confirmed")
    obs = obs.replace(np.nan, "", regex = True)

    current_time = math.floor(time.time())
    start_time = current_time - 1000 * 60 * 5

    obs["Positive"] = ["TRUE" if random.random() < 0.4 else "FALSE" for i in range(200)]
    obs["Updated"] = [random.randint(start_time, current_time) for i in range(200)]

    response = {}
    for i in range(200):
        response[i] = {
                    "County": obs.iloc[i]["Admin2"],
                    "State": obs.iloc[i]["Province_State"],
                    "Lat": obs.iloc[i]["Lat"],
                    "Long": obs.iloc[i]["Long_"],
                    "Positive": obs.iloc[i]["Positive"],
                    "Updated": int(obs.iloc[i]["Updated"])
        }

    return response

app = flask.Flask(__name__)

@app.route("/api", methods=["GET"])

def get_json():
    return jsonify({"response":  gen_response()})

if __name__ == '__main__':
    app.run(debug = True)
