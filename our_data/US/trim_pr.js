const counties = require("./counties.json");

let newFeatures = []
counties.features.forEach(function(feature, ind) {
  if (feature.properties.STATENAME != "Puerto Rico") {
    newFeatures.push(feature)
  }
});

counties.features = newFeatures;
fs = require("fs")
fs.writeFile("./counties_no_pr.json", JSON.stringify(counties, null, 2), function() {return null;});
