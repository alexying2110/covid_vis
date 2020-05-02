## **SAFE Health data request api ##

### **Method:**
  `POST`

### **Parameters*
  
  * **Required:**
    
    * `time=[integer representing current time in Unix time]`

    * `lastTime=[integer representing last time the api was called in Unix time]`

### **Success Response**
  
  The expected JSON formatted response has the format below, and contains an object for each test that was taken at a Unix time greater than `lastTime` and less than or equal to `time`

  * `responses`

    * `[testID]` - A unique hash for the test
      
      * `county` - A string representing the name of the county the test was taken in. This string does not contain muncipality type (i.e. "Hamilton" not "Hamilton County" nor "Hamilton Parish")

      * `state` - A string representing the name of the state the test was taken in

      * `lat` - A string or float representing the latitude the test was taken in (Does not require high precision)

      * `long` - A string or float representing the longitude the test was taken in (Does not require high precision)

      * `positive` - A string that is either "TRUE" or "FALSE", representing the COVID diagnosis

      * `updated` - An integer representing time the user took the test in Unix time

      * `meta` - An JSON object representing any additional meta data you are comfortable sharing (ideally would at least contain age, race, and insurance status)
