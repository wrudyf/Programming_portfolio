process.stdin.setEncoding("utf8");

if (process.argv.length != 3) {
    process.stdout.write(`Usage error for billboard`);
    process.exit(1);
}

const prompt = "Type stop to shutdown the server\n"
process.stdout.write(prompt);
process.stdin.on("readable", function() {
    let dataInput = process.stdin.read();
    if (dataInput !== null){
        let command = dataInput.trim();
        if (command === "stop"){
            process.stdout.write("Shutting down server \n");
            process.exit(0);
        }
        else {
            process.stdout.write("Invalid command: " + command + "\n");
        }
        process.stdout.write(prompt);
        process.stdin.resume();
    }
})

const portNum = process.argv[2]
const path = require("path");
const express = require("express");
const app = express();
const bodyParser = require("body-parser");

const path2 = require("path")
require("dotenv").config({path: path2.resolve(__dirname, 'creds/.env') })
const uri = process.env.MONGO_CONNECTION_STRING;
const databaseAndCollection = {db: "billboard", collection: "dadjokes"};
const {MongoClient, ServerApiVersion} = require("mongodb");


const fetch = require('node-fetch');

async function call_insert(elem){
  const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
        await insertApplicant(client, databaseAndCollection, elem);
    } catch(e) {
        console.error(e);
    } finally {
        await client.close();
    }
}

async function insertApplicant(client, databaseAndCollection, applicant){
  const result = await client.db(databaseAndCollection.db).collection(databaseAndCollection.collection).insertOne(applicant);
}

async function showAll() {
  const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
            
            return await lookUpMany(client, databaseAndCollection);
    } catch (e) {
        console.error(e)
    } finally {
        await client.close();
    }
}

async function lookUpMany(client, databaseAndCollection, gpa){
  let filter = {};
  const cursor = client.db(databaseAndCollection.db)
  .collection(databaseAndCollection.collection)
  .find(filter);

  const result = await cursor.toArray();
  return result;
}

async function getJSONData(){
  const url = 'https://dad-jokes.p.rapidapi.com/random/joke';
  const options = {
    method: 'GET',
    headers: {
      'X-RapidAPI-Key': 'e12577c4b0mshbf9901fd90fea14p1e6e6cjsnb93d05fa2c11',
      'X-RapidAPI-Host': 'dad-jokes.p.rapidapi.com'
    }
  };
  const response = await fetch(url, options);
  const result = await response.json();
  return result
}
	

/*
x.then(function(result){
  console.log(result.body[0].punchline);
})
*/
//.setup


app.set ("views", path.resolve(__dirname, "templates"));
app.set ("view engine", "ejs");
app.use(bodyParser.urlencoded({extended: false}));

app.use('/public', express.static('public'));


app.get("/", (request, response) => {
  let s1 = ""
  let x = showAll();
  x.then(function(result) {
    s1 += "<table border=\"1\"><tr><th>Jokes</th></tr>";
    result.forEach((elem) => {
      s1 += "<tr><td>" + elem.joke + "</td></tr>";
    }

    )
    s1 += "</table>";
    let variables = {
      jokes: s1
    }
    response.render("index", variables)

  })
})

app.get("/addJoke", (request, response) => {
  var x = getJSONData();
  var joke_suggestion = "";
  //API BEING USED HERE
  x.then(function(result) {
    joke_suggestion += result.body[0].setup + " "
    joke_suggestion += result.body[0].punchline
    //console.log(result)
    //console.log("inner" + joke_suggestion + "\n")
    //console.log("outer "+ joke_suggestion + "\n")
  let variables = {
    joke_suggestion: joke_suggestion
  }

  response.render("addJoke", variables)
  })
})

app.post("/process", (request, response) => {
  let {joke} = request.body
  console.log(joke)
  let info = {
    joke: joke
  }
  call_insert(info)
  response.render("confirmation")
})

app.listen(portNum, (err) => {
  if (err) {
    console.log("Starting server failed.");
  } else{
    console.log("Web server started and running at http://localhost: " + portNum)
  }
})