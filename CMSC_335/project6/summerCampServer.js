process.stdin.setEncoding("utf8");

if (process.argv.length != 3){
    process.stdout.write(`Usage summerCampServer\n`);
    process.exit(1);
}

const prompt = "Stop to shutdown the server:"
process.stdout.write(prompt);
process.stdin.on("readable", function() {
    let dataInput = process.stdin.read();
    if (dataInput !== null){
        let command = dataInput.trim();
        if (command === "stop"){
            process.stdout.write("Shutting down the server \n");
            process.exit(0);
        }
        else{
            process.stdout.write("Invalid command: " + command + "\n");
        }
        process.stdout.write(prompt);
        process.stdin.resume();

    }
});



const portNum = process.argv[2];
const path = require("path");
const express = require("express");
const app = express();
const bodyParser = require("body-parser");

const path2 = require("path");
require("dotenv").config({path: path2.resolve(__dirname, 'creds/.env') })
const uri = process.env.MONGO_CONNECTION_STRING;

const databaseAndCollection = {db: "summer", collection: "applicants"};
const {MongoClient, ServerApiVersion} = require("mongodb");

//insert
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


//show records
function getApplicantPromise(x, resolveApplicant){
    let answer;
    let promise = new Promise ((resolve) => {
        resolve(answer);
    }
    );
}

async function showApplicant(em) {
    const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
            let email = em;
            return await lookUpOne(client,databaseAndCollection, email);

    } catch (e) {
        console.error(e)
    } finally{
        await client.close();
    }
}

async function lookUpOne(client, databaseAndCollection, email){
    let filter = {email: email};
    const result = await client.db(databaseAndCollection.db)
                        .collection(databaseAndCollection.collection)
                        .findOne(filter);
    if (result) {
        return result;
    }
    else{
        return false;
    }
}

async function showGreater(g) {
    const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
            let gpa = g;
            return await lookUpMany(client, databaseAndCollection, gpa);
    } catch (e) {
        console.error(e)
    } finally {
        await client.close();
    }
}

async function lookUpMany(client, databaseAndCollection, gpa){
    let filter = {gpa: {$gte: gpa}};
    const cursor = client.db(databaseAndCollection.db)
    .collection(databaseAndCollection.collection)
    .find(filter);

    const result = await cursor.toArray();
    return result;
}

async function showAll() {
    const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
        let filter = {};
        const cursor = client.db(databaseAndCollection.db)
        .collection(databaseAndCollection.collection)
        .find(filter);

        const result = await cursor.toArray();
        return result.length
    } catch (e) {
        console.error(e);
    } finally {
        await client.close();
    }
}

async function deleteAll() {
    const client = new MongoClient(uri, { useNewUrlParser: true, useUnifiedTopology: true, serverApi: ServerApiVersion.v1 });
    try {
        await client.connect();
        const result = await client.db(databaseAndCollection.db)
        .collection(databaseAndCollection.collection)
        .deleteMany({});
    } catch (e) {
        console.error(e);
    } finally {
        await client.close();
    }
}

app.set ("views", path.resolve(__dirname, "templates"));

app.set("view engine", "ejs");
app.use(bodyParser.urlencoded({extended: false}));

app.get("/", (request, response) => {
    response.render("index");
})

app.get("/apply", (request, response) => {
    response.render("apply");
})


app.post("/processApplication", (request, response) => {
    let {name, email, gpa, background_info} = request.body
    let date = Date();
    let variables = {
        aname: name,
        aemail: email,
        agpa: gpa,
        abackground_info: background_info,
        adate: date
    }
    
    //send data to db from here
    let info = {
        name: name,
        email: email,
        gpa: gpa,
        background_info: background_info
    }
    call_insert(info);
    response.render("processApplication", variables);
})

app.get("/reviewApplication", (request, response) => {
    
    response.render("review");
})

app.post("/reviewApplicationInfo", (request, response) => {
    //get app info here
    let {email} = request.body
    //console.log(email);
    let x = showApplicant(email);
    //x is promise
    x.then(function(result){
        //console.log(result);
        if (result == false){
            response.render("notfound");
        } else {
        let {name, email, gpa, background_info} = result;
        let variables = {
            aname: name,
            aemail: email,
            agpa: gpa,
            abackground_info: background_info
        }
        //console.log(variables);
        response.render("applicantdata", variables);}
    })
    
})

app.get("/adminGFA", (request, response) => {
    
    response.render("adminGFA")
})

app.post("/processAdminGFA", (request, response) => {
    //query db here **************************************
    let {gpa} = request.body;
    let x = showGreater(gpa);
    x.then(function(result){
        let y = "<table border=\"1\"><tr><th>Name</th><th>GPA</th></tr>";
        result.forEach((elem) => {
            y += "<tr><td>" + elem.name + "</td>";
            y += "<td>" + elem.gpa + "</td></tr>";
        }
        )
        y += "</table>";
        //console.log(y)
        let variables = {
            tab: y
        }
        response.render("processAdminGFA", variables)

    })
    //response.send("test");
})

app.get("/adminRemove", (request, response) => {
    //remove everything here
    response.render("adminRemove");
})

app.post("/removeAll", (request, response) => {
    let x = showAll();
    x.then(function(result){
        let variables = {
            num: result
        }
        deleteAll();
        response.render("removeAll", variables);
    })
    
})

app.listen(portNum, (err) => {
    if (err) {
        console.log("Starting server failed.");
    }
    else{
    console.log("Web server started and running at http://localhost: "+ portNum)}
})