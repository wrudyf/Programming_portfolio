class SysMessage{ 
    #textjson;
    #info;
    #html;
    #htmlporder;
    constructor(text){
        this.#textjson = text;
        let y = JSON.parse(text);
        //parse text for command line info first
        this.#info = "[\n";
        y.itemsList.forEach(element => {
            this.#info += "   {name: ";
            this.#info += element.name;
            this.#info += ", cost: ";
            this.#info += element.cost;
            this.#info += "}, \n";
        });
        this.#info += "]\n";

        //parse text for html 
        this.#html += "<table border =\"1\"><tr><td><strong>Item</strong></td> <td><strong>Cost</strong></td></tr>";
        y.itemsList.forEach(element => {
            this.#html += "<tr>";
            this.#html += "<td>" + element.name + "</td>";
            this.#html += "<td>" + element.cost + "</td>";
            this.#html += "</tr>";

            this.#htmlporder += "<option value=\"" + element.name + "\">" + element.name + "</option>"; 
        });
        this.#html += "</table>";
    }
    get info(){
        return this.#info;
    }
    get html(){
        return this.#html;
    }
    get htmlporder(){
        return this.#htmlporder;
    }
    get textjson(){
        return this.#textjson;
    }
}

process.stdin.setEncoding("utf8");
if (process.argv.length != 3){
    process.stdout.write(`Usage supermarketServer.js jsonFile\n`);
    process.exit(1);
}

const fname = process.argv[2];

const fs = require("fs");

let fileContent = fs.readFileSync(fname, 'utf-8');
let msg = new SysMessage(fileContent)

const prompt = "Type itemsList or stop to shutdown the server: "
process.stdout.write(prompt);
process.stdin.on("readable", function() {
    let dataInput = process.stdin.read();
    if (dataInput !== null){
        let command = dataInput.trim();
        if (command === "itemsList"){
            process.stdout.write(msg.info);
        }
        else if (command === "stop"){
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


const path = require("path");
const express = require("express");
const app = express();
const bodyParser = require("body-parser");
const portNumber = 5000;

app.set ("views", path.resolve(__dirname, "templates"));

app.set("view engine", "ejs");

app.use(bodyParser.urlencoded({extended: false}));

//set route for index
app.get("/", (request, response) => {
    response.render("index");
})

//set route for displayItems
app.get("/catalog", (request, response) => {
    const variables = {
        itemsTable: msg.html
    };
    response.render("displayItems", variables);
})

//set route for ordering 
app.get("/order", (request, response) => {
    const variables = {
        items: msg.htmlporder
    };
    response.render("placeOrder", variables);
})

//set route for order confirmation USING POST
app.post ("/order", (request, response) => {
    let cost = 0;
    let {name, email, delivery, itemsSelected, orderInformation} = request.body;
    
    let x = JSON.parse(msg.textjson);
    let iselected = "<table border=\"1\">";
    iselected += "<tr> <td><strong>Item</strong></td> <td><strong>Cost</strong></td> </tr>";
    x.itemsList.forEach(element => {
        let x2 = itemsSelected.find(e1 =>{ return e1 === element.name });

        //onsole.log("check " + x2);
        if (x2 != undefined){
            iselected += "<tr>";
            iselected += "<td>" + element.name + "</td>";
            iselected += "<td>" + element.cost + "</td>";
            iselected += "</tr>";
            cost += Number(element.cost);
        }
    });
    iselected += "<tr><td>Total Cost:</td><td>" + cost + "</td></tr>";
    iselected += "</table>";
    //console.log(itemsSelected);
    let variables = {
        name: name,
        email: email,
        delivery: delivery,
        itemsSelected: itemsSelected,
        orderInformation: orderInformation,
        orderTable: iselected
    };
    response.render("orderConfirmation", variables);
})

app.listen(portNumber, (err) => {
    if (err) {
        console.log("Starting server failed.");
    }
    else{
        console.log("To access server, look at the port number");
    }
})