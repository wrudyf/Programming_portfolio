
"use strict";
window.onload = main;
//initialize our main function to add event listeners
//to each id elem we need to add listeners to in our page

function main(){
        let time;
        let intseconds = 1200;

        //setting image string
        let img_string = "";
        
        //USE QUERY SELECTOR ON PHOTO VIEWER SYSTEM
        //setting image array
        let img_arr = [];
        
        //setting image array index
        let img_arr_index = Number(0);

        //setting index to keep track of what current image we're on
        let index = Number(0);

        //getting start value
        let s_num = document.getElementById("start_num");
        let start = Number(s_num.value);
        
        //setting index to start value
        index = start;
        //listen to start num changes
        s_num.onchange = () => {
                s_num = document.getElementById("start_num");
                start = Number(s_num.value);
                index = start;
        }

        //getting end value
        let e_num = document.getElementById("end_num");
        let end = Number(e_num.value);
        //listen to end num changes
        e_num.onchange = () => {
                e_num = document.getElementById("end_num");
                end = Number(e_num.value);
        }

        let link = document.getElementById("j_url");
        link.onchange = () => {
                link = document.getElementById("j_url");
        }

        //getting folder name and common image name
        let folder_name = document.getElementById("folder");
        let comm_name = document.getElementById("comname");

        //setting load photo button
        let loadphotosbutton = document.getElementById("loadphotos");
        loadphotosbutton.onclick = loadphotos;
        
        //setting load json button
        let loadjsonbutton = document.getElementById("loadjson");
        loadjsonbutton.onclick = loadjson;

        //setting next photo button
        let nextbutton = document.getElementById("next");
        nextbutton.onclick = nextphoto;

        //setting previous photo button
        let prevbutton = document.getElementById("previous");
        prevbutton.onclick = prevphoto;

        //setting first photo button
        let firstbutton = document.getElementById("first");
        //ANONYMOUS FUNCTION HERE LAMBDA FUNCTION**********************************************************************
        firstbutton.onclick = () => {
                index = start;
                img_arr_index = 0;
                document.getElementById("photo_being_displayed").value = img_arr[img_arr_index];
                //document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index;
                document.getElementById("photo").src = img_arr[img_arr_index];
                //document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
        }

        //setting last photo button
        let lastbutton = document.getElementById("last");
        //ANONYMOUS FUNCTION HERE LAMBDA FUNCTION***********************************************************************
        lastbutton.onclick = () => {
                index = end;
                img_arr_index = img_arr.length - 1;
                //document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index;
                //document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
                document.getElementById("photo_being_displayed").value = img_arr[img_arr_index];
                document.getElementById("photo").src = img_arr[img_arr_index];
        }

        //setting start slideshow button
        let startbutton = document.getElementById("startss");
        startbutton.onclick = () => {
                time = setInterval(nextphoto, intseconds);
        }

        //setting start random slideshow button
        let startrand = document.getElementById("startrs");
        startrand.onclick = () => {
                time = setInterval(randphoto, intseconds);
        }

        function randphoto() {
                img_arr_index = Math.floor(Math.random() * (end - 1));
                console.log(img_arr_index);
                document.getElementById("photo_being_displayed").value = img_arr[img_arr_index];
                //document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index;
                document.getElementById("photo").src = img_arr[img_arr_index];
                //document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
        }
        //setting stop slideshow button
        let stopbutton = document.getElementById("stopss");
        stopbutton.onclick = () => {
                clearInterval(time);
        }

        function loadphotos(){
                if (start > end){
                        //USING QUERY SELECTOR HERE ********************************************************
                        document.querySelector(".photo_viewer_system").innerText = "Error: Invalid Range";
                }
                else{
                        //USING QUERY SELECTOR HERE ********************************************************
                        document.querySelector(".photo_viewer_system").innerText = "Photo Viewer System";
                        let i = start;
                        img_arr = [];
                        while (i <= end){
                                img_string = folder_name.value + "/" + comm_name.value + i + ".jpg";
                                img_arr.push(img_string);
                                console.log(img_arr)
                                i += 1;
                        }
                        document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index + ".jpg";
                        document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
                }
        }

        function loadjson(){
                let url = link.value;
                img_arr = [];
                fetch(url)
                        .then(response => response.json())
                        .then(json =>{ 
                                
                                console.log(json);
                                let x1 = json;
                                let x2 = x1.images[0].imageURL;
                                let size = x1.images.length;
                                let i = 0;
                                while (i < size){
                                        img_arr.push(x1.images[i].imageURL);
                                        i += 1;
                                }
                                document.getElementById("photo_being_displayed").value = img_arr[0];
                                document.getElementById("photo").src = img_arr[0];
                        });
                
        }

        function nextphoto(){
                if (index == end){
                        index = start;
                        img_arr_index = 0;
                }
                else{
                        index += 1;
                        img_arr_index += 1;
                }
                document.getElementById("photo_being_displayed").value = img_arr[img_arr_index];
                //document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index;
                document.getElementById("photo").src = img_arr[img_arr_index];
                //document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
        }

        function prevphoto(){
                if (index == start){
                        index = end;
                        img_arr_index = img_arr.length - 1;
                }
                else{
                        index -= 1;
                        img_arr_index -= 1;
                }
                document.getElementById("photo_being_displayed").value = img_arr[img_arr_index];
                //document.getElementById("photo_being_displayed").value = folder_name.value + "/" + comm_name.value + index;
                document.getElementById("photo").src = img_arr[img_arr_index];
                //document.getElementById("photo").src = folder_name.value + "/" + comm_name.value + index + ".jpg";
        }

}