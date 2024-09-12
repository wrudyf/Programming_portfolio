"use strict";

window.onload = main;

function main(){
    
    let fname = document.getElementById("firstname");
    let lname = document.getElementById("lastname");
    let phone1 = document.getElementById("phone1");
    let phone2 = document.getElementById("phone2");
    let phone3 = document.getElementById("phone3");
    let email = document.getElementById("email");

    
    let age = document.getElementById("age");
    let feet = document.getElementById("feet");
    let inches = document.getElementById("inches");
    let weight = document.getElementById("pounds");

    
    let highbloodpressure = document.getElementById("high_blood_pressure");
    let diabetes = document.getElementById("diabetes");
    let glaucoma = document.getElementById("glaucoma");
    let asthma = document.getElementById("asthma");
    let none = document.getElementById("no_conditions");
    
    
    let conditiontime = document.getElementsByName("period")

    let studytime = document.getElementById("study_time");

    
    let study1num = document.getElementById("study_id_1");
    let study2num = document.getElementById("study_id_2");


    let comments = document.getElementById("comments");

    let submitButton = document.getElementById("f");
    
    function validate(){
        let error_count = 0;
        let error_string = ""
        let conds_checked = 0;
 //       let string = "";
 //       string += fname.value;
 //       string += " ";
 //       string += lname.value;
 //       string += phone1.value + " " + phone2.value + " " + phone3.value;
        let phone = phone1.value + phone2.value + phone3.value;

        if (checkPhone(phone) == false){   
            error_string += "Invalid phone number\n"
            error_count += 1;
        }

        //string += email.value + " ";
        //string += age.value + " "; 
        //string += feet.value + " \n";
        //string += inches.value + " ";
        //string += weight.value + " ";

        //string += "\n"
        //string += "high checked? " + highbloodpressure.checked + " \n";
        if (highbloodpressure.checked == true){
            conds_checked += 1;
        }
        //string += "diabetes checked? " + diabetes.checked + " \n";
        if (diabetes.checked == true){
            conds_checked += 1;
        }
        //string += "glaucoma checked? " + glaucoma.checked + " \n";
        if (glaucoma.checked == true){
            conds_checked += 1;
        }
        //string += "asthma checked? " + asthma.checked + " \n";
        if (asthma.checked == true){
            conds_checked += 1;
        }
        //string += "none checked? " + none.checked + " \n";
        if (none.checked == true){
            conds_checked += 1;
        }

        if (conds_checked == 0){
            error_string += "No conditions selected\n";
            error_count += 1;
        }

        if (conds_checked > 1 && none.checked == true){
            error_string += "Invalid conditions selection\n"
            error_count += 1;
        }

        //check for time period selection
        let time_period = Array.from(conditiontime).find (
            x => x.checked
        )

        if (time_period == undefined){
            error_string += "No time period selected\n";
            error_count += 1;
        }
        //string += studytime.value + " ";
        //string += study1num.value + " " + study2num.value + " \n";
        //string += comments.value;

        //window.alert(string);

        //check for valid study id number
        if (checkStudyId('A', study1num.value) == false){
            error_string += "Invalid study id\n";
            error_count += 1;
            
        }

        if (checkStudyId('B', study2num.value) == false){
            error_string += "Invalid study id\n";
            error_count += 1;
            
        }

        //if (error_count > 0){
        //    window.alert(error_string)
        //}
        if (error_count == 0){
            let x = window.confirm("Do you want to submit the form data?");
            return x;
        }
        else{
            window.alert(error_string);
            return false;
        }

    }
    window.onsubmit = validate;
    function checkStudyId(fc, s){
        let index = 0;
        if (s.charAt(index) != fc){
            return false;
        }
        index += 1;
        while (index < s.length ){
            if (isNaN(s.charAt(index)) == true){
                return false;
            }
            index ++;
        }
        return true;
    }

    function checkPhone(s){
        let correct = true;
        let index = 0;
        let len = s.length;
        while (index < len ){
            if ( isNaN(s.charAt(index)) == true )
            {
                correct = false;
            }
            index ++;
        }
        return correct;
    }
}