<?php
include("Constants.php");
$user = Constants::USER;
$pass = Constants::PASSWORD;
$db = Constants::DB;

$body = $_POST['body'];
$email = $_POST['email'];
$parentid = $_POST['parentId'];
$mysqli = new mysqli('localhost', $user, $pass, $db);
$count = 0;

$parentid += 0;
$body = $mysqli->real_escape_string($body);
$email = $mysqli->real_escape_string($email);

$result = $mysqli->query("select * from rfuente5_POST where id=$parentid");

while ($row=$result->fetch_assoc())
{
	$count = $count + 1;
} 
if ($count == 0){
	echo "INVALID PARENT ID \n";
}
else{
	echo "parent id  exists, check selectAll.php to see if it inserted.\n";
	$q = "insert into rfuente5_POST(email, body, parentId) values('$email', '$body', $parentid)";
	$mysqli->query($q);
	if ($mysqli->affected_rows == -1){
	echo "ERROR, please make sure you put in a valid number, an email that exists in selectAll.php and is less than 30 chars, and that body is less than 100 chars"; 
	}
else{
	echo "Affected rows: " . $mysqli->affected_rows;}
	
	$mysqli->close();
}


?>
