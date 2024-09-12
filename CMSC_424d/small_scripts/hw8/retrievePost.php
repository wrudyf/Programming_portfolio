<?php
include ("Constants.php");
$user = Constants::USER;
$pass = Constants::PASSWORD;
$db = Constants::DB;

$id = $_POST['postid'];
$mysqli = new mysqli('localhost', $user, $pass, $db);
$result = $mysqli->query("select * from rfuente5_POST, rfuente5_USER where rfuente5_POST.email = rfuente5_USER.email AND id = $id");
$tab = "";

$row = $result->fetch_assoc();
if ($row == null){
	$tab = "INVALID POST ID";
}
else{
	$tab .= "<table border=\"1\">";
	$tab .= "<tr> <th>Post ID</th> <th>Body</th> <th>Email of poster</th> <th>Age</th> <th>State</th> <th>Date posted</th> </tr>";
	$st = array();
	while ($row != null){
	$tab2 = "";
	$tab2 .= "<tr>";
	$tab2 .= "<td>" .$row['id'] .  "</td>" ;
	$tab2 .= "<td>" .$row['body'] . "</td>";
	$tab2 .= "<td>" .$row['email'] . "</td>";
	$tab2 .= "<td>" .$row['age'] . "</td>";
	$tab2 .= "<td>" .$row['state'] . "</td>";
	$tab2 .= "<td>" .$row['datePosted'] . "</td>";
	$tab2 .= "</tr>";
	//$tab .= $tab2;	
	array_push($st, $tab2);
	$id2 = $row['parentId'];
	//issue when parent id is null, don't do query
		if ($id2 == null){
		$row = null;
		}
		else{
		$result = $mysqli->query("select * from rfuente5_POST, rfuente5_USER where rfuente5_POST.email = rfuente5_USER.email AND id = $id2");
		$row = $result->fetch_assoc();
		}
	}
	$str = array_pop($st);
	while($str != null){
	$tab .= $str;
	$str = array_pop($st);
	}
	
	$tab .= "</table";
}

include("retrieve.html");
?>
