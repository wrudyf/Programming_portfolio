<?php
include("Constants.php");
$host = Constants::HOST;
$password = Constants::PASSWORD;
$user = Constants::USER;
$db = Constants::DB;

$mysqli = new mysqli('localhost', $user, $password, $db);
$result = $mysqli->query("select * from rfuente5_USER");

$users = "";

while ($row=$result->fetch_assoc())
{
	$users .= "<tr>";
	$users .= "<td>" . $row['email'] . "</td>";
	$users .= "<td>" . $row['age'] . "</td>";
	$users .= "<td>" . $row['state'] . "</td>";
	$users .= "</tr>";
}
$result2 = $mysqli->query("select * from rfuente5_POST");
$posts = "";

while ($row=$result2->fetch_assoc())
{
	$posts .= "<tr>";
	$posts .= "<td>" . $row['id'] . "</td>";
	$posts .= "<td>" . $row['body'] . "</td>";
	$posts .= "<td>" . $row['likes'] . "</td>";
	$posts .= "<td>" . $row['email'] . "</td>";
	$posts .= "<td>" . $row['parentId'] . "</td>";
	$posts .= "<td>" . $row['datePosted'] . "</td>";
	$posts .= "<td>" . $row['threadId'] . "</td>";
	$posts .= "</tr>";
}

include("select.html");
?>

