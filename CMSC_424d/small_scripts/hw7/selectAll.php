<?php
include("constants.php");
$host  = Constants::HOST;
$password = Constants::PASSWORD;
$user = Constants::USER;
$db = Constants::DB;

$mysqli = new mysqli($host, $user, $password, $db);
$result = $mysqli->query("select * from rfuente5_USER");
while ($row = $result->fetch_assoc())
{
    echo $row['email'];
}
?>
