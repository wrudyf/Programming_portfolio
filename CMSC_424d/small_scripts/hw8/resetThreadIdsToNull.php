<?php
include("Constants.php");
$user = Constants::USER;
$password = Constants::PASSWORD;
$db = Constants::DB;

$mysqli = new mysqli('localhost', $user, $password, $db);
$mysqli->query("UPDATE rfuente5_POST set threadId = null");

?>
