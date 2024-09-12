<?php
include("Constants.php");
$password = Constants::PASSWORD;
$user = Constants::USER;
$db = Constants::DB;

$mysqli = new mysqli('localhost', $user, $password, $db);
$result = $mysqli->query("select id, parentId from rfuente5_POST");
$pnotnull;
$pisnull;

while ($row=$result->fetch_assoc())
{
	$id = $row['id'];
	$parentId = $row['parentId'];
	if ($parentId == null){
		$pisnull[$id] = null;
	}
	else{
		$pnotnull[$id] = $parentId;
	}

	echo "$id $parentId \n";
}
echo "null parent ID's\n";
foreach ($pisnull as $i => $i2){
	echo "$i : $i2 ";
	$mysqli->query("UPDATE rfuente5_POST SET threadId = id where id=$i");	
}
echo "\n";
echo "not null parent ID's\n";
foreach ($pnotnull as $i => $i2){
	echo "$i : $i2 ";
	$mysqli->query("UPDATE rfuente5_POST SET threadId = parentId where id=$i");

}
echo "\n";
?>
