<?php
// url and port of the patoline presentation
$url     = "http://".$_SERVER['SERVER_NAME'].":8080/";
// Change with a group id
$groupid = "guest";
// Should match the secret in the presentation.
$secret  = "your very secret password fot the presentation";


// Small script to generate a random connexion, with
// a fixed groupid and a random sessid.

function url($url,$groupid,$secret) {
  $sessid = mt_rand(1000000000,9999999999);
  $key = md5($sessid . '+' . $groupid . $secret);
  return($url . $sessid . '?key=' . $key . '&group=' . $groupid);
}
?>
<!doctype html>
<html>
  <head>
    <meta chearset="utf-8"/>
    <title>Acces page to a Patoline presentation</title>
  </head>
  <body>
    <h1>Acces page to a Patoline presentation</h1>

    <p>
	<a href="<?= url($url,$groupid,$secret) ?>">
	   The presentation starts by clicking here
	</a>
    </p>
  </body>
</html>
