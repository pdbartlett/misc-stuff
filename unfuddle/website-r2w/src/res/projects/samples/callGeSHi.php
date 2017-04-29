<?php

@include "geshi.php";

$g =& new GeSHi(file_get_contents($argv[1]), $argv[2]);
$html = $g->parse_code();

$dst = fopen($argv[3], 'w');
fputs($dst, $html);
fclose($dst);

?>
