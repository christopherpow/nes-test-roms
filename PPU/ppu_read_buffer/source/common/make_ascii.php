<?php

require 'inc/font.php';

$blank = (int)$argv[1];
$white = (int)$argv[2];
$spark = (int)$argv[3];
$charset_file = $argv[4];

$charset = '';
for($a=0x00; $a<0x96; ++$a) $charset .= chr($a + 32);

foreach(explode("\n", file_get_contents($charset_file)) as $line)
  if(preg_match("/^DTE_CHARSET[^\"]*\"(.*)\"[ \t]*(?:;.*)?$/", $line, $mat))
  {
    $charset = $mat[1];
    $charset = preg_replace('/",\$([0-9A-F][0-9A-F])(?:,")?/e',
      'chr(hexdec("\\1"))',
      $charset);
  }

$charset = str_pad($charset, 96, '  SecretMessage.');

for($c=0; $c<96; ++$c)
{
  $s1 = '';
  $s2 = '';
  
  $cc = ord($charset[$c]) - 32;
  if($c == 95) $cc = 95;
  
  for($y=0; $y<8; ++$y)
  {
    $bits = $ascii_chars[$cc*8+$y];
    $c1 = 0x00;
    $c2 = 0x00;
    
    for($x=0; $x<8; ++$x)
    {
      $b = 1 << $x;
      
      if($bits == 0xFF)
      {
        $c1 |= (($spark&1)?$b:0);
        $c2 |= (($spark&2)?$b:0);
      }
      elseif($bits & $b)
      {
        $c1 |= (($white&1)?$b:0);
        $c2 |= (($white&2)?$b:0);
      }
      else
      {
        $c1 |= (($blank&1)?$b:0);
        $c2 |= (($blank&2)?$b:0);
      }
    }
    $s1 .= chr($c1);
    $s2 .= chr($c2);
  }
  print $s1;
  print $s2;
}
