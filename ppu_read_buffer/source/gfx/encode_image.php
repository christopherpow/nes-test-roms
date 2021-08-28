<?php

require 'nes_palette.php';

$SAMPLE_OUTPUT  = $argv[1];
$SPR_OUTPUT    = $argv[2];
$NTA_OUTPUT    = $argv[3];
$ATTR_OUTPUT   = $argv[4];
$PAL_OUTPUT    = $argv[5];

$sprbin = fopen($SPR_OUTPUT, 'wb');
$paldat = fopen($PAL_OUTPUT, 'wb');

$palette = Array();

print "Reading stdin...\n";

$cells      = Array();
$nta_data   = Array();
$attr_data  = Array();
$w = 0;
$h = 0;

foreach(explode("\n", file_get_contents('php://stdin')) as $line)
{
  if(preg_match('/^<'. '?php/', $line)) break;

  if(sscanf($line, ".sprite %d %d %d \$%02X", $x,$y, $attr, $spr_index) == 4)
  {
    fwrite($sprbin, chr($y).chr($spr_index).chr($attr&3).chr($x));
  }
  elseif($line != '' && $line[0] == '.') // palette
  {
    preg_match_all('@\$([0-9A-F]*)@', $line, $mat);
    foreach($mat[1] as $c)
    {
      $c = hexdec($c);
      $signal = GenNTSCsignal($c, 0, 12);
      $rgb = DecodeNTSCsignal($signal, 0,12, 3.9);
      $palette[] = $rgb;
      fwrite($paldat, chr($c));
    }
  }
  elseif(sscanf($line, '%d %d %d %s %X', $x,$y,$attr,$bitmap, $tileno) == 5)
  {
    $cells[$y][$x] = Array($attr, $bitmap, $tileno);

    $nta_data[($y>>3)*32 + ($x>>3)] = $tileno;

    $tx = $x>>4; // 0..15
    $ty = $y>>4; // 0..14
    
    $attrpos = ($ty >> 1) * 8 + ($tx >> 1);
    $shift = 2 * (($tx&1) + 2*($ty&1));
    
    $a = 0x00;
    if(isset($attr_data[$attrpos])) $a = $attr_data[$attrpos];
    
    $a = ($a & ~(3 << $shift)) | ($attr << $shift);

    $attr_data[$attrpos] = $a;
    
    $w = max($w, $x+8);
    $h = max($h, $y+8);
  }
}
print "Done\n";

ksort($nta_data);
ksort($attr_data);

$fp = fopen($NTA_OUTPUT, 'wb');
 foreach($nta_data as $c=>$b) fwrite($fp, chr($b));
fclose($fp);

$fp = fopen($ATTR_OUTPUT, 'wb');
 foreach($attr_data as $c=>$b) fwrite($fp, chr($b));
fclose($fp);

fclose($paldat);
fclose($sprbin);

$im = ImageCreate($w,$h);
foreach($palette as $rgb)
  ImageColorAllocate($im, ($rgb>>16)&0xFF, ($rgb>>8)&0xFF, $rgb&0xFF);
foreach($cells as $y => $xs)
  foreach($xs as $x => $data)
  {
    for($yp=0; $yp<8; ++$yp)
      for($xp=0; $xp<8; ++$xp)
      {
        $c = (int)$data[1][$yp*8+$xp] + $data[0]*4;
        ImageSetPixel($im, $x+$xp, $y+$yp, $c);
      }
  }
ImagePng($im, $SAMPLE_OUTPUT);
ImageDestroy($im);
