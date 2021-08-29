<?php

function GenNTSCsignal($pixel, $phase, $length=8) // Generate NTSC signal corresponding to NES color
{
  // Voltage levels, relative to synch voltage
  $levels = Array(.350, .518, .962,1.550,  // Signal low
                 1.094,1.506,1.962,1.962); // Signal high
  // Decode the NES color ("eeellcccc" format).
  $color = ($pixel & 0x0F);    // 0..15 "cccc"
  $level = ($pixel >> 4) & 3;  // 0..3  "ll"
  $emphasis = ($pixel >> 6);   // 0..7  "eee"
  if($color > 13) { $level = 1;  } // For colors 14..15, level 1 is forced.

  $result = Array();
  for($p=0; $p<$length; ++$p)
  {
    // Generate the square wave:
    $sig = $levels[ $level + 4 * ($color <= 12*(($color+$phase)%12 < 6)) ];
    // When de-emphasis bits are set, some parts of the signal are attenuated:
    if($emphasis & (0264513 >> (3*(($phase%12)>>1)))) $sig *= 0.746;
    // Normalize the signal to 0..1 range, and save:
    $result[] = ($sig - $levels[1]) / ($levels[7]-$levels[1]);
    ++$phase;
  }
  return $result;
}

function DecodeNTSCsignal($signal, $begin,$end, $phase, $saturation = 1.7, $brightness = 1.0)
{
  $length = $end-$begin; $factor = $brightness/($length);  
  if($begin < 0) $begin = 0;
  $c = count($signal);
  if($end > $c)  $end = $c;
  $y = 0; $i = 0; $q = 0;
  for($p = $begin; $p < $end; ++$p)
  {
    $value = $signal[$p] * $factor;
    $y += $value;
    $value *= $saturation;
    $i += $value * cos( (3.141592653/6) * ($phase + $p));
    $q += $value * sin( (3.141592653/6) * ($phase + $p));
  }
  $gamma = 2.0;
  return
    0x10000*(int)min(255, 255.95 * pow(max(0,$y +  0.946882*$i +  0.623557*$q), 2.2 / $gamma))
  + 0x00100*(int)min(255, 255.95 * pow(max(0,$y + -0.274788*$i + -0.635691*$q), 2.2 / $gamma))
  + 0x00001*(int)min(255, 255.95 * pow(max(0,$y + -1.108545*$i +  1.709007*$q), 2.2 / $gamma));
}  

