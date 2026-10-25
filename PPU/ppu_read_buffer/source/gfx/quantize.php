<?php

require 'nes_palette.php';

$SOURCE_IMAGE = $argv[1];
$RESULT_IMAGE = $argv[2];
$RESULT_DATA  = $argv[3];
$MAX_ROUNDS   = (int)$argv[4];

// Convert gamma-corrected RGB into linear RGB
function GammaRGB($rgb)
{
  $r = ($rgb >> 16) & 0xFF; 
  $g = ($rgb >>  8) & 0xFF; 
  $b = ($rgb >>  0) & 0xFF; 
  $y = ($r*299 + $g*587 + $b*114);// / 255e3;
  $r = pow(($r/255.0), 2.2);
  $g = pow(($g/255.0), 2.2);
  $b = pow(($b/255.0), 2.2);
  return Array( $r, $g, $b, $y, $rgb );
}

function MakeCombinations($palette)
{
  $result = Array();
/*
  foreach($palette as $c1 => $rgb1)
  {
    // Make 1-color combinations
    $result[] = Array( Array($c1), $rgb1 );
    // Make 2-color combinations
    foreach($palette as $c2 => $rgb2)
      if($c2 >= $c1)
      {
        if($c2 > $c1)
          $result[] = Array( Array($c1,$c2), Array( ($rgb1[0]+$rgb2[0]),
                                                    ($rgb1[1]+$rgb2[1]),
                                                    ($rgb1[2]+$rgb2[2]) ) );
        foreach($palette as $c3 => $rgb3)
        {
          if($c3 >= $c2)
          {
            if($c3 > $c2 || $c2 != $c1)
            {
              $result[] = Array( Array($c1,$c2,$c3), Array( ($rgb1[0]+$rgb2[0]+$rgb3[0]),
                                                            ($rgb1[1]+$rgb2[1]+$rgb3[1]),
                                                            ($rgb1[2]+$rgb2[2]+$rgb3[2]) ) );
            }
          }
        }
      }
  }
*/

/**/
  foreach($palette as $c1 => $rgb1)
  foreach($palette as $c2 => $rgb2) if($c2 >= $c1)
  foreach($palette as $c3 => $rgb3) if($c3 >= $c2)
  foreach($palette as $c4 => $rgb4) if($c4 >= $c3)
    $result[] = Array( Array($c1,$c2,$c3,$c4), Array( ($rgb1[0]+$rgb2[0]+$rgb3[0]+$rgb4[0]),
                                                      ($rgb1[1]+$rgb2[1]+$rgb3[1]+$rgb4[1]),
                                                      ($rgb1[2]+$rgb2[2]+$rgb3[2]+$rgb4[2]) ) );
/**/
/*
  foreach($palette as $c1 => $rgb1)
  foreach($palette as $c2 => $rgb2) if($c2 >= $c1)
  foreach($palette as $c3 => $rgb3) if($c3 >= $c2)
  foreach($palette as $c4 => $rgb4) if($c4 >= $c3)
  foreach($palette as $c5 => $rgb5) if($c5 >= $c4)
  foreach($palette as $c6 => $rgb6) if($c6 >= $c5)
  foreach($palette as $c7 => $rgb7) if($c7 >= $c6)
  foreach($palette as $c8 => $rgb8) if($c8 >= $c7)
    $result[] = Array( Array($c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8),
          Array( ($rgb1[0]+$rgb2[0]+$rgb3[0]+$rgb4[0]+$rgb5[0]+$rgb6[0]+$rgb7[0]+$rgb8[0]),
                 ($rgb1[1]+$rgb2[1]+$rgb3[1]+$rgb4[1]+$rgb5[1]+$rgb6[1]+$rgb7[1]+$rgb8[1]),
                 ($rgb1[2]+$rgb2[2]+$rgb3[2]+$rgb4[2]+$rgb5[2]+$rgb6[2]+$rgb7[2]+$rgb8[2]) ) );
*/
/*
  foreach($palette as $c1 => $rgb1)
  foreach($palette as $c2 => $rgb2) if($c2 >= $c1)
    $result[] = Array( Array($c1,$c2), Array( ($rgb1[0]+$rgb2[0]),
                                              ($rgb1[1]+$rgb2[1]),
                                              ($rgb1[2]+$rgb2[2]) ) );
*//*
  foreach($palette as $c1 => $rgb1)
  {
    // Make 1-color combinations
    $result[] = Array( Array($c1), $rgb1 );
  }
*/
  return $result;
}
function ColorDifference($a, $b)
{
  #$c0 = pow($a[0] - $b[0], 1/2.2);
  #$c1 = pow($a[1] - $b[1], 1/2.2);
  #$c2 = pow($a[2] - $b[2], 1/2.2);
  $c0 = ($a[0] - $b[0]);
  $c1 = ($a[1] - $b[1]);
  $c2 = ($a[2] - $b[2]);
  return $c0*$c0 + $c1*$c1 + $c2*$c2;
}

function DitherPixel($pixel, $palette, $combinations, $n_elements = 4)
{
  $result = Array();
  #print_r($palette);

  while(count($result) < $n_elements)
  {
    $color_so_far = Array(0,0,0);
    foreach($result as $index)
      for($p=0; $p<3; ++$p)
        $color_so_far[$p] += $palette[$index][$p];

    $best_difference = null;
    $chosen          = null;

    $n_sofar = count($result);

    // For each combination, see how it would help
    foreach($combinations as $info)
    {
      $n_mix = count($info[0]);
      $limit = (int)(($n_elements - $n_sofar) / $n_mix);
      for($n=1; $n<= $limit ; ++$n)
      {
        $mul = 1 / ($n_sofar + $n_mix * $n); // How to achieve a mathematical mean
        $test = Array
        (
          ($color_so_far[0] + $info[1][0] * $n) * $mul,
          ($color_so_far[1] + $info[1][1] * $n) * $mul,
          ($color_so_far[2] + $info[1][2] * $n) * $mul
        );
        $difference = ColorDifference($pixel, $test);
        if(!isset($best_difference) || $difference < $best_difference)
        {
          $best_difference = $difference;
          $chosen          = $info;
        }
      }
    }
    // Add the chosen color(s) into the mix
    foreach($chosen[0] as $c)
      $result[] = $c;
  }
  // Sort the result according to luma
  usort($result,
    function($a,$b)use($palette)
      { return $palette[$a][3]-$palette[$b][3]; });
  return $result;
}

// Dither given image using the given palette
function DitherImage($pixels, $palette, $combinations, $fn)
{
  global $dither8x8;
  $cache = Array();

  $wid = 0;
  $hei = count($pixels);
  foreach($pixels as $y => $xspan)
  {
    $wid = count($xspan);
    break;
  }
  
  $im2 = ImageCreateTrueColor($wid, $hei);
  $result = Array();
  
  $count   = 16;
  $uncount = 1/$count;
  $ypmask = 7;
  $xpmask = 7;
  
  foreach($pixels as $y => $xspan)
  {
    printf("\rQuantizing... %d/%d", $y, $hei); flush();
    foreach($xspan as $x => $pixel)
    {
      $rgb = $pixel[4];
      $mix = &$cache[$rgb];
      if(!isset($mix))
      {
        $mix = DitherPixel($pixel, $palette, $combinations, $count);
      }
      
      $yp = ($y % 8) & $ypmask;
      $xp = ($x % 8) & $xpmask;
      
      #print_r($mix);

      $choose = $mix[ (int) ($dither8x8[$yp][$xp] * $count / 64) ];

      #$choose = $mix[ $dither8x8[$y % 8][$x % 8] >> (6-2) ];

      ImageSetPixel($im2, $x,$y, $palette[$choose][4]);
      
      $m = Array(0,0,0);
      foreach($mix as $c)
      {
        $m[0] += $palette[$c][0];
        $m[1] += $palette[$c][1];
        $m[2] += $palette[$c][2];
      }
      $m[0] *= $uncount;
      $m[1] *= $uncount;
      $m[2] *= $uncount;
      $m[5] = $choose;
      $result[$y][$x] = $m;
      unset($mix);
    }
  }
  ImagePng($im2, $fn);
  $result[-1] = $im2;
  return $result;
}


// Load NES palette
$palette = Array();
for($c=0; $c<64; ++$c)
{
  $signal = GenNTSCsignal($c, 0, 12);
  $rgb = DecodeNTSCsignal($signal, 0,12, 3.9);
  $palette[$c] = GammaRGB($rgb);
}

// Generate 8x8 ordered dithering matrix
$dither8x8 = Array();
for($y=0; $y<8; ++$y)
  for($x=0; $x<8; ++$x)
  {
    $q = $x ^ $y;
    $p = (($x & 4)>>2) + (($x & 2)<<1) + (($x&1)<<4);
    $q = (($q & 4)>>1) + (($q & 2)<<2) + (($q&1)<<5);
    $dither8x8[$y][$x] = $p+$q;
  }

/*for($y=0; $y<8; ++$y)
{
  for($x=0; $x<8; ++$x)
    printf("%3d", ((int)($dither8x8[$y][$x] * 16 / 64)));
  print "\n";
}
exit;*/

// Load source image

$im = ImageCreateFromPng($SOURCE_IMAGE);
$sx = ImageSx($im);
$sy = ImageSy($im);
/* Convert the image into a gamma-corrected array of pixels */
$pixels = Array();
for($y=0; $y<240; ++$y)
for($x=0; $x<$sx; ++$x)
  $pixels[$y][$x] = GammaRGB(0x000000);

for($y=0; $y<$sy; ++$y)
for($x=0; $x<$sx; ++$x)
  $pixels[$y+6][$x] = GammaRGB(ImageColorAt($im, $x,$y));

$sy += 16;


/* Step 1:
     Create four 4-color palettes that best represent the input image
     in total. Since the 0th color of the palettes is always black,
     we have four 3-color palettes actually.
     
     For now, we'll go with a simplified algorithm. Hardcoded palette!
     
     Later, we'll see if the image is refined by substituting some colors.
*/

$f = function($i)
{
  global $palette;
  $p = $palette[$i];
  $p[5] = $i;
  return $p;
};
$palettes = Array
(
/* // For mountains
  0 => Array($f(0x0F),$f(0x21),$f(0x11),$f(0x30)), // grayscale
  1 => Array($f(0x0F),$f(0x21),$f(0x28),$f(0x30)), // greens against gray
  2 => Array($f(0x0F),$f(0x02),$f(0x1A),$f(0x21)), // edge between gray and blue
  3 => Array($f(0x0F),$f(0x0A),$f(0x28),$f(0x33))  // light greens against white
*/
/* / I don't approve this...
  0 => Array($f(0x0F),$f(0x2C),$f(0x11),$f(0x30)), // grayscale
  1 => Array($f(0x0F),$f(0x22),$f(0x28),$f(0x30)), // greens against gray
  2 => Array($f(0x0F),$f(0x02),$f(0x2A),$f(0x21)), // edge between gray and blue
  3 => Array($f(0x0F),$f(0x19),$f(0x28),$f(0x22))  // light greens against white
*/
  // For Thomas Kinkade's #50
  0 => Array($f(0x0F),$f(0x10),$f(0x02),$f(0x30)),
  1 => Array($f(0x0F),$f(0x33),$f(0x38),$f(0x30)),
  2 => Array($f(0x0F),$f(0x37),$f(0x28),$f(0x3D)),
  3 => Array($f(0x0F),$f(0x3B),$f(0x17),$f(0x36))
);
$combinations = Array
(
  0 => MakeCombinations($palettes[0]),
  1 => MakeCombinations($palettes[1]),
  2 => MakeCombinations($palettes[2]),
  3 => MakeCombinations($palettes[3])
);

/* Dither the image fully using all of the four palettes. */
$dithereds = Array();
for($p=0; $p<4; ++$p)
{
  $dithereds[$p] = DitherImage($pixels, $palettes[$p], $combinations[$p], "out{$p}.png");
  print "\nwrote out$p.png\n";
}

$best_totaldiff = null;
$tried = Array();

for($round=0; $round<$MAX_ROUNDS; ++$round)
{
  $s = '';
  for($p=0; $p<4; ++$p)
    for($n=0; $n<4; ++$n)
      $s .= chr( $palettes[$p][$n][5] );
  $tried[$s] = true;
  
  $attributetable = Array();

  $total_diff = 0;
  /* For each 16x16 block, choose the best representation from the source images */
  $im2 = ImageCreateTrueColor($sx,$sy);
  for($y=0; $y<$sy; $y+=16)
  {
    print "\rChoosing tiles, $y/$sy";
    for($x=0; $x<$sx; $x+=16)
    {
      $bestdiff = null;
      $chosen   = 0;
      
      $downx = 1;
      $downy = 1;
      $downmul = 1 / ($downx * $downy);
      for($p=0; $p<4; ++$p)
      {
        $diff = 0;
        for($py=0; $py<16; $py += $downy)
          for($px=0; $px<16; $px += $downx)
          {
            if($downx != 1 || $downy != 1)
            {
              $a = Array(0,0,0);
              $b = Array(0,0,0);
              for($dy=0; $dy<$downy; ++$dy)
              for($dx=0; $dx<$downx; ++$dx)
              {
                for($n=0; $n<3; ++$n)
                {
                  $a[$n] += $dithereds[$p][$y+$py+$dy][$x+$px+$dx][$n];
                  $b[$n] += $pixels[$y+$py+$dy][$x+$px+$dx][$n];
                }
              }
              for($n=0; $n<3; ++$n)
              {
                $a[$n] *= $downmul;
                $b[$n] *= $downmul;
              }
            }
            else
            {
              $a = $dithereds[$p][$y+$py][$x+$px];
              $b = $pixels[$y+$py][$x+$px];
            }
            $diff += ColorDifference($a, $b);
          }
        if(!isset($bestdiff) || $diff < $bestdiff)
        {
          $bestdiff = $diff;
          $chosen   = $p;
        }
      }
      $total_diff += $bestdiff;
      ImageCopy($im2, $dithereds[$chosen][-1], $x,$y, $x,$y, 16,16);
      for($py=0; $py<16; $py+=8)
      for($px=0; $px<16; $px+=8)
        $attributetable[$y+$py][$x+$px] = $chosen;
    }
  }
  
  if(!isset($best_totaldiff) || $total_diff < $best_totaldiff)
  {
    $best_totaldiff  = $total_diff;
    $backup_palettes = $palettes;
    for($p=0; $p<4; ++$p)
    {
      $dither_backups[$p] = $dithereds[$p];
      $comb_backups[$p] = $combinations[$p];
    }

    print "\nGot a new good result (score: $total_diff) with palette:\n";
    for($p=0; $p<4; ++$p)
    {
      print ".byte ";
      for($n=0; $n<4; ++$n)
        printf("$%02X,", $palettes[$p][$n][5]);
      print "\n";
    }
    print "Writing $RESULT_IMAGE and $RESULT_DATA\n";
    
    ImagePng($im2, $RESULT_IMAGE);
    ImageDestroy($im2);
    
    $fp = fopen($RESULT_DATA, 'w');
    for($p=0; $p<4; ++$p)
    {
      fwrite($fp, ".palette ");
      for($n=0; $n<4; ++$n)
        fprintf($fp, "$%02X,", $palettes[$p][$n][5]);
      fwrite($fp, "\n");
    }
    for($y=0; $y<$sy; $y+=8)
      for($x=0; $x<$sx; $x+=8)
      {
        $p = $attributetable[$y][$x];
        $s = '';
        for($py=0; $py<8; ++$py)
          for($px=0; $px<8; ++$px)
          {
            $d = $dithereds[$p][$y+$py][$x+$px][5];
            /*$c = '?';
            for($n=0; $n<4; ++$n)
              if($d[5] == $palettes[$p][$n][5])
                { $c = $n; break; }*/
            $s .= $d;
          }
        fprintf($fp, "%-4d %-4d %d %s\n", $x,$y, $p, $s);
      }
    fclose($fp);
  }
  
  // Issue a random change to one of the palettes
  $palettes = $backup_palettes;
  for($p=0; $p<4; ++$p)
  {
    $dithereds[$p] = $dither_backups[$p];
    $combinations[$p] = $comb_backups[$p];
  }
  $n_attempts = 0;
  do {
    $which_palette = rand(0,3); $p = $which_palette;
    $which_color   = rand(1,3);
    $current_color = $palettes[$p][$which_color][5];

    if($n_attempts < 64)
    {
      $new_color = $current_color;
      $new_color = (((($new_color >> 4) + rand(-1,1)) & 3) << 4)
                 | ((($new_color & 0x0F) + rand(13,15)) % 14);
    }
    else
    {
      $new_color = rand(0x0,0xD) | (rand(0,3)<<4);
    }
    $s = '';
    for($q=0; $q<4; ++$q)
      for($n=0; $n<4; ++$n)
        $s .= chr( $palettes[$q][$n][5] );
    $s[ $which_palette*4 + $which_color] = chr($new_color);
    ++$n_attempts;

  } while($new_color == 0x1D || $new_color == 0x20
       || $new_color == 0x0D
       || $new_color == $current_color
       || isset($tried[$s]));

  $palettes[$p][$which_color] = $f($new_color);

  print "\rTrying for $p: ";
  for($n=0; $n<4; ++$n)
    printf("$%02X,", $palettes[$p][$n][5]);
  print "\n";

  $combinations[$p] = MakeCombinations($palettes[$p]);
  $dithereds[$p] = DitherImage($pixels, $palettes[$p], $combinations[$p], "test{$p}.png");
}
