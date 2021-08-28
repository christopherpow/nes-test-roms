<?php

// OLD
exit;

$IN_IMAGE  = $argv[1];//'image_b.png';
$OUT_IMAGE = $argv[3];//'test2b.png';
$LOGFILE   = $argv[2];//'cv2map2b.lst';

$im = ImageCreateFromPng($IN_IMAGE);
$w = ImageSX($im);
$h = ImageSY($im);

$squares = Array();
$squaresmap = Array();
$squarestrans = Array();

// Find the palettes
for($y=0; $y<$h; $y+=8)
{
  for($x=0; $x<$w; $x+=8)
  {
    $square = Array();
    for($sy=0; $sy<8; ++$sy)
    for($sx=0; $sx<8; ++$sx)
    {
      $c = ImageColorAt($im, $x+$sx, $y+$sy);
      $square[$c] = $c;
    }
    sort($square);
    $s = join(',', $square);
    $squares[$s] = $square;
    $squaresmap[$y][$x] = $s;
  }
}

// Merge palettes
for(;;)
{
  foreach($squares as $n=>$square)
  {
    $c = count($square);
    if($c < 4)
    {
      foreach($squares as $n2=>$square2)
        if(count($square2) > $c)
        {
          foreach($square as $p)
            if(array_search($p, $square2) === false)
              continue 2;
          foreach($squaresmap as &$xlist)
            foreach($xlist as &$s)
              if($s == $n)
                $s = $n2;
          unset($s);
          unset($xlist);
          unset($squares[$n]);
          continue 3;
        }
    }
  }
  break;
}
$c=0;
$revsquares = Array();
foreach($squares as $s => &$s)
{
  $revsquares[$c] = explode(',', $s);
  $s = $c++;
}
unset($s);

#print_r($revsquares);exit;

// Find bitmaps
$bitmaps = Array();
foreach($squaresmap as $y=>$xlist)
  foreach($xlist as $x => $squarespec)
  {
    $square = Array();
    foreach(explode(',', $squarespec) as $c=>$s)
      $square[(int)$s] = $c;

    $bitmap = '';
    for($sy=0; $sy<8; ++$sy)
    for($sx=0; $sx<8; ++$sx)
    {
      $c = ImageColorAt($im, $x+$sx, $y+$sy);
      $bitmap .= $square[$c];
    }
    @$bitmaps[$bitmap] += 1;
  }

function td($a, $b)
{
  $r = 0;
  for($c=0; $c<64; ++$c)
    if($a[$c] != $b[$c])
      ++$r;
  return $r;
}

if(true)
{
  $limit = 256;
  $renamed = Array();

  $diffs = Array();
  for(;;)
  {
    $c = count($bitmaps);
    if($c < $limit)
      break;
    print "$c bitmaps...\r";
    // Sort the bitmaps in order of commonness
    asort($bitmaps);
    // Find two most similar bitmaps, and merge them
    $bestl = 99999;
    $besta = 0; $bestb = 0;
    foreach($bitmaps as $as=>$a)
      foreach($bitmaps as $bs=>$b)
      {
        //if($as == $bs) continue;
        if($b < $a || $as == $bs) continue;
        $l = &$diffs[$as][$bs];
        if(!isset($l))
          $l = td($as,$bs);
        if($l < $bestl)
          { $besta = $as; $bestb = $bs; $bestl = $l; }
        unset($l);
      }
    #print "Merging with $bestl:\n";
    #print "  $besta\n";
    #print "  $bestb\n";
    unset($diffs[$besta]);
    $renamed[$besta] = $bestb;
    $bitmaps[$bestb] += $bitmaps[$besta];
    unset($bitmaps[$besta]);
  }
  print "\n";

  // Rework the image with changed bitmaps
  foreach($squaresmap as $y=>$xlist)
    foreach($xlist as $x => $squarespec)
    {
      $square = Array();
      foreach(explode(',', $squarespec) as $c=>$s)
        $square[(int)$s] = $c;

      $bitmap = '';
      for($sy=0; $sy<8; ++$sy)
      for($sx=0; $sx<8; ++$sx)
      {
        $c = ImageColorAt($im, $x+$sx, $y+$sy);
        $bitmap .= $square[$c];
      }
      for(;;)
      {
        print "b($bitmap)\n";
        $s = @$renamed[$bitmap];
        if(!isset($s)) break;
        $bitmap = $s;
      }
      print "--\n";
      $remapped[$y][$x] = Array($bitmap, $squares[$squarespec]);
    }
  $s = json_encode($remapped);
  file_put_contents($LOGFILE, $s);
  #print $s;
}

$remapped = file_get_contents($LOGFILE);
$remapped = str_replace("\n", '', $remapped);
$remapped = json_decode($remapped, true);

foreach($squaresmap as $y=>$xlist)
  foreach($xlist as $x => $squarespec)
  {
    $bitmap = $remapped[$y][$x][0];
    $square = $remapped[$y][$x][1];
    
    for($sy=0; $sy<8; ++$sy)
    for($sx=0; $sx<8; ++$sx)
    {
      $p = $revsquares[$square][(int)$bitmap[$sy*8+$sx]];
      ImageSetPixel($im, $x+$sx, $y+$sy, $p);
    }
  }
ImagePng($im, $OUT_IMAGE);
