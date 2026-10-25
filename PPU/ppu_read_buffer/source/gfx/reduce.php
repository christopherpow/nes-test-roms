<?php

$IN_DATA   = $argv[1];
$BEGIN_Y   = $argv[2];
$END_Y     = $argv[3];
$OUT_DATA  = $argv[4];
$LIMIT     = $argv[5]; // Number of tiles
$CHR_FILE  = $argv[6];
$RESERVED_TILES = (int) $argv[7];
$SPR_FILE  = $argv[8]; // Separate sprite-chr file if available. If not, same will be used as for tiles.
$SPR_INDEX = (int) $argv[9];

$LIMIT -= $RESERVED_TILES;

$original = Array();
$bitmaps  = Array();
$lines = file($IN_DATA);

$sprites = Array();

foreach($lines as $line)
{
  if(sscanf($line, ".sprite %d %d %d %s", $x,$y, $attr, $pattern) == 4)
  {
    if($y < $END_Y && $y+16 > $BEGIN_Y)
    {
      // This sprite is visible within this region
      $sprites[] = Array($x,$y,$attr, $pattern);
    }
  }
  else
  {
    if($line[0] == '.') continue;
    sscanf($line, '%d %d %d %s', $x,$y,$attr,$bitmap);
    if($y >= $BEGIN_Y && $y < $END_Y)
    {
      @$bitmaps[$bitmap] += 1;
      $original[$y][$x] = Array($attr, $bitmap);
    }
  }
}

$sprite_bitmaps = Array();
foreach($sprites as $sprite) $sprite_bitmaps[$sprite[3]] = true;
$num_unique_sprite_bitmaps = count($sprite_bitmaps);
unset($sprite_bitmaps);

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
  $renamed = Array();

  $diffs = Array();
  $original_count = count($bitmaps);
  for(;;)
  {
    $num_have = count($bitmaps);
    $num_orig = $original_count;
    $n = $num_unique_sprite_bitmaps * 2;
    $num_extra_str = " (+$n)";
    if(empty($SPR_FILE))
    {
      // If sprite-chr is going in same file, add the sprite bitmap count here.
      $num_have += $n;
      $num_orig += $n;
      $num_extra_str = " (-$n)";
    }

    print "$num_orig -> [$num_have]$num_extra_str -> $LIMIT bitmaps...\r";
    if($num_have <= $LIMIT)
      break;


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
  
  $fp = fopen($OUT_DATA, 'w');
  $ch = fopen($CHR_FILE, 'wb');
  $sp = null;
  if(!empty($SPR_FILE)) $sp = fopen($SPR_FILE, 'wb');

  $tiles = Array();
  $sptiles = Array();

  foreach($sprites as $sprite)
  {
    if(empty($SPR_FILE))
    {
      $bitmap = $sprite[3];
      if(!isset($tiles[$bitmap]))
      {
        $tileno = count($tiles);
        $tiles[$bitmap] = $tileno;
        $tiles[$bitmap . "DUMMY"] = 0; // In order to increment tile-count by 2
        fwrite($ch, EncodeTile($bitmap));
      }
      else
        $tileno = $tiles[$bitmap];
    }
    else
    {
      $bitmap = $sprite[3];
      if(!isset($sptiles[$bitmap]))
      {
        $tileno = count($sptiles);
        $sptiles[$bitmap] = $tileno;
        $sptiles[$bitmap . "DUMMY"] = 0; // In order to increment tile-count by 2
        fwrite($sp, EncodeTile($bitmap));
      }
      else
        $tileno = $sptiles[$bitmap];
    }

    fprintf($fp,
      ".sprite %d %d %d \$%02X\n",
      $sprite[0], // x
      $sprite[1], // y
      $sprite[2], // attr
      $tileno + $SPR_INDEX
    );
  }

  // Rework the image with changed bitmaps
  foreach($original as $y=>$xlist)
    foreach($xlist as $x=>$data)
    {
      $attr   = $data[0];
      $bitmap = $data[1];
      for(;;)
      {
        #print "b($bitmap)\n";
        $s = @$renamed[$bitmap];
        if(!isset($s)) break;
        $bitmap = $s;
      }
      
      if(!isset($tiles[$bitmap]))
      {
        $tileno = count($tiles);
        $tiles[$bitmap] = $tileno;
        fwrite($ch, EncodeTile($bitmap));
      }
      else
        $tileno = $tiles[$bitmap];

      fprintf($fp, "%-4d %-4d %d %s %02X\n", $x,$y, $attr, $bitmap, $tileno + $RESERVED_TILES);
    }
  fclose($fp);
  fwrite($ch, str_pad('', 0x1000-((count($tiles)+$RESERVED_TILES)*16),
              'Secret Message. '));
  fclose($ch);
  if(!empty($SPR_FILE))
    fclose($sp);
}

function EncodeTile($gfx)
{
  $n_tiles = strlen($gfx) >> 6;

  $result = str_pad('', 16 * $n_tiles, chr(0));
  $pos = 0;
  for($t=0; $t<$n_tiles; ++$t)
    for($y=0; $y<8; ++$y)
    {
      $byte1 = 0x00;
      $byte2 = 0x00;
      for($x=0; $x<8; ++$x)
      {
        $c = (int) $gfx[$pos++];
        $byte1 |= ($c & 1) << (7-$x);
        $byte2 |= ($c >>1) << (7-$x);
      }
      //if($t>=1) $byte1=$byte2=0xFF;

      $result[$y+0 + $t*16] = chr($byte1);
      $result[$y+8 + $t*16] = chr($byte2);
    }
  return $result;
}
