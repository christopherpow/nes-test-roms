<?php

define('USE_CHARMAP', true); // Whether to rewrite the character set
define('USE_VWF', true);

require 'inc/dte_reader.php';
require 'inc/font.php';

#FontRender("ignored");
#exit;

$inputfn  = $argv[1];
$outputfn = $argv[2];
$fontfn   = $argv[3];

$record = null;
$records = Array();

read_file($inputfn);

$charmap = Array();
$newline        = USE_CHARMAP ? 1 : 10;
$charset_offset = USE_CHARMAP ? 2 : 0x20;

$unique_tiles = Array();
$used_chars = Array();

function RegisterTile($tile, $character, $n = null)
{
  global $unique_tiles, $used_chars, $charmap;

  if(isset($unique_tiles[$tile]))
    return $unique_tiles[$tile];

  if(!isset($n))
  {
    if(strlen($character) >= 2)
    {
      $n = 0x7F; // FIXME: This number should be DTE_BEGIN - 1
      while(isset($used_chars[$n])) --$n;
    }
    else
    {
      $n = 1;
      while(isset($used_chars[$n])) ++$n;
    }
  }
  
  if($character == '«') $character = '"';
  if($character == '»') $character = '"';
  $charmap[$n] = $character;

  $used_chars[$n] = true;

  $unique_tiles[$tile] = $n;

  return $n;
}
function FontTile($index)
{
  global $ascii_chars;
  
  if($index === '«') return "6C6C360000000000";
  if($index === '»') return "6C6CD80000000000";
  
  if(is_string($index)) $index = ord($index) - 0x20;
  $s = '';
  for($y=0; $y<8; ++$y) $s .= @sprintf('%02X', $ascii_chars[$index*8+$y]);
  return $s;
}
function RenderedTile($render, $x)
{
  $s = '';
  for($y=0; $y<8; ++$y) $s .= @sprintf('%02X', $render[$x][$y]);
  return $s;
}

RegisterTile( FontTile(0x5F), chr(0xFF), 0xFF);
if(USE_CHARMAP)
{
  RegisterTile( "\n", "\n", 0x01 );
  /* Just include some predefined characters that
   * we know we will use in the alphabet in certain
   * positions. In particular, '0' should be xxxx0000,
   * and it's nice if 'A' is xxxx1010.
   */
  //    23456789ABCDEF0123456789ABCDEF012..
  $s = " Israel.,d'oh!0123456789ABCDEF:)";
  for($a=0; $a<strlen($s); ++$a)
    RegisterTile( FontTile( $s[$a] ), $s[$a], $charset_offset + $a );
}
else
{
  RegisterTile( "\n", "\n", 0x0A );
  for($c=0; $c<96; ++$c)
    RegisterTile( FontTile($c), chr($c + 0x20), $c + 0x20);
}

if(USE_VWF)
{
  $vwf_strings = Array
  (
  //  'This next test will take a while. For',
  //  'your entertainment, art is provided.',
  //  'Contemplate on the art while the',
  //  'test is in progress.',
  //  'Sequential PPU reading',
  //  'Sequential PPU writes',
  //  'Testing basic PPU memory I/O.',
   // 'Setting PPU address to ',
   // 'PPU memory range',
   // 'CPU RAM',
   // 'DMA with ',
  //  'is ',
  //  'in ',
    #'No-Hit',
    'Hit',
    'Failed ',
    'test',
    'entertainment,',
    'Contemplate',
    'rovi',
    'on the',
    'ill', 'il', 'li',
    'is ', #'ll.',
    'hic', 'rti',
    'dist',
   // 'the ',
    'with',
   //   'does not work.',
 //   'A read from ',
 //   'A writes to ',
 //   'should',
 //   'whole',
 //   'region, ',
 //   'read buffer.'
  );

  foreach($vwf_strings as &$string)
    $string = Array($string, '');
  unset($string);
}

if(USE_CHARMAP)
{
  for($round = 0; $round < 1; ++$round)
  {
    /* ROUND 0: Do everything that does not utilize VWF at all
     * ROUND 1: Do VWF
     */
    foreach($records as $record_no => &$record)
      if(is_array($record))
      {
        $s = $record['elem'];
        $b = strlen($s);

        $has_vwf = false;
        if(USE_VWF)
          foreach($vwf_strings as $stringdata)
            if(strpos($s, $stringdata[0]) !== false)
            {
              $has_vwf = true;
              break;
            }
        
        //if($round == 0 && $has_vwf) continue;
        //if(isset($record['treated'])) continue;

        for($a=0; $a < $b; ++$a)
        {
          $c = ord($s[$a]);
          if($c == 0x00 || $c == 0xFF) continue;
          
          if(USE_VWF)
          {
            foreach($vwf_strings as &$stringdata)
            {
              $string = $stringdata[0];
              if(substr_compare($s, $string, $a, strlen($string)) == 0)
              {
                if($stringdata[1] == '')
                {
                  $result = FontRender($string);
                  DumpRenderedText($result, $result['lastx']);
                  $replacement = '';
                  $x1=0; $x2=0;
                  foreach($result as $x => $ylist) if(is_int($x)) { $x1=$x; break; }
                  foreach($result as $x => $ylist) if(is_int($x)) { $x2=$x+1; }

                  foreach($result as $x => $ylist)
                    if(is_int($x))
                    {
                      $tile = RenderedTile($result, $x);
                      $px = (int)((0+$x-$x1) * strlen($string) / ($x2-$x1));
                      $px2= (int)((1+$x-$x1) * strlen($string) / ($x2-$x1));
                      $piece = substr($string, $px, $px2-$px);
                      print "$x: $x1,$x2 -> $px,$px2 ($piece)\n";
                      $n = RegisterTile($tile, $piece, null);
                      $replacement .= chr($n);
                    }
                  $stringdata[1] = $replacement;
                }
                $s = substr_replace($s, $stringdata[1], $a, strlen($string));
                $a += strlen($stringdata[1]) - 1;
                $b = strlen($s);
                unset($stringdata);
                continue 2;
              }
            }
            unset($stringdata);
          }
          $n = RegisterTile( $s[$a]=="\n" ? "\n" : FontTile($s[$a]), $s[$a] );
          $s[$a] = chr($n);
        }
        $record['elem'] = $s;
        $record['treated'] = true;
      }
  }
  unset($record);

  $n = 0xFF;
  while(!isset($used_chars[$n-1])) --$n;
  $begin = $n;
  $end   = 0xFE;

  asort($charmap);

  $charset  = str_pad('', 256, "\xFF");
  $charset2 = str_pad('', 256, "\x00");

  foreach($charmap as $new_chr => $orig_char)
    if( $new_chr >= $charset_offset)
    {
      $charset[ $new_chr-$charset_offset ]  = $orig_char[0];
      if(strlen($orig_char) > 1)
        $charset2[ $new_chr-$charset_offset ] = $orig_char[1];
    }

  while($charset[ strlen($charset) - 1 ] == "\xFF")
  {
    $charset  = substr($charset,  0, -1);
    $charset2 = substr($charset2, 0, -1);
  }
  $charset2_begin = 0;
  while($charset2[$charset2_begin] == "\x00")
    ++$charset2_begin;

  $s = Array();
  foreach($charmap as $orig_char => $new_chr)
    $s[] = sprintf("%-3s -> %02X", make_string( chr($orig_char) ), ord($new_chr) );
  foreach($s as $x => $s) { printf("%-14s", $s); if($x%5==4) print"\n"; } print "\n";
  printf("begin=\$%02X, end=\$%02X, chars=%d\n", $begin, $end, strlen($charset));
}

$fp = fopen($fontfn, 'wb');
foreach($unique_tiles as $tiledata => $index)
{
  if(strlen($tiledata) == 16)
  {
    for($plane = 0; $plane < 2; ++$plane)
    {
      fseek($fp, (($index - $charset_offset + $plane*0x80) & 0xFF) * 16);

      $color = $plane ? 2 : 1;
      if($index == 0xFF) $color = 3;

      for($n=0; $n<2; ++$n)
        for($a=0; $a<8; ++$a)
        {
          $c = hexdec(substr($tiledata, $a*2,2));
          $cc = $color;
          #if($a >= 6) $cc ^= 3;
          if(!($cc & (1 << $n)))
            $c = 0;
          fwrite($fp, chr($c));
        }
    }
  }
}
fclose($fp);

/*
if(!USE_CHARMAP)
{
  $begin = 0x7B;
  $end   = 0xFE;
  $charset = '';
  $charset_offset = 0x20;
  printf("begin=\$%02X, end=\$%02X\n", $begin, $end);
}
*/

$size = count_size();
$best_size = $size + 4000;

$best_dte     = Array();
$best_records = $records;
$first_records = $records;

for($rounds = 0; $rounds < 8; ++$rounds)
{
  $chosen_dte = Array();
  $records    = $first_records;
  $index = $end;

  while($index >= $begin)
  {
    $dte_pairs = Array();
    foreach($records as $record_no => $record)
      if(is_array($record) && strpos($record['suffix'], 'DO_DTE') !== false)
      {
        $s = $record['elem'];
        $a = 0;
        $b = strlen($s);
        for($a=0; $a+1 < $b; ++$a)
        /*  if($s[$a] != "\xFF"
          && $s[$a] != "\x00"
      # // These two rules are to avoid recursion:
      #    && ord($s[$a]) < $begin
      #    && (ord($s[$a+1]) < $begin || ord($s[$a+1] == "\xFF"))
            )*/
          {
            @$dte_pairs[ 'X' . substr($s, $a, 2) ] += 1;
          }
      }

    arsort($dte_pairs);
    $m = rand(1,2);
    if($rounds < 1) $m = 1;
    for($n=$m; $n-->0; )
      list($pair, $count) = each($dte_pairs);
    //printf("Chose pair: %s for %02X\n", make_string($pair), $index);

    $chosen_dte[$index] = $pair;
    
    $s = substr($pair, 1); $c = chr($index);
    foreach($records as $record_no => &$record)
      if(is_array($record) && strpos($record['suffix'], 'DO_DTE') !== false)
        $record['elem'] = str_replace( $s, $c, $record['elem']);
    unset($record);
    --$index;
  }

  /* Do this common-end-begin reduction only for statistics for now */
  asort($chosen_dte);
  $tmp = $chosen_dte;
  $merged = 0;
  for(;;)
  {
    $suffixes = Array();
    foreach($tmp as $index1 => $pair1)
    {
      if(!isset($suffixes[ 'X' . $pair1[2] ]))
        $suffixes[ 'X' . $pair1[2] ] = $index1;
    }

    $found = false;
    foreach($tmp as $index2 => $pair2)
      if(isset($suffixes[ 'X' . $pair2[1] ]))
      {
        $index1 = $suffixes[ 'X' . $pair2[1] ];
        if($index1 != $index2)
        {
          ++$merged;
          unset($tmp[$index1]);
          unset($tmp[$index2]);
          $found = true;
          break;
        }
      }
    if(!$found) break;
  }

  $table_size = count($chosen_dte)*2 - $merged;
  $size2 = count_size() + strlen($charset)+1 + $table_size;
  if($size2 < $best_size)
  {
    $n = count($chosen_dte);
    print "Round $rounds, Got $size2 (table=$table_size for $n elements)...\n";
    $best_size = $size2;
    $best_dte  = $chosen_dte;
    $best_records = $records;
  }
}
print "Done, got $best_size\n";
$chosen_dte = $best_dte;
$records = $best_records;

/*
  Now, split the DTE table in two tables, one for first character and
  one for the second character, and sort the DTE table in such manner
  that the suffix of one table is the same as the prefix of the other table.
*/

asort($chosen_dte);

$common_end   = Array();
$common_begin = Array();
for(;;)
{
  $suffixes = Array();
  foreach($chosen_dte as $index1 => $pair1)
  {
    if(!isset($suffixes[ 'X' . $pair1[2] ]))
      $suffixes[ 'X' . $pair1[2] ] = $index1;
  }

  $found = false;
  foreach($chosen_dte as $index2 => $pair2)
    if(isset($suffixes[ 'X' . $pair2[1] ]))
    {
      $index1 = $suffixes[ 'X' . $pair2[1] ];
      if($index1 != $index2)
      {
        $common_end[   $index1 ] = $chosen_dte[$index1];
        $common_begin[ $index2 ] = $pair2;
        unset($chosen_dte[$index1]);
        unset($chosen_dte[$index2]);
        $found = true;
        break;
      }
    }
  if(!$found) break;
}

/* Renumber the DTE */
$translate_from = '';
$translate_to   = '';
$renumbering = Array();
$new_dte = Array();
$index = $begin;
foreach($common_end as $original_index => $pair)
{
  $translate_from .= chr($original_index);
  $translate_to   .= chr($index);
  $new_dte[$index++] = $pair;
}
foreach($chosen_dte as $original_index => $pair)
{
  $translate_from .= chr($original_index);
  $translate_to   .= chr($index);
  $new_dte[$index++] = $pair;
}
foreach($common_begin as $original_index => $pair)
{
  $translate_from .= chr($original_index);
  $translate_to   .= chr($index);
  $new_dte[$index++] = $pair;
}

foreach($records as $record_no => &$record)
  if(is_array($record))
    $record['elem'] = strtr($record['elem'], $translate_from, $translate_to);
unset($record);
foreach($new_dte as $index => &$s)
  $s = strtr($s, $translate_from, $translate_to);
unset($s);

$merged = count($common_end);

$size2 = count_size() + strlen($charset)+1;
$table_size = count($new_dte)*2 - $merged;
$size2 += $table_size;

printf("From %d to %d, remain = %.2f %% (merged %d)\n",
  $size, $size2, $size2 * 100.0 / $size, $merged);

ob_start();
 printf("DTE_BEGIN=\$%02X\n".
        "DTE_END=\$%02X\n".
        "DTE_NEWLINE=%d\n".
        "DTE_CHARSET_OFFSET=%d\n".
        "DTE_CHARSET2_BEGIN=%d\n",
        $begin, $end, $newline, $charset_offset, $charset2_begin);
 printf(".segment \"RODATA\"\n");
 print ".byte 10 ; Newline, needed for text_out\n";
 
 if(strlen($charset))
   printf("DTE_CHARSET: .byte %s\n", make_string($charset));
 else
   printf("DTE_CHARSET: ; unused\n");

 if(strlen($charset2) > $charset2_begin)
   printf("DTE_CHARSET2: .byte %s\n", make_string(substr($charset2, $charset2_begin)));
 else
   printf("DTE_CHARSET2: ; unused\n");

 $s1 = '';
 $s2 = '';
 foreach($new_dte as $index => $pair)
 {
   $s1 .= $pair[1];
   $s2 .= $pair[2];
   printf("; .byte %-11s ;\$%02X\n", make_string(substr($pair,1)), $index);
 }
 
 if(strlen($s1) == 0) $s1 .= chr(0);
 if(strlen($s2) == 0) $s2 .= chr(0);
 
 printf("DTE_TABLE0: .byte %s ;part 1\n".
        "DTE_TABLE1: ",
        make_string(substr($s1, 0, strlen($s1)-$merged)));
 if($merged)
   printf(
        ".byte %s ;common part\n".
        "            ", 
        make_string(substr($s1, strlen($s1)-$merged)));
 printf(".byte %s ;part 2\n",
        make_string(substr($s2, $merged)));

foreach($records as $record)
  if(is_array($record))
    printf("%s %s %s\n", $record['prefix'], make_string($record['elem']), $record['suffix']);
  else
    print "{$record}\n";

$s = ob_get_clean();
file_put_contents($outputfn, $s);


function FontRender($s)
{
  global $ascii_chars, $character_margins, $kerning_table;
  $c = function($chr) { return ord($chr) - 0x20; };

  $length = strlen($s);

  if($length == 0) return Array();
  
  $extra = Array();
  
  for(;;)
  {
    $result = Array();
    $c2 = $c($s[0]);
    $x = -$character_margins[$c2][0];
    $extra_candidates = Array();
    
    $allow_right = -$x;

    for($a=0; $a<$length; ++$a)
    {
      if(isset($extra[$a]))
      {
        $x += $extra[$a];
      }

      $b0 = (int)(($x+8)/8) - 1;
      $modulo = ($x+8)%8;
      $b1 = $b0+1;
      
      #print "{$s[$a]}: x=$x, b0=$b0, modulo=$modulo\n";

      if($a > 0)
      {
        if($modulo == 7)
          $category = ($s[$a-1] == ' ') ? 1 : 2;
        else
          $category = ($s[$a] == ' ') ? 3 : 4;

        $m = @$extra[$a] - ($character_margins[$c1][0] /*+ $character_margins[$c1][1]*/);
        $extra_candidates[$a] = $category + $m;
      }
      else//if(false)
      {
        if($x < 0)
        {
          $category = 0;
          $m = @$extra[$a];// - ($character_margins[$c1][0] /*+ $character_margins[$c1][1]*/);
          $extra_candidates[$a] = $category + $m;
        }
      }

      $c1 = $c2;
      if($a+1 < $length) $c2 = $c($s[$a+1]);

      // Render character
      for($y=0; $y<8; ++$y)
      {
        $byte = $ascii_chars[$c1*8+$y];

        if($b0 >= 0)
          @$result[$b0][$y] |= ($byte >> $modulo) & 0xFF;
        
        $v = ((($byte << 8) >> $modulo) & 0xFF);
        #if($v)
        #{
        #  if($b1 > $lastx) $lastx = $b1;
          @$result[$b1][$y] |= $v;
        #}
        // byte1   byte2
        // 7654321076543210
        //        76543210 -- modulo=7
        //       76543210 -- modulo=6
        //      76543210 -- modulo=4
        //     76543210 -- modulo=4
        //    76543210 -- modulo=3
        //   76543210 -- modulo=2
        //  76543210 -- modulo=1
        // 76543210 -- modulo=0
      }
      $x += $kerning_table[$c1][$c2];
    }
    $allow_left = 7 - $character_margins[$c2][1];
    
    if($c2 == 0x00) $allow_left = 7;

    foreach($result as $lastx => $dummy) { }

    for($lastx = $lastx*8+7; $lastx > 0; --$lastx)
    {
      $b = (int)($lastx / 8);
      $modulo = $lastx % 8;
      $bit = 0x80 >> $modulo;
      
      $empty = true;
      for($y=0; $y<8; ++$y)
        if($result[$b][$y])
          { $empty = false; break; }
      if($empty)
      {
        unset($result[$b]);
        $lastx = $b * 8;
      }
      else
      {
        for($y=0; $y<8; ++$y)
          if($result[$b][$y] & $bit)
            break 2;
      }
    }

    //for($b=(int)(($lastx+7)/8); $b<90; ++$b)
    //  unset($result[$b]);

    #DumpRenderedText($result, $lastx);

    /* If there is space after the last pixel,
     * add an empty pixel column somewhere.
     * Candidates are, in descending order of priority:
     *
     * 1. In the beginning of a character that immediately
     *    follows a space where the modulo is 7
     * 2. In the beginning of a character that has a modulo 7
     * 3. In the beginning of a space character
     * 4. In the beginning of a random character except the first one
     */
    if(7-($lastx % 8) <= $allow_left)
      break;

    asort($extra_candidates);
    list($a, $level) = each($extra_candidates);
    $tmp = Array();
    do {
      $tmp[] = $a;
      list($a, $l) = each($extra_candidates);
    } while($l == $level);
    
    //$tmp = array_reverse($tmp);
    $a = $tmp[0];
    //$a = $tmp[ array_rand($tmp) ];
    @$extra[$a] += 1;
  }
  $result['lastx'] = $lastx;
  return $result;
}

function DumpRenderedText($result, $lastx = 99999)
{
  for($y=0; $y<8; ++$y)
  {
    $first = null;
    foreach($result as $b=>$ytable)
    {
      if(!isset($first)) $first = $b;
      if($b === 'lastx') continue;
      $byte = @$ytable[$y];

      $bg = ($b%2) ? '.' :  ' ';
      if($b >= $first + 0x1D) $bg = '!';
      
      for($x=0; $x<8; ++$x)
      {
        if($b*8+$x > $lastx)
          print '%';
        else
          print ($byte & (0x80 >> $x)) ? '#' : $bg;
      }
    }
    print " -- ".join(',', array_keys($result))."\n";
  }
}
