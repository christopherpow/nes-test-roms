<?php

function make_string($s)
{
  global $begin;

  $result = Array();
  $length = strlen($s);
  
  if($length == 0) return '""';
  
  $str = '';
  for($a = 0; $a < $length; ++$a)
  {
    $c = $s[$a];
    $b = ord($c);
    if($b < 32)
    {
      if(strlen($str)) { $e = strlen($str)==1 ? "'" : '"'; $result[] = "$e{$str}$e"; $str = ''; }
      $result[] = (string)$b;
    }
    elseif($b < 0x20 || $b >= 0x7F || $b == 34 || $b == 92 || $b == 39)
    {
      if(strlen($str)) { $e = strlen($str)==1 ? "'" : '"'; $result[] = "$e{$str}$e"; $str = ''; }
      $result[] = sprintf('$%02X', $b);
    }
    else
    {
      $str .= $c;
    }
  }
  if(strlen($str)) { $e = strlen($str)==1 ? "'" : '"'; $result[] = "$e{$str}$e"; $str = ''; }
  return join(',', $result);
}

function read_file($fn)
{
  global $records, $record;

  foreach(explode("\n", file_get_contents($fn)) as $line)
  {
    if(preg_match("/^[ \t]*\.include[ \t]*\"([^\"]*)\"[ \t]*(?:;.*)?\$/", $line, $mat))
    {
      $fn2 = $mat[1];
      if(file_exists($fn2))
        read_file($fn2);
      elseif(file_exists("common/$fn2")
          || $fn2 == 'ascii.chr')
        read_file("common/$fn2");
      else
        $records[] = $line;
    }
    elseif(preg_match("/^[ \t]*\.incbin[ \t]*\"([^\"]*)\"[ \t]*(?:;.*)?\$/", $line, $mat))
    {
      $fn2 = $mat[1];
      if(!file_exists($fn2) && (file_exists("common/$fn2") || $fn2 == 'ascii.chr'))
        $records[] = ".incbin \"common/$fn2\" ;DTE FIXED INCLUDE PATH";
      else
        $records[] = $line;
    }
    elseif(preg_match('/^ *\./', $line))
    {
      if(isset($record)) { $records[] = $record; $record = null; }
      $records[] = $line;
    }
    else
    {
      $match_elem = 'newline|\'.\'|\$[0-9A-F][0-9A-F]|"[^"]*"|0';
      if(strpos($line, 'DO_DTE') !== false
      || strpos($line, 'DTE_CHARMAP') !== false)
      {
        preg_match("/^(.*?)((?:$match_elem)(?:[ \t]*,[ \t]*(?:$match_elem))*)(.*)/", $line, $mat);
        
        if(!isset($record))
        {
          $record = Array('prefix' => $mat[1], 'suffix' => $mat[3], 'elem' => '');
        }
        preg_match_all("/(?:$match_elem)/", $mat[2], $mat);
        foreach($mat[0] as $v)
        {
          if($v == 'newline')
            $record['elem'] .= chr(10);
          elseif($v == '0')
            $record['elem'] .= chr(0);
          elseif($v[0] == '$')
            $record['elem'] .= chr(hexdec(substr($v, 1)));
          else
            $record['elem'] .= substr($v, 1, -1);
        }
      }
      else
      {
        if(isset($record)) { $records[] = $record; $record = null; }
        $records[] = $line;
      }
    }
  }
  if(1)
  foreach($records as &$record)
    if(is_array($record))
    {
      // Convert print_str calls into calls that include an explicit nul terminator,
      // so the nul terminator can also be included within DTE optimization.
      foreach(Array('print_str' => 'print_str_no_nul',
                    'print_ext_str' => 'print_ext_str_no_nul',
                    'print_ext_str_flush' => 'print_ext_str_no_nul_flush',
                    'print_ext_str_flush_rts' => 'print_ext_str_no_nul_flush_rts')
              as $call1 => $call2)
      {
        #if(strpos($record['prefix'], $call1) !== false)
        #  print "GUU\n";
        $rep = preg_replace("/^(.*)$call1([ \t]*)\$/",
                            "\\1 {$call2} \\2",
                            $record['prefix']);
        if($rep != $record['prefix'])
        {
          $record['prefix'] = $rep;
          $record['elem'] .= chr(0);
          $record['suffix'] .= ' ; DTE-ADDED NUL';
          break;
        }
      }
    }
  if(isset($record)) { $records[] = $record; $record = null; }
  unset($record);
}

function count_size()
{
  global $records;
  $result = 0;
  foreach($records as $record)
    if(is_array($record) && strpos($record['suffix'], 'DO_DTE') !== false)
      $result += strlen($record['elem']);
  return $result;
}
