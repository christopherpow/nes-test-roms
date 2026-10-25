<?php

/**
 * Computes the number of bytes that are common between the end of $s1 and beginning of $s2.
 * The $s2_backtracktable must equal to the result of ComputeBackTrackTable($s2),
 * but the caller can cache it for multiple invokations with the same $s2.
 *
 * Using Knuth-Morris-Pratt O(N) algorithm.
 * Code from:
 *   http://stackoverflow.com/questions/1285434/efficient-algorithm-for-string-concatenation-with-overlap
 *
 * @param string $s1 The first string (beginning)
 * @param string $s2 The second string (ending)
 * @param Array $s2_backtracktable A backtrack table, which must be computed using ComputeBackTrackTable($s2).
 * @return int Number of characters matched.
 *   E.g. for 'haphazard' and 'arduous', it returns 3, suggesting
 *        that a merging such as 'haphaz-ard-uous' is possible.
 */
function OverlappedStringLength($s1, $s2, $s2_backtracktable)
{
  $s1length = strlen($s1);
  $s2length = strlen($s2);
  //Trim s1 so it isn't longer than s2
  $s1begin  = max($s1length - $s2length, 0);

  $T = $s2_backtracktable;

  $m = $s1begin;
  $i = 0;
  while($m+$i < $s1length)
  {
    if($s2[$i] == $s1[$m+$i])
    {
      ++$i;
      //<-- removed the return case here, because |s1| <= |s2|
    }
    else
    {
      $m += $i - $T[$i];
      if ($i > 0) $i = $T[$i];
    }
  }
  return $i; //<-- changed the return here to return characters matched
}

/**
 * This function computes the backtrack table that must
 * be passed as parameter for OverlappedStringLength().
 * @internal
 * @param string $s The string to calculate the table for.
 * @return Array
 */
function ComputeBackTrackTable($s) // O(N)
{
  $T = Array();
  $cnd = 0;
  $pos = 2;
  $T[0] = -1;
  $T[1] = 0;
  $length = strlen($s);

  while($pos < $length)
  {
    if ($s[$pos - 1] == $s[$cnd])
      $T[$pos++] = ++$cnd;
    elseif ($cnd > 0)
      $cnd = $T[$cnd];
    else
      $T[$pos++] = 0;
  }
  return $T;
}
