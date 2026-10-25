<?php
/* 

Bisqwit's 1d Bin Packing algorithm implementation (PHP).
Version 1.1.0
 
@author Joel Yliluoma <joel.yliluoma@iki.fi>
@copyright Copyright (C) 2003,2012 Joel Yliluoma (http://iki.fi/bisqwit/)

*/

/**
 * This function organizes items to holes
 * so that the following goals are met:
 *
 *  1) a hole is selected for all items
 *  2) hole may contain multiple items
 *  3) total size of items in one hole
 *     may not exceed the size of hole
 *   
 * If possible, this goal is desirable:
 *  4) free space is gathered together instead
 *     of being scattered in different holes
 *
 * The algorithm behind this is as follows:
 *
 *   Items are handled in size order, biggest-first.
 *   Each item is assigned to the fullest bin at the moment.
 *   
 * This produces quite good results in general cases
 * in O(n*log(n)) complexity, but may sometimes fail
 * when all bins have very little space.
 *
 * Users are welcomed to improve the algorithm.
 *
 * @access public
 * @param Array $bins_sizes  Array($bin_key => $bin_size)
 * @param Array $items_sizes Array($item_key => $item_size)
 * @return Array             Array($item_key => $bin_key)
 *
 *   Keys can be of any type that PHP allows. Integers, strings are fine.
 * If an item could not be placed in any bin, it is not included in result.
 *
 * @todo After a successful result, if there are singular items
 *       residing in a half-filled hole, try moving them into
 *       holes having other items.
 */
function PackBins($bins_sizes, $items_sizes)
{
  $result = Array();

  // Sort items by size (largest first). Keep item indexes.
  uasort($items_sizes, function($a,$b) { return $b-$a; } );
  
  foreach($items_sizes as $item_key => $item_size)
  {
    // Find the fullest bin large enough to store this item.
    
    // Sort bins by remaining size (smallest first). Keep bin indexes.
    
    // This is a stable-sort version of asort($bins_sizes);
    $s = Array();
    foreach($bins_sizes as $bin_key => $bin_size) $s[] = Array($bin_key, $bin_size);
    usort($s, function($a,$b)
              {
                $c = $a[1] - $b[1];
                if($c != 0) return $c;
                if($a[0] < $b[0]) return -1;
                return ($a[0] > $b[0]) ? 1 : 0;
              });
    $bins_sizes = Array();
    foreach($s as $d)
      $bins_sizes[$d[0]] = $d[1];

    foreach($bins_sizes as $bin_key => $bin_size)
    {
      if($bin_size >= $item_size)
      {
        // Place the item.
        $bins_sizes[$bin_key] -= $item_size;
        $result[$item_key]    = $bin_key;
        continue 2; // next item
      }
    }
    // ERROR: Could not assign item anywhere.
    //        Symptom: $result does not include $item_key.
  }

  // Finally, key-sort the result for convenience.
  ksort($result);
  return $result;
}

/**
 * This version of PackBins packs objects into multiple pages of space.
 *
 * @access public
 * @param Array $pages  Array($page_key => Array($bin_key => $bin_size))
 *
 *     Lists the free space on each page.
 *
 * @param Array $items  Array($item_key => Array($item_size, $allowed, $group_key))
 *
 *     If $allowed is not null, it contains either a $page_key, or
 *   an Array of page keys. The item placement will be restricted
 *   to one of these pages. If $allowed is null, it will be placed on
 *   any available page.
 *
 *     If $group_key is not null, then all items with the same $group_key
 *   will be placed in the same page from the minimal selection of those
 *   items' $allowed lists.
 *
 * @return Array       Array($item_key => $bin_key)
 *
 *   Keys can be of any type that PHP allows. Integers, strings are fine.
 * If an item could not be placed in any bin, it is not included in result.
 */
function MultiPackBins($pages, $items)
{
  $result = Array();

  // Force that each $spec[1] is an array.
  foreach($items as $item_key => &$spec)
  {
    if(!isset($spec[1]))
      $spec[1] = array_keys($pages); // Any page
    elseif(!is_array($spec[1]))
      $spec[1] = Array( $spec[1] );
    sort($spec[1]);
  }
  unset($spec);
  
  // Utility function for dealing with used space.
  $remove_space = function($page_key,$bin_key, $size) use(&$pages)
  {
    $page = &$pages[$page_key];
    $page[$bin_key] -= $size;
    if(!$page[$bin_key]) unset($page[$bin_key]);
    unset($page);
    if(empty($pages[$page_key])) unset($pages[$page_key]);
  };
  
  /* We have to handle several different cases.
   *
   *              Group:  none  specific
   *  Number of pages:   ------------
   *                  1 |  A     B
   *           multiple |  C     B
   */
  // A) First, put those that have no group and a specific
  //    singular page requirement, into those pages. 
  $subitems_perpage = Array();
  foreach($items as $item_key => $spec)
    if(!isset($spec[2]) && count($spec[1]) == 1)
      $subitems_perpage[$spec[1][0]][$item_key] = $spec[0];
  foreach($subitems_perpage as $page_key => $subitems)
  {
    // Subitems: $item_key=>$size
    $result1 = PackBins($pages[$page_key], $subitems);
    foreach($result1 as $item_key => $bin_key)
    {
      $item = &$items[$item_key];
      $result[$item_key] = $bin_key;
      $remove_space($page_key, $bin_key, $item[0]);
      unset($item);
      unset($items[$item_key]); // Dealt with.
    }
  }
  
  // B) Secondly, put those that have a group, into some page,
  //    in order of strictness.
  $subitems_pergroup = Array();
  foreach($items as $item_key => $spec)
    if(isset($spec[2]))
    {
      $group_key = $spec[2];
      $group = &$subitems_pergroup[$group_key];
      if(!isset($group))
      {
        $group = Array('items' => Array($item_key => $spec[0]), 'pages' => Array());
        foreach($spec[1] as $page_key)
          $group['pages'][$page_key] = $page_key;
      }
      else
      {
        $group['items'][$item_key] = $spec[0];
        foreach($group['pages'] as $page_key)
          if(!array_search($page_key, $spec[1]))
            unset($group['pages'][$page_key]);
      }
    }

  // Sort the groups in ascending order of number of allowed pages.
  uasort($subitems_pergroup, function($a,$b)
    {
      return count($a['pages']) - count($b['pages']);
    });

  foreach($subitems_pergroup as $group_key => $subitems)
  {
    // Subitems: Array('items' => Array($item_key=>$size),
    //                 'pages' => Array($page_key,...) )
    
    // From smallest to largest, try fitting all of these
    // items in that page.
    $page_size = Array();
    foreach($pages as $page_key => $bins)
      foreach($bins as $bin_key => $bin_size)
        @$page_size[$page_key] += $bin_size;
    // Sort the pages in ascending order of remaining space.
    asort($page_size);

    $best_result = Array();
    $best_page   = 0;
    $goal_success = count($subitems['items']);
    foreach($page_size as $page_key => $page_size)
    {
      // Attempt to put all of these items into this page.
      $result2 = PackBins($pages[$page_key], $subitems['items']);
      // Choose the most successful result.
      // Successful is determined by the number of items successfully located.
      $success_level = count($result2);
      if($success_level > count($best_result))
      {
        $best_result = $result2;
        $best_page   = $page_key;
        if($success_level == $goal_success)
          break;
      }
    }
    // Note: It is possible that not all items were assigned a place for.
    $page_key = $best_page;
    foreach($best_result as $item_key => $bin_key)
    {
      $item = &$items[$item_key];
      $result[$item_key] = $bin_key;
      $remove_space($page_key, $bin_key, $item[0]);
      unset($item);
      unset($items[$item_key]); // Dealt with.
    }
  }

  // C) Lastly, put those that have no group restriction,
  //    somewhere, in order of strictness.
  $subitems_per_strictness = Array();
  foreach($items as $item_key => $spec)
    if(!isset($spec[2]))
      $subitems_per_strictness[ count($spec[1]) ]
                              [ join(',', $spec[1]) ]
                              [$item_key] = $spec[0];

  ksort($subitems_per_strictness);
  foreach($subitems_per_strictness as $page_count => $subitems_per_pages)
    foreach($subitems_per_pages as $page_set => $subitems)
    {
      $allowed = Array();
      foreach(explode(',', $page_set) as $page_key)
        $allowed[$page_key] = $page_key;

      // Collect space from those pages
      $subspace = Array();
      $bin_page = Array();
      foreach($allowed as $page_key)
        if(isset($pages[$page_key]))
          foreach($pages[$page_key] as $bin_key => $bin_size)
          {
            $subspace[$bin_key] = $bin_size;
            $bin_page[$bin_key] = $page_key;
          }

      $result3 = PackBins($subspace, $subitems);
      foreach($result3 as $item_key => $bin_key)
      {
        $item = &$items[$item_key];
        $result[$item_key] = $bin_key;
        $remove_space($bin_page[$bin_key], $bin_key, $item[0]);
        unset($item);
        unset($items[$item_key]); // Dealt with.
      }
    }

  // Finally, key-sort the result for convenience.
  return $result;
}
