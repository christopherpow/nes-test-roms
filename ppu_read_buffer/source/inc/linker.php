<?php
/**
 * Linker utility for ROM hacking.
 */

require_once 'inc/ins_defs.php';
require 'inc/overlapfinder.php';
require 'inc/binpacker.php';

/**
 * A Blob is a sequence of bytes that is to be inserted somewhere
 * in the ROM. It can also contain labels, and references to labels
 * within other blobs.
 *
 * @author Joel Yliluoma <joel.yliluoma@iki.fi>
 * @version 1.0.0
 * @access public
 */
class Blob
{
  /**
   * A string of bytes representing the blob data.
   * @var string
   */
  private $data = '';
  
  /**
   * An array of public variables.
   * Contents: Array(name => offset_relative_to_beginning_of_data)
   * @var array
   */
  private $publics = Array();
  
  /**
   * An array of external references, all of which are 16-bit pointers.
   * Contents: Array(offset_relative_to_beginning_of_data => name).
   * @var array
   */
  private $externs = Array();

  /**
   * Constructor
   *
   * @param  string $name Optional public-name to declare, pointing to the beginning of the data
   * @param  string $data Optional initial contents of the blob
   */
  public function __construct($name = '', $data = '')
  {
    $this->data = $data;
    if(!empty($name))
      $this->AddPublicAt($name, 0);
  }
  
  /**
   * Appends a pointer to external symbol. The pointer is 16-bit and 2 dummy bytes are appended to the data.
   *
   * @param  string $name The symbol to be referred to
   */
  public function AppendPointerTo($name)
  {
    $this->externs[strlen($this->data)] = $name;
    $this->data    .= 'xx';
  }
  /**
   * Appends raw data to the blob.
   *
   * @param  string $data The raw data to be appended
   */
  public function AppendRawData($data)
  {
    $this->data .= $data;
  }
  
  /**
   * Patches the raw data at given offset with a pointer to the given linear address.
   * This is a helper function used by Linker::Link(). You don't normally call this.
   *
   * @param  int $offset Offset where to place the pointer, relative to the beginning of this blob (0-based)
   * @param  int $target The address to refer to. The address will be converted into a paged address using Make16bitNESoffset().
   */
  public function PutResolvedPointer($offset, $target)
  {
    $p = Make16bitNESoffset($target);
    $this->data[$offset+0] = chr($p & 0xFF);
    $this->data[$offset+1] = chr($p >> 8);
  }

  /**
   * Declares a public name at the end of current data
   *
   * @param  int $name The symbol to declare
   */
  public function AddPublicHere($name)
  {
    $this->AddPublicAt($name, strlen($this->data));
  }
  /**
   * Declares a public name at the given offset
   *
   * @param  int $name The symbol to declare
   * @param  int $offset The offset, relative to the beginning of this blob, to associate with the symbol
   */
  public function AddPublicAt($name, $offset)
  {
    $this->publics[$name] = $offset;
  }
  
  /**
   * Verifies that if the given blob is to be immersed into this blob, external references don't clash.
   * This is a helper function used by Linker::Merge(). You don't normally call this.
   *
   * @param Blob $blob2  The candidate blob for immersion into this blob
   * @param int  $pos    The position within this blob to immerse the blob into
   * @return bool        false if the immersion is dangerous, true otherwise
   * @internal
   */
  public function VerifyExternsImmersion($blob2, $pos)
  {
    $common_begin = $pos;
    $common_end   = $pos + $blob2->GetSize();
    // Found a position. Test whether the externs match.
    foreach($this->externs as $offs => $name)
      if($offs >= $common_begin && $offs < $common_end)
      {
        // Is this extern found also in the second piece?
        if(!(isset($blob2->externs[ $offs - $pos ])
          &&       $blob2->externs[ $offs - $pos ] == $name))
          return false;
      }
    foreach($blob2->externs as $offs => $name)
    {
      if(!(isset($this->externs[$offs + $pos])
        &&       $this->externs[$offs + $pos] == $name))
        return false;
    }
    return true;
  }
  
  /**
   * Tests whether the given blob can be completely immersed into this blob with no ill effects, to save space
   * This is a helper function used by Linker::Merge(). You don't normally call this.
   *
   * @param Blob $blob2 The blob to immerse here. It must not be greater in size than this blob.
   * @return int  Returns position, relative to the beginning of this blob, where $blob2 is a perfect subset. Returns false if the blob cannot be immersed.
   * @internal
   */
  public function FindImmersion($blob2)
  {
    $length1 = strlen($this->data);
    $length2 = strlen($blob2->data);
    $first_pos = 0;
    $last_pos  = $length1 - $length2;

    for(;;)
    {
      $pos = $first_pos;
      if($blob2->GetSize() > 0)
        $pos = strpos($this->data, $blob2->data, $first_pos);
      if($pos === false)
        return false;
      
      // Found a position. Test whether the externs match.
      if(!$this->VerifyExternsImmersion($blob2, $pos))
      {
        $first_pos = $pos+1;
        continue; // Mad match, try find more matches.
      }

      // Perfect match.
      return $pos;
    }
  }

  /**
   * Proceeds with immersing the given blob into this blob. All publics and externs from the blob will be adopted, and the data will be extended if there is only a partial overlap.
   * This is a helper function used by Linker::Merge(). You don't normally call this.
   *
   * @param Blob $blob2    The blob to be immersed into this blob.
   * @param int  $position The offset, relative to the beginning of this blob, to immerse $blob2 into.
   * @internal
   */
  public function Immerse($blob2, $position)
  {
    $piece_end_pos = $position + strlen($blob2->data);
    $my_end_pos    = strlen($this->data);
    
    $verb = ($piece_end_pos > $my_end_pos) ? 'Merging' : 'Immersing';
    
    #$desc1 = TranslateDialogEnglish($this->data);
    #$desc2 = TranslateDialogEnglish($blob2->data);
    #print "$verb <$desc2> into <$desc1> at $position";
    
    if($piece_end_pos > $my_end_pos)
    {
      // Needs appending some data!
      $this->data .= substr($blob2->data, -($piece_end_pos - $my_end_pos));
    }
    
    #$descr = TranslateDialogEnglish($this->data);
    #print "; Result: <$descr>\n";
    
    // Add all publics from the piece
    foreach($blob2->publics as $name => $offset)
      $this->publics[$name] = $offset + $position;
    // Add all externs from the piece
    foreach($blob2->externs as $offset => $name)
      $this->externs[$offset + $position] = $name;
  }
  
  /**
   * Returns the length of the raw data of the blob.
   * @return int Length of the raw data.
   */
  public function GetSize()
  {
    return strlen($this->data);
  }
  /**
   * Returns the raw data of the blob.
   * @return string The raw data.
   */
  public function GetData()
  {
    return $this->data;
  }
  /**
   * Returns the list of public variables declared in this blob.
   * @return Array An array of public variables.
   *               Contents: Array(name => offset_relative_to_beginning_of_data)
   */
  public function GetPublics()
  {
    return $this->publics;
  }
  /**
   * Returns the list of external references declared in this blob.
   * @return Array An array of external references, all of which are 16-bit pointers.
   *               Contents: Array(offset_relative_to_beginning_of_data => name).
   */
  public function GetExterns()
  {
    return $this->externs;
  }
  
};

/**
 * A Linker manages a list of Blobs, and is ultimately responsible
 * for assigning them places somewhere within the ROM.
 *
 * In a normal workflow, you would use code that looks something like this:
 * <code>
 *    $linker = new Linker;
 *    $space  = Array();
 *    do {
 *      $b = new Blob(...);        // Create an object
 *      ...initialize $b...
 *      ...populate $space...
 *      $linker->AddBlob($b, ...); // Add the object to the project.
 *    } while( ...more blobs to create... );
 *    $linker->Merge();            // Optional: may save some bytes.
 *    $linker->Organize($space);   // Assign addresses for all objects.
 *    $linker->Link();             // Fix the external references.
 *    $result = $linker->GetIPSpatch(); // Acquire the output.
 * </code>
 * @author Joel Yliluoma <joel.yliluoma@iki.fi>
 * @version 1.0.0
 * @access public
 */
class Linker
{
  /**
   * The list of blobs handled by the linker.
   *
   * Contents: Array( Array(blob => Blob, offset => mixed, fixed => bool) ) )
   *                   fixed=false: offset=Array(allowed_pages)
   *                   fixed=true:  offset=final_linear_address
   * @var Array
   */
  private $blobs;

  /** 
   * Optional function which merges blobs where possible, saving space.
   *
   */
  public function Merge()
  {
    // Merge Blobs where possible.
    // Only blobs that are to be linked into same page can be merged.
    // First immerse blobs that are complete subsets of another blob.
    // Then append blobs that are partial subsets at begin or end.
    // Merging two blobs also merges their publics and externs.
    // Note: Comparing $data is not enough. $externs must also interleave.

    // Group blobs by their list of pages
    $blobs_by_page = Array();
    $fixed_blobs   = Array();
    foreach($this->blobs as $n=>$blob)
      if(!$blob['fixed'])
        $blobs_by_page[ join(',', $blob['offset']) ][] = $blob;
      else
        $fixed_blobs[] = $blob;
    $this->blobs = $fixed_blobs;
    unset($fixed_blobs);

    foreach($blobs_by_page as $list_of_pages => &$blobs)
    {
      // Sort the blobs in order of length. Also renumber keys.
      usort($blobs, function($a,$b) { return $b['blob']->GetSize()-$a['blob']->GetSize(); });
      $n_blobs = count($blobs);
      for($a=0; $a<$n_blobs-1; ++$a)
        if(isset($blobs[$a]) && !$blobs[$a]['fixed'])
        {
          $blob1 = &$blobs[$a]['blob'];

          // Check if any of the remaining blobs is a perfect subset of this blob
          for($b=$a+1; $b<$n_blobs; ++$b)
            if(isset($blobs[$b]) && !$blobs[$b]['fixed'])
            {
              $blob2 = &$blobs[$b]['blob'];
              $position = $blob1->FindImmersion($blob2);
              if($position !== false)
              {
                $blob1->Immerse($blob2, $position);
                unset($blob1);        // Undo the references
                unset($blob2);
                unset($blobs[$b]);    // Remove array item (invalidates references)
                $blob1 = &$blobs[$a]['blob']; // Re-establish the reference
              }
            }
        }

      $backtrack   = Array(); // Cache of ComputeBackTrackTable results
      $overlapsize = Array(); // Cache of OverlappedStringLength results
      for(;;)
      {
        // Sort the blobs and remove gaps from the indexes.
        // This is not really necessary, but it might be nice.
        if(false && count($blobs) < $n_blobs)
        {
          usort($blobs, function($a,$b) { return $b['blob']->GetSize() - $a['blob']->GetSize(); });
          $backtrack   = Array(); // requires this, which is not nice
          $overlapsize = Array();
          $n_blobs = count($blobs);
        }

        // Try partial overlap merge.
        $best_overlap = Array('length' => 0, 'a' => 0, 'b' => 0);

        for($a=0; $a<$n_blobs; ++$a) // second part
          if(isset($blobs[$a]) && !$blobs[$a]['fixed'])
          {
            $blob2 = &$blobs[$a]['blob'];
            $s2 = $blob2->GetData();
            
            $s2_backtracktable = &$backtrack[$a];
            if(!isset($s2_backtracktable))
              $s2_backtracktable = ComputeBackTrackTable($s2);
            
            // For each other block, find the largest overlap.
            for($b=0; $b<$n_blobs; ++$b) // first part
              if($a != $b && isset($blobs[$b]) && !$blobs[$b]['fixed'])
              {
                $blob1 = &$blobs[$b]['blob'];
                $s1 = $blob1->GetData();
                
                $overlap = &$overlapsize[$b][$a];
                if(!isset($overlap))
                {
                  $overlap = OverlappedStringLength($s1,$s2, $s2_backtracktable);

                  // Validate overlap. If fails, try a smaller overlap.
                  while($overlap > 0
                     && !$blob1->VerifyExternsImmersion($blob2, strlen($s1)-$overlap))
                    --$overlap;

                  #if($overlap) print "Tested $a,$b -> $overlap\n";
                }

                // Keep the longest overlap.
                if($overlap > $best_overlap['length'])
                  $best_overlap = Array('length' => $overlap, 'a' => $a, 'b' => $b);
                
                unset($overlap);
              }
            unset($s2_backtracktable);
          }
        if($best_overlap['length'] <= 0) break;

        // Found an overlap. Merge those two pieces.
        $a = $best_overlap['a']; $blob2 = &$blobs[$a]['blob'];
        $b = $best_overlap['b']; $blob1 = &$blobs[$b]['blob'];
        $position = $blob1->GetSize() - $best_overlap['length'];
        $blob1->Immerse($blob2, $position);
        unset($blob1);        // Undo the references
        unset($blob2);
        unset($blobs[$a]);    // Remove array item (invalidates references)
        unset($backtrack[$a]); // Not required, but is simple enough and saves memory
        unset($backtrack[$b]); // Required for correctness, because b is changed
        unset($overlapsize[$b]); // Required for correctness, because b is changed
        // Also should unset $overlapsize[anything][$a], but that's too much trouble
      }
    }

    foreach($blobs_by_page as $list_of_pages => &$blobs)
      foreach($blobs as $blob)
        $this->blobs[] = $blob;
  }
  
  /**
   * Add a blob to the linking project.
   *
   * @param Blob $blob       The blob to be added
   * @param mixed $locations When $fixed=true, specifies the linear location within
   *                         the ROM which is the final placement of the Blob.
   *                         When $fixed=false, specifies a list of pages in which the blob can be placed.
   * @param bool $fixed      True when the exact final placement of the blob is known,
   *                         false if only the page is known. 
   */
  public function AddBlob(Blob $blob, $locations, $fixed = false)
  {
    if(!$fixed)
    {
      if(!is_array($locations))
        $locations = Array($locations);
      sort($locations);
    }
    $this->blobs[] =
      Array('blob'   => &$blob,
            'offset' => $locations,
            'fixed'  => $fixed);
  }
  
  /**
   * Assign final locations for each blob that does not have that yet.
   *
   * @param Array $space Tells where free space is located.
   *                     Contents: Array(linear_begin_location => number_of_bytes)
   */
  public function Organize($space)
  {
    // space: Array(begin => length)

    $pages = Array();
    $items = Array();

    foreach($this->blobs as $blob_key => $blob)
      if(!$blob['fixed'])
        $items[$blob_key] = Array( $blob['blob']->GetSize(),
                                   $blob['offset'],
                                   null );
    
    $bins      = Array();
    $page_keys = Array();
    
    $max_page = max(array_keys($space)) + 65536;
    for($page_key = 0; $page_key < $max_page; $page_key += PAGE_SIZE)
    {
      $page_begin = $page_key;
      $page_end   = $page_begin + PAGE_SIZE;

      foreach($space as $begin => $length)
      {
        $end = $begin+$length;
        if($begin < $page_end && $end > $page_begin)
        {
          if($begin < $page_begin) $begin = $page_begin;
          if($end   > $page_end)   $end   = $page_end;
          
          $pages[$page_key][$begin] = $end-$begin;
          $page_keys[$begin] = $page_key;
          $bins[$begin] = $begin;
        }
      }
    }
    
    $solution = MultiPackBins($pages, $items);

    foreach($this->blobs as $blob_key => &$blob)
      if(!$blob['fixed'])
      {
        $item_size = $blob['blob']->GetSize();
        $bin_key   = @$solution[$blob_key];
        if(isset($bin_key))
        {
          $blob['offset'] = $bins[$bin_key];
          $blob['fixed']  = true;
          $bins[$bin_key]                        += $item_size;
          $pages[$page_keys[$bin_key]][$bin_key] -= $item_size;
        }
        else
        {
          print "Organize(): ERROR - Could not assign place for $item_size bytes...\n";
        }
      }

    // Report the remaining space
    foreach($pages as $page_key => $p)
    {
      $s0 = Array(); $t0 = 0;
      $s1 = Array(); $t1 = 0;
      foreach($p as $begin=>$size)
      { 
        $original_size = ($bins[$begin]+$size) - $begin;
        $w = ceil(log10( $original_size ));
        // ^ Number of digits in the original amount of space
        $s0[] = sprintf("%X-%{$w}d", $begin+$original_size, $original_size);
        $s1[] = sprintf("%X-%{$w}d", $bins[$begin]+$size,   $size);
        $t0 += $original_size;
        $t1 += $size;
      }
      $before = join(', ', $s0)." - total: $t0";
      $after  = join(', ', $s1)." - total: $t1";
      if($before == $after)
        echo "Space (unchanged): $before\n";
      else
      {
        echo "Space originally : $before\n";
        echo "Space after      : $after\n";
      }
    }

    // Create filler blobs for unused space
    if(false)
      foreach($pages as $page_key => $p)
        foreach($p as $begin=>$size)
          if($size)
          {
            $b = new Blob(sprintf('filler%X', $begin),
                          str_pad('', $size, "\xFF"));
            $this->AddBlob($b, $bins[$begin], true);
          }
  }
  
  /**
   * Resolves all external symbols and patches the pointers into the code.
   * This function is the culmination of the linking process, called when
   * all the objects (blobs) have been added to the project.
   */
  public function Link()
  {
    // Figure out the location of *each* public name.
    $publics = Array(); // name => offset
    $missing_publics = Array();

    foreach($this->blobs as $index => &$blob)
    {
      $p = $blob['blob']->GetPublics();
      if(!$blob['fixed'])
      {
        print "Link() error: Not all blobs have been successfully situated by Organize().\n";
        foreach($p as $name => $offset)
          $missing_publics[$name] = $offset;
      }
      else
      {
        foreach($p as $name => $offset)
        {
          if(isset($publics[$name]))
            print "Link() error: Public <$name> defined twice\n";

          $offset += $blob['offset'];
          $publics[$name] = $offset;
          
          #printf("Symbol <%s> is at \$%X in blob $index\n", $name, $offset);
        }
      }
    }
    
    // Then, resolve all externs.
    foreach($this->blobs as &$blob)
    {
      $e = $blob['blob']->GetExterns();

      foreach($e as $offset => $name)
      {
        if(!isset($publics[$name]))
        {
          if(isset($missing_publics[$name]))
            print "Link() error: Extern <$name> is in an unsituated blob.\n";
          else
            print "Link() error: Unresolved extern <$name>\n";
        }
        else
        {
          $target = $publics[$name];
          $blob['blob']->PutResolvedPointer($offset, $target);
        }
      }
    }
  }
  
  /**
   * Returns the patch set.
   *
   * @return Array Returns an array: Array(address => string)
   */
  public function GetPatches()
  {
    $result = Array();
    foreach($this->blobs as $index => &$blob)
      $result[] = Array($blob['offset'], $blob['blob']->GetData());
    usort($result, function($a,$b) { return $a[0]-$b[0]; });

    if(true)
    {
      // Merge adjacent patches
      $b=count($result);
      for($a=0; $a<$b; ++$a)
      {
        if(!isset($result[$a])) continue;

        $begin = $result[$a][0];
        $end   = $begin + strlen($result[$a][1]);

        for($c=$a+1; $c<$b; ++$c)
        {
          if(!isset($result[$c])) continue;
          if($result[$c][0] > $end) break;

          $overlap = $end - $result[$c][0];
          $length  = strlen($result[$c][1]);

          if($overlap > 0)
          {
            printf("GetPatches() error: Overlapping patches: \$%04X+%u and \$%04X+%u\n",
              $begin, $end-$begin,
              $result[$c][0], $length);
            $result[$a][1] = substr($result[$a][1], 0, -$overlap);
            $end -= $overlap;
          }
          /*printf("Merging blobs: \$%04X..\$%04X and \$%04X..\$%04X becomes: \$%04X+%u\n",
            $begin, $end-1,
            $result[$c][0], $result[$c][0]+$length-1,
            $begin, ($end-$begin)+$length);*/

          $result[$a][1] .= $result[$c][1];

          $end  += $length;
          unset($result[$c]);
        }
      }
    }
    $p = Array();
    foreach($result as $item)
      $p[$item[0]] = $item[1];
    return $p;
  }

  public function GetPublics()
  {
    $result = Array();
    foreach($this->blobs as $index => &$blob)
    {
      if($blob['fixed'])
      {
        $p = $blob['blob']->GetPublics();
        foreach($p as $name => $offset)
        {
          $offset += $blob['offset'];
          $result[$name] = $offset;
        }
      }
    }
    return $result;
  }
  
  /**
   * Returns the patch set in an IPS forat.
   *
   * @param int $fileoffset An optional header size to be added into the IPS offsets. For example, 16 for NES files.
   * @param bool $verbose true = include list of public symbols; false = don't.
   * @return string The IPS patch file data, to be written into a file.
   * @todo Implement RLE packing. Avoid unintentional extern&global markers.
   */
  public function GetIPSpatch($fileoffset = 0, $verbose = false)
  {
    $result = '';

    if($verbose)
      foreach($this->blobs as $index => &$blob)
      {
        if($blob['fixed'])
        {
          $p = $blob['blob']->GetPublics();
          foreach($p as $name => $offset)
          {
            $offset += $blob['offset'];
            $data = $name . "\0". substr(pack('V', $fileoffset+$offset), 0,3);
            $result .= substr(pack('Nn', 0x00002, strlen($data)), 1);
            $result .= $data;
          }
        }
      }

    $patches = $this->GetPatches();
    foreach($patches as $offset => $data)
    {
      $pos = 0;
      $max = strlen($data);

      #printf("Patch at %X, $max bytes\n", $offset);
      
      while($pos < $max)
      {
        $len = min(20000, $max-$pos);
        
        // TODO: In address, avoid "EOF", extern marker (1) and global marker (2)
        // TODO: RLE packing

        $result .= substr(pack('Nn', $fileoffset+$offset+$pos, $len), 1);
        $result .= substr($data, $pos, $len);
        
        $pos += $len;
      }
    }
    return "PATCH{$result}EOF";
  }
};
  