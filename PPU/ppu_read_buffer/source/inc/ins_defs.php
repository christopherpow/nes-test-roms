<?php

/**
 * Size of a page in bytes. This is used for grouping the free space
 * in the ROM, i.e. restricting an object's placement within a particular
 * region of space.
 */
define('PAGE_SIZE', 0x4000);

/**
 * Convert a linear ROM address into a paged memory address.
 * @param int $target The linear address as seen in the ROM file.
 * @return int The memory address as seen by the NES CPU.
 */
function Make16bitNESoffset($target)
{
  // This is Castlevania II -specific.
  if($target < 0x1C000)
    return 0x8000 + ($target % PAGE_SIZE);
  return   0xC000 + ($target % PAGE_SIZE);
}
