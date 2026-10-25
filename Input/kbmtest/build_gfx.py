#!/usr/bin/env python3

import PIL.Image
import os.path
import datetime
from functools import total_ordering

OUTPUT_DIR = "temp"

now_string = datetime.datetime.now().strftime("%a %b %d %H:%M:%S %Y")

#
# Indexed images
#

# set image palette from RGB tuples
def index_setpal(img,palette): 
    linpal = [p for tup in palette for p in tup[0:3]]
    img.putpalette(linpal)

# load and convert image to indexed palette
def index_img_load(filename, palette):
    src = PIL.Image.open(filename)
    dst = PIL.Image.new("P",src.size,color=0)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            mag = ((255**2)*3)+1
            mat = 0
            for i in range(len(palette)):
                m = sum([(a-b)**2 for (a,b) in zip(p,palette[i])])
                if m < mag: # better match
                    mat = i
                    mag = m
                    if m == 0: # perfect match
                        break
            dst.putpixel((x,y),mat)
    index_setpal(dst,palette)
    return dst

# creates an image that places an error color anywhere the two images are different
# returns (count of error pixels, error image)
def index_img_cmp(imga,imgb,palette,error_color=(255,0,255)):
    assert imga.width == imgb.width and imga.height == imgb.height
    img = PIL.Image.new("P",(imga.width,imga.height),len(palette))
    count = 0
    for y in range(0,imga.height):
        for x in range(0,imga.width):
            pa = imga.getpixel((x,y))
            pb = imgb.getpixel((x,y))
            if pa == pb:
                img.putpixel((x,y),pa)
                count += 1
    index_setpal(img,list(palette)+[error_color])
    return ((imga.width * imga.height) - count, img)

#
# CHR tiles
#

# tile class holds 16 bytes of CHR data
@total_ordering
class Tile:
    def __init__(self,d=([0]*16)):
        self.d = bytes(d)

    # create tile from indexed image (only 2 bits of index are observed)
    def grab(img,tx,ty):
        c = [0]*16
        a = 0
        for y in range(0,8):
            for x in range(0,8):
                p = img.getpixel((tx+x,ty+y))
                c[0+y] = (c[0+y] << 1) | (p &  1)
                c[8+y] = (c[8+y] << 1) | ((p >> 1) & 1)
                if (p&3) != 0 and (p>=4): # any non-0 non-palette-0 colour determines attribute
                    a = p >> 2
        return (Tile(c),a)

    # paste tile onto image
    def draw(self,img,x,y,a):
        for j in range(0,8):
            for i in range(0,8):
                p = ( (self.d[j+0] >> (7-i))       & 1) | \
                    (((self.d[j+8] >> (7-i)) << 1) & 2)
                img.putpixel((x+i,y+j),p+(a<<2))

    # paste tile onto image with transparent 0
    def draw_sprite(self,img,x,y,a):
        for j in range(0,8):
            for i in range(0,8):
                p = ( (self.d[j+0] >> (7-i))       & 1) | \
                    (((self.d[j+8] >> (7-i)) << 1) & 2)
                if p > 0:
                    img.putpixel((x+i,y+j),p+(a<<2))

    # flips
    def flipx(self):
        c = []
        for j in range(0,16):
            a = self.d[j]
            b = 0
            for i in range(8):
                b = (b << 1) | (a & 1)
                a >>= 1
            c.append(b)
        return Tile(c)
    def flipy(self):
        return Tile(bytes(reversed(bytearray(self.d[ 0: 8]))) +
                    bytes(reversed(bytearray(self.d[ 8:16]))))
    def flipxy(self):
        return self.flipy().flipx()

    # swap colours 2 and 3
    def swap23(self):
        c = bytearray(self.d)
        for j in range(0,8):
            c[0+j] ^= c[8+j] # if high bit is set, flip low bit 0123 => 0132
        return Tile(c)

    # comparisons
    def __eq__(self,other):
        return self.d == other.d
    def __ne__(self,other):
        return not (self.d == other.d)
    def __lt__(self,other):
        return self.d < other.d

# blank tile
Tile.BLANK = Tile()

# make CHR tiles directly from image, no omissions or elimination of duplicates
def make_chr(img,tiles=[]):
    tw = img.width // 8
    th = img.height // 8
    for ty in range(0,th):
        for tx in range(0,tw):
            (t,a) = Tile.grab(img,tx*8,ty*8)
            tiles.append(t)
    return tiles

def convert_chr(filename,palette,tiles=[]):
    print("convert chr: " + filename)
    old_tilecount = len(tiles)
    img = index_img_load(filename,palette)
    tiles = make_chr(img,tiles)
    print("%d new tiles. %d total." % (len(tiles)-old_tilecount,len(tiles)))
    print()
    return tiles

# convert list of Tile to binary blob
def tile_dump(tiles):
    d = bytearray()
    for t in tiles:
        d.extend(t.d)
    return d

# convert CHR Tile list into RGB image using given 4-colour palette
def tiles_to_img(tiles, palette, width=16, horizontal=True):
    count = len(tiles)
    columns = width
    rows = (count + (width-1)) // width
    if not horizontal:
        rows, columns = columns, rows
    img = PIL.Image.new("P",(width*8,rows*8),0)
    for t in range(0,count):
        xo = (t % width) * 8
        yo = (t // width) * 8
        if not horizontal:
            yo, xo = xo, yo
        to = t * 16
        tiles[t].draw(img,xo,yo,0)
    index_setpal(img,palette)
    return img

#
# Nametable generation
#

# makes nametable data from indexed 16-colour image
# returns (nametable tile indices, packed attributes, Tile array)
# Note: will modify tiles (append) if it is passed as an argument.
def make_nametable(img,tiles=[]):
    nmt = []
    att = []
    tw = img.width//8
    th = img.height//8
    aw = ((tw+3)//4)*4 # pad attributes to 4 tile regions
    ah = ((th+3)//4)*4
    for ty in range(0,th):
        for tx in range(0,tw):
            ox = tx*8
            oy = ty*8
            (t,a) = Tile.grab(img,ox,oy)
            if t in tiles:
                nmt.append(tiles.index(t))
            else:
                nmt.append(len(tiles))
                tiles.append(t)
            att.append(a)
            #print("%2d,%2d: %02X %2d" % (tx,ty,nmt[-1],att[-1]))
        for i in range(tw,aw): # pad to 4
            att.append(0)
    for j in range(th,ah): # pad to 4
        att.extend([0]*aw)
    attbin = []
    for j in range(0,ah,4):
        for i in range(0,aw,4):
            b = 0
            for k in range(0,4):
                ox = i+(k &1) * 2 # select 2x2 quadrant for 4x4 byte area
                oy = j+(k>>1) * 2
                a = max( # pick any nonzero attribute in the 2x2 tile area area
                    att[(oy+0)*aw+ox+0],
                    att[(oy+0)*aw+ox+1],
                    att[(oy+1)*aw+ox+0],
                    att[(oy+1)*aw+ox+1])
                b = (b >> 2) | (a << 6)
            attbin.append(b)
    return (bytearray(nmt),bytearray(attbin),tiles)

# creates an indexed palette image from nametable data
def nametable_to_img(nmt,att,tiles,palette,tw=32):
    th = len(nmt)//tw
    abw = ((tw+3)//4)
    img = PIL.Image.new("P",(tw*8,th*8))
    for ty in range(0,th):
        for tx in range(0,tw):
            ab = att[((ty//4)*abw) + (tx//4)]
            a = (ab >> ((tx & 2) + ((ty & 2) * 2))) & 3
            tiles[nmt[(ty*tw)+tx]].draw(img,tx*8,ty*8,a)
    index_setpal(img,palette)
    return img

# compiles a nametable from a PNG file, outputs data files, collates tiles
def convert_nametable_img(filename,palette,tiles=[]):
    print("convert nametable: " + filename)
    old_tilecount = len(tiles)
    filebase = os.path.splitext(os.path.basename(filename))[0]
    src = index_img_load(filename,palette)
    (n,a,tiles) = make_nametable(src,tiles)
    filenmt = os.path.join(OUTPUT_DIR,filebase+".nmt") # nametable data
    filepng = os.path.join(OUTPUT_DIR,filebase+".nmt.png") # nametable visualized
    filecmp = os.path.join(OUTPUT_DIR,filebase+".cmp.png") # error comparison
    print("nametable data: " + filenmt)
    open(filenmt,"wb").write(n+a)
    print("nametable preview: " + filepng)
    dst = nametable_to_img(n,a,tiles,palette)
    dst.save(filepng)
    (count,cmpimg) = index_img_cmp(src,dst,palette)
    if count == 0:
        print("no errors.")
    else:
        print("%d pixel errors: %s" % (count,filecmp))
        cmpimg.save(filecmp)
    print("%d new tiles. %d total." % (len(tiles)-old_tilecount,len(tiles)))
    print()
    return tiles

#
# Sprites
#

# makes a sprite from a rectangle of an image
# decomposes as a, 8x8 pixel grid (discarding blank tiles)
# assumes one layer of colour per tile
# returns (sprite list ([a,y,x,t]),tiles)
def make_sprite(img,x,y,w,h,px,py,tiles=[]):
    # crop to given rectangle, pad to 8 pixels, reorient pivot
    imc = img.crop((x,y,x+w,y+h))
    w = ((w+7)//8)*8
    h = ((h+7)//8)*8
    imp = PIL.Image.new("P",(w,h),0)
    imp.paste(imc)
    px -= x
    py -= y
    #st = [[],[],[],[]]
    st = [[],[],[],[],[],[],[],[]] # palette shifted sprites for this demo
    def add_st(t):
        st[0].append(t)
        st[1].append(t.flipx())
        st[2].append(t.flipy())
        st[3].append(t.flipxy())
        # palette shifted variants
        for i in range(4):
            st[i+4].append(st[i+0][-1].swap23())
    for t in tiles:
        add_st(t)
    sprite = []
    for sy in range(0,h,8):
        for sx in range(0,w,8):
            (t,a) = Tile.grab(img,x+sx,y+sy)
            if t == Tile.BLANK:
                continue
            # check for duplicate tile first (including flipped variations)
            ti = -1
            #for i in range(0,4):
            for i in range(0,8): # palette shifted
                if t in st[i]:
                    ti = st[i].index(t)
                    a ^= (i << 6) # flip flags
                    if i >= 4: # palette shifted, swap palettes 0/1, drop "9th" bit
                        a = (a^1) & 0xFF
                    break
            if ti < 0: # not duplicate, add a new one
                ti = len(st[0])
                add_st(t)
            a |= 0x1C # set "unused" middle bits so that a=0 can mark end of sprite
            sprite.append((a,((sy-py)-1),(sx-px),ti))
            #print("%02d: %02X %02X %4d,%4d" % (len(sprite)-1,a,ti,sprite[-1][2],sprite[-1][1]))
    # looking for common tiles:
    #sd = ">"
    #for (a,y,x,t) in sprite:
    #    sd += " %02X" % t
    #print(sd)
    return (sprite,st[0])

# creates an indexed palette image from sprite data
def sprite_to_img(sprite,tiles,palette):
    mx0 = min([s[2] for s in sprite])
    my0 = min([s[1] for s in sprite])
    mx1 = max([s[2] for s in sprite])
    my1 = max([s[1] for s in sprite])
    img = PIL.Image.new("P",(mx1+8-mx0,my1+8-my0),0)
    for (a,y,x,t) in reversed(sprite):
        t = tiles[t]
        if (a & 0x40) != 0:
            t = t.flipx()
        if (a & 0x80) != 0:
            t = t.flipy()
        t.draw_sprite(img,x-mx0,y-my0,a&3)
    index_setpal(img,palette)
    return img

def export_sprites(basename,spriteset):
    print("export sprites: " + basename)
    s = "; Generated: " + now_string + "\n"
    sh = "// Generated: " + now_string + "\n"
    table0 = bytearray()
    table1 = bytearray()
    sdata  = bytearray()
    offset = len(spriteset) * 2 # sdata starts at end of 16-bit offset table
    count = 0
    for (name,sprite) in spriteset:
        s += "SPRITE_%-20s = %d\n" % (name,count)
        sh += "#define SPRITE_%-20s   %d\n" % (name,count)
        count += 1
        table0.append(offset & 0xFF)
        table1.append(offset >> 8)
        for (a,y,x,t) in sprite:
            sdata.append(a)
            sdata.append(y&0xFF) # convert signed to unsigned byte
            sdata.append(x&0xFF)
            sdata.append(t)
        sdata.append(0) # a=0 marks end of sprite
        offset += (len(sprite) * 4) + 1
    s += "SPRITE_%-20s = %d\n" % ("COUNT",count)
    sh += "#define SPRITE_%-20s   %d\n" % ("COUNT",count)
    fileh   = os.path.join(OUTPUT_DIR,basename+".h")
    fileinc = os.path.join(OUTPUT_DIR,basename+".inc")
    filebin = os.path.join(OUTPUT_DIR,basename+".bin")
    print("sprite include: " + fileinc)
    open(fileinc,"wt").write(s)
    print("sprite include: " + fileh)
    open(fileh,"wt").write(sh)
    print("sprite data: " + filebin)
    sbin = table0+table1+sdata
    open(filebin,"wb").write(sbin)
    print("%d sprites. %d bytes." % (len(spriteset),len(sbin)))
    print()

def save_chr(f,tiles):
    filechr = os.path.join(OUTPUT_DIR,f+".chr")
    filepng = os.path.join(OUTPUT_DIR,f+".chr.png")
    print("%d tiles: %s" % (len(tiles),filechr))
    open(filechr,"wb").write(tile_dump(tiles))
    print("tile preview:" + filepng)
    tiles_to_img(tiles,grey_pal).save(filepng)
    print()

#
# KBMTEST stuff
#

grey_pal = [
    (  0,  0,  0),( 85, 85, 85),(170,170,170),(255,255,255),
    ]

bg_tiles = []
sp_tiles = []
spriteset = []

# font
bg_tiles = convert_chr("gfx/font.png",grey_pal,bg_tiles)

# sprites
sp_img = index_img_load("gfx/sprite.png",grey_pal)

(s,sp_tiles) = make_sprite(sp_img,  0, 0,  8, 8,  0, 0, sp_tiles)
spriteset.append(("off",s))
(s,sp_tiles) = make_sprite(sp_img,  8, 0,  8, 8,  8, 0, sp_tiles)
spriteset.append(("on",s))
(s,sp_tiles) = make_sprite(sp_img, 16, 0,  8, 8, 16, 0, sp_tiles)
spriteset.append(("mouse",s))


export_sprites("sprite",spriteset)
save_chr("bg",bg_tiles)
save_chr("sp",sp_tiles)

print("done.")
