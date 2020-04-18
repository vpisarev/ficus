# The script has been derived from the script by Peter Kankowski: https://www.strchr.com/multi-stage_tables
# Here is the original copyright:

# *********************************************************************************************
# Unicode multi-stage table builder
# (c) Peter Kankowski, 2008
# Released under the zlib/libpng license (http://www.opensource.org/licenses/zlib-license.php)

# Build the table, including compacting the identical blocks
# and choosing the smallest possible item size
# *********************************************************************************************

# How to use the script to update _fx_unicode_data.gen.h:
# 1. download fresh UnicodeData.txt: ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt
# 2. run the following command: python unicode_data_gen.py > _fx_unicode_data.gen.h
# --
# the script should be compatible with both Python 2.x and Python 3.x

clhash1 = {}
clhash2 = {}

direct_thresh0 = 4096

class TableBuilder:
    def __init__(self, block_size, thresh):
        self.blocks = {} # Dictionary for finding identical blocks
        self.stage1 = [] # Stage 1 table contains block numbers (indices into stage 2 table)
        self.stage2 = [] # Stage 2 table contains the blocks with property values
        self.prev = -1
        self.count = 0
        self.block_size = block_size
        self.direct_thresh = thresh

    def add_block(self, block, count = 1):
        assert(len(block) == self.block_size)

        # If there is such block in the stage2 table, use it
        tblock = tuple(block)
        start = self.blocks.get(tblock)
        if start is None:
            # Allocate a new block
            start = len(self.stage2) / self.block_size
            self.stage2 += block
            self.blocks[tblock] = start

        # Add 'count' blocks with the same values
        self.stage1 += [start] * count

    def __get_type_size(self, seq):
        type_size = [("uint8_t", 1), ("uint16_t", 2), ("uint32_t", 4),
                     ("int8_t", 1), ("int16_t", 2), ("int32_t", 4)]
        limits = [(0, 255), (0, 65535), (0, 4294967295),
                  (-128, 127), (-32768, 32767), (-2147483648, 2147483647)]
        minval = min(seq)
        maxval = max(seq)
        for num, (minlimit, maxlimit) in enumerate(limits):
            if minlimit <= minval and maxval <= maxlimit:
                return type_size[num], minval, maxval
        else:
            raise (OverflowError, "Too large to fit into C types")

    def print_table(self, prop_name):
        (stage1_type, stage1_size), minval1, maxval1 = self.__get_type_size(self.stage1)
        (stage2_type, stage2_size), minval2, maxval2 = self.__get_type_size(self.stage2)

        # Print header
        total_size = len(self.stage1) * stage1_size + len(self.stage2) * stage2_size + (len(clhash1) + len(clhash2))*4
        print ("// This is automatically generated file. Do not edit it! Edit fx_uni_gen.py instead.")
        print ("//")
        print ("// Contains the lookup tables and _fx_uni_getdata() function that returns")
        print ("// essential information about unicode characters (UTF32).")
        print ("// The tables take %d bytes\n" % total_size)

        # Print the first stage
        print ("static const %s %s_stage1[] = {" % (stage1_type, prop_name))
        line = ''
        for i, block in enumerate(self.stage1):
            line += ("%2d," % block)
            if i % 16 == 15:
                print ("%s // U+%04X" % (line, ((i - 15) * self.block_size)))
                line = ''
        print ("};\n")

        # Print the second stage
        print ("static const %s %s_stage2[] = {" % (stage2_type, prop_name))
        line = ''
        for i, val in enumerate(self.stage2):
            if i % self.block_size == 0:
                print ("\n// block %d" % (i // self.block_size))
            line += ("%2d," % val)
            if i % 16 == 15:
                print (line)
                line = ''
        print ("};\n")

        for k in range(1, 3):
            if k == 1:
                h = clhash1
            else:
                h = clhash2

            items = h.items()
            items = [(i, v) for (v, i) in items]

            print ("static const int _fx_uni_data%d[] = {" % k)
            line = ''
            for (i, v) in sorted(items):
                line += ("%d," % v)
                if (i+1) % 7 == 0:
                    print (line)
                    line = ''
            print ("};\n")

        # Print access function
        func = """
static int %(p)s_getdata(char_ ch)
{
    const unsigned BLOCK_SIZE = %(size)d;
    const unsigned DIRECT_THRESH = %(dt)d;

    if(ch < DIRECT_THRESH) return %(p)s_data1[%(p)s_stage2[ch]];
    if(ch >= 0x110000) return FX_UNICODE_CAT_Unknown;
    unsigned block_offset = %(p)s_stage1[ch / BLOCK_SIZE] * BLOCK_SIZE;
    return %(p)s_data2[%(p)s_stage2[block_offset + ch %% BLOCK_SIZE]];
}"""
        print (func % {'t':stage2_type, 'p':prop_name, 'size':self.block_size, 'dt':self.direct_thresh})

        print ("// %s table, %d bytes" % (prop_name, total_size))
        print ("// stage1: [%d, %d], stage2: [%d, %d], tab1: %d, tab2: %d" % \
            (minval1, maxval1, minval2, maxval2, len(clhash1), len(clhash2)))


# Read UnicodeData.txt file
class UnicodeDataExtractor:
    def __init__(self, block_size, add_block_func):
        self.block_size = block_size
        self.add_block_func = add_block_func
        self.block = []
        self.next_char = 0
        self.def_val = self.__get_hashed(9, 1) # the default value is 'Cn' (General_Category=Unassigned)
        self.def_val = self.__get_hashed(9, 2) # the default value is 'Cn' (General_Category=Unassigned)
        self.span_val = self.def_val
        Ze = 30
        self.fix_category = {0x9: Ze, 0xa: Ze, 0xb: Ze, 0xc: Ze, 0xd: Ze, 0x85: Ze}
        self.categories = {'Lu':0, 'Ll':1, 'Lt':2, 'Lm':3, 'Lo':4, \
                      'Mn':5, 'Me':6, 'Mc':7, \
                      'Nd':8, 'Nl':9, 'No':10, \
                      'Zs':11, 'Zl':12, 'Zp':13, \
                      'Cn':14, 'Cc':15, 'Cf':16, 'Co':17, 'Cs':18, \
                      'Pd':19, 'Ps':20, 'Pe':21, 'Pc':22, 'Po':23, 'Pi':24, 'Pf':25, \
                      'Sm':26, 'Sc':27, 'Sk':28, 'So':29, 'Ze':30}
        self.bidir = {"AL": 0, "AN": 1, "B": 2, "BN": 3, "CS": 4, "EN": 5, "ES": 6, "ET": 7, \
                      "FSI": 8, "L": 9, "LRE": 10, "LRI": 11, "LRO": 12, "NSM": 13, "ON": 14, \
                      "PDF": 15, "PDI": 16, "R": 17, "RLE": 18, "RLI": 19, "RLO": 20, "S": 21, "WS": 22}

    def __get_hashed(self, val, i):
        if i == 1:
            h = clhash1
        elif i == 2:
            h = clhash2
        c = h.get(val)
        if c is None:
            c = len(h)
            h[val] = c
        return c

    def __decode(self, char, chardata):
        val = self.fix_category.get(char)
        if val is None:
            val = self.categories[chardata[2]]
        attr = 0
        if chardata[12]:
            attr = int(chardata[12], 16) - char
        if chardata[13]:
            attr = int(chardata[13], 16) - char
        if chardata[6]:
            attr = int(chardata[6], 10)
        bd = 0
        if chardata[4]:
            bd = self.bidir[chardata[4]]
        code = val + bd*32 + attr*32*32
        if char < direct_thresh0:
            return self.__get_hashed(code, 1)
        else:
            return self.__get_hashed(code, 2)

    def __add_char(self, val):
        # Add to the block while it's not filled
        self.block.append(val)
        if len(self.block) == self.block_size:
            self.add_block_func(self.block)
            self.block = []

    def __add_chars(self, val, count):
        n = min(self.block_size - len(self.block), count)
        for i in range(n):
            self.block.append(val)
        if len(self.block) == self.block_size:
            self.add_block_func(self.block)
            self.block = []

        # Add a lot of blocks in a long span (optimized)
        if (count - n) / self.block_size != 0:
            block = [val] * self.block_size
            self.add_block_func(block, (count - n) // self.block_size)

        # Add the remaining chars
        for i in range((count - n) % self.block_size):
            self.block.append(val)

    def add_line(self, line):
        chardata = line.split(';')
        char = int(chardata[0], 16)
        # Add unassigned characters or the preceding span
        if char > self.next_char:
            self.__add_chars(self.span_val, char - self.next_char)
        val = self.__decode(char, chardata)
        self.__add_char(val)

        # Special mode for character spans
        self.span_val = self.def_val if chardata[1][-8:] != ', First>' else val
        self.next_char = char + 1

    def finish(self):
        for i in range(self.next_char, 0x110000):
            self.__add_char(self.span_val)

block_size = 256
builder = TableBuilder(block_size, direct_thresh0)
extractor = UnicodeDataExtractor(block_size, builder.add_block)
file = open('UnicodeData.txt', 'r')
for line in file:
    extractor.add_line(line)
extractor.finish()
builder.print_table('_fx_uni')
