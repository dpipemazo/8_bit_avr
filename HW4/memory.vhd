---------------------------------------------------------------------
-- Memory Management Unit
--
--
--  Revision History:
--      7 Feb 13  Sean Keenan       Initial revision.
---------------------------------------------------------------------


-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

entity  Memory  is

    port(
        -- Inputs
        IR          : in  opcode_word;                   -- Instruction Register
        XYZ         : in  std_logic_vector(15 downto 0); -- Input from XYZ
        SP          : in  std_logic_vector(15 downto 0); -- Stack Pointer
        RegA        : in  std_logic_vector(7 downto 0);  -- Register A from regs
        CycleCnt    : in  std_logic_vector(1 downto 0);  -- Cycle for instruction we're on
        MemCnst     : in  std_logic_vector(15 downto 0); -- Constant to load from memory
        -- Dealing with memory
        DataDB      : inout std_logic_vector(7 downto 0);-- Memory Data Bus
        AddrB       : out std_logic_vector(15 downto 0)  -- Address Bus
        ReadWrite   : out std_logic;                     -- read/write to main memory
        -- Dealing with register unit
        selXYZ      : out std_logic_vector(1 downto 0);  -- Select read from X/Y/Z
        writeX      : out std_logic;                     -- write to the X register
        writeY      : out std_logic;                     -- write to the Y register
        writeZ      : out std_logic;                     -- write to the Z register
        writeSP     : out std_logic                      -- write to the SP register
    );

end  Memory;

architecture memoryBehavior of Memory is

begin

end memoryBehavior;