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

    -- Map the select lines of the XYZ mux in the register unit
    SelXYZ <=   IR(4) & '0' when (  std_match(IR, OpLDDY) or 
                                    std_match(IR, OpLDDZ) or 
                                    std_match(IR, OpSTDY) or 
                                    std_match(IR, OpSTDZ) ) else

                IR(4 downto 3);

    -- When to write the X register
    writeX <= '1' when( (std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDXI) or 
                            std_match(IR, OpLDYI) or
                            std_match(IR, OpLDZI) or
                            std_match(IR, OpSTXI) or
                            std_match(IR, OpSTYI) or 
                            std_match(IR, OpSTZI))) or 
                        (std_match(CycleCnt, "00") and (
                            std_match(IR, OpLDXD) or
                            std_match(IR, OpLDYD) or 
                            std_match(IR, OpLDZY)))


        ); 

end memoryBehavior;