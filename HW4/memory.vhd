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
        AddrB       : out std_logic_vector(15 downto 0); -- Address Bus
        DataRd      : out std_logic;                     -- read to main memory, active low
        DataWr      : out std_logic;                     -- write to main memory, active low
        -- Dealing with register unit
        selXYZ      : out std_logic_vector(1 downto 0);  -- Select read from X/Y/Z
        writeX      : out std_logic;                     -- write to the X register
        writeY      : out std_logic;                     -- write to the Y register
        writeZ      : out std_logic;                     -- write to the Z register
        writeSP     : out std_logic                      -- write to the SP register
    );

end  Memory;

architecture memoryBehavior of Memory is

-- Signals for the adder
signal AdderInA : std_logic_vector(15 downto 0);
signal AdderInB : std_logic_vector(15 downto 0);

begin

--
-- REGISTER UNIT CONTROL
--


    -- Map the select lines of the XYZ mux in the register unit
    SelXYZ <=   IR(4) & '0' when (  std_match(IR, OpLDDY) or 
                                    std_match(IR, OpLDDZ) or 
                                    std_match(IR, OpSTDY) or 
                                    std_match(IR, OpSTDZ) ) else

                IR(4 downto 3);

    -- When to write the X register
    writeX <= '1' when( (std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDXI) or 
                            std_match(IR, OpSTXI))) or 
                        (std_match(CycleCnt, "00") and (
                            std_match(IR, OpLDXD) or
                            std_match(IR, OpSTXD))) ) else
                '0';

    -- When to write the Y register
    writeY <= '1' when( (std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDYI) or
                            std_match(IR, OpSTYI))) or 
                        (std_match(CycleCnt, "00") and (
                            std_match(IR, OpLDYD) or 
                            std_match(IR, OpSTYD))) ) else
                '0';

    -- When to write the Z register
    writeZ <= '1' when( (std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDZI) or
                            std_match(IR, OpSTZI))) or 
                        (std_match(CycleCnt, "00") and (
                            std_match(IR, OpLDZD) or 
                            std_match(IR, OpSTZD))) ) else
                '0';

    -- When to write the SP register
    writeSP <= '1' when((std_match(CycleCnt, "01") and
                            std_match(IR, OpPUSH)) or 
                        (std_match(CycleCnt, "00") and 
                            std_match(IR, OpPOP))) else
                '0';

--
-- Address Bus Control
--
    
    AdderInA <= SP      when(std_match(IR, OpPUSH) or 
                                std_match(IR, OpPOP)) else
                MemCnst when(std_match(IR, OpSTS) or 
                                std_match(IR, OpLDS)) else
                XYZ;

    AdderInB <= (others => '1') when(  (std_match(CycleCnt, "00") and(
                                        std_match(IR, OpLDXD) or
                                        std_match(IR, OpLDYD) or
                                        std_match(IR, OpLDZD) or
                                        std_match(IR, OpSTXD) or
                                        std_match(IR, OpSTYD) or
                                        std_match(IR, OpSTZD))) or
                                       (std_match(CycleCnt, "01") and
                                        std_match(IR, OpPUSH))) else
                (others =>'0')&'1' when((std_match(CycleCnt, "01") and(
                                        std_match(IR, OpLDXI) or
                                        std_match(IR, OpLDYI) or
                                        std_match(IR, OpLDZI) or
                                        std_match(IR, OpSTXI) or
                                        std_match(IR, OpSTYI) or
                                        std_match(IR, OpSTZI))) or
                                       (std_match(CycleCnt, "00") and
                                        std_match(IR, OpPOP)))  else
                (others => '0')&IR(13)&IR(11 downto 10)&IR(2 downto 0) when(
                                        std_match(IR, OpLDDY) or 
                                        std_match(IR, OpLDDZ) or
                                        std_match(IR, OpSTDY) or
                                        std_match(IR, OpSTZD)) else
                (others => '0');

    -- Now put the sum of AdderInA and AdderInB on the address bus
    AddrB <= AdderInA + AdderInB;

--
-- Data Bus Control 
--

    -- Tri-state the bus when we are not writing to it.    
    DataDB  <=  RegA when( (std_match(CycleCnt, "01") and(
                            std_match(IR, OpSTXI) or
                            std_match(IR, OpSTXD) or
                            std_match(IR, OpSTYI) or
                            std_match(IR, OpSTYD) or
                            std_match(IR, OpSTZI) or
                            std_match(IR, OpSTZD) or
                            std_match(IR, OpSTDY) or
                            std_match(IR, OpSTDZ) or 
                            std_match(IR, OpPUSH))) or 
                           (std_match(CycleCnt, "10") and
                            std_match(IR, OpSTS)) ) else
                (others => 'Z');



end memoryBehavior;