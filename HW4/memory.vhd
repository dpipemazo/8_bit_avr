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
        IR        :  in  opcode_word;                   -- Instruction Register
        XYZAddr   :  in  std_logic_vector(15 downto 0); -- Input from XYZ
        SP        :  in  std_logic_vector(15 downto 0); -- Stack Pointer
        RegA      :  in  std_logic_vector(7 downto 0);  -- Register A from regs
        CycleCnt  :  in  std_logic_vector(1 downto 0);  -- Cycle for instruction we're on
        MemoryCnst : in  std_logic_vector(15 downto 0); -- Constant to load from memory

        MemIn     :  inout std_logic_vector(7 downto 0); -- Memory Data Bus

        selXYZ    :  out std_logic_vector(1 downto 0);  -- Select read from X/Y/Z
        writeXYZ  :  out std_logic_vector(2 downto 0);  -- Selects XYZ from Addr Line
        Addr      :  out std_logic_vector(15 downto 0) -- Address line
    );

end  Memory;

architecture memoryBehavior of Memory is

begin

end memoryBehavior;