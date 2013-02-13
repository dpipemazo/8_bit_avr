----------------------------------------------------------------------------
--
--  Atmel AVR Program Unit Definition
--
--  This is the Atmel AVR Program Unit Definition.
--
--  Revision History:
--     12 Feb 13  Sean Keenan       Initial revision.
--
----------------------------------------------------------------------------

-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

entity  PROG  is

    port(   
        IR        :  in  opcode_word;                   -- Instruction Register
        PCIn      :  in  std_logic_vector(15 downto 0); -- New value for PC from memory unit
        CycleCnt  :  in  std_logic_vector(1 downto 0);  -- The clk of an instruction we're on

        ProgDB    :  in  std_logic_vector(15 downto 0); -- Program Data Bus
        ProgAB    :  out std_logic_vector(15 downto 0)  -- Program Address Bus (PC)
        ProgToWr  :  out std_logic_vector(7 downto 0);  -- Data to write to Memory
        AddPc     :  out std_logic                      -- Flag to denote that we are adding 
                                                        -- PC to a constant
    );

end  PROG;

architecture regBehavior of PROG is

    signal  PC              :  std_logic_vector(15 downto 0);

                                                        -- Not always used, only on certain instructions
    signal  ToUsePCIn       :  std_logic;                     -- Flag to note when to use PCIn

begin

    process (clock)
    begin

        if (rising_edge(clock) )  then


        end if;

    end process;


end regBehavior;