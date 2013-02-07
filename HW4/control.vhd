---------------------------------------------------------------------
-- Control Unit
---------------------------------------------------------------------

-- Import IEEE libraries
library ieee;
use ieee.std_logic_1164.all;

-- Import the custom libraries which Glen gave
library work;
use work.opcodes.all; 

entity Control is

    port(
        IR      : in opcode_word;           -- Instruction register. Will 
                                            -- delete this after HW4.
        ProgDB  : in std_logic(15 downto 0) -- 


    );



