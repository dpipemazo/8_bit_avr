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
        IR      : inout opcode_word;                -- Instruction register.Will 
                                                    -- delete this after HW4.
        ProgDB  : inout std_logic(15 downto 0);     -- Program Data Bus
        SP      : out std_logic_vector(15 downto 0);-- stack pointer
        MemCnst : out std_logic_vector(15 downto 0);-- memory constant from 
                                                    -- LDS and STS instructions
        WriteReg: out std_logic;                    -- write signal for regs.
        RegInSel: out std_logic;                    -- Which input to use for 
                                                    -- the registers. 0 = ALU, 
                                                    -- 1 = Data Data Bus
        CycleCnt: out std_logic(1 downto 0);        -- Which cycle of an 
                                                    -- instruction we are on. 
    );



