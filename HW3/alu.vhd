----------------------------------------------------------------------------
--
--  Atmel AVR ALU Hardware Declaration
--
--  This is the entity which describes the ALU for the ATMEL CPU
--
--  Revision History:
--     Jan 30 13    Dan Pipe-Mazo   BWAAHHHHHHH Inception. 
--
----------------------------------------------------------------------------

-- Import the standard IEEE libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

-- Import the custom libraries which Glen gave for this assignment
library opcodes;
use opcodes.opcodes.all;


entity  ALU  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
        write_reg :  out std_logic;                         -- write enable for registers 
        clk_cycle :  out std_logic                          -- which clock cycle of a 
                                                            --  2 clock instruction we're on
                                                            --  Only matters for ADIW, MUL and SBIW
    );

end  ALU;

architecture behavioral of ALU is 

-- Declare internal signals
signal internal_result : std_logic_vector(7 downto 0);  -- What gets mapped to
                                                        -- output result on clock edges
signal internal_status_reg : std_logic_vector(7 downto 0); -- Internal value of the status
                                                           -- Register.
signal adder_result : std_logic_vector(7 downto 0);     -- output of the adder/subtracter
                                                        -- unit
signal alu_adder_carry : std_logic;                     -- the carry out from the ALU adder
signal adder_b_input : std_logic_vector(7 downto 0);    -- The input to "B" in the adder


-- begin the process
begin

    -- Wire up the alu adder unit. This unit is able to perform
    --  subtraction with and without carry, addition with and 
    --  without carry. 
    aluAdd : entity alu_adder port map(
            Ci <= StatReg(7);           -- Carry flag from status register
            sub <= not IR(11);          -- Bit difference between add and subtract
            useCarry <= IR(12);         -- Whether to use the carry or not
            A <= OperandA;              -- map A to A
            B <= adder_b_input;              -- map B to B
            S <= adder_result;          -- map the result to adder_result
            Cout <= alu_adder_carry    -- map the carry out to a temporary signal
            );

    -- Map the correct input to the adder
    adder_b_input <= "00"&IR(7 downto 6)&IR(3 downto 0) when ((std_match(IR, OpADIW) or std_match(IR, OpSBIW)) and clk_cycle = '1') else
                     OperandB;

    -- When statement to map the correct results to the internal result line
    internal_result <=  adder_result when (std_match(IR, OpADD) or std_match(IR, OpADC) or std_match(IR, OpSUB) or std_match(IR, OpSBC)), 




    -- Clock the internal result to the external result on clock edges
    clockResult : process(clock)
    begin

        -- DFF the result and status registers on clock edges
        if (rising_edge(clock)) then
            Result <= internal_result;
            StatReg <= internal_status_reg;

            
        end if;

    end process clockResult;




