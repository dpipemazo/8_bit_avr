----------------------------------------------------------------------------
--
--  Atmel AVR ALU Hardware Declaration
--
--  This is the entity which describes the ALU for the ATMEL CPU
--
--  This entity contains a few different blocks which each handle different
--  types of commands and then feed into an output mux which selects the 
--  appropriate line based on the input command. The same general idea holds
--  true for the status register as well. We will first describe the
--  different blocks in our system:
--
--  Adder Block : ADD, ADC, SUB, SUBC, ADIW, SBIW, SUBI, NEG, DEC, INC, COM, 
--                CP, CPC, CPI, SBCI, CPSE
--
--  This block consists of one 8-bit adder(/subtracter). With relatively
--  straightforward wiring for ADD/ADC/SUB/SUBC/CP/CPC/CPI. It differs
--  slightly from a standard "adder" block with some of the other commands
--  For ADIW/SBIW we perform a regular ADI first, followed by an ADC (added
--  with zero) on the second clock.
--  For NEG/COM we negate the input, and then either add one (for two's
--  complement for NEG) by setting the carry flag, or add nothing and just
--  share the inverter that we use with NEG. (we also make second operand 0)
--  For DEC/INC we also just set the carry flag to the opposite of what it
--  normally would be for an addition/subtraction, and make the second operand
--  zero, so that we either add or subtract 1
--
--  Shifter Block: ASR, LSR, ROR
--
--  This block shifts results to the right, and sets the carry flag based on
--  the low bit of operandA. It then sets the high bit of the output to the 
--  low bit of operandA if ROR, high bit if ASR, or 0 if LSR.
--
--  AND Block: AND, ANDI
--
--  This block simply takes the AND of OperandA and the internal operand B
--  line, and puts it on it's own line (read below about internal operand B)
--
--  OR Block: OR, ORI
--
--  This block simply takes the OR of OperandA and the internal operand B
--  line, and puts it on it's own line (read below about internal operand B)
--
--  XOR block: EOR
--
--  This block simply takes the XOR of OperandA and the internal operand B
--  line, and puts it on it's own line.
--
--  SWAP Block: SWAP
--
--  Swaps the nibbles on the Operand A and puts it on it's own line
--
--  Bit setting Block: BCLR, BSET
--
--  Sets the approrpiate bit right on the Flags Mux
--
--  Bit Loading Block: BLD
--
--  Loads the appropriate bit from the Status Register and puts it into
--  the output line, without altering the other bits on OperandA
--
--  An important note about instructions that use immediates:
--  In order to simplify logic inside each of the different blocks described
--  above the operandB is internally changed to the immediate value inside
--  the ALU. This simplifies much of the internal logic
--
--  In addition this block supplies the clk_cycle line to the Register Block,
--  as mentioned, this is done so that logic isn't duplicated across the two
--  blocks. This line indicates if we are on the second clock of a two-clock
--  instruction (specifically ADIW, or SPIW)
--
--  To briefly touch on how the flags work, each bit of the status register
--  is attached to a large mux, which sets the appropriate bit or input line
--  based on the Block that executed, or the instructions that executed.
--
--  Revision History:
--     Jan 30 13    Dan Pipe-Mazo   BWAAHHHHHHH Inception. 
--     Jan 31 13    Dan Pipe-Mazo   Comments + debugging 
--     Jan 31 13    Sean Keenan     Comments + debugging
--      Feb 7 13    Dan and Sean    Updated for new control unit and LDI
--      Feb 23 13   Dan Pipe-Mazo   Trimmed some fat for optimization
--
----------------------------------------------------------------------------
    

-- Import the standard IEEE libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

-- Import the custom libraries which Glen gave for this assignment
library work;
use work.opcodes.all;
use work.alu_adder;

entity  ALU  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  buffer std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  buffer std_logic_vector(7 downto 0);   -- status register
        cycle_cnt :  in std_logic_vector(1 downto 0);       -- which clock cycle of a 
                                                            --  2 clock instruction we're on
                                                            --  Only matters for ADIW, MUL and SBIW
        result_zero : buffer std_logic                      -- Internal result 0 line, active high
    );

end  ALU;

architecture behavioral of ALU is 

-- Declare internal signals
signal internal_status_reg : std_logic_vector(7 downto 0); -- Internal value of the status
   
-- Intermediate results                                                        -- Register.
signal adder_result : std_logic_vector(7 downto 0);     -- output of the adder/subtracter
                                                        -- unit
signal shift_result : std_logic_vector(7 downto 0);
signal add_result : std_logic_vector(7 downto 0);
signal and_result : std_logic_vector(7 downto 0);
signal or_result  : std_logic_vector(7 downto 0);
signal xor_result : std_logic_vector(7 downto 0);
signal swap_result: std_logic_vector(7 downto 0);
signal bitset_result : std_logic_vector(7 downto 0);

-- Carry Flags
signal adder_carries : std_logic_vector(7 downto 0); 
signal shift_carry : std_logic;

-- Translated operand B
signal internal_op_b : std_logic_vector(7 downto 0);

-- Adder signals 
signal adder_a_input : std_logic_vector(7 downto 0);
signal adder_b_input : std_logic_vector(7 downto 0); 
signal adder_sub_input : std_logic;
signal adder_carry_input : std_logic;

-- begin the process
begin

    --
    -- Translate Operand B into the proper register or constant
    --
    internal_op_b <="00"&IR(7 downto 6)&IR(3 downto 0) when(std_match(IR, OpADIW) or
                                                            std_match(IR, OpSBIW)) else
                    IR(11 downto 8) & IR(3 downto 0)    when(std_match(IR, OpANDI) or 
                                                            std_match(IR, OpCPI) or
                                                            std_match(IR, OpORI) or
                                                            std_match(IR, OpSBCI) or
                                                            std_match(IR, OpSUBI) or
                                                            std_match(IR, OpLDI)) else 
                    OperandB;


    --
    -- INSTRUCTIONS: ADD, ADC, SUB, SUBC, ADIW, SBIW, SUBI, NEG, DEC, INC, COM, 
    --               CP, CPC, CPI, SBCI, LDI, CPSE
    --

    -- Wire up the alu adder unit. This unit is able to perform
    --  subtraction with and without carry, addition with and 
    --  without carry. 
    aluAdd : entity alu_adder port map(
            Ci => adder_carry_input,           -- Carry flag from status register
            sub => adder_sub_input,            -- Bit difference between add and subtract
            A => adder_a_input,                -- map A to A
            B => adder_b_input,                -- map B to B
            S => adder_result,               -- map the result to adder_result
            Carry => adder_carries              -- map the carry out to a temporary signal
            );

    -- Map the correct input to the adder
    adder_a_input <= not OperandA when ( std_match(IR, OpNEG) or std_match(IR, OpCOM)) else
                     "00000000"       when ( std_match(IR, OpLDI)) else
                     OperandA;


    adder_b_input <= "00000000"     when (((std_match(IR, OpADIW) or std_match(IR, OpSBIW)) and std_match(cycle_cnt, "01")) or
                                        std_match(IR, OpINC) or std_match(IR, OpDEC) or 
                                        std_match(IR, OpNEG) or std_match(IR, OpCOM)) else
                     internal_op_b;

    adder_sub_input <=  '0' when (std_match(IR, OpINC) or std_match(IR, OpNEG) or
                                  std_match(IR, OpCOM) or std_match(IR, OpADD) or
                                  std_match(IR, OpADC) or std_match(IR, OpADIW) or
                                  std_match(IR, opLDI)) else
                        '1';--when (std_match(IR, OpSUBI) or std_match(IR, OpDEC) or
                            --     std_match(IR, OpCP) or std_match(IR, OpCPC) or
                            --      std_match(IR, OpCPI) or std_match(IR, OpSUB) or
                            --      std_match(IR, OpSUBC) or std_match(SBCI) or 
                            --      std_match(IR, OpSBIW))  else 

    adder_Carry_input <= '1'        when (  std_match(IR, OpNEG) or std_match(IR, OpDEC) or
                                            std_match(IR, OpINC)) else
                         StatReg(0) when ( std_match(IR, OpADC) or std_match(IR, OpSBC) or
                                             std_match(IR, OpSBCI) or std_match(IR, OpCPC) or
                                             ((std_match(IR, OpADIW) or std_match(IR, OpSBIW)) and std_match(cycle_cnt, "01"))) else
                         '0';   --when (((std_match(IR, OpADIW) or std_match(IR, OpSBIW)) and clk_cycle = '0') or
                                --              std_match(IR, OpSUBI) or std_match(IR, OpADD) or 
                                --             std_match(IR, OpSUB) or std_match(IR, OpCOM), ;


    -- 
    -- INSTRUCTIONS: ASR, LSR, ROR
    --

    shift_result(6 downto 0) <= OperandA(7 downto 1);
    shift_result(7) <=  StatReg(0) when (std_match(IR, OpROR)) else
                        OperandA(7) when (std_match(IR, OpASR)) else
                        '0';
    shift_carry <= OperandA(0);

    --
    -- INSTRUCTIONS: AND, ANDI
    --
    and_result <= OperandA and internal_op_b;

    --
    -- INSTRUCTIONS: OR, ORI
    --
    or_result <= OperandA or internal_op_b;

    --
    -- INSTRUCTIONS: XOR
    --
    xor_result <= OperandA xor internal_op_b;

    --
    -- INSTRUCTION: SWAP
    --
    swap_result <= OperandA(3 downto 0) & OperandA(7 downto 4);

    --
    -- INSTRUCTIONS: BCLR, BSET
    --
    -- NOTE: implemented in flags section
    --

    --
    -- INSTRUCTION: BSET
    --
    bitset_result(0) =  '1' when std_match(IR(2 downto 0), "000") else
                        OperandA(0);

    bitset_result(1) =  '1' when std_match(IR(2 downto 0), "001") else
                        OperandA(1);

    bitset_result(2) =  '1' when std_match(IR(2 downto 0), "010") else
                        OperandA(2);

    bitset_result(3) =  '1' when std_match(IR(2 downto 0), "011") else
                        OperandA(3);

    bitset_result(4) =  '1' when std_match(IR(2 downto 0), "100") else
                        OperandA(4);

    bitset_result(5) =  '1' when std_match(IR(2 downto 0), "101") else
                        OperandA(5);

    bitset_result(6) =  '1' when std_match(IR(2 downto 0), "110") else
                        OperandA(6);

    bitset_result(7) =  '1' when std_match(IR(2 downto 0), "111") else
                        OperandA(7);


    --
    -- MAP INDIVIDUAL RESULTS TO INTERNAL RESULT IN MUX
    --

    -- When statement to map the correct results to the internal result line
    Result <= adder_result          when(   std_match(IR, OpADD)  or 
                                            std_match(IR, OpADC)  or 
                                            std_match(IR, OpSUB)  or 
                                            std_match(IR, OpSBC)  or 
                                            std_match(IR, OpADIW) or 
                                            std_match(IR, OpSBIW) or 
                                            std_match(IR, OpSUBI) or
                                            std_match(IR, OpINC)  or
                                            std_match(IR, OpDEC)  or
                                            std_match(IR, OpNEG)  or
                                            std_match(IR, OpCP)   or
                                            std_match(IR, OpCPC)  or
                                            std_match(IR, OpCPI)  or
                                            std_match(IR, OpSBCI) or
                                            std_match(IR, OpCOM)  or 
                                            std_match(IR, OpLDI)  or 
                                            std_match(IR, OpCPSE)) else
                       shift_result when(   std_match(IR, OpROR)  or
                                            std_match(IR, OpASR)  or
                                            std_match(IR, OpLSR)) else
                       and_result   when(   std_match(IR, OpAND)  or
                                            std_match(IR, OpANDI)) else
                       or_result    when(   std_match(IR, OpOR)   or
                                            std_match(IR, OpORI)) else
                       xor_result   when(   std_match(IR, OpEOR)) else
                       swap_result  when(   std_match(IR, OpSWAP))else
                       bitset_result;-- when(  std_match(IR, OpBLD)) else

    --
    --
    -- FLAGZ
    --
    --


    --
    -- CARRY FLAG
    --
    internal_status_reg(0)  <=  -- Use the high carry bit out of the adder on
                                -- a standard add or subtract
                                adder_carries(7) when(std_match(IR, OpADD) or 
                                                std_match(IR, OpADC) or 
                                                std_match(IR, OpSUB) or 
                                                std_match(IR, OpSBC) or 
                                                std_match(IR, OpADIW)or 
                                                std_match(IR, OpSBIW)or 
                                                std_match(IR, OpSUBI)or
                                                std_match(IR, OpCP)  or
                                                std_match(IR, OpCPC) or
                                                std_match(IR, OpCPI) or
                                                std_match(IR, OpSBCI)) else

                                -- Use the shift carry result when performing
                                --  a shift
                               shift_carry when(std_match(IR, OpROR) or
                                                std_match(IR, OpASR) or
                                                std_match(IR, OpLSR)) else

                                -- Carry flag always set on these instructions
                               '1' when( (std_match(IR, OpBSET) and std_match(IR(6 downto 4), "000")) or
                                          std_match(IR, OpCOM) or 
                                         (std_match(IR, OpNEG) and (result_zero = '0') )) else

                                -- Carry flag never set on these instructions
                               '0' when( (std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "000")) or
                                         std_match(IR, OpNEG)) else

                               -- Else, just hold state
                               StatReg(0);

    --
    -- ZERO FLAG
    --

    -- Need to make this a line which we can pipe out to the Program Unit
    --  in order to be able to skip on a CPSE. 
    result_zero <= not OR_REDUCE(Result);

    internal_status_reg(1) <=   -- Set on a bitset
                                '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "001") ) else

                                -- clear on a bitclear
                                '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "001") ) else

                                -- If all bits are 0, after the first clock of any 
                                --  valid instruction, then this is set. 
                                result_zero           when (   (std_match(cycle_cnt, "00") and(
                                                                    std_match(IR, OpADIW) or
                                                                    std_match(IR, OpSBIW))) or

                                                                std_match(IR, OpADC ) or
                                                                std_match(IR, OpADD ) or
                                                                std_match(IR, OpAND ) or
                                                                std_match(IR, OpANDI) or
                                                                std_match(IR, OpASR ) or
                                                                std_match(IR, OpCOM ) or
                                                                std_match(IR, OpCP  ) or
                                                                std_match(IR, OpCPI ) or
                                                                std_match(IR, OpDEC ) or
                                                                std_match(IR, OpEOR ) or
                                                                std_match(IR, OpINC ) or
                                                                std_match(IR, OpLSR ) or
                                                                std_match(IR, OpNEG ) or
                                                                std_match(IR, OpOR  ) or
                                                                std_match(IR, OpORI ) or
                                                                std_match(IR, OpROR ) or
                                                                std_match(IR, OpSBC ) or
                                                                std_match(IR, OpSBCI) or
                                                                std_match(IR, OpSUB ) or
                                                                std_match(IR, OpSUBI) ) else
                                -- On the second clock of an ADIW or SBIW, 
                                --  or the only clock of a CPC, only set if  
                                --  the zero flag was already set.
                                result_zero and StatReg(1) when (
                                                                std_match(IR, OpCPC) or
                                                               (std_match(cycle_cnt, "01") and(
                                                                    std_match(IR, OpADIW) or
                                                                    std_match(IR, OpSBIW)))) else
                                -- Else, hold state
                                StatReg(1);

    --
    -- NEGATIVE FLAG
    --
    internal_status_reg(2) <=   -- Set on a bitset
                                '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "010") ) else

                                -- clear on a bitclear
                                '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "010") ) else

                                -- Set to the high bit of the result for 
                                --    any valid instruction
                                Result(7)             when (    std_match(IR, OpADC ) or
                                                                std_match(IR, OpADD ) or
                                                                std_match(IR, OpADIW) or
                                                                std_match(IR, OpAND ) or
                                                                std_match(IR, OpANDI) or
                                                                std_match(IR, OpASR ) or
                                                                std_match(IR, OpCOM ) or
                                                                std_match(IR, OpCP  ) or
                                                                std_match(IR, OpCPC ) or
                                                                std_match(IR, OpCPI ) or
                                                                std_match(IR, OpDEC ) or
                                                                std_match(IR, OpEOR ) or
                                                                std_match(IR, OpINC ) or
                                                                std_match(IR, OpLSR ) or
                                                                std_match(IR, OpNEG ) or
                                                                std_match(IR, OpOR  ) or
                                                                std_match(IR, OpORI ) or
                                                                std_match(IR, OpROR ) or
                                                                std_match(IR, OpSBC ) or
                                                                std_match(IR, OpSBCI) or
                                                                std_match(IR, OpSBIW) or
                                                                std_match(IR, OpSUB ) or
                                                                std_match(IR, OpSUBI) ) else
                                -- else, hold state. 
                                StatReg(2);

    --
    -- SIGNED OVERFLOW FLAG
    --
    internal_status_reg(3) <=   -- Set on a bitset
                                '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "011") ) else

                                -- These instructions always clear the signed
                                --  overflow flag
                                '0' when(   ( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "011") ) or 
                                                std_match(IR, OpAND) or 
                                                std_match(IR, OpANDI) or
                                                std_match(IR, OpCOM) or
                                                std_match(IR, OpEOR) or
                                                std_match(IR, OpOR) or
                                                std_match(IR, OpORI)) else

                                -- If we are doing a shift, perform an XOR of 
                                --  the carry flag and the negative flag
                                internal_status_reg(2) xor internal_status_reg(0) when(
                                                std_match(IR, OpROR) or
                                                std_match(IR, OpASR) or
                                                std_match(IR, OpLSR)) else

                                -- For an add instruction, perform the 
                                --  xor of the carry into and out of bit 7
                                adder_carries(7) xor adder_carries(6) when(
                                                std_match(IR, OpADD) or
                                                std_match(IR, OpADC) or
                                                std_match(IR, OpADIW) or
                                                std_match(IR, OpINC)) else

                                -- On a negate instruction, only set if the 
                                --  result is equal to 0x80;
                                not(not result(7) or result(6) or result(5) or 
                                        result(4) or result(3) or result(2) or 
                                        result(1) or result(0)) when (
                                                std_match(IR, OpNEG)) else

                                -- On a subtract, set this negate the xor of 
                                --  the carry into and out of bit 7. 
                                not adder_carries(7) xor adder_carries(6) when(
                                                std_match(IR, OpSUB)  or 
                                                std_match(IR, OpSBC)  or 
                                                std_match(IR, OpSBIW) or 
                                                std_match(IR, OpSUBI) or
                                                std_match(IR, OpDEC)  or
                                                std_match(IR, OpCP)   or
                                                std_match(IR, OpCPC)  or
                                                std_match(IR, OpCPI)  or
                                                std_match(IR, OpSBCI)) else

                                -- Else, hold state
                                StatReg(3);

    --
    -- SIGN BIT
    --
    internal_status_reg(4) <=   -- Set on a bitset
                                '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "100") ) else

                                -- clear on a bitclear
                                '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "100") ) else

                                -- Always perform the xor of the negative flag
                                --  and the signed overflow flag
                                internal_status_reg(2) xor internal_status_reg(3) when (
                                                std_match(IR, OpADC ) or
                                                std_match(IR, OpADD ) or
                                                std_match(IR, OpADIW) or
                                                std_match(IR, OpAND ) or
                                                std_match(IR, OpANDI) or
                                                std_match(IR, OpASR ) or
                                                std_match(IR, OpCOM ) or
                                                std_match(IR, OpCP  ) or
                                                std_match(IR, OpCPC ) or
                                                std_match(IR, OpCPI ) or
                                                std_match(IR, OpDEC ) or
                                                std_match(IR, OpEOR ) or
                                                std_match(IR, OpINC ) or
                                                std_match(IR, OpLSR ) or
                                                std_match(IR, OpNEG ) or
                                                std_match(IR, OpOR  ) or
                                                std_match(IR, OpORI ) or
                                                std_match(IR, OpROR ) or
                                                std_match(IR, OpSBC ) or
                                                std_match(IR, OpSBCI) or
                                                std_match(IR, OpSBIW) or
                                                std_match(IR, OpSUB ) or
                                                std_match(IR, OpSUBI) ) else

                                -- Else, hold state
                                StatReg(4);

    --
    -- HALF CARRY
    --
    internal_status_reg(5) <= -- On an add, simply use the carry out of bit 3
                              adder_carries(3) when (   std_match(IR, OpADC) or
                                                        std_match(IR, OpADD)) else

                              -- On a subtract, need to flip the carry out of
                              --    bit 3.
                              not adder_carries(3) when(std_match(IR, OpCP) or
                                                        std_match(IR, OpCPC) or
                                                        std_match(IR, OpCPI) or
                                                        std_match(IR, OpSBC) or
                                                        std_match(IR, OpSBCI) or
                                                        std_match(IR, OpSUB) or
                                                        std_match(IR, OpNEG) or
                                                        std_match(IR, OpSUBI)) else

                            -- Set on a bitset
                            '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "101") ) else

                            -- Clear on a bitclear
                            '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "101") ) else

                            -- Else, hold state
                            StatReg(5);
    --
    -- INSTRUCTIONS: BST
    --
    internal_status_reg(6) <=   -- Set the T register to the bit of OperandA
                                -- described by the low 3 bits of IR on a BST
                                OperandA(to_integer(unsigned(IR(2 downto 0)))) when( std_match(IR, OpBST)) else

                                -- Set on a bitset
                                '1' when( std_match(IR, OpBSET) and std_match(IR(6 downto 4), "110") ) else

                                -- clear on a bitclear
                                '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "110") ) else

                                -- Else, hold state
                                StatReg(6);   

    --
    -- INTERRUPT BIT
    --
    internal_status_reg(7) <=   -- Set on a bitset or RETI
                                '1' when( (std_match(IR, OpBSET) and std_match(IR(6 downto 4), "111") ) or
                                          std_match(IR, OpRETI) ) else

                                -- Clear on a bitclear
                                '0' when( std_match(IR, OpBCLR) and std_match(IR(6 downto 4), "111") ) else
                                  
                                -- Else, hold state
                                StatReg(7);           


    -- Clock the internal result to the external result on clock edges
    clockResult : process(clock)
    begin

        -- DFF the result and status registers on clock edges
        if (rising_edge(clock)) then
            StatReg <= internal_status_reg;

        end if;

    end process clockResult;



end behavioral;


-- --
-- -- Define the testable ALU entity (Commented out for full implementation of entire
-- -- AVR CPU. Uncomment to run ALU_tb)
-- --
-- library ieee;
-- use ieee.std_logic_1164.all;

-- -- Import the custom libraries which Glen gave for this assignment
-- library work;
-- use work.opcodes.all;
-- use work.alu;
-- use work.control;

-- entity  ALU_TEST  is

--     port(
--         IR        :  in  opcode_word;                       -- Instruction Register
--         OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
--         OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
--         clock     :  in  std_logic;                         -- system clock
--         Result    :  out std_logic_vector(7 downto 0);      -- ALU result
--         StatReg   :  out std_logic_vector(7 downto 0)       -- status register
--     );

-- end  ALU_TEST;

-- architecture arch of ALU_TEST is 

--     signal clock_cycle : std_logic_vector(1 downto 0);
--     signal result_buffer : std_logic_vector(7 downto 0);
--     signal stat_reg_buffer : std_logic_vector( 7 downto 0);



--     -- Signals that are output that we trash
--     -- signal Result    : std_logic_vector(7 downto 0);  -- Trash ALU result
--     -- signal StatReg   : std_logic_vector(7 downto 0);  -- Trash Status Reg result
--     signal SP        : std_logic_vector(15 downto 0);
--     signal MemCnst   : std_logic_vector(15 downto 0);
--     signal XYZ       : std_logic_vector(15 downto 0);
--     signal IR_out    : std_logic_vector(15 downto 0);
--     signal Addr      : std_logic_vector(15 downto 0);  -- Address bus
--     signal RegInSel  : std_logic;                     -- 0 = ALU, 1 = Memory Data Bus
--     signal WriteReg  : std_logic;                     -- Write signal for registers

--     -- Constants
--     constant reset      : std_logic := '1';          -- Don't reset in these tests
--     constant Write_SP   : std_logic := '0';          -- Don't want to write to SP in tests
--     constant Zero16Bits : std_logic_vector(15 downto 0) := (others => '0');

-- begin
    
--     ConTest : entity Control port map(clock => clock,    -- Clock
--                                       reset => reset,    -- Reset is held high (not reset)
--                                       SP_in => Addr,     -- SP should be off of Addr Bus
--                                       Write_SP => Write_SP,
--                                       IR_in  => IR,
--                                       IR_out => IR_out,    -- Same instruction register 
--                                       ProgDB => Zero16Bits,-- Not testing "m" instructions 
--                                       SP => SP,            -- Trash SP
--                                       WriteReg => WriteReg, 
--                                       RegInSel => RegInSel,
--                                       CycleCnt => clock_cycle);

--     ALUTst: entity ALU port map( IR, OperandA, OperandB, clock, result_buffer, stat_reg_buffer, clock_cycle);

--     Result <= result_buffer;
--     StatReg <= stat_reg_buffer;

-- end arch;