----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Bench
--
--  This is the test bench for testing the ATMEL AVR CPU
--
--  Revision History:
--     23 Jan 13    Dan Pipe-Mazo   Initial Revision
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.opcodes.all;
use work.alu;

-- Test bench entity declaration
entity alu_tb is
end alu_tb;

-- Test bench architecture declaration
architecture TB_ARCHITECTURE of alu_tb is 
    
    -- Declare the ALU_TEST component
    component  ALU_TEST  is

        port(
            IR        :  in  opcode_word;                       -- Instruction Register
            OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
            OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
            clock     :  in  std_logic;                         -- system clock
            Result    :  out std_logic_vector(7 downto 0);      -- ALU result
            StatReg   :  out std_logic_vector(7 downto 0)       -- status register
        );

    end  component;

    -- Signals to map to the I/O of the component
    signal IR : opcode_word;
    signal OperandA : std_logic_vector(7 downto 0);
    signal OperandB : std_logic_vector(7 downto 0);
    signal clock    : std_logic;
    signal Result   : std_logic_vector(7 downto 0);
    signal StatReg  : std_logic_vector(7 downto 0);

    --Signal used to stop clock signal generators. should always be FALSE
    signal  END_SIM  :  BOOLEAN := FALSE;

    --
    -- OPCODE TYPE DEFINITION ENUM
    --
    type curr_op is(
        OP_ADC,
        OP_ADD,
        OP_ADIW,
        OP_AND,
        OP_ANDI,
        OP_ASR,
        OP_BCLR,
        OP_BLD,
        OP_BSET,
        OP_BST,
        OP_COM,
        OP_CP,
        OP_CPC,
        OP_CPI,
        OP_DEC,
        OP_EOR,
        OP_INC,
        OP_LSR,
        OP_NEG,
        OP_OR,
        OP_ORI,
        OP_ROR,
        OP_SBC,
        OP_SBCI,
        OP_SBIW,
        OP_SUB,
        OP_SUBI,
        OP_SWAP
    );

begin

    -- Declate the unit under test and map that shit
    UUT: ALU_TEST
        port map(

            IR => IR, 
            OperandA => OperandA, 
            OperandB => OperandB,
            clock => clock, 
            Result => Result,
            StatReg => StatReg

        );


    -- Make the system clock
    make_clock: process
    begin

        -- this process generates a 20 ns period, 50% duty cycle clock
        -- For sake of easy testing, can_read_vals will be the same duty cycle 
        -- as the system clock

        -- only generate clock while still have stimulus vectors

        if END_SIM = FALSE then
            clock <= '1';
            wait for 10 ns;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            clock <= '0';
            wait for 10 ns;
        else
            wait;
        end if;

    end process make_clock;    -- end of clock process


    --
    -- ACTUALLY TEST THE ALU
    --
    do_test: process

        -- Variables for generating random inputs
        variable seed1, seed2: positive;
        variable rand1, rand2: real;
        variable int_randA, int_randB: integer;
        variable rand_inptA, rand_inptB, expected: std_logic_vector(7 downto 0);
        variable op : curr_op;
        variable temp_op : std_logic_vector(15 downto 0);
        variable check_bit : std_logic;
        variable stat_lp : integer;

    begin

        --
        -- MAKE SURE BCLR WORKS
        --
        for stat_lp in 0 to 7 loop
            temp_op := OpBCLR;
            temp_op(6 downto 4) := std_logic_vector(to_unsigned(stat_lp, 3));
            IR <= temp_op;
            wait for 20 ns;
        end loop;

        -- want to offset the cycle a tad
        wait for 1 ns;

        -- Loop forever
        while ( END_SIM = FALSE ) loop

            -- Loop over all of the instructions, testing them
            --  with random inputs
            for op in curr_op loop 

                --
                -- Create the random variables
                --
                UNIFORM(seed1, seed2, rand1);
                UNIFORM(seed1, seed2, rand2);
                int_randA := INTEGER(TRUNC(rand1*256.0));
                int_randB := INTEGER(TRUNC(rand2*256.0));
                rand_inptA := std_logic_vector(to_unsigned(int_randA, rand_inptA'length));
                rand_inptB := std_logic_vector(to_unsigned(int_randB, rand_inptB'length));

                --
                -- Assign the random variables to the inputs A and B
                --
                OperandA <= rand_inptA;
                OperandB <= rand_inptB;

                --
                --
                -- Break out the test cases
                --
                --

                --
                -- INSTRUCTION: ADC
                --
                if ( op = OP_ADC ) then

                    -- Move the correct instruction in
                    IR <= OpADC;

                    -- Do the add
                    expected := std_logic_vector(unsigned(rand_inptA) + unsigned(rand_inptB));

                    -- Compensate for the carry if there is one
                    if (StatReg(0) = '1') then
                        expected := std_logic_vector(unsigned(expected) + 1);
                    end if;

                    -- Now wait for the answer
                    wait for 14 ns;

                    -- Now check the answer
                    assert(result = expected) report "Wrong Answer random input OpADC test";

                --
                -- INSTRUCTION: ADD
                --
                elsif ( op = OP_ADD ) then

                    -- Move the correct instruction in
                    IR <= OpADD;

                    -- Do the add
                    expected := std_logic_vector(unsigned(rand_inptA) + unsigned(rand_inptB));

                    -- Now wait for the answer
                    wait for 14 ns;

                    -- And check the answer
                    assert(result = expected) report "Wrong answer random input OpADD test";

                --
                -- INSTRUCTION: ADIW
                --
                elsif ( op = OP_ADIW ) then

                    --
                    -- FIRST CLOCK, add of a constant
                    --

                    -- Build up the correct instruction word
                    temp_op := OpADIW;

                    -- Take the bottom 6 bits of the random input B 
                    --  variable and put them into the temp_op
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(7 downto 6) := rand_inptB(5 downto 4);

                    -- now put the instruction on the instruction bus
                    IR <= temp_op;

                    -- Now calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) + unsigned("00"&rand_inptB(5 downto 0)));

                    -- now wait for the first answer
                    wait for 14 ns;

                    assert(result = expected) report "Wrong answer random input OpADIW test clock 1";

                    wait for 6 ns;

                    --
                    -- SECOND CLOCK, add carry (leaving operandA on the bus)
                    --

                    if (StatReg(0) = '1') then
                        expected := std_logic_vector(unsigned(rand_inptA) + 1);
                    else
                        expected := rand_inptA;
                    end if;

                    -- Now wait for the second answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpADIW test clock 2";

                --
                -- INSTRUCTION: AND
                --
                elsif ( op = OP_AND ) then

                    -- Put the correct instruction on the bus
                    IR <= OpAND;

                    -- Do the AND
                    expected := rand_inptA and rand_inptB;

                    -- wait for the answer
                    wait for 14 ns;

                    assert(result = expected) report "Wrong answer random input OpAND test";

                --
                -- INSTRUCTION: ANDI
                --
                elsif ( op = OP_ANDI ) then

                    -- Create the instruction word
                    temp_op := OpANDI;

                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- Put the instruction word on the bus
                    IR <= temp_op;

                    -- Do the AND
                    expected := rand_inptA and rand_inptB;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpANDI test";

                --
                -- INSTRUCTION: ASR
                --
                elsif ( op = OP_ASR ) then

                    -- Put the instruction word on the bus
                    IR <= OpASR;

                    -- Do the shift
                    expected(6 downto 0) := rand_inptA(7 downto 1);
                    expected(7) := rand_inptA(7);

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpASR test";

                --
                -- INSTRUCTION: BCLR
                --
                elsif ( op = OP_BCLR ) then

                    wait for 14 ns;

                --     -- Need to build up the instruction word
                --     temp_op := OpBCLR;
                --     -- Use the bottom 3 bits of random input A to 
                --     --  choose which bit of the status register to 
                --     --  clear
                --     temp_op(6 downto 4) := rand_inptA(2 downto 0);

                --     -- Put the instruction on the instruction bus
                --     IR <= temp_op;

                --     -- wait for the answer
                --     wait for 14 ns;

                --     -- check the answer
                --     assert(StatReg(to_integer(unsigned(rand_inptA(2 downto 0)))) = '0') report "Wrong answer random input OpBCLR test";


                --
                -- INSTRUCTION: BLD
                --
                elsif ( op = OP_BLD ) then

                    -- Need to build up the instruction word
                    temp_op := OpBLD;
                    temp_op(2 downto 0) := rand_inptB(2 downto 0);

                    -- Make a note of the T register
                    check_bit := StatReg(6);

                    -- Put the instruction word on the IR bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result(to_integer(unsigned(rand_inptB(2 downto 0)))) = check_bit) report "Wrond answer random input OpBLD test";

                --
                -- INSTRUCTION: BSET
                --
                elsif( op = OP_BSET ) then

                    wait for 14 ns;

                --     -- Need to build up the instruction word
                --     temp_op := OpBSET;
                --     -- Use the bottom 3 bits of random input A to 
                --     --  choose which bit of the status register to 
                --     --  clear
                --     temp_op(6 downto 4) := rand_inptA(2 downto 0);

                --     -- Put the instruction on the instruction bus
                --     IR <= temp_op;

                --     -- wait for the answer
                --     wait for 14 ns;

                --     -- check the answer
                --     assert(StatReg(to_integer(unsigned(rand_inptA(2 downto 0)))) = '1') report "Wrong answer random input OpBSET test";

                --
                -- INSTRUCTION: BST
                --
                elsif ( op = OP_BST ) then

                    -- Need to build up the instruction word
                    temp_op := OpBST;
                    temp_op(2 downto 0) := rand_inptB(2 downto 0);

                    -- Put the instruction on the instruction bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(StatReg(to_integer(unsigned(rand_inptB(2 downto 0)))) = rand_inptA(to_integer(unsigned(rand_inptB(2 downto 0)))) ) report "Wrong answer random input OpBST test";

                --
                -- INSTRUCTION: COM
                --
                elsif ( op = OP_COM ) then 

                    -- Put the instruction word on the bus
                    IR <= OpCom;

                    -- wait for the answer
                    wait for 14 ns;

                    -- calculate the expected result
                    expected := not rand_inptA;

                    -- Check the answer
                    assert(result = expected) report "Wrong answer random input OpCOM test";

                --
                -- INSTRUCTION: CP
                --
                elsif( op = OP_CP ) then
                    -- Put the instruction word on the bus
                    IR <= OpCP;

                    -- wait for the answer
                    wait for 14 ns;

                --
                -- INSTRUCTION: CPC
                --
                elsif ( op = OP_CPC ) then

                    -- Put the instruction word on the bus
                    IR <= OpCPC;

                    -- wait for the answer
                    wait for 14 ns;

                --
                -- INSTRUCTION: CPI
                --
                elsif ( op = OP_CPI ) then

                    -- Build up the instruction word
                    temp_op := OpCPI;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- Put the instruction word on the bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait for 14 ns;

                --
                -- INSTRUCTION: DEC
                --
                elsif( op = OP_DEC ) then 

                    -- Put the instruction on the bus
                    IR <= OpDEC;

                    -- Calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - 1);

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpDEC test";

                --
                -- INSTRUCTION: EOR
                --
                elsif ( op = OP_EOR ) then

                    -- Put the instruction on the bus
                    IR <= OpEOR;

                    -- calculate the expected result
                    expected := rand_inptA xor rand_inptB;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpEOR test";

                --
                -- INSTRUCTION: INC
                --
                elsif (op = OP_INC ) then

                    -- Put the instruction on the bus
                    IR <= OpINC;

                    -- Calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) + 1);

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpINC test";

                --
                -- INSTRUCTION: LSR
                --
                elsif ( op = OP_LSR ) then

                    -- Put the instruction on the bus
                    IR <= OpLSR;

                    -- Calculate the expected result
                    expected(6 downto 0) := rand_inptA(7 downto 1);
                    expected(7) := '0';

                    --wait for the answer
                    wait for 14 ns;

                    --check the answer
                    assert(result = expected) report "Wrong Answer random input OpLSR test";

                --
                -- INSTRUCTION: NEG
                --
                elsif ( op = OP_NEG ) then

                    -- Put the instruction on the bus
                    IR <= OpNEG;

                    -- Calculate the expected result
                    expected := std_logic_vector(unsigned(not rand_inptA) + 1);

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected ) report "Wrong answer random input OpNEG test";

                --
                -- INSTRUCTION: OR
                --
                elsif ( op = OP_OR ) then

                    -- Put the instruction on the bus
                    IR <= OpOR;

                    -- calculate the expected result
                    expected := rand_inptA or rand_inptB;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpOR test";

                --
                -- INSTRUCTION: ORI
                --
                elsif ( op = OP_ORI ) then

                    -- Make the instruction to put on the bus
                    temp_op := OpORI;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- Put the instruction on the bus
                    IR <= temp_op;

                    -- calculate the expected answer
                    expected := rand_inptA or rand_inptB;

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected) report "Wrong answer random input OpORI test";

                --
                -- INSTRUCTION: ROR
                --
                elsif ( op = OP_ROR ) then

                    -- Put the instruction on the bus
                    IR <= OpROR;

                    -- calculate the expected result
                    expected(6 downto 0) := rand_inptA(7 downto 1);
                    expected(7) := rand_inptA(0);

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected ) report "Wrong answer random input OpROR test";

                --
                -- INSTRUCTION: SBC
                --
                elsif ( op = OP_SBC ) then

                    -- Put the instruction on the bus
                    IR <= OpSBC;

                    -- calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - unsigned(rand_inptB));

                    -- compensate for the carry flag
                    if (StatReg(0) = '1') then
                        expected := std_logic_vector(unsigned(expected) - 1);
                    end if;

                    -- wait for answer
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected ) report "Wrong answer random input OpSBC test";

                --
                -- INSTRUCTION: SBCI
                --
                elsif ( op = OP_SBCI ) then

                    -- Need to make the opcode to put on the bus
                    temp_op := OpSBCI;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- Put the instruction on the bus
                    IR <= temp_op;

                    -- calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - unsigned(rand_inptB));

                    -- compensate for the carry flag
                    if (StatReg(0) = '1') then
                        expected := std_logic_vector(unsigned(expected) - 1);
                    end if;

                    -- wait for answer
                    wait for 14 ns;

                    -- check the answer
                    assert(result = expected) report "Wrong answer random input OpSBCI test";

                --
                -- INSTRUCTION: SBIW
                --
                elsif ( op = OP_SBIW ) then

                    -- Need to make the opcode to put on the bus
                    temp_op := OpSBIW;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(7 downto 6) := rand_inptB(5 downto 4);

                    -- Now put the opcode on the line
                    IR <= temp_op;

                    -- calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - unsigned("00"&rand_inptB(5 downto 0)));

                    -- wait for the answer
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected ) report "Wrong answer random input OpSBIW test clock 1";

                    wait for 6 ns;

                    -- Now need to do the second clock, which is just a subtract
                    --  with carry
                    if (StatReg(0) = '1') then
                        expected := std_logic_vector(unsigned(rand_inptA) - 1);
                    else
                        expected := rand_inptA;
                    end if;

                    -- now wait for the answer
                    wait for 14 ns;

                    assert( result = expected ) report "Wrong answer random input OpSBIW test clock 2";

                --
                -- INSTRUCTION: SUB
                --
                elsif ( op = OP_SUB ) then

                    -- Put the opcode on the bus
                    IR <= OpSUB;

                    -- calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - unsigned(rand_inptB));

                    -- wait for the answer to show up
                    wait for 14 ns;

                    -- check the answer
                    assert( result = expected ) report "Wrong answer random input OpSUB test";

                --
                -- INSTRUCTION: SUBI
                --
                elsif ( op = OP_SUBI ) then

                    -- Need to create the opcode for the bus
                    temp_op := OpSUBI;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- now put the opcode on the line
                    IR <= temp_op;

                    -- calculate the expected result
                    expected := std_logic_vector(unsigned(rand_inptA) - unsigned(rand_inptB));

                    -- wait for the answer to show up
                    wait for 14 ns;

                    -- check the answer
                    assert ( result = expected ) report "Wrong answer input OpSUBI test";

                --
                -- INSTRUCTION: SWAP
                --
                elsif ( op = OP_SWAP ) then

                    -- put the opcode on the bus 
                    IR <= OpSwap;

                    -- calculate the expected result
                    expected(3 downto 0) := rand_inptA(7 downto 4);
                    expected(7 downto 4) := rand_inptA(3 downto 0);

                    -- wait for the result to show up
                    wait for 14 ns;

                    -- check the answer
                    assert ( result = expected ) report "Wrong answer input OpSWAP test";

                --
                -- DONE LOOPING THROUGH INSTRUCTIONS! YAY
                --
                end if;

                -- we only waited for 14 ns to check the answer, now
                --  wait the remaining 5 ns for the next clock
                wait for 6 ns;

                -----
                --
                -- NOW CHECK THE FLAGS
                --
                -----

                --
                -- CARRY FLAG
                --
                if ( op = OP_ADC  or op = OP_ADD or op = OP_ADIW) then
                    
                    -- compute the carry flag
                    check_bit := ((rand_inptA(7)) and rand_inptB(7)) or (rand_inptB(7) and not result(7)) or (not result(7) and rand_inptA(7));
                    assert(StatReg(0) = check_bit ) report "Carry Flag incorrect on add operation";

                elsif(  op = OP_CP   or op = OP_CPC or op = OP_CPI  or
                        op = OP_SBIW or op = OP_SUB or op = OP_SUBI or
                        op = OP_SBC  or op = OP_SBCI) then

                    check_bit := (not rand_inptA(7) and rand_inptB(7)) or (rand_inptB(7) and result(7)) or (result(7) and not rand_inptA(7));
                    assert(StatReg(0) = check_bit ) report "Carry Flag incorrect on subtract operation";

                elsif( op = OP_NEG ) then

                    check_bit := OR_REDUCE(result);
                    assert(StatReg(0) = check_bit ) report "Carry Flag incorrect on negate operation";


                elsif ( op = OP_COM ) then
                    assert(StatReg(0) = '1') report "COM instruction carry flag incorrect";

                elsif ( op = OP_ASR or op = OP_ROR or op = OP_LSR ) then
                    assert(StatReg(0) = rand_inptA(0)) report "Shifter Unit carry flag incorrect";

                end if;

                --
                -- ZERO FLAG
                --
                if ( not ( op = OP_BCLR or op = OP_BLD or 
                           op = OP_BSET or op = OP_BST or 
                           op = OP_SWAP ) ) then

                    if ( OR_REDUCE(result) = '0' ) then
                        check_bit := '1';
                    else
                        check_bit := '0';
                    end if;

                    assert(StatReg(1) = check_bit) report "Zero flag incorrect";

                end if;

                --
                -- NEGATIVE FLAG
                --
                if ( not ( op = OP_BCLR or op = OP_BLD or 
                           op = OP_BSET or op = OP_BST or 
                           op = OP_SWAP ) ) then

                    assert(StatReg(2) = result(7)) report "Negative Flag Incorrect";

                end if;

                --
                -- SIGNED OVERFLOW FLAG
                --

                if ( op = OP_ADC  or op = OP_ADD or op = OP_ADIW or 
                     op = OP_INC ) then

                    check_bit := (rand_inptA(7) and rand_inptB(7) and not result(7)) or (not rand_inptA(7) and not rand_inptB(7) and result(7));
                    assert(StatReg(3) = check_bit) report "Arithmetic add operation Signed Overflow Incorrect";

                elsif(  op = OP_CP   or op = OP_CPC  or op = OP_CPI  or
                        op = OP_SBIW or op = OP_SUB  or op = OP_SUBI or
                        op = OP_SBC  or op = OP_SBCI or op = OP_DEC) then

                    check_bit := (rand_inptA(7) and not rand_inptB(7) and not result(7)) or (not rand_inptA(7) and rand_inptB(7) and result(7));
                    assert(StatReg(3) = check_bit) report "Arithmetic subtract operation Signed Overflow Incorrect";

                elsif( op = OP_NEG ) then
                    check_bit := (result(7) and not result(6) and not result(5) and not result(4) and not result(3) and not result(2) and not result(1) and not result(0));
                    assert(StatReg(3) = check_bit) report "Arithmetic negate operation Signed Overflow Incorrect";

                elsif( op = OP_AND  or op = OP_ANDI or op = OP_COM  or
                       op = OP_EOR  or op = OP_OR   or op = OP_ORI ) then
                    assert(StatReg(3) = '0') report "Logical operation Signed Overflow Incorrect";

                elsif( op = OP_ASR  or op = OP_LSR  or op = OP_ROR ) then
                    check_bit := result(7) xor rand_inptA(0);
                    assert(StatReg(3) = check_bit) report "Shift operation signed overflow incorrect";

                end if;

                --
                --  SIGN BIT
                --

                if ( not ( op = OP_BCLR or op = OP_BLD or 
                           op = OP_BSET or op = OP_BST or 
                           op = OP_SWAP ) ) then

                    assert( StatReg(4) = (StatReg(3) xor StatReg(2))) report "Sign flag incorrect";

                end if;

                --
                -- HALF CARRY
                --

                if ( op = OP_ADD  or op = OP_ADC  or op = OP_NEG ) then 

                    check_bit := (rand_inptA(3) and rand_inptB(3)) or (rand_inptB(3) and not result(3)) or (rand_inptA(3) and not result(3));
                    assert(StatReg(5) = check_bit) report "half-carry flag incorrect on add"; 
                     
                elsif(  op = OP_CPC  or op = OP_CPI  or op = OP_CP   or 
                        op = OP_SBC  or op = OP_SBCI or op = OP_SUB  or 
                        op = OP_SUBI ) then

                    check_bit := (not rand_inptA(3) and rand_inptB(3)) or (rand_inptB(3) and result(3)) or (result(3) and not rand_inptA(3));
                    assert(StatReg(5) = check_bit) report "half-carry flag incorrect on subtract";
                
                end if;

            -- End the for loop
            end loop;

        -- End the while loop
        end loop;

    -- End the test process
    end process do_test;

-- All done describing the architecture
end TB_ARCHITECTURE;
















                    


















