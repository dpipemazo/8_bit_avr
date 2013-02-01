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
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

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
        OP_FMUL,
        OP_FMULS,
        OP_FMULSU,
        OP_INC,
        OP_LSR,
        OP_MUL,
        OP_MULS,
        OP_MULSU,
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
            sysclk <= '0';
            wait for 10 ns;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            sysclk <= '1';
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
        variable int_randA, int_randB, int_expected: integer;
        variable rand_inptA, rand_inptB, expected: std_logic_vector(7 downto 0);
        variable op : curr_op;
        variable temp_op : std_logic_vector(15 downto 0);
        variable check_bit : std_logic;

    begin

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
                    int_expected := int_randA + int_randB;

                    -- Compensate for the carry if there is one
                    if (StatReg(0) = '1') then
                        int_expected := int_expected + 1;
                    end if;

                    -- Compensate for overflow
                    if (int_expected >= 256) then
                        int_expected := int_expected - 256;
                    end if;

                    -- Now wait for the answer
                    wait for 20 ns;

                    -- Now check the answer
                    assert(conv_integer(result) = int_expected) report "Wrong Answer random input OpADC test";

                --
                -- INSTRUCTION: ADD
                --
                elsif ( op = OP_ADD ) then

                    -- Move the correct instruction in
                    IR <= OpADD;

                    -- Do the add
                    int_expected := int_randA + int_randB;

                    -- Compensate for overflow
                    if (int_expected >= 256) then
                        int_expected := int_expected - 256;
                    end if;

                    -- Now wait for the answer
                    wait 20 ns;

                    -- And check the answer
                    assert(conv_integer(result) = int_expected) report "Wrong answer random input OpADD test";

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
                    int_expected = int_randA + conv_integer(rand_inptB(5 downto 0));

                    -- compensate for overflow
                    if (int_expected >= 256) then
                        int_expected := int_expected - 256;
                    end if;

                    -- now wait for the first answer
                    wait 20 ns;

                    assert(conv_integer(result) = int_expected) report "Wrong answer random input OpADIW test clock 1";

                    --
                    -- SECOND CLOCK, add carry (leaving operandA on the bus)
                    --

                    if (StatusReg(0) = '1') then
                        int_expected = int_randA + 1;
                    else
                        int_expected = int_randA;
                    end if;

                    -- Compensate for overflow
                    if (int_expected >= 256) then
                        int_expected := int_expected - 256;
                    end if;

                    -- Now wait for the second answer
                    wait 20 ns;

                    -- check the answer
                    assert(conv_integer(result) = int_expected) report "Wrong answer random input OpADIW test clock 2";

                --
                -- INSTRUCTION: AND
                --
                elsif ( op = OP_AND ) then

                    -- Put the correct instruction on the bus
                    IR <= OpAND;

                    -- Do the AND
                    expected_result = rand_inptA and rand_inptB;

                    -- wait for the answer
                    wait 20 ns;

                    assert(result = expected_result) report "Wrong answer random input OpAND test";

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
                    expected_result = rand_inptA and rand_inptB;

                    -- wait for the answer
                    wait 20 ns;

                    -- check the answer
                    assert(result = expected_result) report "Wrong answer random input OpANDI test";

                --
                -- INSTRUCTION: ASR
                --
                elsif ( op = OP_ASR ) then

                    -- Put the instruction word on the bus
                    IR <= OpASR;

                    -- Do the shift
                    expected_result(6 downto 0) := rand_inptA(7 downto 1);
                    expected_result(7) := rand_inptA(7);

                    -- wait for the answer
                    wait 20 ns;

                    -- check the answer
                    assert(result = expected_result) report "Wrong answer random input OpASR test";

                --
                -- INSTRUCTION: BCLR
                --
                elsif ( op = OP_BCLR ) then

                    -- Need to build up the instruction word
                    temp_op := OpBCLR;
                    -- Use the bottom 3 bits of random input A to 
                    --  choose which bit of the status register to 
                    --  clear
                    temp_op(6 downto 4) := rand_inptA(2 downto 0);

                    -- Put the instruction on the instruction bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait 20 ns;

                    -- check the answer
                    assert(StatReg(conv_integer(rand_inptA(2 downto 0))) = '0') report "Wrong answer random input OpBCLR test";


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
                    wait 20 ns;

                    -- check the answer
                    assert(result(conv_integer(rand_inptB(2 downto 0))) = check_bit) report "Wrond answer random input OpBLD test"

                --
                -- INSTRUCTION: BSET
                --
                elsif( op = OP_BSET ) then

                    -- Need to build up the instruction word
                    temp_op := OpBSET;
                    -- Use the bottom 3 bits of random input A to 
                    --  choose which bit of the status register to 
                    --  clear
                    temp_op(6 downto 4) := rand_inptA(2 downto 0);

                    -- Put the instruction on the instruction bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait 20 ns;

                    -- check the answer
                    assert(StatReg(conv_integer(rand_inptA(2 downto 0))) = '1') report "Wrong answer random input OpBCLR test";

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
                    wait 20 ns;

                    -- check the answer
                    assert(StatReg(conv_integer(rand_inptB(2 downto 0))) = rand_inptA(conv_integer(rand_inptB(2 downto 0))) ) report "Wrong answer random input OpBST test";

                --
                -- INSTRUCTION: COM
                --
                elsif ( op = OP_COM ) then 

                    -- Put the instruction word on the bus
                    IR <= OpCom;

                    -- wait for the answer
                    wait 20 ns;

                    -- Check the answer
                    assert(conv_integer(result) = (-1*int_randA + 1) + 1) report "Wrong answer random input OpCOM test";

                --
                -- INSTRUCTION: CP
                --
                elsif( op = OP_CP ) then
                    -- Put the instruction word on the bus
                    IR <= OpCP

                    -- wait for the answer
                    wait 20 ns;

                --
                -- INSTRUCTION: CPC
                --
                elsif ( op = OP_CPC )

                    -- Put the instruction word on the bus
                    IR <= OpCPC;

                    -- wait for the answer
                    wait 20 ns;

                --
                -- INSTRUCTION: CPI
                --
                elsif ( op = OP_CPI ) 

                    -- Build up the instruction word
                    temp_op := OpCPI;
                    temp_op(3 downto 0) := rand_inptB(3 downto 0);
                    temp_op(11 downto 8) := rand_inptB(7 downto 4);

                    -- Put the instruction word on the bus
                    IR <= temp_op;

                    -- wait for the answer
                    wait 20 ns;

                --
                -- INSTRUCTION: DEC
                --


                --
                -- DONE LOOPING THROUGH INSTRUCTIONS! YAY
                --
                end if;

                -----
                --
                -- NOW CHECK THE FLAGS
                --
                -----

                --
                -- CARRY FLAG
                --
                if ( op = ( OP_ADC  or OP_ADD   or OP_ADIW or
                            OP_CP   or OP_CPC   or OP_CPI  or
                            OP_NEG  or OP_SBC   or OP_SBCI or
                            OP_SBIW or OP_SUB   or OP_SUBI)) then
                    
                    -- compute the carry flag
                    check_bit := ((not rand_inptA(7)) and rand_inptB(7)) or (rand_inptB(7) and result(7)) or (result(7) and (not rand_inptA(7)));
                    assert(StatRegs(0) = check_bit ) report "Adder Unit carry Flag incorrect";


                elsif ( op = COM ) then
                    assert(StatRegs(0) = '1') report "COM instruction carry flag incorrect";

                elsif ( op = ( OP_ASR or OP_ROR or OP_LSR ) ) then
                    assert(StatRegs(0) = rand_inptA(0)) report "Shifter Unit carry flag incorrect";

                end if;

                --
                -- ZERO FLAG
                --
                if ( op /= ( OP_BCLR or OP_BLD   or OP_BSET or
                                OP_BST  or OP_SWAP ) ) then

                    if ( conv_integer(result) = 0 ) then
                        check_bit := '1';
                    else
                        check_bit := '0';
                    end if;

                    assert(StatRegs(1) = check_bit) report "Zero flag incorrect";

                end if;

                --
                -- NEGATIVE FLAG
                --
                if ( op /= ( OP_BCLR or OP_BLD  or OP_BSET or
                             OP_BST  or OP_SWAP ) ) then

                    assert(StatRegs(2) = result(7)) report "Negative Flag Incorrect";

                end if;

                --
                -- SIGNED OVERFLOW FLAG
                --

                if ( op = ( OP_ADC  or OP_ADD  or OP_ADIW or
                            OP_CPC  or OP_CPC  or OP_CPI  or 
                            OP_DEC  or OP_INC  or OP_NEG  or
                            OP_SBC  or OP_SBCI or OP_SBIW or
                            OP_SUB  or OP_SUBI ) ) then

                    check_bit := (rand_inptA(7) and not rand_inptB(7) and not result(7)) or (not rand_inptA(7) and rand_inptB(7) and result(7));
                    assert(StatRegs(3) = check_bit) report "Arithmetic operation Signed Overflow Incorrect";

                elsif( op =(OP_AND  or OP_ANDI or OP_COM  or
                            OP_EOR  or OP_OR   or OP_ORI ) ) then
                    assert(StatRegs(3) = '0') report "Logical operation Signed Overflow Incorrect";

                elsif( op =(OP_ASR  or OP_LSR  or OP_ROR ) ) then
                    check_bit := result(7) xor rand_inptA(0);
                    assert(StatRegs(3) = check_bit) report "Shift operation signed overflow incorrect"

                end if;

                --
                --  SIGN BIT
                --

                if ( op /= ( OP_BCLR or OP_BLD  or OP_BSET or
                             OP_BST  or OP_SWAP ) ) then

                    assert( StatRegs(4) = (StatRegs(3) xor StatRegs(2))) report "Sign flag incorrect";

                end if;

                --
                -- HALF CARRY
                --

                if ( op = ( OP_ADD  or OP_ADC  or OP_CP   or
                            OP_CPC  or OP_CPI  or OP_NEG  or 
                            OP_SBC  or OP_SBCI or OP_SUB  or 
                            OP_SUBI )) then

                    check_bit := (not rand_inptA(3) and rand_inptB(3)) or (rand_inptB(3) and result(3)) or (not rand_inptA(3) and result(3));
                    assert(StatRegs(5) = check_bit) report "half-carry flag incorrect"; 
                
                end if;

            -- End the for loop
            end loop;

        -- End the while loop
        end loop;

    -- End the test process
    end process do_test;

-- All done describing the architecture
end TB_ARCHITECTURE;
















                    


















