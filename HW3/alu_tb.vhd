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

                    -- Do the AND
                    expected_result = rand_inptA and rand_inptB;

                    -- wait for the answer
                    wait 20 ns;

                    assert(result = expected_result) report "Wrong answer random input OpAND test";

                --
                --
                --
                elsif ( op = OP_ANDI ) then

                    -- Create the 







                    


















