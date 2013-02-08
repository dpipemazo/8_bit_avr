----------------------------------------------------------------------------
--
--  Atmel AVR Memory Test Entity 
--
--
--  Revision History:
--     07 Feb 13  Sean Keenan  Initial Revision
--
----------------------------------------------------------------------------


-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;       -- Include this for random number generation 

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

-- Define Test Bench Entity
entity MEM_tb is
end MEM_tb;

architecture TB_MEM_ARCH of MEM_tb is

  -- Component declaration of the tested unit
  component  MEM_TEST

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

  end component;


  -- Instruction Register
  signal IR           :  opcode_word;

  -- Internal Clock Signal
  signal CLK          :  std_logic;

  -- Reset signal
  signal Reset        :  std_logic;

  -- Program Data Bus
  signal ProgDB       : std_logic_vector(15 downto 0);

  -- Data Address Bus
  signal DataAB       : std_logic_vector(15 downto 0);

  -- Data data bus
  signal DataDB       : std_logic_vector(7 downto 0);

  -- Data read signal
  signal DataRd       : std_logic;

  -- Data write signal
  signal DataWr       : std_logic;

  --Signal used to stop clock signal generators
  signal  END_SIM     :  BOOLEAN := FALSE;


  --
  -- Simple Load and Store Opcodes that only inc/dec or keep X unchanged
  --

  constant loadStoreSimpleSize : integer := 13;

  type LOAD_STR_SIMPLE_OP is array (0 to loadStoreSimpleSize) of std_logic_vector(15 downto 0);

  constant loadStoreSimple : LOAD_STR_SIMPLE_OP := (
      OpLDX,
      OpSTX,
      OpLDXI,
      OpSTXI,
      OpLDXD,
      OpSTXD,
      OpLDYI,
      OpSTYI,
      OpLDYD,
      OpSTYD,
      OpLDZI,
      OpSTZI,
      OpLDZD,
      OpSTZD
    );

  constant lastXCommand : integer := 5;
  constant lastYCommand : integer := 9;
  constant lastZCommand : integer := 13;

  --
  -- Load/Store Opcodes that add a constant to addressed location
  --

  constant loadConstSize : integer := 3;

  type LOAD_CONST_OP is array (0 to loadConstSize) of std_logic_vector(15 downto 0);

  constant loadConst : LOAD_CONST_OP := (
      OpLDDY,
      OpSTDY,
      OpLDDZ,
      OpSTDZ
    );

  --
  -- Commands that use memory (take three clocks)
  --

  constant memoryCmdsSize : integer := 1;

  type MEM_CMD_OP is array (0 to memoryCmdsSize) of std_logic_vector(15 downto 0);

  constant memoryCmds : MEM_CMD_OP := (
      OpLDS,
      OpSTS
    );


  --
  -- Commands that store some value to registers
  --

  constant simpleRegSize : integer := 1;

  type SIMPLE_REG_OP is array (0 to simpleRegSize) of std_logic_vector(15 downto 0);

  constant simpleReg : SIMPLE_REG_OP := (
      OpMOV,
      OpLDI
    );


  --
  -- Commands that store some value to registers
  --

  constant popPushSize : integer := 1;

  type POP_PUSH_OP is array (0 to popPushSize) of std_logic_vector(15 downto 0);

  constant popPush : SIMPLE_REG_OP := (
      OpPOP,
      OpPUSH
    );


begin

  -- Unit Under Test port map
  UUT : MEM_TEST
    port map(
      IR            =>  IR,
      ProgDB        =>  ProgDB,
      Reset         =>  Reset,
      clock         =>  clk,
      DataAB        =>  DataAB,
      DataDB        =>  DataDB,
      DataRd        =>  DataRd,
      DataWr        =>  DataWr
    );

  process

  -- Index used to determine what register we are looking at in our loops
  variable j : integer range 0 to 31;

  -- Variable that temporarily stores an op-code that is then transfered to the IR
  variable temp_op : std_logic_vector(15 downto 0);

  -- Variable that we use to temporarly store the address we want to for Register B
  -- Which we then combine into temp_op, which is then transfered to IR
  variable temp_b_reg : std_logic_vector(4 downto 0);

  -- Variables used for Random Number generation
  variable seed1, seed2: positive;               -- Seed values for random generator
  variable randInt, oldRandInt, randInt2 : integer;
  variable rand: real;                           -- Random real-number value in range 0 to 1.0

  -- Indexes used for the constant arrays filled with Instructions
  variable a, b : integer;

  variable AddressToLoad : std_logic_vector(15 downto 0);

  variable DBValue       : std_logic_vector(7 downto 0);


  variable Reg16Val      : std_logic_vector(7 downto 0);
  variable Reg17Val      : std_logic_vector(7 downto 0);

  begin

  DataDB <= (others <= 'Z');
  ProgDB <= (others <= 'Z');

  -- Ofset our start such that we start 1 ns after a rising clock edge
  wait for 11 ns;

  -- -- Check to make sure that we can write to all of our registers, read out of all of
  -- -- our registers on both lines A and B and make sure that CP doesn't alter any of the
  -- -- registers. We only test the Add and CP command over all of the possible registers

  for b in 0 to 10 loop


    UNIFORM(seed1, seed2, rand);                           -- generate random number
    randInt2 := INTEGER(TRUNC(rand*65536.0));                -- rescale to 0..256, find integer part
    AddressToLoad <= std_logic_vector(to_unsigned(randInt2, AddressToLoad'LENGTH));  -- convert to std_logic_vector

    -- Test the edge cases where the address to load is all zeros and all 1's
    -- This will test our Pre-decrement and post-increment commands more thoroughly
    if b = 0 then
      AddressToLoad := (others <= '0');
    elsif b = 1 then
      AddressToLoad := (others <= '1');
    end if;

    -- Loop over the commands in loadStoreSimpleSize
    for a in 0 to loadStoreSimpleSize loop

      -- If this is a command we load from X
      if a <= lastXCommand then
        registerToLoadInto := 26;
      -- else if this is a command we load from Y
      elsif a <= lastYCommand then
        registerToLoadInto := 28;
      -- else if this is a command we load from Z
      elsif a <= lastZCommand then
        registerToLoadInto := 30;
      else
        -- Not X/Y or Z
      end if;

      -- Load a command that performs a write
      temp_op  := OpLDI;

      -- Use registerToLoadInto (-16 since it's an Immediate Opcode)
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(registerToLoadInto - 16, 4));
        
      temp_op(3 downto 0) := AddressToLoad(3 downto 0);
      temp_op(11 downto 8) := AddressToLoad(7 downto 4);

      IR    <= temp_op;

      wait for 20 ns;

      -- Use registerToLoadInto + 1 (-16 since it's an Immediate Opcode)
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(registerToLoadInto - 16 + 1, 4));
        
      temp_op(3 downto 0) := AddressToLoad(11 downto 8);
      temp_op(11 downto 8) := AddressToLoad(15 downto 12);

      IR    <= temp_op;

      wait for 20 ns;

      -- We want to run the Pre/Post increment a few times, so loop 4 times
      for b in 0 to 3 loop


        -- Generate a value to be put on the DB
        UNIFORM(seed1, seed2, rand);                           -- generate random number
        randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
        DBValue <= std_logic_vector(to_unsigned(randInt2, DBValue'LENGTH));  -- convert to std_logic_vector

        -- We will now load this value into register 16
        -- (for load commands we do this so that we have a random value we overwrite)
        -- (for store commands we do this so that we have a random value to read)

        -- Load a command that performs a write
        temp_op  := OpLDI;

        -- Use register 16 (-16 since it's an Immediate Opcode)
        temp_op(7 downto 4) := std_logic_vector(to_unsigned(16 - 16, 4));
          
        temp_op(3 downto 0) := DBValue(3 downto 0);
        temp_op(11 downto 8) := DBValue(7 downto 4);

        IR    <= temp_op;

        wait for 20 ns;

        -- If a load command we should regenerate DBValue
        if (a % 2) = 0 then
          -- Generate a value to be put on the DB
          UNIFORM(seed1, seed2, rand);                           -- generate random number
          randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
          DBValue <= std_logic_vector(to_unsigned(randInt2, DBValue'LENGTH));  -- convert to std_logic_vector
        end if;

        -- Load a command that performs a Load/Store
        temp_op  := loadStoreSimple(a);

        -- Load/Store to Register 16
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(16, 5));

        IR <= temp_op;

        wait for 18 ns;

        -- We need to pre increment if a > 1 and 
        if (a = 2 or a = 3 or a = 6 or a = 7 or a = 10 or a = 11) then
          AddressToLoad := AddressToLoad + ("0000" & "0000" & "0000" & "0001");
        end if;

        assert(AddressToLoad = DataAB)
        report "Address Bus not set Properly after 1st clock"
        severity ERROR;

        assert(DataRd = '1')
        report "Data Read not supposed to be active yet!"
        severity ERROR;

        assert(DataWr = '1')
        report "Data Write not supposed to be active yet!"
        severity ERROR;

        -- Put us 5 ns into a clock
        wait for 6 ns;

        -- If a store command then assert that DataDB = DBValue
        if (a % 2) = 1 then

          assert(DataDB = DBValue)
          report "Data Bus not a valid value 5ns into second clock of Store command (LDI could also be broken)"
          severity ERROR;

        end if;

        wait for 6 ns;

        -- Check that DataRd or DataWr are low based on command
        if (a % 2) = 1 then
          assert(DataRd = '1')
          report "Data Read active on Write command!"
          severity ERROR;

          assert(DataWr = '0')
          report "Data Write not active on Write command"
          severity ERROR;
        else
          assert(DataRd = '0')
          report "Data Read not active on Read command"
          severity ERROR;

          assert(DataWr = '1')
          report "Data Write active on Read command!"
          severity ERROR;    
        end if;

        wait for 4 ns;

        -- If a load command put DBValue on DataDB now
        if (a % 2) = 0 then

          DataDB <= DBValue;

        end if;

        wait for 4 ns;

        -- Check all of the values

        assert(AddressToLoad = DataAB)
        report "Address Bus not set Properly after 2nd clock"
        severity ERROR;

        wait for 2 ns;

        -- We are now 1 ns ahead of the clk again.

        DataDB <= (others <= 'Z');

        -- If a load command, then we should check we wrote to the register
        -- Unfortunately, we have to do this by running a Store command, and
        -- checking if the value appears on the bus. 
        -- (which is also potetially buggy)
        if (a % 2) = 0 then
          -- Load a command that allows us to read, and shouldn't change X
          temp_op  := OpSTX;

          -- Use register 16 
          temp_op(8 downto 4) := std_logic_vector(to_unsigned(16, 5));

          IR    <= temp_op;

          wait for 30 ns;

          -- 11 ns into second clock

          -- Make sure that the data data bus has the expected value
          -- of register 16
          assert (DataDB = DBValue)
          report "On the load command, register was not written properly"
          severity ERROR;

          wait for 10 ns;

        end if;

        -- We need to post decrement if any of these values
        if (a = 4 or a = 5 or a = 8 or a = 9 or a = 12 or a = 13) then
          AddressToLoad := AddressToLoad - ("0000" & "0000" & "0000" & "0001");
        end if;

      end loop;
    end loop;
  end loop;


  -- Test OpMOV, we'll do this by storing a random value to register 16 and 17
  -- And then we'll move register 17 into register 16 and check that register 16
  -- has the value that was in register 17.
  -- We repeat this 5 times, just for good measure 
  -- (mostly to make sure the random values aren't the same...)
  for a in 0 to 4 loop
    
    --
    -- Write out random value to Register 17
    --

    UNIFORM(seed1, seed2, rand);                           -- generate random number
    randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
    Reg17Val <= std_logic_vector(to_unsigned(randInt2, Reg17Val'LENGTH));  -- convert to std_logic_vector

    -- Load a command that performs a write
    temp_op  := OpLDI;

    -- Use register 17 (-16 since it's an Immediate Opcode)
    temp_op(7 downto 4) := std_logic_vector(to_unsigned(17 - 16, 4));
      
    temp_op(3 downto 0) := Reg17Val(3 downto 0);
    temp_op(11 downto 8) := Reg17Val(7 downto 4);

    IR    <= temp_op;

    wait for 20 ns;

    --
    -- Write out random value to Register 16
    --

    -- Generate a value to be put on the DB
    UNIFORM(seed1, seed2, rand);                           -- generate random number
    randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
    Reg16Val <= std_logic_vector(to_unsigned(randInt2, Reg16Val'LENGTH));  -- convert to std_logic_vector

    -- Load a command that performs a write
    temp_op  := OpLDI;

    -- Use register 17 (-16 since it's an Immediate Opcode)
    temp_op(7 downto 4) := std_logic_vector(to_unsigned(16 - 16, 4));
      
    temp_op(3 downto 0) := Reg16Val(3 downto 0);
    temp_op(11 downto 8) := Reg16Val(7 downto 4);

    IR    <= temp_op;

    wait for 20 ns;

    --
    -- Move Register 17 into Register 16
    --

    -- Load a command that performs a write
    temp_op  := OpMOV;

    -- Use register 16 in Operand A
    temp_op(8 downto 4) := std_logic_vector(to_unsigned(16, 5));
      
    -- Use register 17 in Operand B
    temp_b_reg := std_logic_vector(to_unsigned(17, temp_b_reg'LENGTH));
    temp_op(9) := temp_b_reg(4);
    temp_op(3 downto 0) := temp_b_reg(3 downto 0);

    IR    <= temp_op;

    wait for 20 ns;

    --
    -- Read Register 16 and make sure it has the value stored in Register 17
    --

    -- Load a command that allows us to read, and shouldn't change X
    temp_op  := OpSTX;

    -- Use register 16
    temp_op(8 downto 4) := std_logic_vector(to_unsigned(16, 5));

    IR    <= temp_op;

    wait for 30 ns;

    -- 11 ns into second clock

    -- Make sure that the data data bus has the expected value
    -- of register 16
    assert (DataDB = Reg17Val)
    report "On the load command, register was not written properly"
    severity ERROR;

    wait for 10 ns;

  end loop;

  -- for a in 0 to dontWriteOpSize loop
  --   for b in 0 to writeOpSize loop
  --     for j in 0 to 31 loop

  --       -- Load a command that performs a write
  --       temp_op  := writeOp(b);

  --       -- Use Register j to write to
  --       temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

  --       -- If we are past register 0 (and thus have loaded a value into reg j - 1)
  --       -- and we are using instructions that use the second operand (a > 8)
  --       -- Then load register j-1 into RegBOut
  --       if (j > 0 and a > 8) then
  --         temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
  --         temp_op(9) := temp_b_reg(4);
  --         temp_op(3 downto 0) := temp_b_reg(3 downto 0);
  --       end if;

  --       IR  <= temp_op;

  --       -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
  --       oldRandInt := randInt;

  --       -- Generate a random number to store in reg (j)
  --       UNIFORM(seed1, seed2, rand);                          -- generate random number
  --       randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
  --       RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

  --       wait for 18 ns;

  --       -- If we are past register 0 (and thus have loaded a value into reg j -1)
  --       -- and we are using instructions that use the second operand (a > 8)
  --       -- Then we should check if B is read correctly
  --       if (j > 0 and a > 8) then
  --         assert (to_integer(unsigned(RegBOut)) = oldRandInt) 
  --         report "Non-Writing cmd altered register OR Reg B not valid"
  --         severity ERROR;
  --       end if;

  --       wait for 2 ns;

  --       -- Check to see if we have written properly (and that we aren't writing)

  --       temp_op  := dontWriteOp(a);
  --       temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

  --       -- If we are past register 0 (and thus have loaded a value into reg j - 1)
  --       -- We also want to make sure that the instruction supports a second operand
  --       -- Then load register j-1 into RegBOut
  --       if (j > 0 and b > 0) then
  --         temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
  --         temp_op(9) := temp_b_reg(4);
  --         temp_op(3 downto 0) := temp_b_reg(3 downto 0);
  --       end if;

  --       IR <= temp_op;

  --       -- Generate a random number to store in reg (j)
  --       -- We do this to make sure that dontWriteCommands are in fact not writing
  --       UNIFORM(seed1, seed2, rand);                           -- generate random number
  --       randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
  --       RegIn <= std_logic_vector(to_unsigned(randInt2, RegIn'LENGTH));  -- convert to std_logic_vector

  --       wait for 18 ns;

  --       -- And then make sure that infact the result has not changed
  --       assert(to_integer(unsigned(RegAOut)) = randInt) 
  --       report "Did not store Register Properly in Write command, or not reading properly"
  --       severity ERROR;

  --       wait for 2 ns;

  --     end loop;
  --   end loop;
  -- end loop;
  

  -- -- Now look at the registers that only operate on the second half of registers
  -- for a in 0 to secondHalfWriteSize loop
  --   for j in 0 to 15 loop

  --     -- Load a command that performs a write
  --     temp_op  := secondHalfWrite(a);

  --     -- Use Register j
  --     temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

  --     -- If we are past register 0 (and thus have loaded a value into reg j - 1)
  --     -- Then load register j-1 into RegBOut
  --     if (j > 0) then
  --       temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
  --       temp_op(9) := '1';
  --       temp_op(3 downto 0) := temp_b_reg(3 downto 0);
  --     end if;

  --     IR  <= temp_op;

  --     -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
  --     oldRandInt := randInt;

  --     -- Generate a random number to store in reg (j)
  --     UNIFORM(seed1, seed2, rand);                          -- generate random number
  --     randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
  --     RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

  --     wait for 18 ns;

  --     -- Check that Register B is valid if we have data on Register B
  --     if (j > 0) then
  --       assert (to_integer(unsigned(RegBOut)) = oldRandInt) 
  --       report "CPI altered register OR Reg B not valid"
  --       severity ERROR;
  --     end if;

  --     wait for 2 ns;

  --     -- Check to see if we have written properly (and that we aren't writing)
  --     -- OpCPI is the only Register that operates on only the second half of all registers
  --     -- and doesn't write, so lets use that.
  --     temp_op  := OpCPI;
  --     temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

  --     IR <= temp_op;

  --     -- Generate a random number to store in reg (j)
  --     -- We do this to make sure that dontWriteCommands are in fact not writing
  --     UNIFORM(seed1, seed2, rand);                           -- generate random number
  --     randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
  --     RegIn <= std_logic_vector(to_unsigned(randInt2, RegIn'LENGTH));  -- convert to std_logic_vector

  --     wait for 18 ns;

  --     -- And then make sure that infact the result has not changed
  --     assert(to_integer(unsigned(RegAOut)) = randInt) 
  --     report "Did not store Register Properly with second half reg commands"
  --     severity ERROR;

  --     wait for 2 ns;

  --   end loop;
  -- end loop;

  -- -- Test ADIW and SBIW commands (and BSET/BCLR commands)

  -- for a in 0 to twoClocksSize loop

  --   -- First we will load registers 24-31 with their own register number
  --   -- So that we can test these instructions more easily

  --   -- While we are at it, we will also make sure that BSET/BCLR don't alter
  --   -- the values stored using ADD, while also making sure that the registers
  --   -- are set up the values that we expect

  --   for j in 24 to 31 loop

  --     -- Load a command that performs a write
  --     temp_op  := OpADD;

  --     -- Use Register j as the RegAOut
  --     temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));
  --     IR    <= temp_op;

  --     RegIn <= std_logic_vector(to_unsigned(j, RegIn'LENGTH));  -- convert to std_logic_vector

  --     wait for 20 ns;

  --     -- Make sure that BCLR does not alter registers
  --     RegIn <= "00000000";
  --     IR    <= OpBCLR;

  --     wait for 20 ns;

  --     -- Make sure that BSET does not alter registers
  --     RegIn <= "00000000";
  --     IR    <= OpBSET;

  --     wait for 20 ns;

  --     -- Run CP such that we can read, but not alter registers
  --     temp_op  := OpCP;
  --     -- Use Register j as the RegAOut
  --     temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));
  --     IR    <= temp_op;

  --     wait for 18 ns;

  --     -- Make sure that "j" is stored as the value inside Register A (aka: j)
  --     assert(to_integer(unsigned(RegAOut)) = j) 
  --     report "OpBCLR/OpBSET overwrote value (or ADD/CP are broken)"
  --     severity ERROR;

  --     wait for 2 ns;

  --   end loop;

  --   -- Loop through the 4 possible register set inputs for ADIW/SBIW
  --   for j in 0 to 3 loop

  --     -- Load the two Clock command we are on
  --     temp_op := twoClocks(a);

  --     -- Set bits 5/4 based on j
  --     temp_op(5 downto 4) := std_logic_vector(to_unsigned(j, 2));
  --     IR <= temp_op;

  --     -- Store new value in register (j * 2)
  --     RegIn <= std_logic_vector(to_unsigned(j*2, RegIn'LENGTH));

  --     wait for 18 ns;

  --     -- Assert that we are reading the proper register
  --     assert(to_integer(unsigned(RegAOut)) = 24 + j*2)
  --     report "Output RegA on First clock of ADIW/SBIW out wrong"
  --     severity ERROR;

  --     wait for 2 ns;

  --     -- Store new value in register (j * 2) + 1
  --     RegIn <= std_logic_vector(to_unsigned(j*2 + 1, RegIn'LENGTH));

  --     wait for 18 ns;

  --     -- Assert that we are reading the proper register
  --     assert(to_integer(unsigned(RegAOut)) = 24 + j*2 + 1)
  --     report "Output RegA on Second clock of ADIW/SBIW out wrong"
  --     severity ERROR;

  --     wait for 2 ns;

  --     -- Now check that the command wrote to the registers properly

  --     -- Run CP such that we can read, but not alter registers
  --     temp_op  := OpCP;
  --     -- Get the appropriate register (low bit 0 on first clock)
  --     temp_op(8 downto 7) := "11";
  --     temp_op(6 downto 5) := std_logic_vector(to_unsigned(j, 2));
  --     temp_op(4)          := '0';
  --     IR    <= temp_op;

  --     wait for 18 ns;

  --     -- Make sure that "j*2" is stored as the value inside Register A
  --     assert(to_integer(unsigned(RegAOut)) = j*2) 
  --     report "First Write (on First clock) of ADIW/SBIW failed"
  --     severity ERROR;

  --     wait for 2 ns;

  --     -- Run CP such that we can read, but not alter registers
  --     temp_op  := OpCP;
  --     -- Get the appropriate register (low bit 1 on first clock)
  --     temp_op(8 downto 7) := "11";
  --     temp_op(6 downto 5) := std_logic_vector(to_unsigned(j, 2));
  --     temp_op(4)          := '1';
  --     IR    <= temp_op;

  --     wait for 18 ns;

  --     -- Make sure that "j*2" is stored as the value inside Register A
  --     assert(to_integer(unsigned(RegAOut)) = j*2 + 1) 
  --     report "Second Write (on Second clock) of ADIW/SBIW failed"
  --     severity ERROR;

  --     wait for 2 ns;

  --   end loop;
  -- end loop;

  -- -- Finished Simulation
  -- END_SIM <= TRUE;
  -- wait;
  end process;


  CLOCK_CLK : process

  begin

    -- this process generates a 20 ns period, 50% duty cycle clock

    -- only generate clock if still simulating

    if END_SIM = FALSE then
      CLK <= '0';
      wait for 10 ns;
    else
      wait;
    end if;

    if END_SIM = FALSE then
      CLK <= '1';
      wait for 10 ns;
    else
      wait;
    end if;

  end process;

end architecture ; -- TB_MEM_ARCH