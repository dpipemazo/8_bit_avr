----------------------------------------------------------------------------
--
--  Atmel AVR Memory Test Entity 
--
--  Iterates through all of the Load and Store commands, making sure that
--  each command operates as indended. The only command that isn't looped
--  through is LDI, which is used to setup registers before they are used,
--  and in being used so frequently, is also tested throughly in the test
--  bench. In addition, the MOV command is tested seperately, after all of 
--  the load and store commands. 
--
--  The largest loop is one that loads 11 different starting addresses for
--  all of the commands to start at, one of these cases are all zeroes, and
--  another is all ones, the rest are all random starting addresses.
--
--  Inside this loop we initialize either the X, Y, or Z registers based on
--  where we are in the array (the array is ordered by all X operands, then
--  Y operands, then Z, then all others that don't operate on registers).
--
--  We then write a random value to register 16, and if needed calculate
--  our new address line value.
--  (for load commands we do this so that we have a random value we overwrite)
--  (for store commands we do this so that we have a random value to read)
--
--  We then make sure the signals match what we expect based on the timing
--  diagrams.
--
--  Then if we are performing a load type op we check to make sure that we
--  wrote to the register. (if we didn't the old value will have been random
--  and we won't match)
--
--  Finally we loop this 4 times, in order to make sure that the address
--  is post or pre decrementing/incrementing properly based on the instruction
--
--  After this, we test MOV by storing a random value to register 16 and 17
--  And then we'll move register 17 into register 16 and check that register 16
--  has the value that was in register 17.
--  We repeat this 5 times, just for good measure 
--  (mostly to make sure the random values aren't the same...)
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
  -- All of the load store commands, including Push/Pop
  --

  constant loadStoreSize : integer := 21;

  type LOAD_STR_SIMPLE_OP is array (0 to loadStoreSize) of std_logic_vector(15 downto 0);

  constant loadStoreArray : LOAD_STR_SIMPLE_OP := (
      -- X Commands
      OpLDX,  -- Index: 0  - 
      OpSTX,  -- Index: 1  - 
      OpLDXI, -- Index: 2  - 
      OpSTXI, -- Index: 3  - 
      OpLDXD, -- Index: 4  - 
      OpSTXD, -- Index: 5  - 
      -- Y Commands
      OpLDYI, -- Index: 6  - 
      OpSTYI, -- Index: 7  - 
      OpLDYD, -- Index: 8  - 
      OpSTYD, -- Index: 9  - 
      OpLDDY, -- Index: 10 - 
      OpSTDY, -- Index: 11 - 
      -- Z Commands
      OpLDZI, -- Index: 12 - 
      OpSTZI, -- Index: 13 - 
      OpLDZD, -- Index: 14 - 
      OpSTZD, -- Index: 15 - 
      OpLDDZ, -- Index: 16 - 
      OpSTDZ, -- Index: 17 - 
      -- LDS / STS commands
      OpLDS,  -- Index: 18 -
      OpSTS,  -- Index: 19 -
      -- Push / POP commands
      OpPOP,  -- Index: 20 -
      OpPUSH  -- Index: 21 -
    );

  constant lastXCommand   : integer := 5;
  constant lastYCommand   : integer := 11;
  constant lastZCommand   : integer := 17;
  constant lastMemCommand : integer := 19;

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
  variable initialAddressToLoad : std_logic_vector(15 downto 0);

  -- Locally keep track of the stack pointer since we can't read it
  variable localSP       : std_logic_vector(15 downto 0);

  variable DBValue       : std_logic_vector(7 downto 0);


  variable Reg16Val      : std_logic_vector(7 downto 0);
  variable Reg17Val      : std_logic_vector(7 downto 0);

  variable displacement  : std_logic_vector(5 downto 0);

  variable registerToLoadInto : integer;

  begin

  DataDB <= (others => 'Z');
  ProgDB <= (others => 'Z');

  --
  -- First Reset signal
  --

  reset <= '0';

  -- We will locally keep track of the stack pointer since we can't read it
  -- On a reset the stack pointer goes to all 1's so we set our local copy
  localSP := (others => '1');

  wait for 20 ns;

  reset <= '1';

  -- Ofset our start such that we start 1 ns after a rising clock edge
  wait for 11 ns;

  --
  -- Check all of the Simple Load and store commands
  --

  -- Run all of the simple load and store commands with 9 random addresses, and 
  -- two addresses that are all 1's and all 0's
  for b in 0 to 10 loop


    UNIFORM(seed1, seed2, rand);                           -- generate random number
    randInt2 := INTEGER(TRUNC(rand*65536.0));                -- rescale to 0..256, find integer part
    AddressToLoad := std_logic_vector(to_unsigned(randInt2, AddressToLoad'LENGTH));  -- convert to std_logic_vector

    -- Test the edge cases where the address to load is all zeros and all 1's
    -- This will test our Pre-decrement and post-increment commands more thoroughly
    if b = 0 then
      AddressToLoad := (others => '0');
    elsif b = 1 then
      AddressToLoad := (others => '1');
    end if;

    -- Loop over the commands in loadStoreSize
    for a in 0 to loadStoreSize loop

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
        -- Means we don't care what we write to our registers XYZ
        -- Either LDS/STS, or PUSH/POP
      end if;

      -- Load a command that performs a write
      temp_op  := OpLDI;

      -- Use registerToLoadInto (-16 since it's an Immediate Opcode)
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(registerToLoadInto - 16, 4));
        
      temp_op(3 downto 0) := AddressToLoad(3 downto 0);
      temp_op(11 downto 8) := AddressToLoad(7 downto 4);

      IR    <= temp_op;

      wait for 20 ns;

      -- Load a command that performs a write
      temp_op  := OpLDI;

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
        DBValue := std_logic_vector(to_unsigned(randInt2, DBValue'LENGTH));  -- convert to std_logic_vector

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
        if (a mod 2) = 0 then
          -- Generate a value to be put on the DB
          UNIFORM(seed1, seed2, rand);                           -- generate random number
          randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..256, find integer part
          DBValue := std_logic_vector(to_unsigned(randInt2, DBValue'LENGTH));  -- convert to std_logic_vector
        end if;

        -- Load a command that performs a Load/Store
        temp_op  := loadStoreArray(a);

        -- Load/Store to Register 16
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(16, 5));
      
        -- Save the initial Address to load for the LDDZ, LDDY, STDY, STDZ, LDS, STS commands 
        initialAddressToLoad := AddressToLoad;

        -- If Push or Pop
        if (a = 20 or a = 21) then
          AddressToLoad := localSP;
        end if;

        -- If we are loading with unsigned displacement then calculate what our new address
        -- should be with the displacement, and pass a random displacement into the op
        if (a = 10 or a = 11 or a = 16 or a = 17) then
          -- Generate a value to be put on the DB
          UNIFORM(seed1, seed2, rand);                           -- generate random number
          randInt2 := INTEGER(TRUNC(rand*64.0));                 -- rescale to 0..64, find integer part
          displacement := std_logic_vector(to_unsigned(randInt2, displacement'LENGTH));  -- convert to std_logic_vector

          temp_op(2 downto 0) := displacement(2 downto 0);
          temp_op(11 downto 10) := displacement(4 downto 3);
          temp_op(13) := displacement(5);

          AddressToLoad := std_logic_vector(unsigned(AddressToLoad) + unsigned(displacement));

        end if; 

        IR <= temp_op;

        wait for 18 ns;

        -- We need to pre decrement if any of these values
        if (a = 4 or a = 5 or a = 8 or a = 9 or a = 14 or a = 15) then
          AddressToLoad := std_logic_vector(unsigned(AddressToLoad) - 1);
        end if;

        -- If a pop instruction, pre increment
        if (a = 20) then
          AddressToLoad := std_logic_vector(unsigned(AddressToLoad) + 1);
          localSP := AddressToLoad;
        end if;

        -- assert(AddressToLoad = DataAB)
        -- report "Address Bus not set Properly after 1st clock"
        -- severity ERROR;

        assert(DataRd = '1')
        report "Data Read not supposed to be active yet!"
        severity ERROR;

        assert(DataWr = '1')
        report "Data Write not supposed to be active yet!"
        severity ERROR;

        -- Put us 5 ns into a clock
        wait for 6 ns;

        -- If we are LDS or STS then wait an extra clock with "mmmm" on the
        -- Program Data Bus
        if (a = 18 or a = 19) then
          -- Load a random value into Address to Load to be used
          -- which is the "memory address" that we load on the ProgDB
          -- We then expect the address to be equal to this value
          UNIFORM(seed1, seed2, rand);                           -- generate random number
          randInt2 := INTEGER(TRUNC(rand*65536.0));               -- rescale to 0..65526, find integer part
          AddressToLoad := std_logic_vector(to_unsigned(randInt2, AddressToLoad'LENGTH));  -- convert to std_logic_vector
          ProgDB <= AddressToLoad;
        
          -- Wait for one full clock
          wait for 20 ns;

          -- Set the program Data bus back to high impedance
          ProgDB <= (others => 'Z');

        end if;

        -- If a store command then assert that DataDB = DBValue
        if (a mod 2) = 1 then

          assert(DataDB = DBValue)
          report "Data Bus not a valid value 5ns into second clock of Store command (LDI could also be broken)"
          severity ERROR;

        end if;

        wait for 6 ns;

        -- Check that DataRd or DataWr are low based on command
        if (a mod 2) = 1 then
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
        if (a mod 2) = 0 then

          DataDB <= DBValue;

        end if;

        wait for 4 ns;

        -- Check all of the values

        assert(AddressToLoad = DataAB)
        report "Address Bus not set Properly after 2nd clock"
        severity ERROR;

        wait for 2 ns;

        -- We are now 1 ns ahead of the clk again.

        DataDB <= (others => 'Z');

        -- If a load command, then we should check we wrote to the register
        -- Unfortunately, we have to do this by running a Store command, and
        -- checking if the value appears on the bus. 
        -- (which is also potetially buggy)
        if (a mod 2) = 0 then
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

        -- We need to post increment if a > 1 and 
        if (a = 2 or a = 3 or a = 6 or a = 7 or a = 12 or a = 13) then
          AddressToLoad := std_logic_vector(unsigned(AddressToLoad) + 1);
        end if;

        -- If a push instruction, post decrement
        if (a = 21) then
          AddressToLoad := std_logic_vector(unsigned(AddressToLoad) - 1);
          localSP := AddressToLoad;
        end if;

        -- Reset Address to load (since we don't alter it) for 
        -- displacement instructions and for LDS/STS instructions
        -- and Push/POP commands
        if (a = 10 or a = 11 or a = 16 or a = 17 or a = 18 or a = 19) then
          AddressToLoad := initialAddressToLoad;
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
    Reg17Val := std_logic_vector(to_unsigned(randInt2, Reg17Val'LENGTH));  -- convert to std_logic_vector

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
    Reg16Val := std_logic_vector(to_unsigned(randInt2, Reg16Val'LENGTH));  -- convert to std_logic_vector

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

  -- Finished Simulation
  END_SIM <= TRUE;
  wait;
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