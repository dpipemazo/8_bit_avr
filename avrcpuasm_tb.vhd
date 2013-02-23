---------------------------------------------------------
--
--  AVR CPU TEST BENCH
--
---------------------------------------------------------

--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.opcodes.all;
use work.avr_cpu;
use work.prog_memory;

entity AVRCPU_tb is
end AVRCPU_tb;

architecture  TB_AVR_CPU  of AVRCPU_tb is

    -- Component Declaration

    component  AVR_CPU
        port(
            ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
            Reset   :  in     std_logic;                       -- reset signal (active low)
            INT0    :  in     std_logic;                       -- interrupt signal (active low)
            INT1    :  in     std_logic;                       -- interrupt signal (active low)
            clock   :  in     std_logic;                       -- system clock
            ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
            DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
            DataWr  :  out    std_logic;                       -- data memory write enable (active low)
            DataRd  :  out    std_logic;                       -- data memory read enable (active low)
            DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
        ); 
    end  component;

    -- Signals
    signal ProgDB : std_logic_vector(15 downto 0);
    signal Reset  : std_logic;                    
    signal INT0   : std_logic;                    
    signal INT1   : std_logic;                    
    signal clock  : std_logic;                    
    signal ProgAB : std_logic_vector(15 downto 0);
    signal DataAB : std_logic_vector(15 downto 0);
    signal DataWr : std_logic;                    
    signal DataRd : std_logic;                    
    signal DataDB : std_logic_vector(7 downto 0);  
    --Signal used to stop clock signal generators
    signal  END_SIM     :  BOOLEAN := FALSE;

    -- Constants


    constant SimpleReadsSize : integer := 82;

    type SIMPLE_READS_TYPE is array (0 to SimpleReadsSize) of std_logic_vector(7 downto 0);

    constant SimpleReadsArray : SIMPLE_READS_TYPE := (
      X"FF", X"00", X"00", X"FF", X"FE", -- ADC command
      X"7D", X"FF", X"00", X"00",        -- ADD command
      X"45", X"7D", X"00", X"00",        -- ADIW command
      X"55", X"00", X"00",               -- AND command
      X"55", X"FF", X"AA",               -- ANDI command
      X"FF", X"00", X"38", X"D5",        -- ASR command
      X"00", X"FF", X"AA", X"55",        -- ST command
                                         -- CP, CPC, CPI have no results
      X"FF", X"7F", X"6F", X"FE",        -- DEC command
      X"FF", X"AA", X"AA", X"55", X"00", -- EOR command
      X"01", X"FF", X"00", X"80",        -- INC command
      X"7F", X"38", X"00", X"40",        -- LSR command
      X"01", X"00", X"80", X"82",        -- NEG command
      X"FF", X"FF", X"AA",               -- OR command
      X"FF", X"7D",                      -- ORI command
      X"FF", X"B8", X"00", X"C0", X"00", -- ROR command
      X"01", X"DF", X"7E",               -- SBC/SBCI
      X"80", X"7F", X"D0",               -- SBC/SBCI
      X"FD", X"FF", X"FD", X"FF",        -- SBIW command
      X"02", X"80", X"7F",               -- SUB command
      X"81", X"E0", X"D1",               -- SUBI command
      X"28", X"E7", X"00",               -- SWAP command
      X"80",X"00",X"55",X"D5",X"E0",X"D1"-- PUSH command
    );

    constant SimpleAddrSize : integer := 25;

    type SIMPLE_ADDR_TYPE is array (0 to SimpleAddrSize) of std_logic_vector(15 downto 0);

    constant SimpleAddrArray : SIMPLE_ADDR_TYPE := (
      X"5555", X"AAAA",                                     
      X"FFFF", X"FFFE", X"FFFE", X"FFFF", X"0000",   -- ST X
      X"FFC0", X"FFC1", X"FFC0", X"FFC0",            -- ST  Y
      X"FFFC", X"FFC2", X"FFD6", X"FFC1",            -- STD Y
      X"0080", X"0081", X"0080", X"0080",            -- ST  Z
      X"009E", X"0081", X"00BF", X"00A0",            -- STD Z
      X"FFDF", X"FFDF", X"001F"                      -- ST/STD Y
    );

    constant SimpleLoadSize : integer := 13;

    type SIMPLE_LOAD_TYPE is array (0 to SimpleLoadSize) of std_logic_vector(15 downto 0);

    constant SimpleLoadArray : SIMPLE_LOAD_TYPE := (
      X"AAAA", X"5555",                     -- LDS
      X"0000", X"FFFF", X"FFFF", X"0000",   -- LD
      X"FFFF", X"0000", X"FFFF", X"001D",   -- LD/LDD
      X"FFC0", X"FFC1", X"FFC0", X"FFFC"    -- LD/LDD
    );

    constant SimpleJmpSize : integer := 18;

    type SIMPLE_JMP_TYPE is array (0 to SimpleJmpSize) of std_logic_vector(7 downto 0);

    constant SimpleJmpArray : SIMPLE_JMP_TYPE := (
      X"A5",                      -- JMP
      X"5A",                      -- RJMP (Forwards)
      X"42",                      -- RJMP (Backwards)
      X"FF",                      -- IJMP
      X"A5",                      -- CALL
      X"A5",                      -- RCALL
      X"36",                      -- ICALL
      X"B0", X"B1", X"B2", X"B3", -- Branch Instructions
      X"B4", X"B5", X"B6", X"B7", -- Branch Instructions
      X"36",                      -- ICALL
      X"B8", X"B9",               -- More Branch Instructions
      X"42"                       -- Skip's all pass
    );

    begin

    UUT : AVR_CPU
        port map(
            ProgDB => ProgDB,
            Reset  => Reset,
            INT0   => INT0,
            INT1   => INT1,
            clock  => clock,
            ProgAB => ProgAB,
            DataAB => DataAB,
            DataWr => DataWr,
            DataRd => DataRd,
            DataDB => DataDB
    );

    ROM : entity PROG_MEMORY 
	     port map(
            ProgAB => ProgAB,
            Reset  => Reset,
            ProgDB => ProgDB
	 );

    -- Make the interrupt lines high
    INT0 <= '1';
    INT1 <= '1';

    --
    -- ACTUALLY TEST THE ASM CODE
    --
    do_test: process

    variable a : integer;

    begin

        --
        -- Reset the system, to get it into the correct state
        --
        ProgDB <= (others => 'Z');
		  DataAB <= (others => 'Z');
        reset <= '0';
        wait for 11 ns;
        reset <= '1';

        for a in 0 to SimpleReadsSize loop
            wait until (DataWr = '0');
            assert (DataDB = SimpleReadsArray(a))
            report "Failed a regular simple command"
            severity ERROR;
            wait for 12 ns;
        end loop;

        for a in 0 to SimpleAddrSize loop
            wait until (DataWr = '0');
            assert (DataAB = SimpleAddrArray(a))
            report "Failed a Store command"
            severity ERROR;
            wait for 12 ns;
        end loop;

        wait for  8 ns;

        wait for 120 ns;

        for a in 0 to SimpleLoadSize loop
            
            wait for 39 ns;
            if (a <= 1) then
                wait for 20 ns;
            end if;
            assert (DataAB = SimpleLoadArray(a))
            report "Failed a Load command"
            severity ERROR;

        end loop;

        for a in 0 to SimpleJmpSize loop
            wait until (DataWr = '0');
            assert (DataDB = SimpleJmpArray(a))
            report "Failed a jmp command"
            severity ERROR;
            wait for 12 ns;
        end loop;

    -- Finished Simulation
    END_SIM <= TRUE;
    wait;
    
    end process;

    -- MAKE THE CLOCK

    CLOCK_CLK : process
    begin

    -- this process generates a 20 ns period, 50% duty cycle clock
    -- only generate clock if still simulating

    if END_SIM = FALSE then
      clock <= '0';
      wait for 10 ns;
    else
      wait;
    end if;

    if END_SIM = FALSE then
      clock <= '1';
      wait for 10 ns;
    else
      wait;
    end if;

  end process;

end architecture;























