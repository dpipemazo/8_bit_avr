----------------------------------------------------------------------------
--
--  Atmel AVR Program Memory
--
--  This component describes a program for the AVR CPU.  It creates the 
--  program in a small (334 x 16) ROM.
--
--  Revision History:
--     11 May 00  Glen George       Initial revision (from 5/9/00 version of 
--                                  progmem.vhd).
--     28 Jul 00  Glen George       Added instructions and made memory return
--                                  NOP when not mapped.
--      7 Jun 02  Glen George       Updated commenting.
--     16 May 04  Glen George       Added more instructions for testing and
--                                  updated commenting.
--
----------------------------------------------------------------------------


--
--  PROG_MEMORY
--
--  This is the program memory component.  It is just a 306 word ROM with no
--  timing information.  It is meant to be connected to the AVR CPU.  The ROM
--  is always enabled and may be changed when Reset it active.
--
--  Inputs:
--    ProgAB - address bus (16 bits)
--    Reset  - system reset (active low)
--
--  Outputs:
--    ProgDB - program memory data bus (16 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity  PROG_MEMORY  is

    port (
        ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
        Reset   :  in   std_logic;                      -- system reset
        ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
    );

end  PROG_MEMORY;


architecture  ROM  of  PROG_MEMORY  is

    -- define the type for the ROM (an array)
    type  ROMtype  is array(0 to 458) of std_logic_vector(15 downto 0);

    -- define the actual ROM (initialized to a simple program)
    signal  ROMbits  :  ROMtype  :=  ( X"9488", X"94f8", X"94c8", X"94b8", 
                                       X"9498", X"94d8", X"94a8", X"94e8", 
                                       X"9448", X"9418", X"9478", X"9408", 
                                       X"9428", X"9468", X"9458", X"9438", 
                                       X"e000", X"2e00", X"2e10", X"2e20", 
                                       X"2e30", X"2e40", X"2e50", X"2e60", 
                                       X"2e70", X"f807", X"f813", X"f821", 
                                       X"f836", X"f840", X"f855", X"f864", 
                                       X"f872", X"ed4f", X"2e84", X"e044", 
                                       X"2e94", X"e74f", X"2ea4", X"e041", 
                                       X"2eb4", X"ef4d", X"2ec4", X"e440", 
                                       X"2ed4", X"ef47", X"2ee4", X"e140", 
                                       X"2ef4", X"fa85", X"fa92", X"faa7", 
                                       X"fab0", X"fac1", X"fad6", X"fae3", 
                                       X"faf4", X"ef0f", X"ef1f", X"e020", 
                                       X"e730", X"e040", X"e75e", X"e76e", 
                                       X"e870", X"e485", X"e890", X"efa0", 
                                       X"efbf", X"e5c5", X"eada", X"e7e0", 
                                       X"e3ff", X"1f01", X"930c", X"1f02", 
                                       X"930c", X"1f21", X"932c", X"1f95", 
                                       X"939c", X"1d60", X"936c", X"0f95", 
                                       X"939c", X"0f01", X"930c", X"0f24", 
                                       X"932c", X"0d70", X"937c", X"9600", 
                                       X"938c", X"939c", X"9650", X"93ac", 
                                       X"93bc", X"231c", X"931c", X"231d", 
                                       X"931c", X"220f", X"920c", X"f807", 
                                       X"7fcf", X"93cc", X"7f0f", X"930c", 
                                       X"7fdf", X"93dc", X"9505", X"930c", 
                                       X"95b5", X"93bc", X"95e5", X"93ec", 
                                       X"95d5", X"93dc", X"9500", X"930c", 
                                       X"9500", X"930c", X"95c0", X"93cc", 
                                       X"95c0", X"93cc", X"1710", X"175a", 
                                       X"15f3", X"0710", X"0751", X"0715", 
                                       X"0750", X"e4e0", X"e7ff", X"371f", 
                                       X"37e0", X"3af0", X"2fe3", X"951a", 
                                       X"931c", X"940a", X"920c", X"95ea", 
                                       X"93ec", X"951a", X"931c", X"2f1c", 
                                       X"271d", X"931c", X"271c", X"931c", 
                                       X"2721", X"932c", X"2720", X"932c", 
                                       X"2788", X"938c", X"9583", X"938c", 
                                       X"9563", X"937c", X"9563", X"937c", 
                                       X"9403", X"920c", X"95aa", X"e8f0", 
                                       X"95a6", X"93ac", X"95e6", X"93ec", 
                                       X"9546", X"934c", X"95f6", X"93fc", 
                                       X"9501", X"930c", X"9561", X"936c", 
                                       X"9401", X"920c", X"9551", X"935c", 
                                       X"2b21", X"932c", X"2b2c", X"932c", 
                                       X"2b61", X"936c", X"6f1f", X"931c", 
                                       X"679d", X"939c", X"9517", X"931c", 
                                       X"9537", X"933c", X"9507", X"930c", 
                                       X"9407", X"920c", X"9507", X"930c", 
                                       X"e5f0", X"0b01", X"930c", X"47f0", 
                                       X"93fc", X"0aa4", X"92ac", X"e79f", 
                                       X"e781", X"47af", X"93ac", X"0b91", 
                                       X"939c", X"4a80", X"938c", X"e08d", 
                                       X"e090", X"9740", X"938c", X"939c", 
                                       X"9700", X"938c", X"939c", X"e7ef", 
                                       X"e7ff", X"1b01", X"930c", X"1be1", 
                                       X"93ec", X"1bf4", X"93fc", X"e5e0", 
                                       X"e7f1", X"574f", X"934c", X"57e0", 
                                       X"93ec", X"5af0", X"93fc", X"9552", 
                                       X"935c", X"94a2", X"92ac", X"95b2", 
                                       X"93bc", X"93af", X"93bf", X"93cf", 
                                       X"93df", X"93ef", X"93ff", X"900f", 
                                       X"901f", X"efbf", X"efaf", X"efdf", 
                                       X"ecc0", X"e0f0", X"e8e0", X"9200", 
                                       X"5555", X"9210", X"aaaa", X"922c", 
                                       X"923e", X"924d", X"925d", X"926c", 
                                       X"9279", X"8288", X"929a", X"82a8", 
                                       X"aebc", X"82ca", X"8ade", X"82e9", 
                                       X"92f1", X"8300", X"9312", X"8320", 
                                       X"8f36", X"8341", X"af57", X"a360", 
                                       X"efdf", X"eec0", X"937a", X"9389", 
                                       X"af9f", X"e0b0", X"e0a0", X"efdf", 
                                       X"efcf", X"efff", X"ece0", X"9000", 
                                       X"aaaa", X"9010", X"5555", X"907c", 
                                       X"909e", X"914d", X"915c", X"9069", 
                                       X"8178", X"916a", X"8cfe", X"9041", 
                                       X"80d0", X"9022", X"ad14", X"940c", 
                                       X"0141", X"e56a", X"e57a", X"937c", 
                                       X"c003", X"ea85", X"938c", X"cff9", 
                                       X"e4ec", X"e0f1", X"e402", X"930c", 
                                       X"9409", X"e0b0", X"e0c0", X"930c", 
                                       X"ef0f", X"930c", X"940e", X"01bf", 
                                       X"d06e", X"ebef", X"e0f1", X"9509", 
                                       X"17cb", X"f020", X"ef01", X"930c", 
                                       X"940c", X"0154", X"eb00", X"930c", 
                                       X"f3bc", X"eb01", X"930c", X"f3a1", 
                                       X"eb02", X"930c", X"f421", X"ef02", 
                                       X"930c", X"940c", X"0154", X"eb03", 
                                       X"930c", X"e659", X"0f55", X"f745", 
                                       X"eb04", X"930c", X"2bbb", X"f022", 
                                       X"ef03", X"930c", X"940c", X"0154", 
                                       X"eb05", X"930c", X"2bcc", X"f2e2", 
                                       X"f422", X"ef04", X"930c", X"940c", 
                                       X"0154", X"eb06", X"930c", X"2bbb", 
                                       X"f69a", X"1bcb", X"f023", X"ef05", 
                                       X"930c", X"940c", X"0154", X"eb07", 
                                       X"930c", X"95ca", X"f64b", X"30b1", 
                                       X"f63c", X"94f8", X"f22f", X"940e", 
                                       X"01c5", X"f617", X"fbe1", X"f606", 
                                       X"fbe3", X"f38e", X"0fee", X"f410", 
                                       X"940c", X"0154", X"eb08", X"930c", 
                                       X"0fee", X"f748", X"f025", X"ef06", 
                                       X"930c", X"940c", X"0154", X"eb09", 
                                       X"930c", X"1367", X"cffc", X"1367", 
                                       X"940c", X"01a3", X"1368", X"e860", 
                                       X"fd66", X"ef6f", X"fd63", X"940c", 
                                       X"01a3", X"fd67", X"ea65", X"ff60", 
                                       X"e060", X"ff65", X"940c", X"01a3", 
                                       X"ff61", X"940c", X"01bb", X"e402", 
                                       X"930c", X"940c", X"0000", X"efbf", 
                                       X"e7cf", X"e0d0", X"ea05", X"930c", 
                                       X"9508", X"ef9f", X"e7af", X"e6e6", 
                                       X"e306", X"930c", X"9518" );


begin


    -- the address has changed - read the new value
    ProgDB <= ROMbits(CONV_INTEGER(ProgAB)) when (CONV_INTEGER(ProgAB) <= ROMbits'high)  else
              X"E0C0";


    -- process to handle Reset
    process(Reset)
    begin

        -- check if Reset is low now
        if  (Reset = '0')  then
            -- reset is active - initialize the ROM (nothing for now)
        end if;

    end process;

end  ROM;
