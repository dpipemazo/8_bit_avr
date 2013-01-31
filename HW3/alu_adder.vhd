----------------------------------------------------------------------------
--
--  n-Bit Subtracter in VHDL
--
--  This is an implementation of an n-bit adder in VHDL.  It uses a dataflow
--  type architecture with a full adder component.  The n-bit adder is a
--  generic model (parameterized on n).
--
--  Entities included are:
--     FullAdder - full adder
--     Subtracter     - n-bit subtracter
--
--  Revision History:
--     16 Apr 98  Glen George       Initial revision.
--     10 May 99  Glen George       Fixed generic size on Adder (off by 1).
--      7 Nov 99  Glen George       Updated formatting.
--      4 Nov 05  Glen George       Updated commenting.
--      22 Jan 13 Dan Pipe-Mazo     Updated for Subtracter.
--                                  Updated to use std_logic instead of bit.
--      30 Jan 13 Dan Pipe-Mazo     Updated to do add/subtract/carry operations
--                                  modified for 8-bit width
--
----------------------------------------------------------------------------
--
-- Imports
--
library ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;

--
--  FullAdder entity declaration (used in n-bit adder)
--

entity  FullAdder  is

    port (
        A, B  :  in  std_logic;       --  addends
        Cin   :  in  std_logic;       --  carry in input
        Sum   :  out  std_logic;      --  sum output
        Cout  :  out  std_logic       --  carry out output
    );

end  FullAdder;


--
--  FullAdder dataflow architecture
--

architecture  dataflow  of  FullAdder  is
begin

    Sum <= A xor B xor Cin;
    Cout <= (A and B) or (A and Cin) or (B and Cin);

end  dataflow;


--
--  n-Bit Subtracter
--      parameter (bitsize) is the number of bits in the adder
--

--
-- Imports
--
library ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;

entity  alu_adder  is

    generic (
        bitsize : integer := 8      -- default width is 8-bits
    );

    -- Note: We do not need a carry in on this entity, since it will be used
    --  strictly as a subtracter.
    port (
        Ci   : in std_logic;       -- carry vector
        sub  :  in std_logic;      -- Add or subtract
        useCarry:  in std_logic;   -- Use the carry or not
        A, B :  in  std_logic_vector((bitsize - 1) downto 0);     -- Performing X - Y
        S    :  out  std_logic_vector((bitsize - 1) downto 0);    -- sum out
        Co   :  out  std_logic                                    -- carry out

    );

end  alu_adder;


architecture  dataflow  of  alu_adder  is

    component  FullAdder
        port (
            A, B  :  in  std_logic;       --  inputs
            Cin   :  in  std_logic;       --  carry in input
            Sum   :  out  std_logic;      --  sum output
            Cout  :  out  std_logic       --  carry out output
        );
    end  component;

    signal  carry : std_logic_vector(bitsize downto 0);   -- intermediate carries
    signal  internal_Y : std_logic_vector( (bitsize - 1) downto 0);

begin

    -- Figure out what our carry in is
    --  The following cases are possible:
    --  1. Subtract and do not use carry -> 1
    --  2. Subtract and use carry -> not Ci
    --  3. Add and do not use carry -> 0
    --  4. Add and use carry -> Ci

    carry(0) <= sub xor (Ci and useCarry);  

    -- Now figure out if we need to invert the Y signal to do 
    --  subtraction. 
                      
    internal_B <= not B when (sub = '1') else
    internal_B <= B;

    -- Now that we have inverted the carry in and inverted the value being
    --  subtracted, we can just treat this as a string of full adders. 

    Adders:  for i in  A'range  generate    -- generate bitsize full adders
    begin

        FAx: FullAdder  port map  (A(i), internal_B(i), carry(i), S(i), carry(i + 1));

    end generate;

    -- Finally, invert the carry out if we are subtracting
    Co <= carry(carry'high);        

end  dataflow;